(ns kurtharriger.clj-mergetool.util.simulator
  (:require [babashka.fs :as fs]
            [babashka.process :refer [sh]]
            [meander.epsilon :as m]))

(defn git [{:keys [dir throw? log]} & args]
  {:pre [(string? dir)]}
  (log :run args)
  (let [{:keys [out err exit] :as r} (apply sh {:dir dir} args)]
    (log :out out)
    (when (pos? exit)
      (log :err err)
      (when throw? (throw (ex-info "git command failed" r))))
    r))

(defn init-repo [& {:keys [print? dir] :as opts}]
  (let [[tmp-dir? dir] (if dir [false dir] [true (str (fs/create-temp-dir {:prefix "clj-mergetool-"}))])
        log (fn [& args] (when print? (apply println args)))
        repo {:dir dir
              :tmp-dir? tmp-dir?
              :log log}]
    (when (fs/exists? dir) (fs/delete-tree dir))
    (fs/create-dirs dir)
    (log :cd "cd" dir)
    (git repo "git init")
    (git repo "git config merge.conflictStyle diff3")
    repo))

(defn branch! [repo branch]
  (git repo "git checkout -b" branch))

(defn write! [{:keys [dir] :as repo} content]
  (let [content (m/match content
                  [:file ?filename] (slurp (str ?filename))
                  ?string ?string)]
    (spit (fs/file dir "example.clj") content)))

(defn commit! [{:keys [dir] :as repo} msg]
  (git repo "git add .")
  (git repo "git commit -a -m" msg))

(defn checkout! [{:keys [dir] :as repo} branch]
  (git repo "git checkout" branch))

(defn merge! [{:keys [dir] :as repo} branch]
  (git (assoc repo :throw? false) "git merge" branch))

(defn delete! [{:keys [dir tmp-dir?] :as repo}]
  (when (and dir tmp-dir?)
    (fs/delete-tree dir)))

(defn read-content [{:keys [dir] :as repo}]
  (slurp (fs/file dir "example.clj")))

(defn prepare-repo! [repo base left right]
  (-> (doto repo
        (write! base)
        (commit! "base")
        (branch! "left")
        (write! left)
        (commit! "left")
        (checkout! "main")
        (branch! "right")
        (write! right)
        (commit! "right")
        (checkout! "main")
        (git "git diff left")
        (git "git diff right")
        (merge! "left"))))

