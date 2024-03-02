(ns kurtharriger.util.simulator
  (:require [clojure.string :as str :refer [triml]]
            [babashka.fs :as fs]
            [babashka.process :refer [process shell sh]]))

(defn git [{:keys [dir] :as repo} & args]
  {:pre [(string? dir)]}

  (apply println args)
 (println "--------------------")
  (let [r (apply sh {:dir dir} args)]
    (println (:out r))
    (when (pos? (:exit r))
      (println (:err r))
      (throw (ex-info "git command failed" r)))
    r)
  (println "===================="))

(defn init-repo []
  (let [dir (str (fs/create-temp-dir {:prefix "clj-mergetool-"}))
        repo {:dir dir}]
    (println "cd" dir)
    (git repo "git init")
    repo))

(defn branch! [{:keys [dir] :as repo} branch]
  (git repo "git checkout -b" branch))

(defn write! [{:keys [dir] :as repo} content]
  (spit (fs/file dir "example.clj") content))

(defn commit! [{:keys [dir] :as repo} msg]
  (git repo "git add .")
  (git repo "git commit -a -m" msg))

(defn checkout! [{:keys [dir] :as repo} branch]
  (git repo "git checkout" branch))

(defn merge! [{:keys [dir] :as repo} branch]
  (git repo "git merge" branch))

(defn delete! [{:keys [dir] :as repo}]
  (when dir
    #_(fs/delete-tree dir)))

(defn print-content [{:keys [dir] :as repo}]
  (println (slurp (fs/file dir "example.clj"))))

(defn merge-result [base left right]
  (let [repo (init-repo)]
    (try
      (-> (doto repo
            (write! base)
            (commit! "base")
            (branch! "left")
            (write! left)
            (commit! "left")
            (branch! "right")
            (write! right)
            (commit! "right")
            (checkout! "main")
            (git "git diff left")
            (git "git diff right")
            (merge! "left")
            (merge! "right"))
          (print-content))

      (finally
        (delete! repo)))))

(defn stripl [s]
  (-> s
      (str/split-lines)
      (->> (map str/triml)
           (str/join "\n"))))

(comment
  (merge-result (stripl "The common ancestor version.\n
                 item 1\n
                 item 2\n
                 item 3\n"
                        )

                (stripl "The left version.\n\n
                 item 0\n
                 item 1\n
                 item 2\n")

                (stripl "The right version.\n
                 item 0\n
                 item 3\n
                 item 2\n"))

;; end comment
  )
