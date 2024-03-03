(ns kurtharriger.util.simulator
  (:require [clojure.string :as str :refer [triml]]
            [babashka.fs :as fs]
            [babashka.process :refer [process shell sh]]
            [editscript.core :as e]
            [clojure.tools.reader.edn :as edn]
            [rewrite-clj.zip :as z]))

(defn git [{:keys [dir print?] :as repo} & args]
  {:pre [(string? dir)]}
  (when print? (apply println args))
  (let [r (apply sh {:dir dir} args)]
    (when print? (println (:out r)))
    (when (pos? (:exit r))
      (when print? (println (:err r)))
      (throw (ex-info "git command failed" r)))
    r))

(defn init-repo [& {:keys [print? delete] :as opts :or {delete true}}]
  (let [dir (str (fs/create-temp-dir {:prefix "clj-mergetool-"}))
        repo {:dir dir
              :print? print?
              :delete delete}]
    (when print? (println "cd" dir))
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

(defn delete! [{:keys [dir delete] :as repo}]
  (when (and dir delete)
    (fs/delete-tree dir)))

(defn read-content [{:keys [dir] :as repo}]
  (slurp (fs/file dir "example.clj")))

(defn prepare-merge! [repo base left right]
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

(defn merge-result [base left right]
  (let [repo (init-repo)]
    (try
      (prepare-merge! repo base left right)
      (try
        (merge! repo "right")
        [:success (read-content repo)]
        (catch Exception _
          [:conflict (read-content repo)]))
      (finally
        (read-content repo)
        (delete! repo)))))

(defn example [n]
  {:base  (slurp (str "test/kurtharriger/util/examples/ex" n "/base.clj"))
   :left  (slurp (str "test/kurtharriger/util/examples/ex" n "/left.clj"))
   :right (slurp (str "test/kurtharriger/util/examples/ex" n "/right.clj"))})

(defn merge-result-example [n]
  (let [{:keys [base left right]} (example n)]
    (merge-result base left right)))

(comment
  (println (merge-result-example 1))
  (println (merge-result-example 2))

  #_end_comment)

(comment
;; sorta works, edn only seems to read first form and whitespace is not preserved

  (let [{:keys [base left right]} (example 1)
        base (edn/read-string base)
        left (edn/read-string left)
        right (edn/read-string right)
        left-diff (e/diff base left)
        right-diff (e/diff base right)

        full-diff (e/combine left-diff right-diff)

        result (e/patch base full-diff)]
    (prn result))


  #_end_comment)
