(ns kurtharriger.clj-mergetool.util.simulator
  (:require [babashka.fs :as fs]
            [babashka.process :refer [sh]]))

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
    (git repo "git config merge.conflictStyle diff3")
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
      (let [error (try
                    (merge! repo "right")
                    (catch Exception e
                      e))]
        {:conflict (some? error)
         :content (read-content repo)})
      (finally
        (read-content repo)
        (delete! repo)))))

(defn example [n]
  {:base  (slurp (str "test-resources/examples/ex" n "/base.clj"))
   :left  (slurp (str "test-resources/examples/ex" n "/left.clj"))
   :right (slurp (str "test-resources/examples/ex" n "/right.clj"))})

(defn merge-result-example [n]
  (let [{:keys [base left right]} (example n)]
    (merge-result base left right)))


(comment
  ;; instead of storing base right left can use diff3 config option to create a
  ;; single file from which the base, left, and right can be extracted
  ;; this might make it easier to use clj-mergetool on 'as needed' to
  ;; try to resolve conflicts until its robust enough to use as an acutal merge tool
  ;; another option when running clj-mergetool in a git repo is
  ;; to use git show :1:example.clj :2:example.clj :3:example.clj
  ;; to pull the base, left, and right files into temp files rather
  ;; than configuring as a merge tool
  ;; diff3 conflict files are nice for when diff isn't as expected and
  ;; someone wants to file an issue and attach the conflict file
  ;; but git :1:example.clj :2:example.clj :3:example.clj is probably
  ;; the best way to use the tool as needed in a repo
  (for [i (range 1 4)
        :let [conflict-file (str "test-resources/examples/ex" i "/conflict.clj")]]

    (spit conflict-file (:content (merge-result-example i))))



  ;;
  )
