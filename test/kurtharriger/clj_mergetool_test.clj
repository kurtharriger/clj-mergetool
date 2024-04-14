(ns kurtharriger.clj-mergetool-test
  (:require
   [babashka.fs :as fs]
   [clojure.test :refer :all]
   [kurtharriger.clj-mergetool :as mt]
   [kurtharriger.clj-mergetool.util.simulator :as s]))



(comment
  (defn test-repo [n]
    (let [sim-dir (str "local/test/" n)]
      (s/prepare-repo! (s/init-repo :dir sim-dir)
                       [:file (str "test-resources/examples/ex" n "/base.clj")]
                       [:file (str "test-resources/examples/ex" n "/left.clj")]
                       [:file (str "test-resources/examples/ex" n "/right.clj")])))

  (test-repo 4)

  :rcf)

(comment
  (let [sim-dir "local/test/cljc"
        source-dir "local/cljc"
        base (fs/file source-dir "file.base.clj")
        left (fs/file source-dir "file.current.clj")
        right (fs/file source-dir "file.other.clj")]
    (s/prepare-repo! (s/init-repo :dir sim-dir) [:file base] [:file left] [:file right]))

  :rcf)





