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
        right (fs/file source-dir "file.other.clj")
        repo (s/init-repo :dir sim-dir)]
    (s/prepare-repo! repo [:file base] [:file left] [:file right])
    (s/merge! repo "right"))

  (mt/show-diff {:opts {:dir "local/test/cljc"}})


  :rcf)






(deftest test-unmerged-files-from-ls-files
  (is (= (mt/unmerged-files-from-ls-files "")  nil))
  (is (= (mt/unmerged-files-from-ls-files
          "100644 458a9013fad8c84f9112d9e19ce28953dc3f8b7d 1	src/clj/fluree/db/conn/ipfs.cljc
100644 0b03604ddb8ea04cc0879cc788bab309cb8fd6a7 2	src/clj/fluree/db/conn/ipfs.cljc
100644 5ff1fa7f0411c45e702e21f03ecf6ba967567a5e 3	src/clj/fluree/db/conn/ipfs.cljc
100644 ff666fe2b876b0a6f0f949ec36885f99853d645a 1	src/fluree/db/json_ld/commit.cljc
100644 4c8fe116636b936c4c6346f80315f8b7a0bae2fe 3	src/fluree/db/json_ld/commit.cljc"
          )
         ["src/clj/fluree/db/conn/ipfs.cljc"]))

  )
