(ns kurtharriger.notebook.notes005
  (:require [kurtharriger.clj-mergetool :refer :all]
            [portal.api :as portal]
            [editscript.core :as e]
            [rewrite-clj.parser :as p]
            [rewrite-clj.node :as n]
            [clojure.edn :as edn]))


(comment
  (def ctx (init :merge
                 "test/kurtharriger/examples/ex3/base.clj"
                 "test/kurtharriger/examples/ex3/left.clj"
                 "test/kurtharriger/examples/ex3/right.clj"))

  (-> ctx
      (dissoc :output-file)
      #_(assoc :output-file "/dev/stdout")
      (parse)
      (diff)
      (mergetool)
      (output))

  (def conflict
    (reduce e/combine  [(e/diff '(add 1 2) '(add 1 3))
                        (e/diff '(add 1 2) '(add 1 4))]))

  (type conflict)
  (type (e/get-edits conflict))
  (group-by first (e/get-edits conflict))




  (def ctx {:op :diff,
            :output-file
            "/Users/kurtharriger/dev/personal/mergetool/kurtharriger.clj-mergetool/ex/base.clj",
            :base
            {:filename
             "/Users/kurtharriger/dev/personal/mergetool/kurtharriger.clj-mergetool/ex/base.clj"},
            :left
            {:filename
             "/Users/kurtharriger/dev/personal/mergetool/kurtharriger.clj-mergetool/ex/left.clj"},
            :right
            {:filename
             nil}})

  (-> ctx
      (resolve-input)
      (parse)
      (diff)
      (mergetool)
      (output)
      :exit-code))