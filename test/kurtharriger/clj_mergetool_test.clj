(ns kurtharriger.clj-mergetool-test
  (:require
   [clojure.pprint :as pprint]
   [clojure.test :refer :all]
   [editscript.core :as e]
   [rewrite-clj.node :as n]

   [kurtharriger.clj-mergetool.util.simulator :as simulator]
   [kurtharriger.clj-mergetool :refer :all]
   [rewrite-clj.parser :as p]))



(comment
  (-> (init :merge
            "test/kurtharriger/examples/ex3/base.clj"
            "test/kurtharriger/examples/ex3/left.clj"
            "test/kurtharriger/examples/ex3/right.clj")
      (resolve-input)
      (parse)
      (diff)
      (mergetool)
      (output)))



