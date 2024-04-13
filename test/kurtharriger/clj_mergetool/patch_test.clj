(ns kurtharriger.clj-mergetool.patch-test
  (:require
   [clojure.string :as string]
   [clojure.walk :as walk]
   [editscript.core :as e]
   [rewrite-clj.node :as n]
   [rewrite-clj.node.protocols :as nodep]
   [rewrite-clj.parser :as p]
   [rewrite-clj.zip :as z]
   [clojure.test :refer :all]
   [kurtharriger.clj-mergetool.patch :as patch :refer [focus zipper patch diff]]
   [rewrite-clj.node.protocols :as node]))

(deftest test-focus
  (are [node path f v] (= (f (focus (zipper node) path)) v)
    nil [] z/tag :token
    []  [] z/tag :vector
    [{}] [0] z/tag :map
    {:a []} [:a] z/tag :vector

    [] [] z/sexpr []
    [:a] [0] z/sexpr :a
    #{:a :b} [:a] z/sexpr :a

    {:a [{42 [:d]}]} [:a 0 42 0] z/sexpr :d))


(comment
  ;; clojure reader does not preserve ordering
  ;; of elements in sets or maps as they are
  ;; irrelvant to the semantics of the data
  ;; however when working with textual representation
  ;; of source code we still want to preserve the
  ;; existing order however arbitrary it may be
  ;; rewrite-clj preserves order and whitespace
  ;; that is otherwise discared by edn reader
  (pr-str #{:a :b}) ;; => "#{:b :a}"
  (pr-str #{:b :a}) ;; => #{:b :a}
  (z/string (z/of-string "#{:a :b}")) ;; => "#{:a :b}"
  (z/string (z/of-string "#{:b :a}")) ;; => "#{:b :a}"
  )

(defrecord TestCase [pre post])

(defn read-tests []
  (->
   (p/parse-file-all "test/kurtharriger/clj_mergetool/patch_tests.edn")
   (n/children)

   ;; skip comments and whitespace in test data file
   (->> (filter n/sexpr-able?)
        (partition 2)
        (mapv #(apply ->TestCase %)))))

(defn editscript [{:keys [pre post] :as test-case}]
  (diff (n/sexpr pre) (n/sexpr post)))

(comment
  (clojure.pprint/print-table
   (for [test (read-tests)]
     (assoc test :editscript (pr-str (editscript test)))))
  ;
  )

(comment
  (clojure.pprint/print-table
   (for [{:keys [pre post] :as test} (read-tests)
         :let [editscript  (try (editscript test) (catch Exception e))
               patched (try (patch  pre editscript) (catch Exception e))]]
     {:pre pre
      :post post
      :editscript (e/get-edits editscript)
      :patched patched
      :equal? (= (n/string patched) (n/string post))
      :sexpr=? (= (n/sexpr patched) (n/sexpr post))}))

  :rcf)

(deftest test-patch-sexpr
  (doseq [{:keys [pre post] :as test} (read-tests)]
    (is  (= (n/sexpr post) (n/sexpr (patch (zipper pre) (editscript test)))))))

(comment
  ;; todo: in future figure out diff representation
  ;; that can reconstruct sets and maps entries in
  ;; same order as they appear in the source code
  (deftest test-patch-str
    (doseq [{:keys [pre post] :as test} (read-tests)]
      (is  (= (n/string post) (n/string (patch (zipper pre) (editscript test))))))))


(comment
  (-> (p/parse-file-all "test-resources/examples/ex3/right.clj")
      (z/of-node)
      (z/down)
      (z/right)
      (z/down)
      (z/right)
      (z/right)
      (z/right)
      (z/right)
      (z/node)
      (n/string))

  (let [base (p/parse-file-all "test-resources/examples/ex3/base.clj")
        right (p/parse-file-all "test-resources/examples/ex3/right.clj")]
    (-> (diff base right)
        (e/get-edits)
        (first)
        (last)
        (n/string)))

  (let [base (p/parse-file-all "test-resources/examples/ex3/base.clj")
        right (p/parse-file-all "test-resources/examples/ex3/right.clj")
        editscript (diff base right)]
    (-> (patch base editscript)
        (n/string)
        (println)))

  (let [base (p/parse-file-all "test-resources/examples/ex3/base.clj")
        right (p/parse-file-all "test-resources/examples/ex3/right.clj")
        editscript (diff base right)]
    (-> editscript
        (e/get-edits)
        (->> (map last)
             (first))
        meta
        :key-node
        meta))

  :rcf)
