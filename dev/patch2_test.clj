(ns patch2-test
  (:require
   [editscript.core :as e]
   [rewrite-clj.node :as n]
   [rewrite-clj.parser :as p]
   [rewrite-clj.zip :as z]
   [clojure.test :refer :all]
   [patch2 :refer
    [focus zipper patch]]))

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
  (run-test test-focus)
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
  (e/diff (n/sexpr pre) (n/sexpr post)))

(comment
  (clojure.pprint/print-table
   (for [test (read-tests)]
     (assoc test :editscript (pr-str (editscript test)))))
  ;
  )

(comment
  (let [{:keys [pre post] :as test} (first (read-tests))]
    (pr pre post))

  (doseq [{:keys [pre post] :as test} (drop 42 (read-tests))]
    (pr pre post)
    (assert (= (n/sexpr post) (n/sexpr (patch (n/coerce pre) (n/coerce post))))
            (pr test))))

#_(deftest test-patch-sexpr
    (doseq [{:keys [pre post] :as test} (take 20 (read-tests))]
      (is  (= (n/sexpr post) (n/sexpr (:anode (patch (n/coerce pre) (n/coerce post))))))))


(comment
  (run-tests)
  ;; todo: in future figure out diff representation
  ;; that can reconstruct sets and maps entries in
  ;; same order as they appear in the source code
  (deftest test-patch-str
    (doseq [{:keys [pre post] :as test} (read-tests)]
      (is  (= (n/string post) (n/string (patch pre post)))))))
;; => nil
