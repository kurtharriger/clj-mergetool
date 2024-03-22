(ns kurtharriger.notebook02.tests01
  (:require [clojure.java.io :as io]
            [editscript.core :as e]
            [rewrite-clj.parser :as p]
            [rewrite-clj.zip :as z]
            [rewrite-clj.node :as n]))

(comment
 ;; clojure reader does not preserve ordering
 ;; of elements in sets or maps as they are
 ;; irrelvant to the semantics of the data
 ;; however we want to operate on the textual
 ;; representation of the code with desire
 ;; to preserve existing order and whitespace
 ;; where it makes sense thus we read the test
 ;; cases using rewrite-clj which preserves order
 ;; and whitespace
  (pr-str #{:a :b}) ;; => "#{:b :a}"
  (pr-str #{:b :a}) ;; => #{:b :a}
  (z/string (z/of-string "#{:a :b}")) ;; => "#{:a :b}"
  (z/string (z/of-string "#{:b :a}")) ;; => "#{:b :a}"
  )


(defrecord TestCase [pre post])
(def tests
  (->
   (p/parse-file-all "dev/kurtharriger/notebook02/tests.edn")
   (n/children)

   ;; skip comments and whitespace in test data file
   (->> (filter n/sexpr-able?)
        (partition 2)
        ;(map (partial map n/string))
        (mapv #(apply ->TestCase %)))))

(defn test-str [{:keys [pre post]}]
  (str (n/string pre) " " (n/string post)))

(defn test-strs [{:keys [pre post]}]
  [(n/string pre)  (n/string post)])

(defn test-exprs [{:keys [pre post]}]
  [(n/sexpr pre) (n/sexpr post)])

(defn test-expr-str [{:keys [pre post]}]
  (str (pr-str (n/sexpr pre)) " " (pr-str (n/sexpr post))))



(comment
  ;; the following tests are logically equal
  ;; but not textually equal
  (->> tests
       (map (juxt  test-expr-str test-str))
       (filter  (fn [[expr-str test-str]] (not= test-str expr-str)))))


(defmethod print-method TestCase [{:keys [pre post]} writer]
  (print-method {:pre (n/string pre) :post (n/string post)}
                writer))

