(ns kurtharriger.notebook02.notes02
  (:require [clojure.test :as test]
            [clojure.pprint :as pprint]
            [rewrite-clj.zip :as z]
            [rewrite-clj.node :as n]
            [rewrite-clj.parser :as p]
            [rewrite-clj.node.protocols :as protocols]
            [editscript.core :as e]
            [editscript.edit :refer [IEditScript]]
            [clojure.string :as str]
            [meander.epsilon :as m]
            [kurtharriger.notebook02.tests01 :as t]
            [kurtharriger.notebook02.zip01
             :refer [zipper children root move-to-child focus]]))


(defmulti add-child (fn [zipper key value] (z/tag zipper)))
(defmulti remove-child (fn [zipper key] (z/tag zipper)))



(comment
  ;; note: the only operation expected at the root is replace (:r)
  ;; if the root is a set, map or vector the editscript
  ;; the editscript to add or remove would include the key
  ;; or index as last node in path which is a bit unintuitive
  ;; to me as the focus of the zipper needs to be (butlast path)
  ;; with (last path) indicating the key or index where value is
  ;; to be inserted and the logic depends on the type set, map, vec
  ;; but a replace operation however occurs at the full path
  ;; and does not care about the type
  (e/diff [] [:a]) ;; => [[[] :r [:a]]]
  (e/diff [:a] []) ;; => [[[] :r []]]

  (e/diff [:a] [:a :b]) ;; => [[[1] :+ :b]]
  (e/diff {:a :A} {:a :A :b :B}) ;; => [[[:b] :+ :B]]
  (e/diff #{:a} #{:a :b}) ;; => [[[:b] :+ :b]]

  ;; replace
  (e/diff [#{:a}] [{}]) ;; => [[[0] :r {}]]
  )

#_(defn patch*  [zipper [path op & op-args :as edit]]
    (zip/root
     (case op
       :r (let [[value] op-args]
            (z/replace     (focus path) value))
       :+ (let [[key value] op-args]
            (add-child     (focus (butlast path) key value)))
       :- (let [[key] op-args]
            (remove-child  (focus (butlast path) key))))))

(defn patch*  [zipper edit]
  (root
   (m/match edit
     [?path :r ?value] (z/replace    (focus zipper ?path) ?value)
     [?path :+ ?value] (add-child    (focus zipper (butlast ?path)) (last ?path) ?value)
     [?path :-]        (remove-child (focus zipper (butlast ?path)) (last ?path)))))

(->  nil
     (zipper)
     (patch* [[] :r #{:a}])
     (root)
     (z/sexpr))

(->  #{:a}
     (zipper)
     (patch* [[] :r #{:b}])
     (z/sexpr))


(defmethod add-child :map [zipper key value]
  (-> zipper
      (z/append-child key)
      (z/append-child value)))


(defmethod add-child :set [zipper key value]
  (assert (= key value))
  (-> zipper
      (z/append-child value)))


(defmethod add-child :default [zipper index value]
  (assert (#{:list :vector} (z/tag zipper)) (str "unsupported add-child tag " (z/tag zipper)))
  (assert (number? index) "expecting index for insert")
  (if (>= index (count (children zipper)))
    (-> zipper (z/append-child value))
    (-> zipper
        (focus [index])
        (z/insert-left value))))

(defmethod remove-child :map [zipper key]
  (-> zipper
      (focus [key])  ;positions at value
      (z/remove)
      (z/remove)))

(defmethod remove-child :default [zipper key]
  (-> zipper
      (focus [key])
      (z/remove)))

(defn patch [node editscript]
  (z/root (reduce patch* (zipper node) (e/get-edits editscript))))

(test/deftest test-patch-sexpr
  (doseq [{:keys [pre post]} t/tests]
    (test/is  (= (n/sexpr post) (n/sexpr (patch (zipper pre) (e/diff (n/sexpr pre) (n/sexpr post))))))))

(test/run-test test-patch-sexpr)

;; it is currently expected that ordering of keys in maps and sets are not alwasy preserved
(comment
  (test/deftest test-patch-str
    (doseq [{:keys [pre post]} t/tests]
      (test/is (= (n/string post) (n/string (patch (zipper pre) (e/diff (n/sexpr pre) (n/sexpr post))))))))

  (test/run-test test-patch-str))