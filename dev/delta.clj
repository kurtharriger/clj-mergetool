(ns delta
  (:require [clojure.pprint :as pprint]
            [rewrite-clj.zip :as z]
            [rewrite-clj.node :as n]
            [rewrite-clj.parser :as p]
            [editscript.core :as e]
            [meander.epsilon :as m]
            [editscript.edit :as edit]))

;; exploring alternative diff such postions
;; are relative rather than hardcoded which
;; makes it easier to walk both in a zipper
;; advancing or not as needed

(e/diff [:a] [:a :b :c])
;; => [[[1] :+ :b] [[2] :+ :c]]
; would be (retiain (retain) (insert :b) (insert :c))

;; notice that the path of the second edit is dependent
;; on the state of the left document after the first
;; edit is applied.
(e/diff [:a :b :c] [:a])
;; => [[[1] :-] [[1] :-]]
; would be (retain (delete) (delete))

(e/diff [:a :b] [:b :c])
;; => [[[0] :-] [[1] :+ :c]]
; would be (retain (delete) (retain) (insert :c)))

(e/diff [:a :b :c] [:b :c :d])
;; => [[[0] :-] [[2] :+ :d]]
; would be (retain (delete) (retain) (insert :c)))

(e/diff [:a :b :c :d] [:b :c :d :e])
;; => [[[0] :-] [[3] :+ :e]]
; would be (retain (delete) (retain 2) (insert :c)))

(e/diff [:a :b :c] [:a :d])
;; => [[[1] :-] [[1] :r :d]]
; in this case editscirpt is deleting the b and replacing the c
;; but any of the following would produce the same result
;; (retain (retain) (delete) (replace :d)))
;; (retain (retain) (replace :d) (delete)))
;; (retain (retain) (delete 2) (insert :d)))
;; (retain (retain) (insert :d) (delete 2)))
;; (retain (retain) (delete) (insert :d) (delete)))

;; the specific nodes are not used but for now adding to
;; make results easier to verify
(defn retain [len anodes bnodes]
  {:pre [(= len (count anodes))
         (= len (count bnodes))
         (= anodes bnodes)]}
  [:retain len anodes])


(defrecord SeqDeltaState [azip bzip edit-tree edits delta i])
(defmethod print-method SeqDeltaState [s ^java.io.Writer w]
  (binding [*print-length* nil]
    (print-method
     (-> s
         (update :azip z/sexpr)
         (update :bzip z/sexpr)) w)))

(defmethod clojure.pprint/simple-dispatch SeqDeltaState [s] (pr s))

(defn retain-seq [{:keys [azip bzip edits delta i] :as state}]
  {:pre [(not-empty edits)] :post [(not-empty edits)]}
  (let [[pos _] (first edits)]
    (if (< i pos)
      (let [gap (- pos i)
            anodes (map z/node (take gap (iterate z/next azip)))
            bnodes (map z/node (take gap (iterate z/next bzip)))
            retain  (retain gap anodes bnodes)
            delta (conj delta retain)
            azip  (first (drop gap (iterate z/next azip)))
            bzip  (first (drop gap (iterate z/next bzip)))]
        (-> state
            (assoc :delta delta)
            (assoc :i pos)
            (assoc :azip azip)
            (assoc :bzip bzip)))
      state)))


(defn edit-tree* [edits]
  (reduce (fn [m [path & args]]
            (assoc-in m path (vec args))) {} edits))
(defn edit-tree* [edits]
  (reduce (fn [m [path & args]]
            (update-in m path (fn [cur] (if (and cur (not= cur (vec args))) [cur (vec args)] [(vec args)])))) {} edits))

(let [a [:a :b]
      b [:a :b :c]
      c [:a :b :d :e]
      bscript (e/diff a b)
      cscript (e/diff a c)
      editscript (e/combine bscript cscript)

      btree  (edit-tree* (e/get-edits bscript))
      ctree (edit-tree* (e/get-edits cscript))
      combinedtree (merge-with merge btree ctree)]
  [:bscript bscript
   :cscirpt cscript
   ;:editscript editscript
   :editscript (edit-tree* (e/get-edits editscript))
   :btree btree
   :ctree ctree
   :combinedtree combinedtree
   :eresult (e/patch a editscript)])
;; => [:bscript
;;     [[[2] :+ :c]]
;;     :cscirpt
;;     [[[2] :+ :d] [[3] :+ :e]]
;;     :editscript
;;     {2 [[[:+ :c]] [:+ :d]], 3 [[:+ :e]]}
;;     :btree
;;     {2 [[:+ :c]]}
;;     :ctree
;;     {2 [[:+ :d]], 3 [[:+ :e]]}
;;     :combinedtree
;;     {2 [[:+ :c] [[:+ :d]]], 3 [[:+ :e]]}
;;     :eresult
;NOTE:  should be a conflict since its unclear if :c should be before or after  the :d :e
;;     [:a :b :d :e :c]]
(defrecord Zipper [zipper])

(defn wrap-zipper [zipper]
  (->Zipper zipper))

(defmethod print-method Zipper [s ^java.io.Writer w]
  (print-method (z/string (:zipper s)) w))

(defmethod clojure.pprint/simple-dispatch Zipper [s] (pr s))

(defn zmap [^Zipper {zipper :zipper} f  & args]
  (wrap-zipper (apply f zipper args)))


(defrecord EditTreeBuilderState [m zip])
(defn edit-tree-with-source [edits zip]
  (reductions (fn [m [path & args]]
                (update-in m path
                           (fn [cur] (if (and cur (not= cur (vec args))) [cur (vec args)] [(vec args)])))) (->EditTreeBuilderState {} zip) edits))


(let [a [:a :b]
      b [:a :c]
      c [[:a] :b]
      expected [:b :c :d]
      bscript (e/diff a b)
      cscript (e/diff a c)
      fullscript (e/combine bscript cscript)
      e (edit-tree-with-source (e/get-edits bscript) (wrap-zipper (z/of-node (n/coerce b))))]
  [:a a
   :b b
   :c c
   :bscript bscript
   :cscript cscript
   :fullscript fullscript
   :e e
   :r   (e/patch a fullscript)])
