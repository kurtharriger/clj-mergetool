(ns zipper
  (:refer-clojure :exclude [next find replace remove])
  (:require [clojure.pprint :as pprint]
            [rewrite-clj.zip :as z]
            [rewrite-clj.node :as n]
            [rewrite-clj.parser :as p]
            [clojure.walk :as walk]
            [clojure.test :refer [deftest is are run-test]]))

(defrecord Zipper [zipper])

(defmethod print-method Zipper [s ^java.io.Writer w]
  (.write w "#z ")
  (print-method (z/string (:zipper s)) w))

(defmethod clojure.pprint/simple-dispatch Zipper [s] (pr  s))
(defn zipper
  "Coerce provided value into rewrite-clj zipper.

   In clojure code and tests it is often convienent to use
   clojure forms as follows, but note that the order is not
   preservered by the clojure reader as it is symantically
   irrelvant to the data.

   (z/root-string (zipper #{:a :b}))
   ;; => \"#{:b :a}\"

   However when manipulating textual representation of
   code we want to preserve the existing order.

   To preserve order provide a rewrite-clj node parsed
   from a string or a file

   (-> \"#{:a :b}\"
      (p/parse-string)
      (zipper)
      (z/root-string))
   ;; => \"#{:a :b}\""
  [node]
  (if (instance? Zipper node) node
      (->Zipper (z/of-node (n/coerce node)))))

;; factories
(def of-node #(-> % z/of-node ->Zipper))

;; fns returning zippers
(defn zf [f] (fn [z & args] (->Zipper (apply f (:zipper z) args))))


(def next (zf z/next))
(def down (zf z/down))
(def right (zf z/right))

;; fns returning values
(defn zq [f] (fn [z & args] (apply f (:zipper z) args)))
(def end? (zq z/end?))
(def tag (zq z/tag))
(def sexpr-able? (zq z/sexpr-able?))
(def sexpr (zq z/sexpr))
(def child-sexprs (zq z/child-sexprs))

(def root-node (zq z/root))
(def root-string (zq z/root-string))

;; composed fns
(comment
  (-> nil
      (n/coerce)
      (of-node))

  (-> #{:a :b}
      (zipper)
      (root-string))
  ;; => "#{:b :a}"

  (-> "#{:a :b}"
      (p/parse-string)
      (zipper)
      (root-string))
  ;; => "#{:a :b}"
  )

(defn zroot [zipper]
  (of-node (root-node zipper)))


(defn children [zipper]
  (->> zipper
       (down)
       (iterate right)
       (take-while (complement end?))))

(comment
  (->> (zipper #{:a :b})
       (down)
       (iterate right)
       ;(map (complement end?))
       (take 10))
  (children (zipper #{:a :b}))
 ;;
  )

;; z/root returns a node not a zipper

;(ns-unmap *ns* 'move-to-child)

(def h (-> (make-hierarchy)
           (derive :vector ::seq)
           (derive :list ::seq)))

(ns-unmap *ns* 'move-to-child)
(defmulti move-to-child
  (fn [zipper id] (tag zipper)) :hierarchy #'h)

;; will focus on the map value at key
(defmethod move-to-child :map [zipper k]
  (->> zipper
       (children)
       (partition 2)
       (filter (fn [[kz]] (= k (sexpr kz))))
       (first)
       (second)))

(defmethod move-to-child :set [zipper id]
  (first (filter #(= id (sexpr %)) (children zipper))))

(defmethod move-to-child ::seq [zipper n]
  (println :move)
  (prn zipper n)
  (nth (children zipper) n))

(defn focus
  "Positions the zipper at the path"
  [zipper path]
  (reduce move-to-child zipper path))


(deftest test-focus
  (are [node path f v]
       (let [z (zipper node)]
         (println node path)
         (= (f (focus z path)) v))
    nil [] tag :token
    []  [] tag :vector
    [{}] [0] tag :map
    {:a []} [:a] tag :vector

    [] [] sexpr []
    [:a] [0] sexpr :a
    #{:a :b} [:a] sexpr :a

    {:a [{42 [:d]}]} [:a 0 42 0] sexpr :d))

(comment
  (run-test #'test-focus)
  (-> [:a] zipper tag)
  (-> [:a] zipper (move-to-child 0))
;;
  )