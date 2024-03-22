(ns kurtharriger.notebook02.notes01
  (:require [clojure.test :as test]
            [rewrite-clj.zip :as z]
            [rewrite-clj.node :as n]
            [rewrite-clj.parser :as p]
            [rewrite-clj.node.protocols :as protocols]
            [clojure.string :as str]))

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
   ;; => \"#{:a :b}\"

   "
  [node]
  (vary-meta
   (if (::zipper (meta node)) node
       (z/of-node (n/coerce node)))
   assoc ::zipper true))

(comment
  (-> nil
      (n/coerce)
      (z/of-node))

  (-> #{:a :b}
      (zipper)
      (z/root-string))
  ;; => "#{:b :a}"


  (-> "#{:a :b}"
      (p/parse-string)
      (zipper)
      (z/root-string))
  ;; => "#{:a :b}"
  )

(defn children [zipper]
  (->> zipper
       (z/down)
       (iterate z/right)
       (take-while (complement z/end?))))

;; z/root returns a node not a zipper
(defn move-root [zipper]
  (z/of-node (z/root zipper)))

(ns-unmap *ns* 'move-to-child)
(defmulti move-to-child
  (fn [zipper id] (z/tag zipper)))


(defmethod move-to-child :map [zipper id]
  (->> zipper
       (children)
       (partition 2)
       (filter (fn [[kz]] (= id (z/sexpr kz))))
       (first)
       (second)))

(defn move-to-index [zipper n]
  (nth (children zipper) n))

;; list or vector
(defmethod move-to-child :default [zipper id]
  (println :zipper (z/tag zipper) :id id)
  (move-to-index zipper id))

(defn focus [zipper path]
  (reduce move-to-child zipper path))

(comment
  (-> (zipper nil)
      (focus  [])
      (z/tag))

  ;; => :token

  (-> (zipper [{}])
      (focus [])
      (z/tag))
  ;; => :vector

  (-> (zipper [{}])
      (move-to-child 0)
      (z/tag))
   ;; => :map

  (-> (zipper [{}])
      (focus [0])
      (z/tag))

   ;; => :map

  (-> (zipper [:a])
      (move-to-child 0)
      (z/sexpr))
  ;; => :a


  (-> (zipper [{}])
      (focus [0])
      (z/sexpr))
  ;; => {}
  )



(do
  (test/deftest test-move-to-child
    (test/are [node path f val]
              (= val (f (focus (zipper node) path)))

      nil [] z/tag :token
      []  [] z/tag :vector
      [{}] [0] z/tag :map
      {:a []} [:a] z/tag :vector

      {:a [{42 [:d]}]} [:a 0 42 0] z/sexpr :d))
  (test/run-tests))