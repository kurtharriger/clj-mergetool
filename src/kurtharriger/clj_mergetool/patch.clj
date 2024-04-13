(ns kurtharriger.clj-mergetool.patch
  (:require [clojure.walk :as walk]
            [rewrite-clj.zip :as z]
            [rewrite-clj.node :as n]
            [rewrite-clj.parser :as p]
            [editscript.core :as e]
            [meander.epsilon :as m]))

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
(defn root [zipper]
  (z/of-node (z/root zipper)))

;(ns-unmap *ns* 'move-to-child)
(defmulti move-to-child
  (fn [zipper id] (z/tag zipper)))

;; will focus on the map value at key
(defmethod move-to-child :map [zipper k]
  (->> zipper
       (children)
       (partition 2)
       (filter (fn [[kz]] (= k (z/sexpr kz))))
       (first)
       (second)))

(defmethod move-to-child :set [zipper id]
  (first (filter #(= id (z/sexpr %)) (children zipper))))

;; :lst, :vec
(defmethod move-to-child :default [zipper n]
  (nth (children zipper) n))

(defn focus
  "Positions the zipper at the path"
  [zipper path]
  (reduce move-to-child zipper path))

(defmulti add-child (fn [zipper key value] (z/tag zipper)))
(defmulti remove-child (fn [zipper key] (z/tag zipper)))

(comment
  ;; note: the only operatiGon expected at the root is replace (:r)
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

(defn patch*  [zipper edit]
  (root
   (m/match edit
     [?path :r ?value] (z/replace    (focus zipper ?path) ?value)
     [?path :+ ?value] (add-child    (focus zipper (butlast ?path)) (last ?path) ?value)
     [?path :-]        (remove-child (focus zipper (butlast ?path)) (last ?path)))))

(comment
  (->  nil
       (zipper)
       (patch* [[] :r #{:a}])
       (root)
       (z/sexpr))

  (->  #{:a}
       (zipper)
       (patch* [[] :r #{:b}])
       (z/sexpr)))

(defmethod add-child :map [zipper key value]
  (-> zipper
      (z/append-child key)
      (z/append-child value)))

(defmethod add-child :set [zipper key value]
  ;(assert (= key value))
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


(defn reverse-from
  "lazily traverse vector in reverse from specified index"
  [coll pos]
  {:pre (vector? coll)}
  (map (partial nth coll) (range (dec pos) -1 -1) coll))

(comment
  (->> (range 30) (take 25) (reverse))
  (->> (reverse-from (range 30) 25))
  :rcf)


(defn make-add-leading-metadata
  "Given a node returns a function that when provided a node "
  [root-node]
  (let [rnodes (vec (tree-seq n/inner? n/children root-node))
        rindex (into {} (map vector rnodes (range)))]
    (fn [node]
      (when-let [pos (rindex node)]
        (vary-meta node assoc ::leading-whitespace
                   (->> (reverse-from rnodes pos)
                        (take-while n/whitespace-or-comment?)
                        (reverse)
                        (vec)))))))

(defn diff
  "Creates an editscript over and containing rewrite-clj nodes.
   The values within the editscript are replaced with the source nodes from the right document so that whitespace within those nodes is preserved.
   Leading whitespace for the node is stored on ::leading-whitespace meta property for use post patching.
   "
  [left right]
  (let [left (n/coerce left)
        right (n/coerce right)
        add-leading-metadata (make-add-leading-metadata right)
        rzip (zipper right)
        editscript (e/diff (n/sexpr left) (n/sexpr right))
        edits (e/get-edits editscript)]
    edits
    (-> (for [edit edits]
          (m/match edit
            [?path :r ?value] (let [rnode (add-leading-metadata (z/node (focus rzip ?path)))]
                                ;todo
                                ;rewrite-clj sexpr may prefix a (do ..) in some forms causing assert
                                ;removing for now
                                ;(assert (= (n/sexpr rnode) ?value) (str "expected " ?value " got " (n/sexpr rnode)))
                                [?path :r rnode])
            [?path :+ ?value] (let [rnode (add-leading-metadata (z/node (focus rzip ?path)))]
                                ;(assert (= (n/sexpr rnode) ?value) (str "expected " ?value " got " (n/sexpr rnode)))
                                [?path :+ rnode])
            ?other ?other))
        vec
        e/edits->script)))


(defn expand-leading-whitespace-meta
  "Inserts leading whitespace "
  [root-node]
  (walk/postwalk (fn [node]
                   (if (and (n/node? node) (n/inner? node))
                     (n/replace-children
                      node
                      (mapcat (fn [node]
                                (if-let [lw (::leading-whitespace (meta node))]
                                  (conj lw node)
                                  [node])) (n/children node)))
                     node))
                 root-node))

(defn patch
  "Applies an editscript patch to rewrite-clj node."
  [node editscript]
  (-> (reduce patch* (zipper node) (e/get-edits editscript))
      (z/root)
      (expand-leading-whitespace-meta)))
