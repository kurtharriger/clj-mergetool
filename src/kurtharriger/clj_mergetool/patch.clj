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
      (z/append-child (if-let [node (-> value meta :key-node)] node key))
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

;(def !debug (atom nil))

(defn diff
  "Creates an editscript over and containing rewrite-clj nodes.
   The values within the editscript are replaced with the source nodes from the right document so that whitespace within those nodes is preserved.
   Leading whitespace for the node is stored on ::leading-whitespace meta property for use post patching.
   "
  [left right]
  ;(reset! !debug [left right])
  (let [left (n/coerce left)
        right (n/coerce right)
        add-leading-metadata (make-add-leading-metadata right)
        rzip (zipper right)
        editscript (e/diff (n/sexpr left) (n/sexpr right))
        edits (e/get-edits editscript)]
    (-> (for [edit edits]
          (m/match edit
            [?path ?replace-or-add ?value]
            (let [node (add-leading-metadata (z/node (focus rzip ?path)))
                   ;; if node is a map value than also grab the map key
                  node (if (= (z/tag (focus rzip (butlast ?path))) :map)
                         (vary-meta node assoc :key-node (add-leading-metadata
                                                          (z/node (z/left (focus rzip ?path)))))
                         node)]
              [?path ?replace-or-add node])
            ?other ?other))
        (vec)
        (e/edits->script))))

(comment

  (apply diff @!debug)
  (def !debug2 (atom nil))
  (let [[left right] @!debug
        add-leading-metadata (make-add-leading-metadata right)
        rzip (zipper right)
        editscript (e/diff (n/sexpr left) (n/sexpr right))
        edits (e/get-edits editscript)]
    edits))
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


(defn encode-reader-macro-nodes
  "n/sexpr represents reader macro nodes as read-strings that are opaque to editscript
     rewrite these as maps tagged with metadata for post patching replacement"
  [node]
  (walk/postwalk
   (fn [n]
     (if (and (n/node? n) (= :reader-macro (n/tag n)))
       (let [[token children] (n/children n)]
         (-> children
             (n/children)
             (n/map-node)
             (vary-meta merge
                        {::reader-macro true
                         ::reader-macro-token token})))
       n)) node))

(defn decode-reader-macro-nodes
  "n/sexpr represents reader macro nodes as read-strings that are opaque to editscript
     rewrite these as vectors tagged with metadata for post patching replacement"
  [node]
  (walk/prewalk
   (fn [n]
     (if (and (n/node? n) (-> n meta ::reader-macro))
       (-> n
           (n/children)
           (n/list-node)
           (list)
           (->> (cons (-> n meta ::reader-macro-token)) (n/reader-macro-node)))
       n)) node))


(comment
  (-> "#?(:clj 1  :cljs 2)"
      (p/parse-string)
      (n/string))

;; => "#?(:clj 1  :cljs 2)"

  (-> "#?(:clj 1  :cljs 2)"
      (p/parse-string)
      (n/sexpr))
      ;; => "(read-string \"#?(:clj 1  :cljs 2)\")"

  (-> "#?(:clj 1 :cljs 2)"
      (p/parse-string)
      (encode-reader-macro-nodes)
      ((juxt meta identity)))
  ;; => [#:kurtharriger.clj-mergetool.patch{:reader-macro true} [[:clj 1] [:cljs 2]]]

  (-> "#?(:cljs 1 :clj #_comment 2)"
      (p/parse-string)
      (n/children))
  ;; => (<token: ?> <list: (:cljs 1 :clj #_comment 2)>)

  (-> "#?(:cljs 1 :clj #_comment 2)"
      (p/parse-string)
      (encode-reader-macro-nodes)
      (decode-reader-macro-nodes)
      (n/string))

  (-> "#?@(:cljs 1 :clj #_comment 2)"
      (p/parse-string)
      (encode-reader-macro-nodes)
      (decode-reader-macro-nodes)
      (n/string))



  :rcf)

(defn encode-forms-nodes
  "n/sexpr represents reader macro nodes as read-strings that are opaque to editscript
     rewrite these as maps tagged with metadata for post patching replacement"
  [node]
  (walk/postwalk
   (fn [n]
     (if (and (n/node? n) (= :forms (n/tag n)))
       (-> n
           (n/children)
           (n/list-node)
           (vary-meta assoc ::forms true))
       n)) node))

(defn decode-forms-nodes
  "n/sexpr represents reader macro nodes as read-strings that are opaque to editscript
     rewrite these as vectors tagged with metadata for post patching replacement"
  [node]
  (walk/postwalk
   (fn [n]
     (if (-> n meta ::forms)
       (-> n
           (n/children)
           (n/forms-node))
       n)) node))

(defn encode-fn-nodes
  "n/sexpr represents reader macro nodes as read-strings that are opaque to editscript
     rewrite these as maps tagged with metadata for post patching replacement"
  [node]
  (walk/postwalk
   (fn [n]
     (if (and (n/node? n) (= :fn (n/tag n)))
       (-> n
           (n/children)
           (n/list-node)
           (vary-meta assoc ::fn true))
       n)) node))

(defn decode-fn-nodes
  "n/sexpr represents reader macro nodes as read-strings that are opaque to editscript
     rewrite these as vectors tagged with metadata for post patching replacement"
  [node]
  (walk/postwalk
   (fn [n]
     (if (-> n meta ::fn)
       (-> n
           (n/children)
           (n/fn-node))
       n)) node))

(defn encode-uneval-nodes
  "n/sexpr represents reader macro nodes as read-strings that are opaque to editscript
     rewrite these as maps tagged with metadata for post patching replacement"
  [node]
  (walk/postwalk
   (fn [n]
     (if (and (n/node? n) (= :uneval (n/tag n)))
       (-> n
           (n/children)
           (n/list-node)
           (vary-meta assoc ::uneval true))
       n)) node))

(defn decode-uneval-nodes
  "n/sexpr represents reader macro nodes as read-strings that are opaque to editscript
     rewrite these as vectors tagged with metadata for post patching replacement"
  [node]
  (walk/postwalk
   (fn [n]
     (if (-> n meta ::uneval)
       (-> n
           (n/children)
           (n/uneval-node))
       n)) node))

(comment
  (let [base (parse-string-all "(ns test)\n(def a 1)")
        current (parse-string-all "(ns test)\n(def a 2)")]
    [base current (e/diff base current)])
  ;; => [<forms:
  ;;      (ns test)
  ;;      (def a 1)
  ;;    > <forms:
  ;;      (ns test)
  ;;      (def a 2)
  ;;    > [[[:children] :r (<list: (ns test)> <newline: "\n"> <list: (def a 2)>)]]]


  (let [base (parse-string-all "(ns test)\n(def a 1)")
        current (parse-string-all "(ns test)\n(def a 2)")
        base  (vec (n/child-sexprs base))
        current (vec (n/child-sexprs current))]
    [base current (e/diff base current)])
  ;; => [[(ns test) (def a 1)] [(ns test) (def a 2)] [[[1 2] :r 2]]]

  (let [base (parse-string-all "(ns test)\n(def a 1)")]
    (n/tag base))

  (let [base (parse-string-all "(ns test)\n(def a 1)")
        current (parse-string-all "(ns test)\n(def a 2)")
        encode #(n/sexpr (n/list-node (n/children %)))
        [base current] (mapv encode [base current])]
    [base current (e/diff base current)])

  (let [base (parse-string-all "(ns test)\n(def a 1)")]
    (-> base
        (encode-forms-nodes)
        (decode-forms-nodes)
        (n/string)))


  :rcf)


(comment
  (-> "#(+ %  1)"
      (p/parse-string-all)
      (n/sexpr))
    ;; => ((fn* [p1__23888#] (+ p1__23888# 1)))


  (-> "#(+ %  1)"
      (p/parse-string-all)
      (n/children)
      (first)
      ((juxt n/tag n/string)))
    ;; => [:fn "#(+ %  1)"]

  (-> "(fn add [x] (+ x  1))"
      (p/parse-string-all)
      (n/children)
      (first)
      (n/children)
      (first)
      ((juxt n/tag n/string)))
    ;; => [:token "fn"]


  (-> "#(+ 1  %)"
      (p/parse-string-all)
      (encode-fn-nodes)
      (decode-fn-nodes)
      (encode-forms-nodes)
      (decode-forms-nodes)
      (n/string)))


(defn parse-string-all [content]
  (-> (p/parse-string-all content)
      (encode-reader-macro-nodes)
      (encode-fn-nodes)
      (encode-uneval-nodes)
      (encode-forms-nodes)))

(defn patch
  "Applies an editscript patch to rewrite-clj node."
  [node editscript]
  (-> (reduce patch* (zipper node) (e/get-edits editscript))
      (z/root)
      (expand-leading-whitespace-meta)
      (decode-reader-macro-nodes)
      (decode-fn-nodes)
      (decode-uneval-nodes)
      (decode-forms-nodes)))


(comment
  (let [base (parse-string-all (slurp "test-resources/examples/ex4/base.clj"))
        right (parse-string-all (slurp "test-resources/examples/ex4/right.clj"))
        editscript (diff base right)]
    (-> base
        (patch editscript)
        (zipper)
        (focus [1 2])
        (z/node)
        ;(n/children)
        ;(->> (map meta))
        (decode-reader-macro-nodes)
        ; (n/children)
        ;;  (->> (cons (n/token-node '?)))
        ;;  (n/reader-macro-node))
        ;((juxt meta identity))
        ))

  *e
  :rcf)
