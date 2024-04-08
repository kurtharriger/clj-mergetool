(ns patch2
  (:require [clojure.pprint :as pprint]
            [rewrite-clj.zip :as z]
            [rewrite-clj.node :as n]
            [rewrite-clj.parser :as p]
            [editscript.core :as e]
            [meander.epsilon :as m]
            [clojure.test :refer [deftest is are run-test]]))

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

(defrecord PatchState [azip bzip])
;(defmethod print-method PatchState [s w] (print-method (update-vals s z/sexpr) w))
(remove-method print-method PatchState)
;(defmethod pprint/simple-dispatch PatchState [s] (pr (update-vals s z/sexpr)))
(remove-method pprint/simple-dispatch PatchState)

(->PatchState (zipper [:a]) (zipper [:b]))



(defn patch*
  "This patch implementation ignores value from editscript and instead pulls
   the value from the source node."
  [^PatchState {:keys [azip bzip] :as s} [aedit]]
  (prn :azip (z/string azip) :bzip (z/string bzip) :edit aedit)
  (-> (m/match aedit
        [?apath :r _] (assoc s :azip (z/replace (focus azip ?apath) (z/node (focus bzip ?apath))))
        [?apath :+ _] (assoc s :azip (add-child (focus azip (butlast ?apath)) (last ?apath) (z/node (focus bzip ?apath))))
        [?apath :-] (assoc s :azip (remove-child (focus azip (butlast ?apath)) (last ?apath))))
      (update :azip root)))

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
; removed assertion since key is the sexpr from path
; but value now is a rewrite-clj
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


(defn patch
  "Applies an editscript patch to rewrite-clj node."
  [anode bnode]
  (let [anode (n/coerce anode)
        bnode (n/coerce bnode)
        aeditscript (-> (e/diff (n/sexpr anode) (n/sexpr bnode))
                        (e/get-edits))
        beditscript (-> (e/diff (n/sexpr bnode) (n/sexpr anode))
                        (e/get-edits))
        {:keys [azip bzip]} (reduce patch* (->PatchState (zipper anode) (zipper bnode))
                                    (map vector aeditscript beditscript))]
      ;(assert (= (z/sexpr azip) (z/sexpr bzip)))
    (z/root azip)))

(defrecord PrintableNode [node])
;; pr/prn is readable its just pprint that seems unreadable
;(defmethod print-method PrintableNode [{node :node} w] (print-method (n/string node) w))
;(remove-method print-method PrintableNode)

;; pprint prints the map which is verbose and hard to read
;; use the use the print-method defined by rewrite-clj instead
;; which would be the default simple-dispatch if node wasn't also a map
(defmethod pprint/simple-dispatch PrintableNode [{node :node}] (pr node))
;(remove-method pprint/simple-dispatch PrintableNode)

;;todo override calva pprint?
;; after bit more research discoverd I can toggle calva pprint on and off

(defn ppatch [& args] (->PrintableNode (apply patch args)))
(comment
  (ppatch [:a] [:b])

  (ppatch [:a :b] [:a])
  (ppatch [:a] [:a :b])
  (ppatch [:a] [:a :b :c])


;;
  (ppatch [:a :b] [:b :a])
  (ppatch nil [])
  (ppatch [] nil)

  (ppatch #{} #{:a})
  (ppatch #{:b :c} #{:a})

  ;; failed replacing value in set
  ;; due to assertion in add-child
  (ppatch #{:a} #{:b})

  (-> #{:b}
      (zipper)
      (focus [:b])
      (z/sexpr))


  :rcf)