(ns patch2
  (:require [clojure.pprint :as pprint]
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
   the value from the source node.  However because the editscript postions
   are dependenat on the application of prior edits the inverse edit must also
   be applied to the source document at the same time to keep positions in sync"
  [^PatchState {:keys [azip bzip] :as s} [aedit bedit]]
  (-> (m/match [aedit bedit]
        [[?apath :r _] [?bpath :r _]] (-> s
                                          (assoc :azip (z/replace    (focus azip ?apath) (z/node (focus bzip ?bpath))))
                                          (assoc :bzip (z/replace    (focus bzip ?bpath) (z/node (focus azip ?apath)))))
        [[?apath :+ _] [?bpath :-]] (-> s
                                        (assoc :azip (add-child (focus azip (butlast ?apath)) (last ?apath) (z/node (focus bzip ?bpath))))
                                        (assoc :bzip (remove-child (focus bzip (butlast ?bpath)) (last ?bpath))))
        [[?apath :-] [?bpath :+ _]] (-> s
                                        (assoc :azip (remove-child (focus azip (butlast ?apath)) (last ?apath)))
                                        (assoc :bzip (add-child (focus bzip (butlast ?bpath)) (last ?bpath) (z/node (focus azip ?apath)))))
        [[?apath :+ _] [?bpath :+ _]] (-> s
                                          (assoc :azip (add-child (focus azip (butlast ?apath)) (last ?apath) (z/node (focus bzip ?bpath))))
                                          (assoc :bzip (add-child (focus bzip (butlast ?bpath)) (last ?bpath) (z/node (focus bzip ?apath)))))
        [[?apath :-] [?bpath :-]] (-> s
                                      (assoc :azip (remove-child (focus azip (butlast ?apath)) (last ?apath)))
                                      (assoc :bzip (remove-child (focus bzip (butlast ?bpath)) (last ?bpath)))))
      (update :azip root)
      (update :bzip root)))


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

(defrecord PatchResult [anode bnode])
(defmethod pprint/simple-dispatch PatchResult [s] (pr (update-vals s n/sexpr)))
;(remove-method pprint/simple-dispatch PatchResult)

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
    (->PatchResult (z/root azip) (z/root bzip))))

(comment
  (patch [:a] [:b])
  (patch [:a :b] [:a])
  (patch [:a] [:a :b])
  (patch [:a] [:a :b :c])

;;
  (patch [:a :b] [:b :a])
    ;;
  )