(ns kurtharriger.notebook.notes009
  (:require [editscript.core :as e]
            [rewrite-clj.zip :as z]
            [rewrite-clj.parser :as p]
            [clojure.zip :as zip]
            [meander.epsilon :as m]
            [rewrite-clj.node :as n]))

;; in notes008 noticed that the last path component
;; does not typically exist on add instead of trying
;; to navigate to the path and then perform the edit
;; lets try to navigate to all butlast and include
;; the final path component as on operation specific
;; parameter

(defn edits [base left]
  (let [edit-script (e/diff base left)]
    (assert (= left (e/patch base edit-script)))
    (e/get-edits edit-script)))

(defn move-to-map-key [zipper key]
  (let [kloc (->> zipper
                  (z/down)
                  (iterate z/right)
                  (take-while (complement z/end?))
                  (partition 2)
                  (filter (fn [[kz]] (= key (z/sexpr kz))))
                  (ffirst))]
    (or kloc
        (-> zipper
            (z/append-child key)
            (z/down)
            (z/rightmost)))))

(defn move-to-index [zipper n]
  (nth (iterate z/right (z/down zipper)) n))

;; z/root returns a node not a zipper
(defn move-root [zipper]
  (z/of-node (z/root zipper)))

(defn focus* [zipper path]
  (reduce (fn [zipper p]
            (cond
              (= (z/tag zipper) :map) (move-to-map-key zipper p)
              (number? p)   (move-to-index zipper p)))
          (move-root zipper) path))

(defn pr-str-meta [node]
  (binding [*print-meta* true]
    (pr-str node)))

(defn zipper [node]
  (vary-meta
   (cond
     (::zipper   (meta node)) node
     (nil? node) (z/of-node (n/forms-node []))
     :else       (z/of-string (pr-str-meta node)))
   assoc ::zipper true))

(z/sexpr (zipper nil))
(z/sexpr (zipper ""))
(z/sexpr (zipper {:a 1}))
(meta (zipper {:a 1}))
(-> (zipper  {:a 1})
    (zipper))

(defn zedit [edit]
  (-> (m/match edit
        [?path :+ ?value] [(vec (butlast ?path)) :+ (last ?path) ?value]
        [?path :-]  [(vec (butlast ?path)) :- (last ?path)]
        [?path :r ?value] [(vec (butlast ?path)) :r (last ?path) ?value])
      (vary-meta assoc :zedit/edit edit)))

(defn zedits [base left]
  (mapv zedit (edits base left)))

;(ns-unmap *ns* 'patch)

(defmulti patch (fn [zipper zedit] (second zedit)))

(defmethod patch :+ [zipper [path op key value :as zedit]]
  (let [zipper (focus* zipper path)]
    (cond
      (= (z/tag zipper) :map) (-> zipper
                                  (z/append-child key)
                                  (z/append-child value))

      :else
      (if (pos? key)
        (-> zipper
            (move-to-index (dec key))
            (z/insert-right value))
        (-> zipper (z/append-child value))))))

(edits [0] [0 1])     ;; => [[[1] :+ 1]]
(zedits [0] [0 1])    ;; => [[[] :+ 1 1]]

(edits {:a 1} {:a 1 :b 2}) ;; => [[[:b] :+ 2]]
(zedits {:a 1} {:a 1 :b 2}) ;; => [[[] :+ :b 2]]

(zedits {:a 1 :b []} {:a 1 :b [2]})
;; => [[[] :r :b [2]]]


(-> (zipper {:a 1})
    (patch [[] :+ :b 2])
    (z/root-string))
;; => "{:a 1 :b 2}"



(zedits [0] [0 1])    ;; => [[[] :+ 1 1]]
(-> (zipper [0])
    (patch [[] :+ 1 1])
    (z/root-string))
;; => "[0 1]"

(zedits (list 0) (list 0 1))    ;; => [[[] :+ 1 1]]
(-> (zipper (list 0))
    (patch [[] :+ 1 1])
    (z/root-string))


;; todo:
;; edit-script appears to return a position in the set
;; but this returned position is always end of set
;; symantically position  may seem a bit unnecessary
;;  as sets are unordered however the textural representation
;; in code does have order and thus perhaps the following
;; should return more specific insert positions
(edits #{0 1} #{0 1 2}) ;; => [[[2] :+ 2]]
(edits #{0 1} #{0 2 1}) ;; => [[[2] :+ 2]]

(-> (zipper #{0 1})
    (patch [[] :+ 2 2])
    (z/root-string))

;; note on vectors edit script returns edits in order they need applied
;; with lower indecies inserted first
(edits [0 1] [0 2 3 1])
;; however on sets edit-script is returning the in reverse order
(edits #{0 1} #{0 2 3 1}) ;; => [[[3] :+ 3] [[2] :+ 2]]

(zedits #{0 1} #{0 2 3 1})  ;; => [[[] :+ 3 3] [[] :+ 2 2]]
(defmacro pr-err [body]
  `(try ~body (catch Exception e# (-> e# bean :message))))
(pr-err (-> (zipper  #{0 1})
            (patch [[] :+ 3 3])
            (patch [[] :+ 2 2])
            (z/root-string)))
;; => "Insert at top"

(-> (zipper  #{0 1})
    ;; (patch [[] :+ 3 3])
    ;; (patch [[] :+ 2 2])
    (patch [[] :+ 2 2])
    (patch [[] :+ 3 3])
    (z/root-string))

;; for now the easist fix is probably to ignore position
;; when inserting into sets for now
(defmethod patch :+ [zipper [path op key value :as zedit]]
  (let [zipper (focus* zipper path)]
    (println :e (z/tag zipper) key value)
    (cond
      (= (z/tag zipper) :map)
      (-> zipper
          (z/append-child key)
          (z/append-child value))

      ;; note: edit-script always returns edits to set at end
      ;; and in reverse, for now just ignore position and insert
      ;; at the end
      ;; (edits #{0 1} #{0 2 3 1}) ;; => [[[3] :+ 3] [[2] :+ 2]]
      (= (z/tag zipper) :set)
      (do (println :insert value) (z/append-child zipper value))

      :else
      ;; index returned for add at target positon not position to add from
      ;; eg (edits [0] [0 1]) ;; => [[[1] :+ 1]]
      ;;  edit-script on empty vector represented as replace not add
      ;; (edits [] [0]) ;; => [[[] :r [0]]]
      ;; thus edit-script does not do an add at 0
      ;; however in future I may rewrite [[[] :r [0]]]
      ;; as [[[] :+ 0 0]] as 2 adds can be merged
      ;; (still w/ conflict as order is unclear)
      ;; but 2 replaces operations cannot be merged
      (if (pos? key)
        (-> zipper
            (move-to-index (dec key))
            (z/insert-right value))
        (-> zipper (z/append-child value))))))

(-> (zipper  #{0 1})
    (focus* [])
    (patch [[] :+ 3 3])
    (patch [[] :+ 2 2])
    (z/root-string))
(edits [0] [0 1])
;; => [[[1] :+ 1]]


;; note: edit-script on empty vector represented as replace
;; however
;; => "[0 1]"
(edits [] [0]) ;; => [[[] :r [0]]]

(zedits [] [0])
;; => [[[] :r nil [0]]]

(-> (zipper [0])
    (patch [[] :+ 0 1])
    (z/root-string))
