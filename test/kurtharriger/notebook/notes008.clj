(ns kurtharriger.notebook.notes008
  (:require [editscript.core :as e]
            [rewrite-clj.zip :as z]
            [rewrite-clj.parser :as p]
            [clojure.zip :as zip]
            [rewrite-clj.node :as n]))

(defn edits [base left]
  (let [edit-script (e/diff base left)]
    (assert (= left (e/patch base edit-script)))
    (e/get-edits edit-script)))

(edits {:a 1} {:a 1 :b 2})
;; => [[[:b] :+ 2]]

;; do we just add the key when navigating?
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
  (first (drop (dec n) (iterate z/right (z/down zipper)))))

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

(defn focus [node path]
  (let [zipper (if node
                 (z/of-string (pr-str-meta node))
                 (z/of-node (n/forms-node [])))]
    (focus* zipper path)))

(-> (focus {:a 1} [:b])
    (z/insert-right 2)
    (z/root-string))
;; => "{:a 1 :b 2}"



;;
(edits {:a 1 :b 2} {:a 1 :c 2 :d 4})
;; => [[[:b] :-] [[:c] :+ 2] [[:d] :+ 4]]
(-> (focus {:a 1 :b 2} [:b])
    (z/remove)
    (z/right)
    (z/remove)
    (focus* [:c])
    (z/insert-right 2)
    (focus* [:d])
    (z/insert-right 4)
    (z/root-string))
;; => "{:a 1 :c 2 :d 4}"


(edits [:a 1 :b 2] [:a 1 :c 2 :d 4])
;; => [[[2] :r :c] [[4] :+ :d] [[5] :+ 4]]

;; bug in move-index?
(= (focus [:a 1 :b 2] [1])
   (focus [:a 1 :b 2] [0])
     ;(z/replace :c)
     ;(focus* [4])
     ;(z/insert-right :d)
    ;;  (z/right)
    ;;  (z/remove)
    ;;  (focus* [:c])
    ;;  (z/insert-right 2)
    ;;  (focus* [:d])
    ;;  (z/insert-right 4)
   )
;; => true


;; redefine with fix
^{:clj-kondo/ignore true}
(defn move-to-index [zipper n]
  (first (drop n (iterate z/right (z/down zipper)))))

(= (focus [:a 1 :b 2] [1])
   (focus [:a 1 :b 2] [0]))
;; => false

(defmacro pr-err [body]
  `(try ~body (catch Exception e# (-> e# bean :message))))

;; but now the path does not exit
(edits [1] [1 2 3]) ;; => [[[1] :+ 2] [[2] :+ 3]]

(focus [1] [0])
(pr-err (-> (focus [1] [1])
            (n/sexpr)))
;; => "No implementation of method: :sexpr* of protocol: #'rewrite-clj.node.protocols/Node found for class: nil"


;; again maybe just add index with arbitrary/nil value
^{:clj-kondo/ignore true}
(defn move-to-index [zipper n]
  (let [right (fn [z]
                (or (z/right z)
                    (-> z
                        (z/insert-right nil)
                        (z/right))))]
    (nth (iterate right (z/down zipper)) n)))

(edits [] [:a]) ;; => [[[] :r [:a]]]
(edits [0] [:a]) ;; => [[[0] :r :a]]

;; index seems bit different than I would expect
;; for insert at end... these 2 return same diff
(edits [0] [0 1])     ;; => [[[1] :+ 1]]
(edits [0 2] [0 1 2]) ;; => [[[1] :+ 1]]

;; if positioned at end of vector I need to replace
;; not insert
(edits [0] [0 1])     ;; => [[[1] :+ 1]]
(-> (focus [0] [1])
    (z/replace 1)
    (z/root-string))
;; => "[0 1]"


;;  but if element is in vector its a replace
(edits [0 1] [0 :a]) ;; => [[[1] :r :a]]
(-> (focus [0 1] [1])
    (z/replace :a)
    (z/root-string))
;; => "[0 :a]"


; or insert-left of position
(edits [0 2] [0 1 2]) ;; => [[[1] :+ 1]]
(-> (focus [0 2] [1])
    (z/insert-left 1)
    (z/root-string))
;; => "[0 1 2]"


;; it seems like it would make more sense  if
;; the following is [[[1] :r 1]] or [[0] :+ 1]
;; also seems bit weird to insert left  which makes me think
;; maybe I should make one less step but then [] and [0]
;; resolve to same location [] should be vector and [0] should
;; be the first element as blelow insert 1 left of first
(edits [0] [1 0]) ;; => [[[0] :+ 1]]

(defn add-value [z v]
  (if (z/rightmost? z)
    (z/replace z v)
    (z/insert-left z v)))

(edits [0] [0 1])     ;; => [[[1] :+ 1]]
(-> (focus [0 1] [1])
    (add-value 1)
    (z/root-string))
;; => "[0 1]"


;; hmmm... that doesn't work
;; still rightmost in both cases
(edits [0 2] [0 1 2]) ;; => [[[1] :+ 1]]
(-> (focus [0 2] [1])
    (add-value 1)
    ;(z/insert-left 1 )
    (z/root-string))
;; => "[0 1]"


;; todo: when adding map key should we also add arbitrary value and replace as needed?
;; thinking perhaps no becasue adding a key is [key] :+ val not [key] :r val

(edits {:a {:b [1 3]}} {:a {:b [1 2 3] :c 3}}) ;; => [[[:a :c] :+ 3]]

