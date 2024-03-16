(ns kurtharriger.notebook.notes006
  (:require [editscript.core :as e]
            [rewrite-clj.zip :as z]
            [rewrite-clj.node :as n]))

(defn edits [base left]
  (e/get-edits (e/diff base left)))

(edits nil [1]) ;; => [[[] :r [1]]]

;; note: empty vector to non-empty vector
;; represented as replace not add
;; this is probably okay for vectors
;; since vectors are orderded if 2 users
;; insert values into vector its unclear
;; which edits should come first
(edits [] [1])    ;; => [[[] :r [1]]]

(edits [] [1 2])  ;; => [[[] :r [1 2]]]

(edits [1] [1 2]) ;; => [[[1] :+ 2]]

(edits [1] [1 2 3]) ;; => [[[1] :+ 2] [[2] :+ 3]]

(edits [1] [2])   ;; => [[[0] :r 2]]

(edits [1] [])    ;; => [[[] :r []]]

(edits [] nil)    ;; => [[[] :r nil]]

(edits {} {:a 1}) ;; => [[[] :r {:a 1}]]

;; similarly with empty map to non-empty map
;; represented as replace not add
;; which is less ideal for merging.
;; it would be better if represented as 2 adds
;; tood: post process edit?
(edits {} {:a 1 :b 2}) ;; => [[[] :r {:a 1, :b 2}]]

(edits {:a 1} {:a 1 :b 2}) ;; => [[[:b] :+ 2]]

(edits {:a 1} {:a 1 :b 2 :c 3}) ;; => [[[:b] :+ 2] [[:c] :+ 3]]

(edits {:a 1 :b 2} {:a 1}) ;; => [[[:b] :-]]

(edits {:a 1} {:a 2}) ;; => [[[:a] :r 2]]

(edits {:a 1} {:b 1}) ;; => [[[:a] :-] [[:b] :+ 1]]

(edits {:a 1} {})     ;; => [[[] :r {}]]

(edits {:a 1} nil)    ;; => [[[] :r nil]]

(defn move-to-map-key [zipper key]
  (->> zipper
       (z/down)
       (iterate z/right)
       (take-while (complement z/end?))
       (partition 2)
       (filter (fn [[kz]] (= key (z/sexpr kz))))
       (ffirst)))

(defn move-to-index [zipper n]
  (first (drop (dec n) (iterate z/right (z/down zipper)))))

;; z/root returns a node not a zipper
(defn move-root [zipper]
  (z/of-node (z/root zipper)))

(defn focus* [zipper path]
  (reduce (fn [zipper p]
            (cond
              (number? p)   (move-to-index zipper p)
              (keyword? p)  (move-to-map-key zipper p)))
          (move-root zipper) path))


(defn focus [node path]
  (let [zipper (if node
                 (z/of-string (pr-str node))
                 (z/of-node (n/forms-node [])))]
    (focus* zipper path)))

(-> (focus nil [])
    (z/root)
    (n/sexpr))

(edits nil [1]) ;; => [[[] :r [1]]]

(-> (focus nil [])
    (z/edit (constantly []))
    (z/root)
    (n/sexpr))

;; note: empty vector to non-empty vector
;; represented as replace not add
;; this is probably okay for vectors
;; since vectors are orderded if 2 users
;; insert values into vector its unclear
;; which edits should come first
(edits [] [1])    ;; => [[[] :r [1]]]
(-> (focus [] [])
    (z/edit (constantly [1]))
    (z/root)
    (n/sexpr))


(edits [] [1 2])  ;; => [[[] :r [1 2]]]
(-> (focus [] [])
    (z/edit (constantly [1 2]))
    (z/root)
    (n/sexpr))

(edits [1] [1 2]) ;; => [[[1] :+ 2]]
(-> (focus [1] [1])
    (z/insert-right 2)
    (z/root)
    (n/sexpr))

(edits [1] [1 2 3]) ;; => [[[1] :+ 2] [[2] :+ 3]]
(-> (focus [1] [1])
    (z/insert-right 2)
    (focus* [2])
    (z/insert-right 3)
    (z/root)
    (n/sexpr))
    ;; todo consider a more compressed representation: [[[1] :+ 2 3]]
    ;; no need to refocus to 2 when we are already there
    ;; but do need to add z/next
(-> (focus [1] [1])
    (z/insert-right 2)
    (z/next)
    (z/insert-right 3)
    (z/root)
    (n/sexpr))

(edits [1] [2])   ;; => [[[0] :r 2]]
(-> (focus [1] [0])
    (z/edit (constantly 2))
    (z/root)
    (n/sexpr))

;; edit replaces rather than deletes
(edits [1] [])    ;; => [[[] :r []]]
(-> (focus [1] [])
    (z/edit (constantly []))
    (z/root)
    (n/sexpr))

;; todo: convert to delete?  => [[[0] :- ]]
(-> (focus [1] [0])
    (z/remove)
    (z/root)
    (n/sexpr))

(edits [] nil)    ;; => [[[] :r nil]]
(-> (focus [] [])
    (z/edit (constantly nil))
    (z/root)
    (n/sexpr))
;; or if  => [[[] :- ]
(-> (focus [] [])
    (z/remove)
    (z/root)
    (n/sexpr))

(edits {} {:a 1}) ;; => [[[] :r {:a 1}]]
(-> (focus {} [])
    (z/edit (constantly {:a 1}))
    (z/root)
    (n/sexpr))

;; todo prefer add operation?:
;; [[[:a] :+ 1]]] ?
;; the above representation isn't quite ideal as key doesn't exist yet to focus
;; maybe [[[] :+ {:a 1} ]]] or [[[] :+ :a 1]]]
(-> (focus {} [])
    ;(z/edit (constantly {:a 1}))
    (z/append-child :a)
    (z/append-child 1)
    (z/root)
    (n/sexpr))


;; similarly with empty map to non-empty map
;; represented as replace not add
;; which is less ideal for merging.
;; it would be better if represented as 2 adds
;; tood: post process edit?
(edits {} {:a 1 :b 2}) ;; => [[[] :r {:a 1, :b 2}]]
(-> (focus {} [])
    (z/edit (constantly {:a 1 :b 2}))
    (z/root)
    (n/sexpr))

(edits {:a 1} {:a 1 :b 2}) ;; => [[[:b] :+ 2]]
;; this doesn't work since key doesn't exist...
;(-> (focus {:a 1} [:b]))
; perhaps when operation is add and last element in path is keyword
; rewrite as [[[] :+ :b 2]] ?
(-> (focus {:a 1} [])
    (z/append-child :b)
    (z/append-child 2)
    (z/root)
    (n/sexpr))

(edits {:a 1} {:a 1 :b 2 :c 3}) ;; => [[[:b] :+ 2] [[:c] :+ 3]]
;; todo rewrite as [[[] :+ :b 2]] [[] :+ :c 3]
;; then compress to [[[] :+ :b 2 :c 3]]
(-> (focus {:a 1} [])
    (z/append-child :b)
    (z/append-child 2)
    (z/append-child :c)
    (z/append-child 3)
    (z/root)
    (n/sexpr))

(edits {:a 1 :b 2} {:a 1}) ;; => [[[:b] :-]]
;; when removeing a map key we also need to remove the value
;; todo: do we rewrite [[[:b] :-]] as  [[[:b] :- :-]] ?
;; or [[[:b] :- 2]]  ?
;; or just have special logic when last path element is keyword
(-> (focus {:a 1 :b 2} [:b])
    (z/remove)
    ; note: need to move to value otherwise you get {:a 2} !
    (z/next)
    (z/remove)
    (z/root)
    (n/sexpr)) ;; => {:a 2}


(edits {:a 1} {:a 2}) ;; => [[[:a] :r 2]]
;; here we want to replace value not map key
(-> (focus {:a 1} [:a])
    (z/next)
    (z/edit (constantly 2))
    (z/root)
    (n/sexpr)) ;; => {:a 2}


;; if vector focus is in value
(edits {:a [1]} {:a [2]}) ;; => [[[:a 0] :r 2]]
;; here we want to replace value not map key
;; todo focus on [:a 0] doesn't work right now
;; because focus is on map key not map value
;; (-> (focus {:a [1]} [:a 0])
;;     ;(z/edit (constantly 2))
;;     (z/root)
;;     (n/sexpr))

(edits {:a 1} {:b 1}) ;; => [[[:a] :-] [[:b] :+ 1]]

(-> (focus {:a 1} [:a])
    (z/remove)
    ; note need to move to value
    ; otherwise you get (do :b 1)
    (z/next)
    (z/remove)
    (focus* [])
    (z/append-child :b)
    (z/append-child 1)
    (z/root)
    (n/sexpr)) ;; => {:b 1}


(-> (focus {:a 1} [:a])
    (z/remove)
    ;; note need to move to value otherwise you get nil not {}
    (z/next)
    (z/remove)
    (z/root)
    (n/sexpr)) ;; => {}


(edits {:a 1} {})     ;; => [[[] :r {}]]
(-> (focus {:a 1} [])
    (z/edit (constantly {}))
    (z/root)
    (n/sexpr)) ;; => {}


(edits {:a 1} nil)    ;; => [[[] :r nil]]
(-> (focus {:a 1} [])
    (z/edit (constantly nil))
    (z/root)
    (n/sexpr)) ;; => nil



