(ns kurtharriger.notebook.notes007
  (:require [editscript.core :as e]
            [rewrite-clj.zip :as z]
            [rewrite-clj.parser :as p]
            [rewrite-clj.node :as n]))

(defn edits [base left]
  (e/get-edits (e/diff base left)))

;; (defn move-to-map-key [zipper key]
;;   (let [z zipper
;;         z (z/down z)
;;         z (iterate z/right z)
;;         z (take-while (complement z/end?) z)
;;         z (partition 2 z)
;;         z (filter (fn [[kz]] (= key (z/sexpr kz))) z)
;;         z (ffirst z)]
;;     z))
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


(edits {{:k :v} 1} {{:k :v} 2}) ;; => [[[{:k :v}] :r 2]]

(-> (focus {{:k :v} 1} [{:k :v}])
    (z/right)
    (z/edit (constantly 2))
    (z/root)
    (n/sexpr))

(-> (focus {:a ^:vectormeta []} [:a])
    (z/right)
    (z/root)
    (n/sexpr)
    (get :a)
    (meta))

;; todo:
;; meta is preserved but representation
;; changes slightly
(-> (focus {:a ^:vectormeta []} [:a])
    (z/right)
    (z/string))
;; => "^{:vectormeta true} []"


(-> (pr-str-meta {:a ^:vectormeta []})
    (z/of-string)
    (z/root-string))
;; => "{:a ^{:vectormeta true} []}"

;; this however is due to z/string pr-string hack
;; shouldn't be an issue when parsing from file
(-> (z/of-string "{:a ^:vectormeta []}")
    (z/root-string))
;; => "{:a ^:vectormeta []}"


(-> (focus {:a (with-meta [] {:vectormeta true})} [])
    (z/root-string))
;; => "{:a ^{:vectormeta true} []}"


(-> (z/of-string "{:a (with-meta [] {:vectormeta true})}")
    (z/root-string))
;; => "{:a (with-meta [] {:vectormeta true})}"

;; todo: editscript however does not support metadata
(edits {:a ^{:vectormeta true} []} {:a ^{:vectormeta false} []})

(-> (focus {:a ^{:vectormeta true} []} [:a])
    (z/right)
    (z/string))
;; => "^{:vectormeta true} []"

(-> (focus {:a ^{:vectormeta true} []} [:a])
    (z/right)
    (z/down)
    (z/right)
    (z/string))
;; => "[]"


(-> (focus {:a ^{:vectormeta true} []} [:a])
    (z/right)  ;; "^{:vectormeta true} []"
    (z/down)   ;; ^{:vectormeta true}
    (z/down)   ;; ":vectormeta"
    (z/string))
;; => ":vectormeta"

(-> (focus {:a ^:vectormeta []} [:a])
    (z/right)  ;; "^{:vectormeta true} []"
    (z/down)   ;; ^{:vectormeta true}
    (z/down)   ;; ":vectormeta"
    (z/right)  ;; true
    (z/string))
;; => "true"

(-> (focus {:a ^:vectormeta []} [:a])
    (z/right)  ;; "^{:vectormeta true} []"
    (z/down)   ;; ^{:vectormeta true}
    (z/down)   ;; ":vectormeta"
    (z/right)  ;; true
    (z/edit (constantly false))
    (z/root-string))
;; => "{:a ^{:vectormeta false} []}"


(-> (focus {:a ^{:vectormeta true} []} [:a])
    (z/right)
    (z/down)
    (z/string))
;; => "{:vectormeta true}"



;; how about reader literals
(-> (pr-str {:a #inst "2024-01-01T00:00:00.000-00:00"})
    (z/of-string)
    (z/root-string))
;; => "{:a #inst \"2024-01-01T00:00:00.000-00:00\"}"


(-> (focus {:a #inst "2024-01-01T00:00:00.000-00:00"} [:a])
    (z/right)
    (z/sexpr))
;; => (read-string "#inst \"2024-01-01T00:00:00.000-00:00\"")

(-> (focus {:a #inst "2024-01-01T00:00:00.000-00:00"} [:a])
    (z/right)
    (z/string))
;; => "#inst \"2024-01-01T00:00:00.000-00:00\""


(edits #inst "2024-01-01T00:00:00.000-00:00"
       "2024-01-01T00:00:00.000-00:00")
;; => [[[] :r "2024-01-01T00:00:00.000-00:00"]]

(edits "2024-01-01T00:00:00.000-00:00"
       #inst "2024-01-01T00:00:00.000-00:00")
;; => [[[] :r #inst "2024-01-01T00:00:00.000-00:00"]]

(-> (focus "2024-01-01T00:00:00.000-00:00" [])
    (z/edit (constantly #inst "2024-01-01T00:00:00.000-00:00"))
    (z/root-string))
;; => "#inst \"2024-01-01T00:00:00.000-00:00\""

