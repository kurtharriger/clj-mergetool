(ns kurtharriger.notebook.notes002
  (:require [editscript.diff.a-star :as astar]
            [editscript.core :as e]
            [editscript.edit :as edit]
            [rewrite-clj.zip :as z]
            [rewrite-clj.node :as n]
            [rewrite-clj.parser :as p]
            [kurtharriger.util.simulator :refer [example]]))

(defn example [n]
  {:base  (p/parse-file-all (str "test/kurtharriger/examples/ex" n "/base.clj"))
   :left  (p/parse-file-all (str "test/kurtharriger/examples/ex" n "/left.clj"))
   :right (p/parse-file-all (str "test/kurtharriger/examples/ex" n "/right.clj"))})


;; can we extend edits script to support diffing of clj-rewrite nodes?
;; (defprotocol INode
;;   (get-path [this] "Get the path to the node from root")
;;   (get-value [this] "Get the actual data")
;;   (get-children [this] "Get all children node in a map")
;;   (add-child [this node] "Add a child node")
;;   (get-key [this] "Get the key of this node")
;;   (get-parent [this] "Get the parent node")
;;   (get-first [this] "Get the first child node")
;;   (get-last [this] "Get the last child node")
;;   (get-next [this] "Get the next sibling node")
;;   (set-next [this node] "Set the next sibling node")
;;   (set-order [this o] "Set the traversal order of this node")
;;   (^long get-order [this] "Get the order of this node in traversal")
;;   (^long get-size [this] "Get the size of sub-tree, used to estimate cost")
;;   (set-size [this s] "Set the size of sub-tree"))

;; (deftype Node [^PersistentVector path
;;                value
;;                parent
;;                ^:unsynchronized-mutable children
;;                ^:unsynchronized-mutable first
;;                ^:unsynchronized-mutable last
;;                ^:unsynchronized-mutable next
;;                ^:unsynchronized-mutable index
;;                ^:unsynchronized-mutable ^long order
;;                ^:unsynchronized-mutable ^long size]
;;   INode
;;   (get-path [_] path)
;;   (get-key [this] (-> this get-path peek))
;;   (get-value [_] value)
;;   (get-parent [_] parent)
;;   (get-children [_] children)
;;   (get-first [_] first)
;;   (get-last [_] last)
;;   (get-next [_] next)
;;   (set-next [_ n] (set! next n))
;;   (get-order [_] order)
;;   (set-order [this o] (set! order (long o)) this)
;;   (get-size [_] size)
;;   (set-size [this s] (set! size (long s)) this)
;;   (add-child [_ node]
;;     (set! children (assoc children (get-key node) node))
;;     (when last (set-next last node))
;;     (when-not first (set! first node))
;;     (set! last node)
;;     node))

(comment
  ;; quick step through debugger to see what creates
  ;; the above nodes

  (let [{:keys [base left right]} (example 2)
        left-diff (e/diff base left)]
    (pr-str left-diff))

  #_end_comment)

(comment

 ;; explore with portal
 ;; clj -A:dev/reloaded:repl/reloaded:repl/headless
  (let [base (-> (example 1) :base)]
    (tap> base))


  #_end_comment
    ;;
  )

;; we need to extend IType for startes
;; (defprotocol IType
;; (get-type [this] "Return a type keyword, :val, :map, :lst, etc."))

;; I want to be able to extend rewrite-clj nodes to support the protocol
;; for starters might want to do this but for some reason it doesn't work
;; and there is a general rule that when extending a type you need
;; to own either the protocol or the type. its bad idea to extend a type
;; you don't own as others may extend it in a way that conflicts
;; so instead we construct a wrapper node that delegates as needed

(comment
  ;; don't do this.. proof of concept
  (extend-protocol edit/IType
    rewrite_clj.node.forms.FormsNode
    (get-type [_] :lst))


  (let [{:keys [base left right]} (example 2)
        left-diff #break (e/diff base left)]
    (pr-str left-diff))


  ;; Nodes seem to be created within the algorithm
  ;; by index-collection and index-value and thus editscript
  ;; does not seem to be direclty extensible via the protocol
  ;; and i wasn't sure how I would implement get-path without
  ;; some details from editscript.  However it might work  to
  ;; modify edit script to use a factory method here and also
  ;; then edit script will provide the path when creating the
  ;; node

  ;;  (let [node (->Node path data parent {} nil nil nil 0 0 1)]
  )

(deftype  Node [#_PersistentVector path
                value
                parent
                ^:unsynchronized-mutable children
                ^:unsynchronized-mutable first
                ^:unsynchronized-mutable last
                ^:unsynchronized-mutable next
                ^:unsynchronized-mutable index
                ^:unsynchronized-mutable ^long order
                ^:unsynchronized-mutable ^long size]
  astar/INode
  (get-path [_] path)
  (get-key [this] (-> this astar/get-path peek))
  (get-value [_] value)
  (get-parent [_] parent)
  (get-children [_] children)
  (get-first [_] first)
  (get-last [_] last)
  (get-next [_] next)
  (set-next [_ n] (set! next n))
  (get-order [_] order)
  (set-order [this o] (set! order (long o)) this)
  (get-size [_] size)
  (set-size [this s] (set! size (long s)) this)
  (add-child [_ node]
    (set! children (assoc children (astar/get-key node) node))
    (when last (astar/set-next last node))
    (when-not first (set! first node))
    (set! last node)
    node))


(defn create-node [path value parent children first last next index order size]
  (->Node path value parent children first last next index order size))

(defn index-collection
  [type order path data parent]
  (let [node #break (astar/->Node path data parent {} nil nil nil 0 0 1)]
    (astar/add-child parent node)
    (case type
      (:map :vec) (#'astar/associative-children order path data node)
      :set        (#'astar/set-children order path data node)
      :lst        (#'astar/list-children order path data node))
    (let [^long cs (->> (#'astar/get-children node) vals (map #'astar/get-size) (reduce +))
          size     (+ (#'astar/get-size node) cs)]
      (doto node
        (#'astar/set-order @order)
        (#'astar/set-size size))
      (#'astar/inc-order order size))
    node))

(alter-var-root #'astar/index-collection (constantly index-collection))



(let [{:keys [base left right]} (example 2)
      left-diff #break (e/diff base left)]
  (pr-str left-diff))


;; wait.. I editscript diff works on the sexp and rewrite-clj has
;; zipper that skips whitespace and thus perhaps the patch path
;; may still work..  the replacement likely does not include the whitespace
;; thus I may still need to pull the replacement text from the file
;; being merged which also likely requires modifications to editscript
;; but using the zipper to apply the patch is still liekly needed regardless
;; and not wasted work