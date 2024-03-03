(ns kurtharriger.util.simulator
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

  #_end_comment)


(comment

  (let [base (-> (example 1) :base)]
    (binding [*print-meta* true]
      (:children base))

    #_end_comment)

