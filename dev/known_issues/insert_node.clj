(ns known-issues.insert-node
  (:require [editscript.core :as e]))


;; edit script does not preserve whitespace
;; using rewrite-clj minimizes the disruption
;; to the edited document, however the inserts
;; in the editscript sexprs not rewrite-clj nodes
;; and thus do not contain any whitespace information
;; about the node being inserted

;; one potential solution is to fork edit script to
;; understand how to diff rewrite-clj nodes thus
;; the insert edits would contain rewrite-clj nodes

;; another potential solution is to pull the inserted
;; node from the right document rather than the editscript
;; I recall doing a similar thing in the past, but
;; using a different editscript representation.

;; rather than [path op args] editscript where the path
;; is the position in the edited document so far
;; it was represented as a sequence of relative edits
;; and thus one could easily walk both documents and
;; the editscript in parallel to create an inverse
;; delta. I'm not sure this works with editscript
;; because the path is dependant on the state of
;; the left document after all previously seen edits

;; for example:

(e/diff [:a] [:a :b :c])
;; => [[[1] :+ :b] [[2] :+ :c]]
; would be (retiain (retain) (insert :b) (insert :c))

;; notice that the path of the second edit is dependent
;; on the state of the left document after the first
;; edit is applied.
(e/diff [:a :b :c] [:a])
;; => [[[1] :-] [[1] :-]]
; would be (retain (delete) (delete))

(e/diff [:a :b] [:b :c])
;; => [[[0] :-] [[1] :+ :c]]
; would be (retain (delete) (retain) (insert :c)))

(e/diff [:a :b :c] [:b :c :d])
;; => [[[0] :-] [[2] :+ :d]]
; would be (retain (delete) (retain) (insert :c)))

(e/diff [:a :b :c :d] [:b :c :d :e])
;; => [[[0] :-] [[3] :+ :e]]
; would be (retain (delete) (retain 2) (insert :c)))

(e/diff [:a :b :c] [:a :d])
;; => [[[1] :-] [[1] :r :d]]
; in this case editscirpt is deleting the b and replacing the c
;; but any of the following would produce the same result
;; (retain (retain) (delete) (replace :d)))
;; (retain (retain) (replace :d) (delete)))
;; (retain (retain) (delete 2) (insert :d)))
;; (retain (retain) (insert :d) (delete 2)))
;; (retain (retain) (delete) (insert :d) (delete)))

;; so far the editscript path does match the right document
;; even after replace so maybe it will work to pull source

;; this issue is now mostly fixed by updated edit script with the source nodes
;; so that whitespace within those nodes is preserved.  There is sitll issue
;; preserving leading whitespace on inserted/replaced nodes