(ns kurtharriger.util.simulator
  (:require [clojure.string :as str :refer [triml]]
            [editscript.core :as e]
            [clojure.tools.reader.edn :as edn]
            [rewrite-clj.zip :as z]
            [rewrite-clj.node :as n]
            [rewrite-clj.parser :as p]
            [kurtharriger.util.simulator :refer :all]))


(comment
  (println (merge-result-example 1))
  (println (merge-result-example 2))

  #_end_comment)

(comment
    ;; test simpliest thing that could possibly work
    ;; edn reader only seems to read first form
    ;; and whitespace is not preserved as expected
    ;; but it basically works producing the correct
    ;; merge excepting the loss of whitespace

  (let [{:keys [base left right]} (example 2)
        base (edn/read-string base)
        left (edn/read-string left)
        right (edn/read-string right)
        left-diff (e/diff base left)
        right-diff (e/diff base right)

        full-diff (e/combine left-diff right-diff)

        result (e/patch base full-diff)]
    (pr-str result))
  ;; => "(defn analyze-data [data] (let [processed (mapv process-item data) filtered (filter relevant? processed) results (reduce combine-results {} filtered)] results))"




  #_end_comment)

(comment
  ;; diff doesn't seem to work with clj-rewrite nodes it just
  ;; replaces the children, however diffing the edn worked
  ;; clj-rewrite probably returns its own datastructure
  ;; to store positions and such which would be expected to
  (let [{:keys [base left right]} (example 2)

        base (p/parse-string-all base)
        left (p/parse-string-all left)
        right (p/parse-string-all right)

        left-diff (e/diff base left)
        right-diff (e/diff base right)

        full-diff (e/combine left-diff right-diff)

        result (e/patch base full-diff)]
    ;; (prn base)
    ;; (prn left)
    ;; (prn right)
    (prn left-diff)
    (prn right-diff)
    (prn full-diff)
    (prn result)
    (pr-str result))
  ;; => "<forms:\n  (defn analyze-data [data]\n    (let [processed (mapv process-item data) ; Changed map to mapv for vector output\n          results (reduce combine-results {} processed)]\n      results))\n  \n>"
  )


(comment
  ;; clj vs edn
  (let [{:keys [base left right]} (example 2)
        base-edn (edn/read-string base)
        base-clj (p/parse-string-all base)]
    (prn (type base-edn))     ;; clojure.lang.PersistentList
    (prn (type base-clj)) ;;  rewrite_clj.node.forms.FormsNode


    (println (= base-edn base-clj)) ;; false
    ;; sexp returns unwraps the clj-rewrite node
    (println (= base-edn (n/sexpr base-clj))) ;; true
    )

  ;; what would be required to extend the protocols for diffing
  ;; notes002.clj

  #_end_comment)


