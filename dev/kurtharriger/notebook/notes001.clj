(ns kurtharriger.notebook.notes001
  (:require [clojure.string :as str :refer [triml]]
            [editscript.core :as e]
            [clojure.tools.reader.edn :as edn]
            [rewrite-clj.zip :as z]
            [rewrite-clj.node :as n]
            [rewrite-clj.parser :as p]
            [kurtharriger.util.simulator :refer :all]))


(comment

;; chatgpt generated examples a bit lame but good enough
;; to start.

  (-> (merge-result-example 1) second println)
;; <<<<<<< HEAD
;; (defn render-page [content]
;;   (println "Rendering page"))

;; (defn query-database [query]
;;   (println "Querying database"))
;; =======
;; (defn query-database [query]
;;   (println "Query database with cache"))

;; (defn render-page [content]
;;   (println "Rendering page with cache"))
;; >>>>>>> right


  (-> (merge-result-example 2) second println)
;; (defn analyze-data [data]
;; <<<<<<< HEAD
;;   (let [processed (map process-item data)
;;         filtered (filter relevant? processed)
;;         results (reduce combine-results {} filtered)]
;; =======
;;   (let [processed (mapv process-item data) ; Changed map to mapv for vector output
;;         results (reduce combine-results {} processed)]
;; >>>>>>> right
;;     results))


  (-> (merge-result-example 3) second println)
;; {:deps {org.clojure/clojure {:mvn/version "1.10.1"}
;; <<<<<<< HEAD
;;         org.clojure/core.async {:mvn/version "1.3.612"}}}
;; =======
;;         org.clojure/core.async {:mvn/version "1.3.610"}
;;         clj-http {:mvn/version "3.11.0"}}}
;; >>>>>>> right





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
  ;; => "<forms:  (defn analyze-data [data]    (let [processed (mapv process-item data) ; Changed map to mapv for vector output          results (reduce combine-results {} processed)]      results))  >"
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


(comment

  (let [{:keys [base left right]} (example 3)

        base (edn/read-string base)
        left (edn/read-string left)
        right (edn/read-string right)

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
    (println result))

;; [[[:deps org.clojure/core.async :mvn/version] :r "1.3.612"]]
;; [[[:deps clj-http] :+ #:mvn{:version "3.11.0"}]]
;; [[[:deps org.clojure/core.async :mvn/version] :r "1.3.612"] [[:deps clj-http] :+ #:mvn{:version "3.11.0"}]]
;; {:deps {org.clojure/clojure #:mvn{:version "1.10.1"}, org.clojure/core.async #:mvn{:version "1.3.612"}, clj-http #:mvn{:version "3.11.0"}}}
;; {:deps {org.clojure/clojure #:mvn{:version 1.10.1}, org.clojure/core.async #:mvn{:version 1.3.612}, clj-http #:mvn{:version 3.11.0}}}

  #_end_comment)