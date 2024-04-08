(ns known-issues.reader-conditional-test
  (:require
   [clojure.pprint :as pprint]
   [clojure.test :refer :all]
   [editscript.core :as e]
   [rewrite-clj.node :as n]
   [rewrite-clj.parser :as p]))

(comment
  ;; todo: figure out how to support cljs reader
  ;; conditionals

  ;; current implementation is using editscript
  ;; on clojure forms and then applying the edits
  ;; to a rewrite-clj zipper to preserve whitespace
  ;;
  ;; although rewrite-clj can preserve these
  ;; forms as rewrite-clj nodes the conversion
  ;; to clojure forms using n/sexpr here is lossy

  ;; fixing this probably means updating
  ;; editscript so that it can diff rewrite-clj
  ;; nodes rather than clojure forms

  (-> "#?(:clj 1  :cljs 2)"
      (p/parse-string)
      (n/string))
;; => "#?(:clj 1  :cljs 2)"


  (-> "#?(:clj 1  :cljs 2)"
      (p/parse-string)
      (n/sexpr)
      (n/coerce)
      (n/string))
;; => "(read-string \"#?(:clj 1  :cljs 2)\")"
  )