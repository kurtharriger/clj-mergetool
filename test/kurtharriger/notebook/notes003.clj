(ns kurtharriger.notebook.notes003
  (:require [editscript.diff.a-star :as astar]
            [clojure.tools.reader.edn :as edn]
            [editscript.core :as e]
            [editscript.edit :as edit]
            [rewrite-clj.zip :as zip]
            [rewrite-clj.node :as n]
            [rewrite-clj.parser :as p]
            [kurtharriger.util.simulator :refer [example]]
            [rewrite-clj.zip :as z]))


(let [{:keys [base left right]} (example 2)
      base (edn/read-string base)
      right (edn/read-string right)
      right-diff (e/diff base right)]
  (tap> base)
  (tap> right)
  (pr-str right-diff))
;; => "[[[3 1 1 0] :r mapv]]"



(let [{:keys [base left right]} (example 2)
      base (p/parse-string-all base)
      z (zip/of-node base)


;; => "[[[3 1 1 0] :r mapv]]"
      z (z/down z)

;; => "[[3 1 1 0] :r mapv]"
      z (z/right z)
      z (z/right z)
      z (z/right z)

;; => "[[x 1 1 0] :r mapv]"
      z (z/down z)
      z (z/right z)

;; => "[[x x 1 0] :r mapv]"
      z (z/down z)
      z (z/right z)

;; => "[[x x x 0] :r mapv]"
      z (z/down z)

      z (z/edit z (fn [_] (n/token-node (symbol "mapv"))))

;; done
      n (zip/root-string z)]
  (tap> n)
  (pr-str n))
