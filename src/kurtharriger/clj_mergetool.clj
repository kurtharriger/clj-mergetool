(ns kurtharriger.clj-mergetool
  (:require [rewrite-clj.zip :as z]
            [rewrite-clj.node :as n]
            [rewrite-clj.parser :as p]
            [editscript.core :as e]
            [meander.epsilon :as m]
            [babashka.cli :as cli])
  (:gen-class))


(defn -main [& [op base left right ec]]
  (println :op op)
  (println :base base)
  (println :left left)
  (println :right right)
;; indicate a conflict
  (println :ec (parse-long ec))
  (System/exit (parse-long ec)))
