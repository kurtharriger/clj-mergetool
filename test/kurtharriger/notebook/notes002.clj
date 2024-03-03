(ns kurtharriger.util.simulator
  (:require [clojure.string :as str :refer [triml]]
            [editscript.core :as e]
            [clojure.tools.reader.edn :as edn]
            [rewrite-clj.zip :as z]
            [rewrite-clj.node :as n]
            [rewrite-clj.parser :as p]
            [kurtharriger.util.simulator :refer :all]))


;; can we extend edits script to support diffing of clj-rewrite nodes?


