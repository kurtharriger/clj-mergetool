(ns kurtharriger.notebook.notes004
  (:require
   [kurtharriger.util.simulator :refer [example]]
   [editscript.diff.a-star :as astar]
   [clojure.tools.reader.edn :as edn]
   [editscript.core :as e]
   [editscript.edit :as edit]
   [rewrite-clj.zip :as zip]
   [rewrite-clj.node :as n]
   [rewrite-clj.parser :as p]
   [rewrite-clj.zip :as z]
   [meander.epsilon :as m]))


(let [{:keys [base left right]} (example 2)
      base (edn/read-string base)
      right (edn/read-string right)
      right-diff (e/diff base right)]
  (tap> base)
  (tap> right)
  (pr-str right-diff))
;; => "[[[3 1 1 0] :r mapv]]"


;; manual translation of the above diff
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

      ;; also rename process-item to process-data
      ;; just for another step
      z (z/right z)
      z (z/edit z (constantly (symbol "process-data")))

      ;; z (z/up z)
       ;;_ (tap> (z/string z))
;; done

      _ (tap> z)
      n (zip/root-string z)]
  (pr-str n))

;; lets learn meander
(m/match
 '[[3 1 1 0] :r mapv]
  [?path :r ?replacement]
  ['zreplace ?path ?replacement])

(m/match
 '[[3 1 1 0] :r mapv]
  [?path :r ?replacement]
  `('zreplace ~?path ~?replacement))

(m/search
 '[[[3 1 1 0] :r mapv]
   [[3 1 1 1] :r process-data]]
 (m/scan [?path :r ?replacement])
 `('zreplace ~?path ~?replacement))

;; lets change path to zipper movements
(defn zpath [path]
  (let [down (for [nav path
                   r (cons :down
                           (cond
                             (number? nav) (repeat nav :right)
                                                 ;; todo keyword nav in map
                             :else (throw (ex-info "not implemented" {}))))]
               r)
        up (repeat (count path) :up)]
    [down up]))


(zpath [3 1 1 0])
;; => [(:down :right :right :right :down :right :down :right :down) (:up :up :up :up)]

(zpath [0])
;; => [(:down) (:up)]

(zpath [:a])
;; => Error printing return value (ExceptionInfo) at kurtharriger.notebook.notes004/zpath$iter$fn (NO_SOURCE_FILE:90).
;;    not implemented

;; run down or up of zpath
(defn run-zpath* [z zpath*]
  (reduce (fn [z nav]
            (condp  = nav
              :down (z/down z)
              :right (z/right z)
              :up (z/up z))) z zpath*))


(defn at-zpath [z [down up] f]
  (-> z
      (run-zpath* down)
      (f)
      (run-zpath* up)))

;; lets try run-nav
(let [{:keys [base left right]} (example 2)
      base (p/parse-string-all base)
      z (zip/of-node base)

;; => "[[3 1 1 0] :r mapv]"
;;      [down up] (zpath [3 1 1 0])

      z (at-zpath z (zpath [3 1 1 0])
                  (fn [z] (z/edit z n/token-node (symbol "mapv"))))

      z (at-zpath z (zpath [3 1 1 1])
                  (fn [z] (z/edit z n/token-node (symbol "process-data"))))

      n (zip/root-string z)]
  (pr-str n))
;; => "\"(defn analyze-data [data]\\n  (let [processed (mapv process-data data)\\n        results (reduce combine-results {} processed)]\\n    results))\\n\""



