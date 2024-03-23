(ns kurtharriger.clj-mergetool
  (:require [clojure.pprint :refer [pprint]]
            [rewrite-clj.zip :as z]
            [rewrite-clj.node :as n]
            [rewrite-clj.parser :as p]
            [editscript.core :as e]
            [babashka.cli :as cli]
            [babashka.fs :as fs]
            [kurtharriger.clj-mergetool.patch :as patch])
  (:gen-class))


(defn init [op base-filename left-filename right-filename]
  {:op    (if-not (keyword? op) (keyword op) op)
   ;:output-file base-filename
   :base  {:filename base-filename}
   :left  {:filename left-filename}
   :right {:filename right-filename}})

(defn resolve-input [ctx]
  (-> ctx
      (update-in [:base :filename] #(when % (str (fs/absolutize %))))
      (update-in [:left :filename] #(when % (str (fs/absolutize %))))
      (update-in [:right :filename] #(when % (str (fs/absolutize %))))
      (update-in [:output-file] #(when % (str (fs/absolutize %))))))

(defn parse [ctx]
  (reduce
   (fn [ctx k]
     (let [filename (get-in ctx [k :filename])]
       (assoc-in ctx [k :parsed]
                 (when filename (p/parse-file-all filename)))))
   ctx [:base :left :right]))

(defn diff
  [ctx]
  (let [base (some-> ctx :base :parsed n/sexpr)
        mkdiff (partial e/diff base)
        editscripts (->> (map #(some-> ctx % :parsed n/sexpr mkdiff) [:left :right])
                         (filter (complement nil?))
                         (seq))
        editscript (when editscripts (reduce e/combine editscripts))
        edits (when editscript (e/get-edits editscript))

        ;; todo: this is check is not very good
        ;; since editscript replace ops and add/remove
        ;; ops on same structure may have different paths
        {conflicts true}
        (group-by (fn [v] (> (count v) 1))
                  ; more than one edit at same path
                  (map second (group-by first edits)))]
    (assoc ctx
           :diff edits
           :editscript editscript
           :conflicts conflicts)))

(defn mergetool [{:keys [op base editscript conflicts] :as ctx}]
  (if-not (and (= :merge op) (empty? conflicts))
    ctx
    (let [base (-> base :parsed)
          _ (prn (type base) (type editscript))
          patched (patch/patch base editscript)
          result (n/string patched)]
      (assoc ctx :merged result))))


(defn output [{:keys [op output-file merged conflicts diff] :as ctx}]
  (cond
    (= op :diff)
    (do
      (when diff (pprint diff))
      (assoc ctx :exit-code 0))

    (and (= op :merge) conflicts)
    (do
    ;; todo write merged with conflicts
      (when conflicts (pprint conflicts))
      (assoc ctx :exit-code 1))

    (= op :merge)
    (do (if output-file (spit output-file merged)
            (when merged (println merged)))
        (assoc ctx :exit-code 0))))


(defn -main [& [op base left right]]
  (System/exit
   (-> (init op base left right)
       (resolve-input)
       ;(#(do (pprint %) %))
       (parse)
       (diff)
       (mergetool)
       (output)
       :exit-code)))


(comment
  (-> (init :merge
            "test/kurtharriger/examples/ex3/base.clj"
            "test/kurtharriger/examples/ex3/left.clj"
            "test/kurtharriger/examples/ex3/right.clj")
      (resolve-input)
      (parse)
      (diff)
      (mergetool)
      (output)))