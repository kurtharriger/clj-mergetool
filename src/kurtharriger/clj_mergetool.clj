(ns kurtharriger.clj-mergetool
  (:require [clojure.pprint :refer [pprint]]
            [rewrite-clj.zip :as z]
            [rewrite-clj.node :as n]
            [rewrite-clj.parser :as p]
            [editscript.core :as e]
            [meander.epsilon :as m]
            [babashka.cli :as cli]
            [editscript.diff.a-star :as a]
            [babashka.fs :as fs])
  (:gen-class))


(defn init [op base-filename left-filename right-filename]
  {:op    (if-not (keyword? op) (keyword op) op)
   :output-file base-filename
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
        diffs (->> (map #(some-> ctx % :parsed n/sexpr mkdiff) [:left :right])
                   (filter (complement nil?))
                   (seq))
        diff (when diffs (reduce e/combine diffs))
        diff (when diff (e/get-edits diff))

        {conflicts true patch false}
        (group-by (fn [v] (> (count v) 1))
                  ; more than one edit at same path
                  (map second (group-by first diff)))]
    (assoc ctx
           :diff diff
           :patch patch
           :conflicts conflicts)))


(defn move-to-map-key [zipper key]
  (->> zipper
       (z/down)
       (iterate z/right)
       (take-while (complement z/end?))
       (partition 2)
       (filter (fn [[kz]] (= key (z/sexpr kz))))
       (ffirst)))

(defn move-to-map-value [zipper key]
  (-> zipper
      (move-to-map-key key)
      (z/right)))

(defn move-to-index [zipper n]
  (first (drop n (iterate z/right (z/down zipper)))))

(defn edit [zipper patch]
  ;; todo implement edit
  (println "edit not implemeted:")
  (println (pr-str patch))
  zipper)

(defn mergetool [{:keys [op base diff conflicts] :as ctx}]
  (if-not (and (= :merge op) (empty? conflicts))
    ctx
    (let [zipper (-> base :parsed z/of-node)
          patched (reduce edit zipper diff)
          result (z/root-string patched)]
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
       (parse)
       (diff)
       (mergetool)
       (output)
       :exit-code)))


