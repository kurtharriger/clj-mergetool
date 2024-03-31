(ns kurtharriger.clj-mergetool
  (:require [clojure.pprint :refer [pprint]]
            [clojure.string :as string]
            [rewrite-clj.zip :as z]
            [rewrite-clj.node :as n]
            [rewrite-clj.parser :as p]
            [editscript.core :as e]
            [babashka.cli :as cli]
            [babashka.fs :as fs]
            [babashka.process :refer [sh check]]
            [kurtharriger.clj-mergetool.patch :as patch])
  (:gen-class))


(defn init [op base-filename left-filename right-filename]
  {:op    (if-not (keyword? op) (keyword op) op)
   :output-file left-filename
   :base  {:filename base-filename}
   :left  {:filename left-filename}
   :right {:filename right-filename}})

#_(defn resolve-input [ctx]
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

(defn patch [{:keys [op base editscript conflicts] :as ctx}]
  (if-not (and (= :merge op) (empty? conflicts))
    ctx
    (let [base (-> base :parsed)
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


(defn mergetool [{{:keys [ancestor current other output]} :opts dir :dir}]
  (-> {:op    :merge
       :base  {:filename ancestor}
       :left  {:filename current}
       :right {:filename other}
       :output (case output
                 :overwrite current
                 :stdout nil
                 (fs/path dir output))}
      (parse)
      (diff)
      (patch)
      (output)))

(defn unmerged-files [{dir :dir}]
  (when-let
   [out (-> (sh {:dir dir} "git diff --name-only --diff-filter=U")
            (check)
            (:out)
            (not-empty))]
    (string/split-lines out)))

(defn load-from-index [{dir :dir :as opts} & files]
  (let [parse (fn [file n]
                (-> (sh opts "git" "show" (str ":" n ":" file))
                    (check)
                    (:out)
                    (p/parse-string-all)))
        files* (or (not-empty files)
                   (unmerged-files opts))]
    (not-empty
     (for [file files*
           :let [filename (str (fs/path dir file))]]
       {:base {:filename filename
               :parsed (parse file 1)}
        :left {:filename filename
               :parsed (parse file 2)}
        :right {:filename filename
                :parsed (parse file 3)}
        :output-file filename}))))


(comment
  (unmerged-files {})
  (load-from-index {})
  (load-from-index {})

  (unmerged-files {:dir "local/ex"})
  (load-from-index {:dir "local/ex"})
  (load-from-index {:dir "local/ex"} "base.clj")

 ;;
  )

(defn remerge [{{files :files} :opts dir :dir}]
  (let [results (->> (apply load-from-index {:dir dir} files)
                     (map #(assoc % :op :merge))
                     (map diff)
                     (map patch)
                     (map output))
        exit-code (apply max (map :exit-code results))]
    {:exit-code exit-code
     :results results}))


(comment
 ;;
  (load-from-index {:dir "local/ex"})
  (remerge {:dir "local/ex" :files ["base.clj"]})

  ;;
  )

(defn help [dispatch-args]
  (println "Usage: clj-mergetool git remerge [filename ...]"))

(defn -main [& args]
  (let [result (cli/dispatch
                [{:cmds ["git" "mergetool"] :fn #'mergetool :spec {:output {:default :overwrite}} :args->opts [:ancestor :current :other]}
                 {:cmds ["git" "remerge"] :fn #'remerge :spec {:files {:coerce []}} :args->opts [:files]}
                 {:cmds [] :fn #'help}] args)]
    (when-let [ec (:exit-code result)]
      (System/exit ec))))


