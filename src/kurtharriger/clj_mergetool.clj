(ns kurtharriger.clj-mergetool
  (:require [clojure.pprint :refer [pprint]]
            [clojure.string :as string]
            [rewrite-clj.zip :as z]
            [rewrite-clj.node :as n]
            [rewrite-clj.parser :as p]
            [editscript.core :as e]
            [meander.epsilon :as m]
            [babashka.cli :as cli]
            [babashka.fs :as fs]
            [clojure.java.io :as io]
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
  (let [base (some-> ctx :base :parsed)
        mkdiff (partial patch/diff base)
        editscripts (->> (map #(some-> ctx % :parsed mkdiff) [:left :right])
                         (filter (complement nil?))
                         (seq))
        editscript (when editscripts (reduce e/combine editscripts))
        edits (when editscript (distinct (e/get-edits editscript)))

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

(defn patch [{:keys [op base editscript] :as ctx}]
  (if-not (and (= :merge op))
    ctx
    (let [base (-> base :parsed)
          patched (patch/patch base editscript)
          result (n/string patched)]
      (assoc ctx :merged result))))


(defn output [{:keys [op output-file merged conflicts diff] :as ctx}]
  (cond
    (= op :diff)
    (do
      (when diff (prn diff))
      (assoc ctx :exit-code 0))

    (= op :merge)
    (do (if output-file (spit output-file merged)
            (when merged (println merged)))
        (if conflicts
          (binding [*out* *err*]
            (println "Potentially conflicting edits at same location")
            (prn conflicts)
            (assoc ctx :exit-code 1))
          (assoc ctx :exit-code 0)))))


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

(defn unmerged-files-from-ls-files [stdout]
  ;; files deleted by us or them are still reported as unmerged
  ;; only keep unmerged files with all 3 versions
  ;; see test-unmerged-files-from-ls-files
  (->> (frequencies
        (for [line (string/split-lines stdout)]
          (last (string/split line #"\s+" 4))))
       (filter #(= 3 (val %)))
       (map first)
       (not-empty)))

(defn unmerged-files [{dir :dir}]
  (-> (sh {:dir dir} "git ls-files -u")
      (check)
      (:out)
      (not-empty)
      (unmerged-files-from-ls-files)))

(defn load-from-index [{dir :dir :as opts} & files]
  (let [parse (fn [file n]
                (-> (sh opts "git" "show" (str ":" n ":" file))
                    (check)
                    (:out)
                    (patch/parse-string-all)))
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
  (load-from-index {:opts {:dir "local/test/cljc"}})
  (load-from-index {:dir "local/ex"} "base.clj")

 ;;
  )

(defn remerge [{{files :files dir :dir} :opts}]
  (let [results (->> (apply load-from-index {:dir dir} files)
                     (map #(assoc % :op :merge))
                     (map diff)
                     (map patch)
                     (map output))
        exit-code (if (not-empty results) (apply max (map :exit-code results)) 0)]
    {:exit-code exit-code
     :results results}))


(defn show-diff [{{files :files dir :dir} :opts}]
  (let [results (->> (apply load-from-index {:dir dir} files)
                     (map #(assoc % :op :diff))
                     (map diff)
                     (map output))
        exit-code (if (not-empty results) (apply max (map :exit-code results)) 0)]
    {:exit-code exit-code
     :results results}))


(comment
 ;;
  (load-from-index {:dir "local/db"})

  (remerge {:dir "local/db" })
  (remerge {:dir "local/db" :files ["local/db/src/clj/fluree/db/conn/ipfs.cljc"]})

  ;;
  )

(defn help [dispatch-args]
  (println "Usage: clj-mergetool remerge [filename ...]"))


; In native image (slurp (io/resource "VERSION")) throws
;  Exception in thread "main" java.lang.IllegalArgumentException: Cannot open <nil> as a Reader.
; not sure why so pulling version into source at compile time via macro
(defmacro read-version []
  (slurp (io/resource "VERSION")))

(defn show-version [_]
  (println (read-version)))

(defn -main [& args]
  (let [result (cli/dispatch
                [{:cmds ["mergetool"] :fn #'mergetool :spec {:output {:default :overwrite}} :args->opts [:ancestor :current :other]}
                 {:cmds ["remerge"] :fn #'remerge :spec {:files {:coerce []}} :args->opts [:files]}
                 {:cmds ["diff"] :fn #'show-diff :spec {:files {:coerce []}} :args->opts [:files]}
                 {:cmds ["version"] :fn #'show-version}
                 {:cmds [] :fn #'help}] args)]
    (when-let [ec (:exit-code result)]
      (System/exit ec))))


(comment
  (let [ctx (first (load-from-index {:dir "local/test/cljc"}))
        [base left] (m/match ctx {:base {:parsed ?b}
                                  :left {:parsed ?l}} [?b ?l])]
    (patch/diff base left))

  :rcf)