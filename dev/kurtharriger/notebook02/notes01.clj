(ns kurtharriger.notebook02.notes01
  (:require [clojure.test :as test]
            [clojure.pprint :as pprint]
            [rewrite-clj.zip :as z]
            [rewrite-clj.node :as n]
            [rewrite-clj.parser :as p]
            [rewrite-clj.node.protocols :as protocols]
            [editscript.core :as e]
            [clojure.string :as str]
            [kurtharriger.notebook02.tests01 :as t]))

(defn zipper
  "Coerce provided value into rewrite-clj zipper.

   In clojure code and tests it is often convienent to use
   clojure forms as follows, but note that the order is not
   preservered by the clojure reader as it is symantically
   irrelvant to the data.

   (z/root-string (zipper #{:a :b}))
   ;; => \"#{:b :a}\"

   However when manipulating textual representation of
   code we want to preserve the existing order.

   To preserve order provide a rewrite-clj node parsed
   from a string or a file

   (-> \"#{:a :b}\"
      (p/parse-string)
      (zipper)
      (z/root-string))
   ;; => \"#{:a :b}\"

   "
  [node]
  (vary-meta
   (if (::zipper (meta node)) node
       (z/of-node (n/coerce node)))
   assoc ::zipper true))

(comment
  (-> nil
      (n/coerce)
      (z/of-node))

  (-> #{:a :b}
      (zipper)
      (z/root-string))
  ;; => "#{:b :a}"


  (-> "#{:a :b}"
      (p/parse-string)
      (zipper)
      (z/root-string))
  ;; => "#{:a :b}"
  )

(defn children [zipper]
  (->> zipper
       (z/down)
       (iterate z/right)
       (take-while (complement z/end?))))

;; z/root returns a node not a zipper
(defn move-root [zipper]
  (z/of-node (z/root zipper)))

(ns-unmap *ns* 'move-to-child)
(defmulti move-to-child
  (fn [zipper id] (z/tag zipper)))


(defmethod move-to-child :map [zipper id]
  (->> zipper
       (children)
       (partition 2)
       (filter (fn [[kz]] (= id (z/sexpr kz))))
       (first)
       (second)))

(defn move-to-index [zipper n]
  (nth (children zipper) n))

;; list or vector
(defmethod move-to-child :default [zipper id]
  (move-to-index zipper id))

(defn focus [zipper path]
  (reduce move-to-child zipper path))

(comment
  (-> (zipper nil)
      (focus  [])
      (z/tag))

  ;; => :token

  (-> (zipper [{}])
      (focus [])
      (z/tag))
  ;; => :vector

  (-> (zipper [{}])
      (move-to-child 0)
      (z/tag))
   ;; => :map

  (-> (zipper [{}])
      (focus [0])
      (z/tag))

   ;; => :map

  (-> (zipper [:a])
      (move-to-child 0)
      (z/sexpr))
  ;; => :a


  (-> (zipper [{}])
      (focus [0])
      (z/sexpr))
  ;; => {}
  )



(do
  (test/deftest test-move-to-child
    (test/are [node path f val]
              (= val (f (focus (zipper node) path)))

      nil [] z/tag :token
      []  [] z/tag :vector
      [{}] [0] z/tag :map
      {:a []} [:a] z/tag :vector

      {:a [{42 [:d]}]} [:a 0 42 0] z/sexpr :d))
  (test/run-tests))


;; ----------------------------
(defn map-add-value [zipper key value]
  (-> zipper
      (z/append-child key)
      (z/append-child value)))

(defn replace-map-value [zipper key value]
  (-> zipper
      (move-to-child key)
      (z/replace value)))

(defn set-add-value [zipper index value]
      ;; note: edit-script always returns edits to set at end
      ;; and in reverse, for now just ignore position and insert
      ;; at the end
      ;; (edits #{0 1} #{0 2 3 1}) ;; => [[[3] :+ 3] [[2] :+ 2]]
  (z/append-child zipper value))

(defn seq-add-value [zipper index value]
      ;; index returned for add at target positon not position to into
      ;; eg (edits [0] [0 1]) ;; => [[[1] :+ 1]]
      ;; eg (edits [1] [0 1]) ;; => [[[0] :+ 0]]
      ;;  edit-script on empty vector represented as replace not add
      ;; (edits [] [0]) ;; => [[[] :r [0]]]
      ;; in future I may rewrite [[[] :r [0]]]
      ;; as [[[] :+ 0 0]] as 2 adds can be merged
      ;; (still w/ conflict as order is unclear)
      ;; but 2 replaces operations cannot be merged
      ;; todo: I'm checking nil here but don't think that is needed
  (assert (some? index) "expecting index for seq insert")
  (if (or (nil? index) (>= index (count (children zipper))))
    (-> zipper (z/append-child value))
    (-> zipper
        (move-to-index index)
        (z/insert-left value))))


(defn replace-seq-value [zipper index value]
  (if (nil? index) (z/replace zipper value)
      (-> zipper
          (move-to-child index)
          (z/replace value))))

(defn find-set-item [zipper key]
  (->> (children zipper)
       (drop-while (complement #(= key (z/sexpr %))))
       (first)))

(defn remove-map-key [zipper key]
  (-> zipper
      (move-to-child key)
      (z/remove)
      (z/remove)))

(defn remove-set-value [zipper key]
  (-> zipper
      (move-to-child key)
      (z/remove)))
(defn remove-seq-value [zipper index]
  (-> zipper
      (move-to-index index)
      (z/remove)))
;-----------

(defn patch-dispatch  [zipper [path op & args]]
  (if (empty? path) [:root op]
      [(z/tag (focus zipper (butlast path))) op]))

(defmulti patch patch-dispatch)

;; empty patch (patch zipper nil)
(defmethod patch [:root nil] [zipper [path op value]]
  zipper)

(defmethod patch [:root :r] [zipper [path op value]]
  (-> zipper
      (focus (butlast path))
      (z/replace  value)
      (move-root)))

(->  nil
     (zipper)
     (patch [[] :r #{:a}])
     (move-root)
     (z/sexpr))

(->  #{:a}
     (zipper)
     (patch [[] :r #{:b}])
     (z/sexpr))

(def test-results
  (for [test t/tests
        :let [{:keys [pre post]} test
              editscript (e/diff (n/sexpr pre) (n/sexpr post))
              edit (first (e/get-edits editscript))
              z (zipper pre)
              dispatch (patch-dispatch z edit)
              patch (try
                      (-> (patch z edit)
                          (z/root-string)) (catch Exception e (.getMessage e)))]
        :when (= dispatch [:root :r])]
    {:pre (n/sexpr pre)
     :post (n/sexpr post)
     :edit edit
     :dispach dispatch
     :patch patch
     :str-fail (= (pr-str post) patch)}))

;; (doseq [result test-results]
;;   (println  result))
(pprint/print-table test-results)

(test/deftest root-replace
  (doseq [result test-results]
    (test/is (= (pr-str (:post result)) (:patch result)))))

(test/run-test root-replace)


#_(do
    (doseq [test t/tests
            :let [{:keys [pre post]} test
                  editscript (e/diff (n/sexpr pre) (n/sexpr post))
                  edit (first (e/get-edits editscript))
                  z (zipper pre)
                  dispatch (patch-dispatch z edit)]
            :when (= (first dispatch) :root)]
      (test/is (= (z/sexpr post) (z/sexpr (z/root (patch z edit)))))))


