(ns kurtharriger.notebook.notes011
  (:require
   [clojure.spec.alpha :as s]
   [clojure.spec.gen.alpha :as gen]
   [editscript.core :as e]
   [rewrite-clj.zip :as z]
   [rewrite-clj.parser :as p]
   [clojure.zip :as zip]
   [meander.epsilon :as m]
   [rewrite-clj.node :as n]))

;; pulling together latest implementation ensuring all the latest bits
;; work.

(defn edits [base left]
  (let [edit-script (e/diff base left)]
    (assert (= left (e/patch base edit-script)))
    (e/get-edits edit-script)))

(defn move-to-map-key [zipper key]
  (let [kloc (->> zipper
                  (z/down)
                  (iterate z/right)
                  (take-while (complement z/end?))
                  (partition 2)
                  (filter (fn [[kz]] (= key (z/sexpr kz))))
                  (ffirst))]
    (or kloc
        (-> zipper
            (z/append-child key)
            (z/down)
            (z/rightmost)))))


(defn pr-str-meta [node]
  (binding [*print-meta* true]
    (pr-str node)))

(defn zipper [node]
  (vary-meta
   (cond
     (::zipper   (meta node)) node
     (nil? node) (z/of-node (n/forms-node []))
     :else       (z/of-string (pr-str-meta node)))
   assoc ::zipper true))

(defn zedit [edit]
  (-> (m/match edit
        [?path :+ ?value] [(vec (butlast ?path)) :+ (last ?path) ?value]
        [?path :-]  [(vec (butlast ?path)) :- (last ?path)]
        [?path :r ?value] [(vec (butlast ?path)) :r (last ?path) ?value])
      (vary-meta assoc :zedit/edit edit)))

(defn zedits [base left]
  (mapv zedit (edits base left)))

(defn children [zipper]
  (->> zipper
       (z/down)
       (iterate z/right)
       (take-while (complement z/end?))))

(defn move-to-index [zipper n]
  (nth (children zipper) n))

;; z/root returns a node not a zipper
(defn move-root [zipper]
  (z/of-node (z/root zipper)))

(defn focus* [zipper path]
  (reduce (fn [zipper p]
            (cond
              (= (z/tag zipper) :map) (move-to-map-key zipper p)
              (number? p)   (move-to-index zipper p)))
          (move-root zipper) path))

;(ns-unmap *ns* 'patch)

(defmulti patch (fn [zipper zedit] (second zedit)))
(defmethod patch :default [zipper [path op key value :as zedit]]
  (throw (ex-info "No patch method" {:zedit zedit})))

(defmethod patch :+ [zipper [path op key value :as zedit]]
  (let [zipper (focus* zipper path)]
    (cond
      (= (z/tag zipper) :map)
      (-> zipper
          (z/append-child key)
          (z/append-child value))

      ;; note: edit-script always returns edits to set at end
      ;; and in reverse, for now just ignore position and insert
      ;; at the end
      ;; (edits #{0 1} #{0 2 3 1}) ;; => [[[3] :+ 3] [[2] :+ 2]]
      (= (z/tag zipper) :set)
      (z/append-child zipper value)

      :else
      ;; index returned for add at target positon not position to into
      ;; eg (edits [0] [0 1]) ;; => [[[1] :+ 1]]
      ;; eg (edits [1] [0 1]) ;; => [[[0] :+ 0]]
      ;;  edit-script on empty vector represented as replace not add
      ;; (edits [] [0]) ;; => [[[] :r [0]]]
      ;; in future I may rewrite [[[] :r [0]]]
      ;; as [[[] :+ 0 0]] as 2 adds can be merged
      ;; (still w/ conflict as order is unclear)
      ;; but 2 replaces operations cannot be merged
      (if (or (nil? key) (>= key (count (children zipper))))
        (-> zipper (z/append-child value))
        (-> zipper
            (move-to-index key)
            (z/insert-left value))))))

(defmethod patch :r [zipper [path op key value :as zedit]]
  (let [zipper (focus* zipper path)]
    (cond
      (= (z/tag zipper) :map)
      (-> zipper
          (move-to-map-key key)
          (z/right)
          (z/replace value))

      ;; edit-script deletes and readds indecies within a set
      ;; thus key is always nil
      ;; (zedits #{0} #{1}) ;; => [[[] :- 0] [[] :+ 1 1]]
      ;; in future index would be useful for set as even though
      ;; set is semantically unordered the textural representation
      ;; might be ordered
      (= (z/tag zipper) :set)
      (do (assert (nil? key) "unexpected index for set")
          (z/replace zipper value))

      :else
      ;; nil replaces entire object (likely empty)
      ;; index replaces item at that index
      (if (nil? key) (z/replace zipper value)
          (-> zipper
              (move-to-index key)
              (z/replace value))))))



(defn find-set-item [zipper key]
  (->> (children zipper)
       (drop-while (complement #(= key (z/sexpr %))))
       (first)))

(defmethod patch :- [zipper [path op key value :as zedit]]
  (let [zipper (focus* zipper path)]
    (cond
      (= (z/tag zipper) :map)
      (-> zipper
          (move-to-map-key key)
          (z/right)
          (z/remove)
          (z/remove))

      ;; edit-script deletes and readds indecies within a set
      ;; thus key is always nil
      ;; (zedits #{0} #{1}) ;; => [[[] :- 0] [[] :+ 1 1]]
      ;; in future index would be useful for set as even though
      ;; set is semantically unordered the textural representation
      ;; might be ordered
      (= (z/tag zipper) :set)
      (do (assert (not (nil? key)) "expected set element to remove")
          (-> zipper
              (find-set-item  key)
              (z/remove)))

      :else
      (do
        (assert (not (nil? key)) "expecting index to remove")
        (-> zipper
            (move-to-index key)
            (z/remove))))))

(def tests
  (partition 2 [nil []
                nil [:a]
                nil [:a :b]

                [] [:a]
                [] [:a :b]

                [:a] [:b]
                [:a] [:a :b]
                [:a] [:b :a]
                [:a] [:b :c]

                [:b] [:a :b]
                [:b] [:b :a]

                [:a] [:a :b :c]
                [:b] [:a :b :c]
                [:c] [:a :b :c]

                [:a :b] [:b :c]
                [:a :b] [:c :a]
                [:b :c] [:c :b]

                [:a :b] [:a :b :c]
                [:b :c] [:a :b :c]
                [:a :c] [:a :b :c]

                [:a :b] [:c :b :a]
                [:b :c] [:c :b :a]
                [:a :c] [:c :b :a]

                [:a :b] nil
                [:a :b] []
                [:a :b] [:a]
                [:a :b] [:b]
                [:a :b] [:b :a]

                [:a :b :c] [:b :c]
                [:a :b :c] [:a :b]
                [:a :b :c] [:a :c]

                nil #{}
                nil #{:a}
                nil #{:a :b}

                #{} #{:a}
                #{} #{:a :b}

                #{:a} #{:b}
                #{:a} #{:a :b}
                #{:a} #{:b :a}
                #{:a} #{:b :c}

                #{:b} #{:a :b}
                #{:b} #{:b :a}

                #{:a} #{:a :b :c}
                #{:b} #{:a :b :c}
                #{:c} #{:a :b :c}

                #{:a :b} #{:b :c}
                #{:a :b} #{:c :a}
                #{:b :c} #{:c :a}
                #{:b :c} #{:c :b}

                #{:a :b} #{:c :b :a}
                #{:b :c} #{:c :b :a}
                #{:a :c} #{:c :b :a}

                #{:a :b} nil
                #{:a :b} #{}
                #{:a :b} #{:a}
                #{:a :b} #{:b}
                #{:a :b} #{:b :a}

                nil {}
                nil {:a :A}
                nil {:a :A :b :B}

                {} {:a :A}
                {} {:a :A :b :B}

                {:a :A} {:b :B}

                {:a :A} {:a :A :b :B}
                {:b :B} {:a :A :b :B}

                {:a :A} {:a :A :b :B :c :C}
                {:b :B} {:a :A :b :B :c :C}

                {:a :A :b :B} {:a :A :b :B :c :C}
                {:b :B :c :C} {:a :A :b :B :c :C}
                {:a :A :c :C} {:a :A :b :B :c :C}

                {:a :A :b :B :c :C} {:a :A :b :B}
                {:a :A :b :B :c :C} {:b :B}

                {:b :B :c :C} {:b :B :a :A}
                {:a :A :b :B} {}
                {:a :A :b :B} nil
                {:a :A} nil

                [] [[]]
                [] [[:a]]
                [] [[:a :b]]
                [] [[:a] [:b]]

                [[:a]] [[:a :b]]
                [[:a]] [[:a] [:b]]
                [[:a] [:b]] [[:b] [:a]]

                [#{:a}] [#{:a} #{:b}]
                [#{:a}] [#{:b} #{:a}]
                [#{:a}] [#{:b}]
                [#{:a} #{:b}] [#{:b}]
                [#{:a} #{:b}] [#{:a}]
                [#{:a} #{:b}] []

                [:a] #{:a}
   ;; end
                ]))

(zedits {:a :A} {:b :B})
(-> {:a :A}
    (zipper)
    (patch (first (zedits {:a :A} {:b :B})))
    (z/root-string))
(def results
  (for [[pre post] tests
        :let [zedits' (try (zedits pre post) (catch Exception e e))

              patched (try
                        (-> (reduce patch (zipper pre) zedits')
                            (z/root)
                            (n/sexpr))
                        (catch Exception e e))]]
    {:pre pre
     :post post
     :zedit zedits'
     :patched patched
     :fail (not= post patched)}))

(clojure.pprint/print-table results)

;(assert (= 0 (count (filter :fail results))))


;; seems like all the basics are working now
;; excepting ordering of elements in sets and maps
;; that are programatically equivalent but textually
;; different: #{:a :b} and #{:b :a}