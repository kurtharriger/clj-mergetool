(ns kurtharriger.notebook.notes012
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

; ----------
(defn map-add-value [zipper key value]
  (-> zipper
      (z/append-child key)
      (z/append-child value)))

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
  (if (or (nil? index) (>= index (count (children zipper))))
    (-> zipper (z/append-child value))
    (-> zipper
        (move-to-index index)
        (z/insert-left value))))

(defn replace-map-value [zipper key value]
  (-> zipper
      (move-to-map-key key)
      (z/right)
      (z/replace value)))

(defn replace-set [zipper value]
  (z/replace zipper value))

(defn replace-seq-value [zipper index value]
  (if (nil? index) (z/replace zipper value)
      (-> zipper
          (move-to-index index)
          (z/replace value))))

(defn find-set-item [zipper key]
  (->> (children zipper)
       (drop-while (complement #(= key (z/sexpr %))))
       (first)))

(defn remove-map-key [zipper key]
  (-> zipper
      (move-to-map-key key)
      (z/right)
      (z/remove)
      (z/remove)))

(defn remove-set-value [zipper key]
  (-> zipper
      (find-set-item  key)
      (z/remove)))
(defn remove-seq-value [zipper index]
  (-> zipper
      (move-to-index index)
      (z/remove)))
;-----------

(ns-unmap *ns* 'add-value)
(defmulti add-value
  (fn [tag zipper key value]
    tag))

(defmethod add-value :map [_ zipper key value]
  (map-add-value zipper key value))

(defmethod add-value :set [_ zipper key value]
  (set-add-value zipper key value))

(defmethod add-value :default [_ zipper key value]
  (seq-add-value zipper key value))


;; (defmethod replace-value :map [zipper key value]
;;   (add-map-value zipper key value))

;; (defmethod replace-value :set [zipper key value]
;;   (add-set-value zipper key value))

;; (defmethod replace-value :default [zipper key value]
;;   (add-seq-value zipper key value))



;(ns-unmap *ns* 'patch)

(defmulti patch (fn [zipper zedit] (second zedit)))
(defmethod patch :default [zipper [path op key value :as zedit]]
  (throw (ex-info "No patch method" {:zedit zedit})))

(defmethod patch :+ [zipper [path op key value :as zedit]]
  (let [zipper (focus* zipper path)]
    (add-value (z/tag zipper) zipper  key value)))



(defmethod patch :r [zipper [path op key value :as zedit]]
  (let [zipper (focus* zipper path)]
    (cond
      (= (z/tag zipper) :map)
      (replace-map-value zipper key value)

;; when editscript wants to replace a set
      (= (z/tag zipper) :set)
      (do (assert (nil? key) "key unexpected when replacing entire set")
          (replace-set zipper value))

      :else
      ;; nil replaces entire object (likely empty)
      ;; index replaces item at that index
      (replace-seq-value zipper key  value))))


(defmethod patch :- [zipper [path op key value :as zedit]]
  (let [zipper (focus* zipper path)]
    (cond
      (= (z/tag zipper) :map)
      (remove-map-key zipper key)

      (= (z/tag zipper) :set)
      (remove-set-value zipper key)

      :else
      (remove-seq-value zipper key))))


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
                #{:a} [:a]
   ;; end
                ]))

(zedits {:a :A} {:b :B})
(-> {:a :A}
    (zipper)
    (patch (first (zedits {:a :A} {:b :B})))
    (z/root-string))
(def results
  (for [[pre post] tests
        :let [zedits' (zedits pre post)
              edits' (edits pre post)
              zipper' (zipper pre)
              patched (try
                        (-> (reduce patch zipper' zedits')
                            (z/root)
                            (n/sexpr))
                        (catch Exception e e))]]
    {:pre pre
     :post post
     :edits edits'
     :zedit zedits'
     :op (second (first edits'))
     :focus (try (z/tag (focus* zipper' (butlast (ffirst edits')))) (catch Exception e nil))
     :efocus (try (z/tag (focus* zipper' (ffirst edits'))) (catch Exception e nil))
     :zfocus (try (z/tag (focus* zipper' (ffirst zedits'))) (catch Exception e nil))
     :patched patched
     :fail (not= post patched)}))

(clojure.pprint/print-table [:focus :op :pre :post :edits :zedits] (filter #(= :r (:op %)) results))

(clojure.pprint/print-table (filter :fail results))
;(assert (= 0 (count (filter :fail results))))

;; I may want to revisit my focus implementation
;; it seems odd that I have a replace implementation
;; for sets when editscript mostly represents a change
;; as remove and add.
(edits #{:a} #{:b}) ;; => [[[:a] :-] [[:b] :+ :b]]
;; since the opeation targets the set rather than
;; an item in the set so using butlast when focusing
;; in maps and sets
(z/tag (focus* (zipper #{:a}) (butlast [:a])))

;; howver when there are no elements in common
;; or the type changes it is a replace
(edits #{:a} #{:b :c}) ;; => [[[] :r #{:c :b}]]
(edits #{:a} {}) ;; => [[[] :r {}]]

;; although this initially semes reasonable
;; why is focus on set rather than on the parent
;; container. when the item is in a vector the
;; operation is on the vector postion
(edits [#{:a}] [[:a]]) ;; => [[[0] :r [:a]]]
;; as in we rewrite this to at [] seq-replace 0 [:a]
(zedits [#{:a}] [[:a]]) ;; => [[[0] :r [:a]]]
(z/tag (focus* (zipper [#{:a}]) [])) ;; => :vector

; the reason I see replace on set is that the operation
;; is actually on the root node and the root node is a set
(z/tag (focus* (zipper #{:a}) (butlast []))) ;; => :set
(z/tag (focus* (zipper #{:a}) nil)) ;; => :set
(z/tag (zipper #{:a})) ;; => :set

;; and thus we need replace operations that are type specific
;; with odd cases eg to replace vector the key is nil
;; what I proablaby want is a psuedo :root element type
;; when the path is empty
(edits [] {}) ;; => [[[] :r {}]]
(zedits [] {}) ;; => [[[] :r nil {}]]
(z/tag (focus* (zipper []) nil))

