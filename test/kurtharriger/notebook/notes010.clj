(ns kurtharriger.notebook.notes010
  {:nextjournal.clerk/visibility {:code :hide :result :hide}
   :nextjournal.clerk/auto-expand-results? true}
  [:require
   [editscript.core :as e]
   [rewrite-clj.zip :as z]
   [nextjournal.clerk :as clerk]])

(comment
  (clerk/serve! {:port 7778})

  (clerk/halt!))

(defn edits [base left]
  (let [edit-script (e/diff base left)]
    (assert (= left (e/patch base edit-script)))
    (e/get-edits edit-script)))

(defn eval-examples [& body]
  (for [[pre post] (partition 2 body)
        :let [edits' (edits pre post)
              ops (set (map second edits'))
              lossy (not= (pr-str (e/patch pre (e/diff pre post)))
                          (pr-str post))]]
    {:pre (pr-str pre)
     :post (pr-str post)
     :edits (pr-str edits')
     :lossy lossy
     :count (count edits')
     :ops ops
     :op (if (= 1 (count ops)) (first ops) :mixed)
     :type
     (cond
       (nil? pre) :nil
       (map? pre) :map
       (vector? pre) :vec
       (set? pre) :set
       :else (type pre))}))


(def examples
  (eval-examples
   nil []
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

   [:a] #{:a}))

(comment
  (clojure.pprint/print-table examples)

;;  |                  :pre |                 :post |                             :edits | :lossy | :count |     :ops |    :op | :type |
;;  |-----------------------+-----------------------+------------------------------------+--------+--------+----------+--------+-------|
;;  |                   nil |                    [] |                       [[[] :r []]] |  false |      1 |    #{:r} |     :r |  :nil |
;;  |                   nil |                  [:a] |                     [[[] :r [:a]]] |  false |      1 |    #{:r} |     :r |  :nil |
;;  |                   nil |               [:a :b] |                  [[[] :r [:a :b]]] |  false |      1 |    #{:r} |     :r |  :nil |
;;  |                    [] |                  [:a] |                     [[[] :r [:a]]] |  false |      1 |    #{:r} |     :r |  :vec |
;;  |                    [] |               [:a :b] |                  [[[] :r [:a :b]]] |  false |      1 |    #{:r} |     :r |  :vec |
;;  |                  [:a] |                  [:b] |                      [[[0] :r :b]] |  false |      1 |    #{:r} |     :r |  :vec |
;;  |                  [:a] |               [:a :b] |                      [[[1] :+ :b]] |  false |      1 |    #{:+} |     :+ |  :vec |
;;  |                  [:a] |               [:b :a] |                      [[[0] :+ :b]] |  false |      1 |    #{:+} |     :+ |  :vec |
;;  |                  [:a] |               [:b :c] |          [[[0] :+ :b] [[1] :r :c]] |  false |      2 | #{:r :+} | :mixed |  :vec |
;;  |                  [:b] |               [:a :b] |                      [[[0] :+ :a]] |  false |      1 |    #{:+} |     :+ |  :vec |
;;  |                  [:b] |               [:b :a] |                      [[[1] :+ :a]] |  false |      1 |    #{:+} |     :+ |  :vec |
;;  |                  [:a] |            [:a :b :c] |          [[[1] :+ :b] [[2] :+ :c]] |  false |      2 |    #{:+} |     :+ |  :vec |
;;  |                  [:b] |            [:a :b :c] |          [[[0] :+ :a] [[2] :+ :c]] |  false |      2 |    #{:+} |     :+ |  :vec |
;;  |                  [:c] |            [:a :b :c] |          [[[0] :+ :a] [[1] :+ :b]] |  false |      2 |    #{:+} |     :+ |  :vec |
;;  |               [:a :b] |               [:b :c] |             [[[0] :-] [[1] :+ :c]] |  false |      2 | #{:- :+} | :mixed |  :vec |
;;  |               [:a :b] |               [:c :a] |             [[[0] :+ :c] [[2] :-]] |  false |      2 | #{:- :+} | :mixed |  :vec |
;;  |               [:b :c] |               [:c :a] |             [[[0] :-] [[1] :+ :a]] |  false |      2 | #{:- :+} | :mixed |  :vec |
;;  |               [:b :c] |               [:c :b] |             [[[0] :-] [[1] :+ :b]] |  false |      2 | #{:- :+} | :mixed |  :vec |
;;  |               [:a :b] |            [:a :b :c] |                      [[[2] :+ :c]] |  false |      1 |    #{:+} |     :+ |  :vec |
;;  |               [:b :c] |            [:a :b :c] |                      [[[0] :+ :a]] |  false |      1 |    #{:+} |     :+ |  :vec |
;;  |               [:a :c] |            [:a :b :c] |                      [[[1] :+ :b]] |  false |      1 |    #{:+} |     :+ |  :vec |
;;  |               [:a :b] |            [:c :b :a] | [[[0] :+ :c] [[1] :+ :b] [[3] :-]] |  false |      3 | #{:- :+} | :mixed |  :vec |
;;  |               [:b :c] |            [:c :b :a] |          [[[0] :+ :c] [[2] :r :a]] |  false |      2 | #{:r :+} | :mixed |  :vec |
;;  |               [:a :c] |            [:c :b :a] | [[[0] :+ :c] [[1] :+ :b] [[3] :-]] |  false |      3 | #{:- :+} | :mixed |  :vec |
;;  |               [:a :b] |                   nil |                      [[[] :r nil]] |  false |      1 |    #{:r} |     :r |  :vec |
;;  |               [:a :b] |                    [] |                       [[[] :r []]] |  false |      1 |    #{:r} |     :r |  :vec |
;;  |               [:a :b] |                  [:a] |                         [[[1] :-]] |  false |      1 |    #{:-} |     :- |  :vec |
;;  |               [:a :b] |                  [:b] |                         [[[0] :-]] |  false |      1 |    #{:-} |     :- |  :vec |
;;  |               [:a :b] |               [:b :a] |             [[[0] :-] [[1] :+ :a]] |  false |      2 | #{:- :+} | :mixed |  :vec |
;;  |            [:a :b :c] |               [:b :c] |                         [[[0] :-]] |  false |      1 |    #{:-} |     :- |  :vec |
;;  |            [:a :b :c] |               [:a :b] |                         [[[2] :-]] |  false |      1 |    #{:-} |     :- |  :vec |
;;  |            [:a :b :c] |               [:a :c] |                         [[[1] :-]] |  false |      1 |    #{:-} |     :- |  :vec |
;;  |                   nil |                   #{} |                      [[[] :r #{}]] |  false |      1 |    #{:r} |     :r |  :nil |
;;  |                   nil |                 #{:a} |                    [[[] :r #{:a}]] |  false |      1 |    #{:r} |     :r |  :nil |
;;  |                   nil |              #{:b :a} |                 [[[] :r #{:b :a}]] |  false |      1 |    #{:r} |     :r |  :nil |
;;  |                   #{} |                 #{:a} |                    [[[] :r #{:a}]] |  false |      1 |    #{:r} |     :r |  :set |
;;  |                   #{} |              #{:b :a} |                 [[[] :r #{:b :a}]] |  false |      1 |    #{:r} |     :r |  :set |
;;  |                 #{:a} |                 #{:b} |           [[[:a] :-] [[:b] :+ :b]] |  false |      2 | #{:- :+} | :mixed |  :set |
;;  |                 #{:a} |              #{:b :a} |                     [[[:b] :+ :b]] |  false |      1 |    #{:+} |     :+ |  :set |
;;  |                 #{:a} |              #{:b :a} |                     [[[:b] :+ :b]] |  false |      1 |    #{:+} |     :+ |  :set |
;;  |                 #{:a} |              #{:c :b} |                 [[[] :r #{:c :b}]] |  false |      1 |    #{:r} |     :r |  :set |
;;  |                 #{:b} |              #{:b :a} |                     [[[:a] :+ :a]] |  false |      1 |    #{:+} |     :+ |  :set |
;;  |                 #{:b} |              #{:b :a} |                     [[[:a] :+ :a]] |  false |      1 |    #{:+} |     :+ |  :set |
;;  |                 #{:a} |           #{:c :b :a} |        [[[:c] :+ :c] [[:b] :+ :b]] |  false |      2 |    #{:+} |     :+ |  :set |
;;  |                 #{:b} |           #{:c :b :a} |        [[[:c] :+ :c] [[:a] :+ :a]] |  false |      2 |    #{:+} |     :+ |  :set |
;;  |                 #{:c} |           #{:c :b :a} |        [[[:b] :+ :b] [[:a] :+ :a]] |  false |      2 |    #{:+} |     :+ |  :set |
;;  |              #{:b :a} |              #{:c :b} |           [[[:a] :-] [[:c] :+ :c]] |  false |      2 | #{:- :+} | :mixed |  :set |
;;  |              #{:b :a} |              #{:c :a} |           [[[:b] :-] [[:c] :+ :c]] |  false |      2 | #{:- :+} | :mixed |  :set |
;;  |              #{:c :b} |              #{:c :a} |           [[[:b] :-] [[:a] :+ :a]] |  false |      2 | #{:- :+} | :mixed |  :set |
;;  |              #{:c :b} |              #{:c :b} |                                 [] |  false |      0 |      #{} | :mixed |  :set |
;;  |              #{:b :a} |           #{:c :b :a} |                     [[[:c] :+ :c]] |  false |      1 |    #{:+} |     :+ |  :set |
;;  |              #{:c :b} |           #{:c :b :a} |                     [[[:a] :+ :a]] |  false |      1 |    #{:+} |     :+ |  :set |
;;  |              #{:c :a} |           #{:c :b :a} |                     [[[:b] :+ :b]] |  false |      1 |    #{:+} |     :+ |  :set |
;;  |              #{:b :a} |                   nil |                      [[[] :r nil]] |  false |      1 |    #{:r} |     :r |  :set |
;;  |              #{:b :a} |                   #{} |                      [[[] :r #{}]] |  false |      1 |    #{:r} |     :r |  :set |
;;  |              #{:b :a} |                 #{:a} |                        [[[:b] :-]] |  false |      1 |    #{:-} |     :- |  :set |
;;  |              #{:b :a} |                 #{:b} |                        [[[:a] :-]] |  false |      1 |    #{:-} |     :- |  :set |
;;  |              #{:b :a} |              #{:b :a} |                                 [] |  false |      0 |      #{} | :mixed |  :set |
;;  |                   nil |                    {} |                       [[[] :r {}]] |  false |      1 |    #{:r} |     :r |  :nil |
;;  |                   nil |               {:a :A} |                  [[[] :r {:a :A}]] |  false |      1 |    #{:r} |     :r |  :nil |
;;  |                   nil |        {:a :A, :b :B} |           [[[] :r {:a :A, :b :B}]] |  false |      1 |    #{:r} |     :r |  :nil |
;;  |                    {} |               {:a :A} |                  [[[] :r {:a :A}]] |  false |      1 |    #{:r} |     :r |  :map |
;;  |                    {} |        {:a :A, :b :B} |           [[[] :r {:a :A, :b :B}]] |  false |      1 |    #{:r} |     :r |  :map |
;;  |               {:a :A} |               {:b :B} |           [[[:a] :-] [[:b] :+ :B]] |  false |      2 | #{:- :+} | :mixed |  :map |
;;  |               {:a :A} |        {:a :A, :b :B} |                     [[[:b] :+ :B]] |  false |      1 |    #{:+} |     :+ |  :map |
;;  |               {:b :B} |        {:a :A, :b :B} |                     [[[:a] :+ :A]] |   true |      1 |    #{:+} |     :+ |  :map |
;;  |               {:a :A} | {:a :A, :b :B, :c :C} |        [[[:b] :+ :B] [[:c] :+ :C]] |  false |      2 |    #{:+} |     :+ |  :map |
;;  |               {:b :B} | {:a :A, :b :B, :c :C} |        [[[:a] :+ :A] [[:c] :+ :C]] |   true |      2 |    #{:+} |     :+ |  :map |
;;  |        {:a :A, :b :B} | {:a :A, :b :B, :c :C} |                     [[[:c] :+ :C]] |  false |      1 |    #{:+} |     :+ |  :map |
;;  |        {:b :B, :c :C} | {:a :A, :b :B, :c :C} |                     [[[:a] :+ :A]] |   true |      1 |    #{:+} |     :+ |  :map |
;;  |        {:a :A, :c :C} | {:a :A, :b :B, :c :C} |                     [[[:b] :+ :B]] |   true |      1 |    #{:+} |     :+ |  :map |
;;  | {:a :A, :b :B, :c :C} |        {:a :A, :b :B} |                        [[[:c] :-]] |  false |      1 |    #{:-} |     :- |  :map |
;;  | {:a :A, :b :B, :c :C} |               {:b :B} |              [[[:a] :-] [[:c] :-]] |  false |      2 |    #{:-} |     :- |  :map |
;;  |        {:b :B, :c :C} |        {:b :B, :a :A} |           [[[:c] :-] [[:a] :+ :A]] |  false |      2 | #{:- :+} | :mixed |  :map |
;;  |        {:a :A, :b :B} |                    {} |                       [[[] :r {}]] |  false |      1 |    #{:r} |     :r |  :map |
;;  |        {:a :A, :b :B} |                   nil |                      [[[] :r nil]] |  false |      1 |    #{:r} |     :r |  :map |
;;  |               {:a :A} |                   nil |                      [[[] :r nil]] |  false |      1 |    #{:r} |     :r |  :map |
;;  |                    [] |                  [[]] |                     [[[] :r [[]]]] |  false |      1 |    #{:r} |     :r |  :vec |
;;  |                    [] |                [[:a]] |                   [[[] :r [[:a]]]] |  false |      1 |    #{:r} |     :r |  :vec |
;;  |                    [] |             [[:a :b]] |                [[[] :r [[:a :b]]]] |  false |      1 |    #{:r} |     :r |  :vec |
;;  |                    [] |           [[:a] [:b]] |              [[[] :r [[:a] [:b]]]] |  false |      1 |    #{:r} |     :r |  :vec |
;;  |                [[:a]] |             [[:a :b]] |                    [[[0 1] :+ :b]] |  false |      1 |    #{:+} |     :+ |  :vec |
;;  |                [[:a]] |           [[:a] [:b]] |                    [[[1] :+ [:b]]] |  false |      1 |    #{:+} |     :+ |  :vec |
;;  |           [[:a] [:b]] |           [[:b] [:a]] |      [[[0 0] :r :b] [[1 0] :r :a]] |  false |      2 |    #{:r} |     :r |  :vec |
;;  |               [#{:a}] |         [#{:a} #{:b}] |                   [[[1] :+ #{:b}]] |  false |      1 |    #{:+} |     :+ |  :vec |
;;  |               [#{:a}] |         [#{:b} #{:a}] |                   [[[0] :+ #{:b}]] |  false |      1 |    #{:+} |     :+ |  :vec |
;;  |               [#{:a}] |               [#{:b}] |       [[[0 :a] :-] [[0 :b] :+ :b]] |  false |      2 | #{:- :+} | :mixed |  :vec |
;;  |         [#{:a} #{:b}] |               [#{:b}] |                         [[[0] :-]] |  false |      1 |    #{:-} |     :- |  :vec |
;;  |         [#{:a} #{:b}] |               [#{:a}] |                         [[[1] :-]] |  false |      1 |    #{:-} |     :- |  :vec |
;;  |         [#{:a} #{:b}] |                    [] |                       [[[] :r []]] |  false |      1 |    #{:r} |     :r |  :vec |
;;  |                  [:a] |                 #{:a} |                    [[[] :r #{:a}]] |  false |      1 |    #{:r} |     :r |  :vec |

  (filter (fn [[k v]] (> (count v) 1)) (group-by :edits examples)))

(def type-order (into {} (map vector [:nil :vec :set :map] (range))))
(def ops (group-by :op examples))
;; # replace
{:nextjournal.clerk/visibility {:result :show}}
(clerk/table (sort-by (juxt type-order) (:r ops)))
;; # add
{:nextjournal.clerk/visibility {:result :show}}
(clerk/table (sort-by (juxt type-order :count) (:+ ops)))
;; # remove
{:nextjournal.clerk/visibility {:result :show}}
(clerk/table (sort-by (juxt :count type-order) (:- ops)))

;; # mixed
{:nextjournal.clerk/visibility {:result :show}}
(clerk/table (sort-by (juxt :count type-order) (:mixed ops)))

; # lossy
;; these are also lossy in regard to ordering
;; but discarded in this case because the ordering
;; is not preserved by reader unless. rewrite-clj
;; however does preserve the order
(pr-str #{:a :b}) ;; => "#{:b :a}"
(pr-str #{:b :a}) ;; => "#{:b :a}"
(-> "#{:a :b}"
    (z/of-string)
    (z/root-string)) ;; => "#{:a :b}"

(-> "#{:b :a}"
    (z/of-string)
    (z/root-string)) ;; => "#{:b :a}"

{:nextjournal.clerk/visibility {:result :show}}
(clerk/table (filter :lossy examples))

{:nextjournal.clerk/visibility {:result :show}
 :nextjournal.clerk/budget nil
 :nextjournal.clerk/auto-expand-results? true}
(clerk/table examples)
