(ns kurtharriger.notebook.notes005
  (:require [kurtharriger.clj-mergetool :refer [init resolve-input parse diff mergetool output]]
            [portal.api :as portal]
            [editscript.core :as e]
            [rewrite-clj.parser :as p]
            [rewrite-clj.node :as n]
            [rewrite-clj.zip :as z]
            [meander.epsilon :as m]
            [clojure.edn :as edn]))


(comment
  (def ctx (init :merge
                 "test/kurtharriger/examples/ex3/base.clj"
                 "test/kurtharriger/examples/ex3/left.clj"
                 "test/kurtharriger/examples/ex3/right.clj"))

  (-> ctx
      (dissoc :output-file)
      #_(assoc :output-file "/dev/stdout")
      (parse)
      (diff)
      (mergetool)
      (output))

  (def conflict
    (reduce e/combine  [(e/diff '(add 1 2) '(add 1 3))
                        (e/diff '(add 1 2) '(add 1 4))]))

  (type conflict)
  (type (e/get-edits conflict))
  (group-by first (e/get-edits conflict))
  ;; => {[2] [[[2] :r 3] [[2] :r 4]]}





  (def ctx {:op :merge,
            :output-file
            "/Users/kurtharriger/dev/personal/mergetool/kurtharriger.clj-mergetool/ex/base.clj",
            :base
            {:filename
             "/Users/kurtharriger/dev/personal/mergetool/kurtharriger.clj-mergetool/ex/base.clj"},
            :left
            {:filename
             "/Users/kurtharriger/dev/personal/mergetool/kurtharriger.clj-mergetool/ex/left.clj"},
            :right
            {:filename
             nil}})

  (-> ctx
      (resolve-input)
      (dissoc :output-file)
      (parse)
      (diff)
      (mergetool)
      (output)
      :exit-code)

  (def out (-> ctx
               (resolve-input)
               (dissoc :output-file)
               (parse)
               (diff)
               (mergetool)
               (output)))

  (def zipper (-> out :base :parsed z/of-node))
  (def patch (-> out :patch))
  patch
  ;; => [[[[:deps org.clojure/core.async :mvn/version] :r "1.3.612"]]]

  (-> (z/of-string (pr-str {:a 1 :b 2})))
  ;; => [{:tag :map,
  ;;      :format-string "{%s}",
  ;;      :wrap-length 2,
  ;;      :seq-fn #function[rewrite-clj.node.seq/eval11552/map-seq-fn--11553],
  ;;      :children
  ;;      ({:k :a, :auto-resolved? false, :map-qualifier nil}
  ;;       {:whitespace " "}
  ;;       {:value 1, :string-value "1"}
  ;;       {:commas ","}
  ;;       {:whitespace " "}
  ;;       {:k :b, :auto-resolved? false, :map-qualifier nil}
  ;;       {:whitespace " "}
  ;;       {:value 2, :string-value "2"})}
  ;;     {:l [],
  ;;      :pnodes
  ;;      [{:children
  ;;        ({:tag :map,
  ;;          :format-string "{%s}",
  ;;          :wrap-length 2,
  ;;          :seq-fn #function[rewrite-clj.node.seq/eval11552/map-seq-fn--11553],
  ;;          :children
  ;;          ({:k :a, :auto-resolved? false, :map-qualifier nil}
  ;;           {:whitespace " "}
  ;;           {:value 1, :string-value "1"}
  ;;           {:commas ","}
  ;;           {:whitespace " "}
  ;;           {:k :b, :auto-resolved? false, :map-qualifier nil}
  ;;           {:whitespace " "}
  ;;           {:value 2, :string-value "2"})})}],
  ;;      :ppath nil,
  ;;      :r nil}]


  (def bval (-> (z/of-string (pr-str {:a 1 :b 2}))
                (z/down)   ; :a
                (z/right)  ;  1
                (z/right)  ; :b
                (z/right)  ; 2
                ))
  bval
  ;; => [{:value 2, :string-value "2"}
  ;;     {:l
  ;;      [{:k :a, :auto-resolved? false, :map-qualifier nil}
  ;;       {:whitespace " "}
  ;;       {:value 1, :string-value "1"}
  ;;       {:commas ","}
  ;;       {:whitespace " "}
  ;;       {:k :b, :auto-resolved? false, :map-qualifier nil}
  ;;       {:whitespace " "}],
  ;;      :pnodes
  ;;      [{:children
  ;;        ({:tag :map,
  ;;          :format-string "{%s}",
  ;;          :wrap-length 2,
  ;;          :seq-fn #function[rewrite-clj.node.seq/eval11552/map-seq-fn--11553],
  ;;          :children
  ;;          ({:k :a, :auto-resolved? false, :map-qualifier nil}
  ;;           {:whitespace " "}
  ;;           {:value 1, :string-value "1"}
  ;;           {:commas ","}
  ;;           {:whitespace " "}
  ;;           {:k :b, :auto-resolved? false, :map-qualifier nil}
  ;;           {:whitespace " "}
  ;;           {:value 2, :string-value "2"})})}
  ;;       {:tag :map,
  ;;        :format-string "{%s}",
  ;;        :wrap-length 2,
  ;;        :seq-fn #function[rewrite-clj.node.seq/eval11552/map-seq-fn--11553],
  ;;        :children
  ;;        ({:k :a, :auto-resolved? false, :map-qualifier nil}
  ;;         {:whitespace " "}
  ;;         {:value 1, :string-value "1"}
  ;;         {:commas ","}
  ;;         {:whitespace " "}
  ;;         {:k :b, :auto-resolved? false, :map-qualifier nil}
  ;;         {:whitespace " "}
  ;;         {:value 2, :string-value "2"})}],
  ;;      :ppath
  ;;      {:l [],
  ;;       :pnodes
  ;;       [{:children
  ;;         ({:tag :map,
  ;;           :format-string "{%s}",
  ;;           :wrap-length 2,
  ;;           :seq-fn #function[rewrite-clj.node.seq/eval11552/map-seq-fn--11553],
  ;;           :children
  ;;           ({:k :a, :auto-resolved? false, :map-qualifier nil}
  ;;            {:whitespace " "}
  ;;            {:value 1, :string-value "1"}
  ;;            {:commas ","}
  ;;            {:whitespace " "}
  ;;            {:k :b, :auto-resolved? false, :map-qualifier nil}
  ;;            {:whitespace " "}
  ;;            {:value 2, :string-value "2"})})}],
  ;;       :ppath nil,
  ;;       :r nil},
  ;;      :r nil}]




  (-> (z/of-string (pr-str {:a 1 :b 2}))
    ;(move-to-map-key zipper :b)
      (z/down)
      (->> (#(partition 2 (take-while (complement z/end?) (iterate z/right %))))
           ;(drop 1)
           (filter (fn [[kz vz]]
                     (= :b (z/sexpr kz))))
           (ffirst)))

  (defn move-to-map-key [zipper key]
    (->> zipper
         (z/down)
         (iterate z/right)
         (take-while (complement z/end?))
         (partition 2)
         (filter (fn [[kz vz]] (= key (z/sexpr kz))))
         (ffirst)))


  (-> (z/of-string (pr-str {:a 1 :b 2}))
      (move-to-map-key :b))


  #_(defn move-to-map-value [zipper key]
      (-> zipper
          (move-to-map-key key)
          (z/right)))

  patch
  ;; => [[[[:deps org.clojure/core.async :mvn/version] :r "1.3.612"]]]


  (-> zipper
      (move-to-map-value :deps)
      (move-to-map-value 'org.clojure/core.async)
      (move-to-map-value :mvn/version)
      (z/edit (constantly "1.3.612"))
      (z/root-string))

  (defn move-to-index [zipper n]
    (first (drop n (iterate z/right (z/down zipper)))))

  (-> (z/of-string (pr-str [1 {:a 1 :b [2 3]}]))
      (move-to-index 1)
      (move-to-map-value :b)
      (move-to-index 0)
      (z/edit (constantly 4))
      (z/root-string))

 ;;

  (defn move-path [zipper path]
    (reduce (fn [zipper p]
              (cond
                (number? p)   (move-to-index zipper p)
                (keyword? p)  (move-to-map-value zipper p)))
            zipper path))


  (-> (z/of-string (pr-str [1 {:a 1 :b [2 3]}]))
      (move-path [1 :b 0])
      (z/edit (constantly 4))
      (z/root-string))
  ;; => "[1 {:a 1, :b [4 3]}]"

  ; note: z/root returns node not zipper
  ; use z/root z/of-node to create new zipper at root
  (defn patch1 [zipper edit]
    (m/match edit
      [?path :-] (-> zipper (move-path  ?path) (z/remove) (z/root) (z/of-node))
      [?path :+ ?v] (-> zipper (move-path  ?path) (z/insert-right ?v) (z/root) (z/of-node))
      [?path :r ?v] (-> zipper (move-path  ?path) (z/remove)  (z/insert-right ?v) (z/root) (z/of-node))))


  (let [base [1 2 3]
        zipper (z/of-string (pr-str base))
        left (-> zipper
                 (move-path [1])
                 (z/remove)
                 (z/root)
                 (n/sexpr))
        edits (e/get-edits (e/diff base left))
        edit (first edits)
        zipper (patch1 zipper edit)
        sexpr (-> zipper z/root n/sexpr)]
    (= left sexpr))


  ;; => "[1 3]"


  (-> (z/of-string (pr-str [1 2 4]))
      (move-path [1])
      (z/insert-right 3)
      (z/root-string))

  (let [base [1 2 4]
        zipper (z/of-string (pr-str base))
        left (-> zipper
                 (move-path [0])
                 (z/insert-right 3)
                 (z/root)
                 (n/sexpr))
        edits (e/get-edits (e/diff base left))
        edit (first edits)
        zipper (patch1 zipper edit)
        sexpr (-> zipper z/root n/sexpr)]
    #_(= left sexpr)
    edit)
  ;; => [[1] :+ 3]


;; note: edit script vector indecies are 1 based
 ;; todo fix move-to-next
  (let [base []
        zipper (z/of-string (pr-str base))
        left [1]
        edits (e/get-edits (e/diff base left))
        edit (first edits)]
    #_(= left sexpr)
    edit)
    ;; => [[] :r [1]]

 ;; insert at 0 is edit at root ...
  )


