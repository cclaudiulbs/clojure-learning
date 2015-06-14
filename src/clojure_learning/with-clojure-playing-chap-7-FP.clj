(use 'clojure.repl)

(map [:a :b :c] #{0 1}); (:a :b)

(let [[head :as init-coll] [1 2 3 4 5]]
  head); 1

;; function composition:
(map (comp keyword #(.toLowerCase %) name) '[a b c]); (:a :b :c)
(map (comp
        keyword
        #(.toLowerCase %)
        name) '[a b c]); (:a :b :c)

;; clojure heavy relies on the varargs arity of arguments -> curries make litlle sense...
((partial + 4) 100 200); 304
((comp +) 4 100 200); 304

;; diff between partial & curry: partial attemts to evaluate eagerly the expression
;; on any given argument -> to see if it can return, while curry returns by definition
;; each-time another function back, taking yet another argument, until it resolves.
;; partial application is an implementation of the curry more general process;

;; clojure provides the [complement] function, that returns the opposite boolean value
;; of the truthy -> false;

;; the [complement] function is equivalent to: comp not ...
(let [thruttiness (fn [x] x)]
  ((complement thruttiness) true) ; false
  ((complement thruttiness) 20)   ; false
  ((complement thruttiness) nil)) ; true

;; power of deconstruction in action:
(let [{:keys [foo bar zip]
         :or {zip "cucu"}} {:foo "foo" :bar "bar"}]
    (format "foo is: %1s, bar is %2s, whereas zip defaults to: %3s" foo bar zip ))

;; using syms instead of :keywords
(let [{:syms [foo bar zip]
         :or {zip "cucu"}} {'foo "foo" 'bar "bar"}]
    (format "foo is: %1s, bar is %2s, whereas zip defaults to: %3s" foo bar zip ))


(let [[foo _ zip] ["foo" "bar" "zip"]]
  (format "foo is %1s, while zip is %2s..bar is ignored" foo zip))

(let [[head & tail] [1 2 3 4 5]]
  (format "head is: %1s, while tail is a sequence back deconstructured from the vector DS: %2s" head tail))
;; "head is: 1, while tail is a sequence back deconstructured from the vector DS: (2 3 4 5)"

(let [ {a 0}   [1 2 3 [4 5 6 7]] ] a); 1
(let [ {a 0 b 3}   [1 2 3 [4 5 6 7]] ] b);  [4 5 6 7]
(let [ {[x _ _ z] 3}   [1 2 3 [4 5 6 7]] ] (+ x z));  [4 5 6 7]

(let [ [x y & tail] [1 2 :foo "foo" :bar "bar"]
       {:keys [foo bar]} (apply hash-map tail) ]
    (format "using %1d, with %2s %3s" (+ x y) foo bar))
;; using 3 with foo bar

;; .... and as a one-liner:
(let [ [x y & {:keys [foo bar]}] [1 2 :foo "foo" :bar "bar"]]
    (format "using %1d, with %2s %3s" (+ x y) foo bar))
;; the tail-items must be even -> for the deconstructure in a map to succeed.

;; and back to functions:
(reduce #(assoc %1 %2 (* %2 %2)) {} [1 2 3 4 5]); {5 25, 4 16, ..., 1 1}
(doc assoc); assoc [map key val]
;; in the previous reduce case, the {} initial and first argument of reduce is the map ->
;; used as %1 1st argument of our literal-func -> 1st time invoking [assoc] using {} will
;; return a new map having the key: each-item, value: each-item doubled.
;; reduce can also be used as a transformation function...

;; one way to assign metadata to a function, is to prefix a map, before the func arg list
;; this metadata can be for example a unit-test for that function:
(use '[clojure.test :as suite])
(defn call-twice
  [f x]
  (f x)
  (f x))
(call-twice (partial println) "some")

;; on way to assign metadata to a function, is to prefix a map, before the func arg list
;; this metadata can be for example a unit-test for that function:
(defn join
  {:test (fn[]
           (assert (= (join ":" [1 2 3]) "1:333:1")))}
  [sep coll]
  (apply str (interpose sep coll)))

;; interpose returns another coll using the sep sparsed in the coll, while applying str on coll, will concatenate
;; and we have a join function, for which we provide an assertion test as func-metadata

(suite/run-tests)
;; actual: java.lang.AssertionError: Assert failed: (= (join ":" [1 2 3]) "1:333:1")

(doc interpose)
(interpose ":" [1 2 3]); (1 ":" 2 ":" 3) -> ret a lazy seq inlining the separator as a well

(doc sort); [[coll] [comp coll]]; takes a comparator as well -> sorting on the same types!!!
(sort [3 -1 -3 4 9])
(sort < [3 -1 -3 4 9]); same as without the comparator
(sort > [3 -1 -3 4 9]); from greatest to lower

;; whenever there's need to sort, by establishing a comparator or mutual comparable things
;; the [sort-by] comes in place:
(sort-by :age [{:age 99} {:age 88} {:age 32}]); ({:age 32} {:age 88} {:age 99})

(doc sort-by); the signature: [keyfunc coll] [keyfunc comp coll]

(def plays [{:band "Burial", :plays 979, :loved 9}
            {:band "Eno", :plays 2333, :loved 15}
            {:band "Bill Evans", :plays 979, :loved 9} ])

(def sort-by-loved-ratio (partial sort-by #(/ (:plays %) (:loved %))))
(sort-by-loved-ratio plays)
;({:band "Burial", :plays 979, :loved 9} {:band "Bill Evans", :plays 979, :loved 9} {:band "Eno", :plays 2333, :loved 15})

(use '[clojure.string :as str])

;; sorting by columns -> in the enumerated order(spreadsheet style)
(sort-by (columns [:plays :loved :band]) plays)
;; ({:band "Bill Evans", :plays 979, :loved 9} {:band "Burial", :plays 979, :loved 9} {:band "Eno", :plays 2333, :loved 15})
(defn columns
    [column-names]
    (fn [each-row]
      (vec (map each-row column-names))))

;; similar with one-liner:
(sort-by #(vec (map % [:band :plays :loved])) plays)
;; understanding the complexity :) by de-composing
(map {:name "cc" :age 33} [:name :age]); ("cc" 33)
;; using the entire map as a func in the map that will return each value by :key
({:name "cc" :age 33} :name); "cc"

;;;;;;;;;;;;;;;;;;;;;;;
;; 4clojure practice ;;
;;;;;;;;;;;;;;;;;;;;;;;
(let [ {:keys [head midd tail]} {:head 1 :midd 2 :tail 3}
        inc-with-five (partial + 5)
        res (map inc-with-five '(1 2 3) ) ]
  res) ; (6 7 8)

(let [hof (comp (partial filter #(> % 5)) list)]
  (hof 3 4 5 6 7)) ; (6 7)

;; Write a function which returns the last element in a sequence:
((fn [[_ & tail]]
    (last tail)) [1 3 4]) ; 4

;; but dont use [last] !!! ok here's one using recursions:
((fn [coll]
  (loop [curr (first coll)
         tail (rest coll)]
    (if (empty? tail)
        curr
        (recur (first tail) (rest tail))))) [1 2 3])
; 3

;; and here's one deconstructing the init coll
((fn [[head & tail]]
    (if (empty? tail)
        head
        (recur (rest tail)))) [1 2 3])
; 3

;; or using the in-collection as a function -> to query by last-indice of the in-collection
((fn [in-coll] (in-coll (dec (count in-coll)))) [1 2 3])
; 3

;; Write a function which returns the penultimate element from a sequence:
( (fn [coll]
    (coll (- (count coll) 2))) [1 2 3])

((fn [[penultimate & tail :as init-coll]]
    (if (empty? tail)
      penultimate
      (recur (rest init-coll) ))) [1 2 3])