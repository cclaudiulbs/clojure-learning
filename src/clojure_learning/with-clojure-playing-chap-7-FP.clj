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

;; another one :) that does not depend on hardcoded decimals
( (fn [[penultimate tail-item & tail :as init-coll]]
    (if (empty? tail)
      penultimate
      (recur (rest init-coll) )))
 [1 2 3]); 2

;; small notes:
;; deconstructuring the state of the recurring collection, as it should appear for last elements
;; upfront they are head and next one, but when the recursive invocation reaches the condition
;; obviously they will be bound to the penult + last items
;; rest here does NOT return a nil never, and always returns an empty collection,
;; however the nil? condition works as well since the binding is DONE in this case
;; where the function-args are deconstructed! and NOT in the call to [recur]!


;; small doseq demo
(doseq [x [1 2 3]]
  (println "each: " x)); each 1...each 3

;; pure function:
;;   + a function which given a domain of values -> always returns the same values foreach invocation
;;   + means it is not touching the outside world, apart from the given domain of args
;;   + means it does not has any side effects on outside world(no mutation of outside world)
;;   + means it will NOT rely on the outside world for any dependencies

(doc select-keys); 	Returns a map containing only those entries in map whose key is in keys
(select-keys {:a 1 :b 2 :c 3} [:a :b]); {:b 2 :a 1}
(vals {:a 1 :b 2}); (1 2)

;; lets build a pure-function which is said to be referential transparent. Stop but what means RT?
;; a function is said to be referential transparent if it is a pure function first. It can be
;; seen as a constant during time, and "the reference to function is transparent during time"
;; and being a constant, it can be replaced with its result value and the program executes just as
;; if nothing has changed.
;; demo: this function returns a map with a function applied on the corresponding values bounded to key-args
(defn keys-apply
  [func ks m]
  (let [only (select-keys m ks)]
    (zipmap (keys only) (map func (vals only)))))

(keys-apply
   clojure.string/upper-case
   [:foo :bar]
   {:foo "some-foo" :bar "some-bar" :zip "some-zip"})
; {:foo "SOME-FOO", :bar "SOME-BAR"}

;; then a new function can be build that operates on top of keys-apply, this is also a pure function
;; since it never changes the outside world, and always yields the same result given the same args
(defn manip-map
  [func ks m]
  (merge m (keys-apply func ks m)))

(manip-map clojure.string/upper-case [:foo] {:foo "cclaudiu" :bar "cosar"})
; {:foo "CCLAUDIU", :bar "cosar"} -> do some processing on :foo key -> and merge into the same passed map
; -> replace -> merge mechanism

(doc merge); returns a new map, that consists of the tail-maps-args conj-ed onto the first: "m" in this case

(doc zipmap); ([keys vals])
(clojure.string/upper-case "this")

;;;;;;;;;;;;;;;;;;
; Clojure provides idiomatic named-arguments of functions, via destructuring in conjunction with "&" varargs:
(defn learn-clj
  [& {:keys [book practice] :or {book "Clojure Programming" practice "lighttable"}}]
   (format "Learning clojure from book: %1s, practicing via : %2s" book practice))
(learn-clj :book "The Joy of Clojure")
(learn-clj :practice "4clojure")

;; the & makes the func take varargs. there should be PAIRS normally, that's the meaning of NAMED-ARGS anyway
;; each-pair(if a single one) is passed as func arg -> the destructuring starts working -> by decomposing
;; them into a hash-map first and then looking up by keys, else taking defaults provided via :or macro

;; Clojure has a powerfull support on DbC: via {:pre [(eval-to-true-1) (eval-to-true-2)]
;;                                              :post [(....)]}

;; Clojure Closures :)
;; In a sentence a closure is a function that has access to the values of the outer-context - where it has been created
(defn times-two []
  (let [two 2]
    (fn [x] (* two x))))
((times-two) 5) ; 10
;; first time the function times-two is invoked it creates a closure, that's the returned function, which closure
;; has access over the values from the context where it was created, although there's no reference to the times-two function
;; the returned closure captures them.
