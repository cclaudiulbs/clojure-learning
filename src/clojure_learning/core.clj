;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Chapter IV:: Do Things ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(ns clojure-learning.core)

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))

;; (foo "cclaudiu")

(defn transform
  [callback & tail-args]
  (map callback tail-args))

(def lessThanThirty 
  (fn[someNum] 
    (when (< someNum 30)
      (str "Less than thirty...")
     (if (> someNum 30)
       (do (str "this person is older than 30...going to print something")
         println "printing:: this person is older...")
       (do (str "else...going to return a teenager..."))
      )
     )
   )
)

(transform lessThanThirty 12 31 11)

;; destructuring...
(defn destructureInAction
  "this should be the clojure doc api of this function"
  [[first-param]]
  (println (str "doing something with the DESTRUCTURED first-arg of the collection..." first-param)))

(destructureInAction ["cclaudiu", "mary"])

;; equivalent for this should be:
(defn alternativeDestructuring
  "this is the alternative for the standard destructure that Clojure provides"
  [container]
  (println (str "doing something with the programatically destructured container: " (first container))))

(alternativeDestructuring ["cosar" "dobrotchi"])
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn operate-on-collection1
  "this is the function api doc"
  [[first-arg second-arg & tail-args]]
  (println (str "doing something concretely with only first + second args: " first-arg ", and: " second-arg)
   println (map (fn[each-tail-arg](str "formatting the tail-arg: " each-tail-arg)) tail-args)
    ))
(operate-on-collection1 (list "cclaudiu" "cosar" "other1" "other2"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; destructing maps
(defn- destruct-map
  [{lat :lat lng :lng}]
  (println (str "Latitude value: " lat))
   (println (str "Longitude value: " lng))
)
;; lat + lng are pointing to the values identfied by the map keys :lat :lng
(destruct-map {:lat 23 :lng 44})

;; Deconstruct into ONE operation: using the :keys -> take the values of only the keys that're in the brackets
(defn- destruct-a-map2
  [{:keys [lat lng]}]
  (println (str "Latitude: " lat)
   println (str "Longitude: " lng)))
(destruct-a-map2 {:lat 30 :lng 10})

;; take a reference of the map using the ":as" which aliases the map argument for later references
(defn- use-map-arg-alias
  [{:keys [name, address] :as customer}]
  (println (str "Customer name is: " name)
   println (str "Customer address is: " address))
  println (str "invoke the aliased map on some other function: " customer)  ;; !!!!
)
(use-map-arg-alias {:name "cclaudiu" :address "titan nr 6"})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; functions
((fn[x] (* x 3)) 8)
;; anonymous function(inline lambda expression) that takes 1 arg, and returns the multiplication of it
;; with 3; we then apply this 1-arg-function using the domain of 8

;; a javascriptish way of defining the function would be to BIND an anonymous function
;; to a identificator
(def bound-func-to-name (fn[x](* x 3)))
(bound-func-to-name 8)

;; var funcExpression = function(x){...}
;; except that Clojure DON't have the "=" operator, in clojure we BIND things to other names

;; another more idiomatic way of declaring functions is: (#(< % 2) 1)
(#(< % 2) 1) ;; --> evaluates to "true"
(#(+ %1 %2 20) 1 2) ;; --> 23

;; closures in Clojure :)
;; closures as known, are functions returned by other functions, which returned functions "close-over"
;; the variables defined on the function scope in the moment it was created.
(defn- inc-maker
  "returnes a functions which takes the incremental-by operand:: closes over the inc-maker arg"
  [head-incrementor]
  (fn [tail-incrementor] (+ head-incrementor tail-incrementor)))

(def inc-seven-by (inc-maker 7))  ;; inc-seven-by func still has access to arg passed to inc-maker func
(def ten (inc-seven-by 3))
(println ten) ;;10

;;;;;;;;;;;;;;;;;;;;;;
;; let -> introduces a new scope: bind the variable on the left to the righ-hand-side
(def x 3)
(let [x 1])
(eval x)

(def foo 24)
(println foo)
(let [foo "something else"] foo);; foo refers to "something..." ONLY in let created scope!!!
 
(def myMap {:foo "this is foo" :bar "this bar"})
(println (:foo myMap))

(def names ["foo" "bar" "zip"])
;      deconstruct the names into head-tail
(let [ [head-arg & tail-args] names
;      bind the formatted-names to the result of invoking conj on names
     formatted-names (conj names (str head-arg "...all names are here"))]

     ; the last expression is the one evaluated by let
     formatted-names)
;; the output of let evaluation is: ["foo" "bar" "zip" "foo...all names are here"]

;; (println head-arg) --> runtime error: head-arg is NOW out of scope!!!

(println (vec (take 20(range))))

;; recursivity
(defn- recursive-iteration
  [ [head & tail] ]
  (if (> head 10)
     (do(println "breaking the recursivity on number higher than 10"))
     (do(
          println (str "iterating the first 10 numbers...current: " head))
          (recur (vec tail))
      )))
(recursive-iteration (vec (take 20(range))))

;; regular expressions:
(defn- isMatchingCC
  [subject]
  (re-find #"cc" subject))
(let [matched (isMatchingCC "cclaudiu")] matched)

;; implement the my-reduce in Clojure, which is a minimalist version of standard: "reduce" func
(defn- my-reduce
  [func initial [head & tail]]
    (if(= head nil)
       initial
       (recur func (func initial head) tail))
 )
(let [reduce-result (my-reduce str "a" '("b" "c"))] reduce-result)
(let [sumofnums (reduce + 1 '[2 3])] sumofnums)
(let [reduce-result (my-reduce + 1 '[2 3 4 5])] reduce-result)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Chapter 5:: Functions in Depth ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; how to transform a map data-structure to a list?
(map (fn[[hash-key hash-val]] [hash-key hash-val]) {:name "cclaudiu" :age 33})
;; ([:age 33] [:name "cclaudiu"])
;; by default [map] func returns an enumerable list back
;; but we can treat a hash-map as a sequence and apply map func on it.

;; ups, lets convert the list of vectors back to a hash-map, using the [into] func
(into {}
      (map (fn[[hash-key hash-value]] [hash-key(inc hash-value)]) 
           {:clojure 1 :others 2}))

;; output: {:clojure 2, :others 3}
;; above: deconstruct EACH entry of the hash-map into a [key val] pair

;; In general, programming to abstractions gives us power by letting us use libraries of functions on a data structure regardless of that data structure's implementation.

;;; Clojure Programming ;;;
(read-string "this")

[1 2 3]

(pr " this is multiline 
 string")

(#(+ %1 %2 %3 2) 2 3 4)

(def foo "some foo")
;(foo "this")

(def foo [12 3 "cc" (re-seq #"a|b|c" "abc")])
(first foo)
(last foo) #_ ("a" "b" "c")
(foo 1) ; 3
(nth foo 1) ; 3
(.get foo 0) ; 12

; always read from the most inner expression to the most outer -> simpler to get the job done :)

;; destructuring
(def foo [1 2 3 "four" (fn [](quote 5) )])
(let[[one two three] foo] (+ one two three) )
(let[[one & tail] foo] (+ one ((last tail)) )); (last tail) -> fn; ((last tail)) -> (fn): 6

(def aMap {:a "ei" :b "bi"
           :names ["cc" "dm"]
           :age [33 27]})
(let[ {eis :a bis :b ages :age} aMap] 
  (+ (first ages) (second ages)))    ;; last expression from let is returned

; map using "a" & "b" the value corresponding to index 0 and 1 in a map, and KEEP a reference with ":as" to original vector
(let[{a 0 b 1 :as orig-foo} foo] 
  (conj orig-foo a b))

(def customer {:name "cclaudiu" :age 33 :location "Titan nr 6"})
(let[ {:keys [name age location]} customer ]
  (printf "Customer is: %s; age: %d; address: %s" name age location))

(def user-info ["cclaudiu" 2015 :email "claudiu.cosar@sap.com" :company "sap"])
; print: cclaudiu works at sap and has the email...
(let[[id _ & tail] user-info
     {:keys [email company]} (apply hash-map tail)]
  (str id "works at " company "and has the email " email))

(fn[x] (* x x))
((fn [x] (* x x)) 2); 4
(def square (fn[x] (* x x)))
(pr-str (square 2))

(def minus-1 (fn[x y](do (- x y))))
(def minus-2 (fn[x y] (- x y)))
(minus-1 3 2); 1
(minus-2 5 2); 3

((fn [x y z] (+ x y z)) 1 2 3); same as the following "let" block

(let[[x y z] '(1 2 3)] (+ x y z)); same with:

(let[ x 1 
      y 2
      z 3]
  (+ x y z))

;; defn is a macro that encapsulates: def + fn
;; it uses the let functionality to BIND the arguments for the scope of the function body
;; arity functions are functions overloaded; here's an example:
(def make-sum (fn sum-nrs
                ([x] (sum-nrs x 1))  ; 1-arity overloading -> calling the second arity with a default 1
                ([x y] (+ x y))))
(make-sum 1 3); 4
;; naming the fn allows for recursive calls
;; implementing the my-map using variadic functions
(def my-map (fn map-rec
                ([func [head & tail]]
                  (if(nil? head)
                    []
                    (cons (func head) (map-rec func tail)))
                )))

(def inc-by-one (my-map (fn[x] (+ x 1))  '(1 2 3)))
(pr-str inc-by-one)
(def append-prefix (my-map (fn[each] (str "prefix: " each)) '("cc" "ll")))
(pr-str append-prefix)

; (cons 1 (conj '() 2))
; (def inc-by-one (map (fn[x] (+ x 1)) '(1 2 3)))
; (pr-str inc-by-one)