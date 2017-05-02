(ns clojure-learning.sicp
  (use clojure.repl))

(defn gcd [x y]
  (if (zero? y) x
    (recur y (rem x y))))

(gcd 12 8) ;; 4

;; It is possible to show that starting with any two positive integers and performing repeated reductions 
;; will always eventually produce a pair where the second number is 0. 
;; Then the GCD is the other number in the pair. This method for computing the GCD is known as Euclid’s Algorithm

(gcd 75 45) ;; 15

;; local bindings through let. what if we would not have a [let] that helps in binding locals within functions
;; what would be the approach in that case?
(defn complicated-math-formula [a b]
  (- (+ (* a a) 
        (* b b)) 
     (* 2 a b)))

(complicated-math-formula 3 4) ;; 1

;; same function but implemented through the use of lambdas and self-invocatation
(defn complicated-math-formula [a b]
  (- (+ ((fn [x] (* x x)) a)
        ((fn [y] (* y y)) b))
     ((fn [x y] (* 2 x y)) a b)))

;; 25 - (24)
;; the [let] binding helps pretty much in this case, but we know we can implement the local-bindings through the use of
;; functions and lambdas

;; A let expression is simply syntactic sugar for the underlying lambda application.

;; let's define the composition function that takes a variable functions and returns a function that takes the actual
;; argument that will feed to the last function from the composition-chain::
(defn compose [& funcs]
  (fn [& args]
    (reduce (fn [acc next-fn] (next-fn acc)) 
        (apply (last funcs) args) 
        (butlast funcs))))

((compose dec +) 2 2) ;; 3
((compose dec inc dec inc inc) 3) ;; 4


(defn guess-func [& funcs]
  (fn [& args]
    (reduce (fn [acc next-fn] (next-fn acc)) 
        (apply (last funcs) args) 
          (butlast funcs))))

;; we would like our language to be powerful enough so that we can
;; write a procedure that expresses the concept of summation itself rather than only procedures that compute particular sums.

((fn [a b] (+ a b)) 1 2) ;; 3
(#(+ % %2) 1 2)          ;; 3

(let [var1 exp1
      var2 exp2 ]
  ;; use var1 + var2 in the body of let
)

;; using macros we can implement the let monad very simple, however is interesting to see in the context of functions
;; and runtime how this might be implemented
;; we would like to use it as a normal let::
(let-fn [x (inc 2)
         y (inc 3)]
        (+ x y))

;; the bad-thing is that when with functions the expressions are EVALUATED eagerly, when passed as func-arguments!
;; and NOT when they are actually evaluated inside let;
;; therefore we can emulate them as lambdas when passing into let-binding-context, and let the lambda function
;; handle the function application, this way passing the function to be applied as well!
;; (let-fn [x (fn [inc-fn y] (inc-fn y))]) --> would translate to::
;; (let-fn [x [inc y]] x) --> this way the function application of y is NOT eagerly evaluated when let-fn is called!
;; --> a litlle sintatic sugar for achieving the future application

;; todo:  replace occurences of :x and :y with get-from-map-values in the body
(defn let-fn [[& bindings] vec-body]
  (eval
    (map (fn [each] 
           (if (keyword? each)
              (get (reduce (fn [acc [bound vec-expr]]
                              (assoc acc bound (apply (first vec-expr) (rest vec-expr))))
                        {}  (partition 2 bindings)) each)
              each))
         vec-body)))


(let-fn [:x [inc 2]
         :y [inc 3]
         :z [+ 1 2 3]]
  [+ :x :y :z]) ;; 13

;; another way of implementing the let-binding behavior is through the use of lambdas, and immediate invocation
;; functions::
((fn [x y] (+ (inc x) (inc y))) 2 3)
;; however this pattern is not making explicit the bound locals values of x and y.
((fn [x y] (+ (inc x) (inc y)))
      2 3) ;; 7

;; the scope of a local variable definied through the use of lambda expression, is the body of that lambda-s function.

;; Define a procedure double that takes a procedure of one argument as argument and returns a procedure that applies the
;; original procedure twice::
(defn invoke-twice [func]
  (fn [x] (func (func x))))

((invoke-twice inc) 0) ;; 2

;; Why do we want compound data in a programming language? For the same reasons that we want
;; compound procedures: to elevate the conceptual level at which we can design our programs, to increase the modularity of our designs
;; compound data objects enables us to deal with data at a higher conceptual level than that of the primitive data objects of the language

The general technique of isolating the parts of a program that deal
with how data objects are represented from the parts of a program that deal with how data objects are
used is a powerful design methodology called data abstraction

;; example::
(defn linear-combination [a b x y]
  (add (mul a x) (mul b y)))

where add and mul are not the primitive procedures + and * but rather more complex things that will
perform the appropriate operations for whatever kinds of data we pass in as the arguments a, b, x, and
y. The key point is that the only thing linear-combination should need to know about a, b, x,
and y is that the procedures add and mul will perform the appropriate manipulations. From the
perspective of the procedure linear-combination, it is irrelevant what a, b, x, and y are and
even more irrelevant how they might happen to be represented in terms of more primitive data

abstraction as a technique for coping with complexity

a procedure used as an element in creating a more complex procedure
could be regarded not only as a collection of particular operations but also as a procedural abstraction.
That is, the details of how the procedure was implemented could be suppressed, and the particular
procedure itself could be replaced by any other procedure with the same overall behavior. In other
words, we could make an abstraction that would separate the way the procedure would be used from
the details of how the procedure would be implemented in terms of more primitive procedures

Data abstraction is a methodology that
enables us to isolate how a compound data object is used from the details of how it is constructed from
more primitive data objects.

The basic idea of data abstraction is to structure the programs that are to use compound data objects so
that they operate on ‘‘abstract data.’’ That is, our programs should use data in such a way as to make
no assumptions about the data that are not strictly necessary for performing the task at hand.

The interface between these two parts of our system will be a set of procedures, called selectors and
constructors, that implement the abstract data in terms of the concrete representation

operations on rational numbers defined in terms of the selector and constructor
procedures numer, denom, and make-rat.

In general, the underlying idea of data
abstraction is to identify for each type of data object a basic set of operations in terms of which all
manipulations of data objects of that type will be expressed, and then to use only those operations in
manipulating the data.

data-abstraction can be expressed also in terms of a concept that "hides-away" the details of "HOW" the implementation is done,
revealing operations that define the "WHAT" an operation DOES. 
asking for the "WHAT" NOT the "HOW" is a particular term when talking about abstractions.
By definition an abstract thing CHANGE/ADAPTS easily to a new concept, since it does NOT "DEFINE" anything.
Coding with abstractions makes your code more adaptable to future-change.

Programs that use rational numbers manipulate them solely in terms of
the procedures supplied ‘‘for public use’’ by the rational-number package: add-rat, sub-rat,
mul-rat, div-rat, and equal-rat?. These, in turn, are implemented solely in terms of the
constructor and selectors make-rat, numer, and denom, which themselves are implemented in
terms of pairs. The details of how pairs are implemented are irrelevant to the rest of the
rational-number package so long as pairs can be manipulated by the use of cons, car, and cdr.

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
ADVANTAGES of data-abstraction::
This simple idea has many advantages. One advantage is that it makes programs much easier to
maintain and to modify. Any complex data structure can be represented in a variety of ways with the
primitive data structures provided by a programming language.
When changing the internal-data-structure the public operations do NOT need to change at all!

A data is defined in terms of the operations it expose. It is not enough that any random-operations could define the
data-structure we intend to build. It mandatory should obey the contract that the operations expose.
So for a rational-number: let x 5/2 -> the number should return 5, while the denom operation should return 2,
when the make-ratio constructor function is called.

Playing around, we can implement the structural-primitive operations that stand at the foundation of make-rationals 
constructor function:
(defn cons-this [x y]
  (fn [idx]
    (cond (zero? idx) x :else y)))

(defn first-from [idx-func] (idx-func 0))
(defn last-from [idx-func] (idx-func 1))

(first-from (cons-this 1 2))
(last-from (cons-this 1 2))

These primitives operations are defining the contract for implementing a rational-number.
The [cons-this] constructor function acts as a constructor, which returns a closure-func back. The closure returned
will close-over the arguments of its enclosing-lexical function (x, y), and will take an argument which is the index;
this index for first-from operation will be identified by 0, while for the 1 it will identify the last-from operation.
Depending on these indexes the logic is hidden into the closure, that when it closes over x and y, if 0 (first-from) ->
get me the x, else get me the y.

The returned closure, which takes one argument and returns either x or y, depending whether the argument is 0 or 1;
The rational-numbers are expressed here by PAIRS. so a pair is considered a list of 2 items.


