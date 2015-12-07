(ns clojure-learning.clojure-monads-from-scratch
  (use (clojure [repl] [test])))

;; Functors
;; Taking Haskell Type signatures can make a big difference when explaining and understanding monads
;; Taking the multiplication function from clojure, the Func signature is:
;; (*):: Num a => a -> a -> a
;; Num a here is a type constraint: the domain-args Types follow after =>
;; This is a function which takes two arguments; in haskell this function is curried by default so
;; * is a function which takes an [a] -> and returns another function which takes another [a], the
;; result of invoking the second function will result in a value of type a. a - should be an instance of Num;

;; In a nutshell, Functors are things that can be mapped over(using a mapper function).
;; Haskell defines Functors as:

;; class Functor f where
;;    fmap:: (a -> b) -> f a -> f b
;; 
;; By definition each functor should define a function [fmap] which takes a function, that knows how to 
;; convert a value of type a -> to a value of type b, and the second argument a Functor f, which wrapps
;; a value of type [a]; the result of applying fmap is another Functor [f] with the value of type [b];

;; There can be Functors of many types. For instance the List Functor defines a function [fmap]:
;;  fmap:: (a -> b) -> [a] -> [b]
;; 
;; fmap is a function which takes as 1st argument a function, that knows how to convert each element of type a
;; from the List Functor to a value of type b; the result of applying [fmap] is another List Functor that wrapps
;; elements of type b

;; looks familiar? -> should do -> because that's the essential function map from any functionalish langs.

;; defining Functors in clojure using: records and protocols
(defprotocol Functor
  (fmap [func functor] "Functor takes a func that maps over a functor"))

(defrecord List [wrapped-val]
  Functor
  (fmap [functor func] 
    (->List (map func (:wrapped-val functor)))))

;; the essence is: define a Functor protocol, that exports a single fmap func
;; then define a Functor that wrapps ANY value-container; the map function applies the func arg function
;; over the extracted value by record-label from the passed in functor; it handles the wrapping again in
;; another Functor back;
;; demo:
(def langs-functor (->List ["clojure" "js"]))  ;; List{:wrapped-val ["clojure" "js"]}
(fmap langs-functor clojure.string/upper-case) ;; List{:wrapped-val ["CLOJURE" "JS"]}

;; Functors have 2 rules:
;; 1. Identity:: mapping an identity function over the Functor is the same as applying the identity func to Functor
;; itself
(deftest test-functor-core-rules
  (testing "identity-rule for the functor"
    (is (= (fmap langs-functor identity)
           (identity langs-functor))) ;; true
  )
  (testing "composition-rule: if 2 funcs f and g, are composed the result of composing them is another function h"
           "that when applied on the Functor yields the same result as first applying g on the Functor and then f on"
           "the result of applying g"
    (let [f #(+ 10 %)
          g #(* 2 %)
          nums-functor (->List [2 3 4])]
      (is (= (fmap nums-functor (comp f g))
             (-> nums-functor
                 (fmap g)
                 (fmap f)))))  ;; true
))

;;;;;;;;;;
;; Applicative Functors
;;;;;;;;;;
;; Suppose we have some functions wrapped in a Functor, and we want to apply those wrapped functions on another Functor.
;; Here's how Haskell defines Applicative-Functors:
;; class (Functor functor) => Applicative f where
;;    pure:: a -> functor a
;;    (<*>):: functor (a -> b) -> functor a -> functor b

;; Applicative Functors are so called: Functors on steroids; They define by contract 2 operations: pure and apply
;; hence if a functor is to be applicative, it must be a Functor in the beginning;

;; the [pure] function is quite simple: it takes a value of type [a] and wrapps it into a Functor
;; the [<*>] function is the alias for apply: is a function that takes 2 arguments:
;;    the 1st argument is a Functor that wrapps a function, which knows how to convert a type a into a type b
;;    the 2nd argument is a Functor of type, on which the function wrapped in the 1st Functor arg knows how to operate
;;    the result of applying the Functor-of-func and Functor-of-a's -> is another Functor-of-type-b's

;; building on the previous List functor, the type signatures for list-functor are:
;;  pure:: a -> [a]
;;  <*>:: [(a -> b)] -> [a] -> [b]

;; The Types should allign! based on the [pure] function we define the apply functor contract;
(defmulti pure (fn dispatch-fn [functor-type _] functor-type))
(defmethod pure List [_ v] (->List (into [] v)))         ;; takes some value and wrapps it inside a List Functor

(defmulti <*> (fn dispatch-fn [functor-fns functor-vals] (class functor-fns)))
(defmethod <*> (class List) [functor-fns functor-vals]
  (->List
    (for [func (:wrapped-val functor-fns)
          v    (:wrapped-val functor-vals)]
        (func v))))

;; the apply <*> func is more interesting: It takes functor-fns and functor-vals, and using for-comprehension
;; it sequencing over each function wrapped inside the Functor and value-v wrapped inside the values-functor
;; and applies the func on v, building a sequence back, with all the combinations, and wraps the sequence result
;; back into a List Functor that is the same as -> List-b

;;demo: think about [juxt] a litlle bit :)
(def fns-functor (pure List [#(+ 10 %)]))
(def xs-functor (pure List [2 3 4]))

(<*> fns-functor xs-functor)  ;; List{:wrapped-val (12 13 14)}

;; Applicative Functors need as well to obey some rules: 
;; 1. Identity
;; 2. Composition
;; 3. Homomorphism
;; 4. Interchange
