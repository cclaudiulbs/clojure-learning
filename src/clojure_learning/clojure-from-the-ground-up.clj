(ns types
  (use clojure.repl))
(+) ;; 0
(*) ;; 1

(<= 1 2 3) ;; is smaller-than 1 than 2, and is smaller 2 than 2?
(<= 1 1 2) ;; true is smaller-or-equal 1 than 1? true, next is smaller-or-equal 1 than 2 -> true

(= 2 2.0) ;; false -> strong comparison
(== 2 2.0) ;; true -> loose comparison -> 2 gets converted to double first

(type "clojure") ;; java.lang.String

;; string representation for nil is "" empty string
(str nil) ;; ""

;; in clojure the only negative values are false and nil.
;; The job of symbols is to refer to things, to point to other values.
;; When evaluating a program, symbols are looked up and replaced by their corresponding values.

;; FUNCTOR: a way to combine a HOF Type with a collection Type that obeys several rules:
;; clojure.core/map is NOT a true FUNCTOR! because clojure.core/map does always return a LIST even if applied on another
;; data-structure.
;; another property of a FUNCTOR is that when given a function that transforms a collection Type
;; passing the "identity" function for transformation -> it should result the collection itself.
(map identity [1 2 3]) ;; (1 2 3)
(identity [1 2 3])     ;; [1 2 3] -> NOT ENOUGH!
;; mapping the identity function over a collection should just returns the collection itself.
;; the second property of a FUNCTOR is: that if mapping a function as:
;; (map bar-fn (map foo-fn coll)) can be written as composiing the functions: foo-fn and bar-fn into one operation.
;; (map (comp bar-fn foo-fn) coll) -> TRUE functor.

;; juxt is an applicative functor:
(map (juxt inc dec inc) [1 2]) ;; ([2 0 2] [3 1 3])
;; is an applicative functor because it takes a collection of functions and applies each func on each argument.
;; resulting always a collection of the computed things.
((juxt inc dec inc) 2) ;; [3 1 3]

(require '[clojure.xml :as xml]
         '[clojure.zip :as zip])

(source xml/parse)