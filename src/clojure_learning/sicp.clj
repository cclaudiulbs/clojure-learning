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
