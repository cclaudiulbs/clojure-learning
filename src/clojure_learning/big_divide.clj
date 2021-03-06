;;;;;;;;;;;;;;;;;
;; The Big Divide
;; Difficulty:	Medium
;; Topics:	math
;; Write a function which calculates the sum of all natural numbers under n (first argument) which are evenly 
;; divisible by at least one of a and b
;; Note: Some test cases have a very large n, so the most obvious solution will exceed the time limit.
(ns clojure_learning.big-divide
    (use [clojure.repl])
    (require [clojure.test :refer :all]))

(defn find-sum-of-nums-div-with 
  [until-num or-div-x or-div-y]
  (apply +
      (reduce 
        (fn [acc x]
          (if (or (zero? (rem x or-div-x)) (zero? (rem x or-div-y)))
            (conj acc x)
            acc))
        [] (range 0 until-num))))

;; wtf?! that was easy :)
(find-sum-of-nums-div-with 1000 3 5) ;; 233168

;; while this takes a huge time to process! -> implement the lazy version!!!
;; (find-sum-nums-div-with 100000000 3 5)

(defn find-sum-of-nums-div-with
  [until-num or-div-x or-div-y]
    (letfn [(find-divisibles-from [curr bound step]
              (lazy-seq
                (when (< curr bound)
                  (cons curr (find-divisibles-from (+ curr step) bound step)))))]
      (find-divisibles-from 0 until-num or-div-x)))

(class (find-sum-of-nums-div-with 100000000 3 5))
(do 
  (println (str "start: " (java.util.Date.)))
  (println (find-sum-of-nums-div-with 10000000 3 5))
  (println (str "start: " (java.util.Date.))))

(find-sum-of-nums-div-with 13 3 0)

(letfn [(divisible? [with x] (or (zero? (rem x with)) ))]
  (filter (partial divisible? 3) (range 0 1000)))

