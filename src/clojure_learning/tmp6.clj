(ns tmp6
  (:use clojure.repl))

;; Insert between two items
;; Difficulty:	Medium
;; Topics: seqs core-functions
;; Write a function that takes a two-argument predicate, a value, and a collection;
;; and returns a new collection where the value is inserted between every two items that satisfy the predicate.
;; (= '(1 :less 6 :less 7 4 3) (__ < :less [1 6 7 4 3]))
;; (= '(2) (__ > :more [2]))
;; (= [0 1 :x 2 :x 3 :x 4]  (__ #(and (pos? %) (< % %2)) :x (range 5)))

;; 1st version using HOFs:
(defn into-if-predicate
  [predicate insertion xs]
  (if-let [[head & tail] (seq xs)]
    (reduce
       (fn [acc x]
         (if (predicate (last acc) x)
           (conj acc insertion x)
           (conj acc x)))
       [head] tail)
    []))

(into-if-predicate < :less [1 6 7 4 3]) ;; [1 :less 6 :less 7 4 3] -> OK

;; 2nd version using low-level recursion
(defn into-if-predicate
  [predicate insertion xs]
  (loop [[head secnd & tail] xs
         acc []]
    (if (nil? head) acc
      (if (nil? secnd)
        (conj acc head)
        (if (predicate head secnd)
          (recur (cons secnd tail) (conj acc head insertion))
          (recur (cons secnd tail) (conj acc head)))))))

(into-if-predicate < :less [1 6 7 4 3]) ;; -> [1 :less 6 :less 7 4 3]
(into-if-predicate < :more [2]) ;; -> [2]
(into-if-predicate #(and (pos? %) (< % %2)) :x (range 5)) ;; -> [0 1 : x 2 :x 3 :x 4]
(into-if-predicate > :more ())

;; absolute test :)
(take 12 (->> [0 1]
                 (iterate (fn [[a b]] [b (+ a b)]))
                 (map first) ; fibonacci numbers
                 (into-if-predicate (fn [a b] ; both even or both odd
                       (= (mod a 2) (mod b 2)))
                     :same)))
;; FAILS WITH --> ArithmeticException: IntegerOverflow

;; last test fails due to IntegerOverflow exception -> means realized + eager evaluation
;; for this reason: [reduce] fails because it realizes all the collection while yielding the result
;; Solution is to build a lazy-seq and to accumulate in that lazy-seq
;; (that's the reason why: take 12 is placed :)

;; 3rd version! using purely lazy-seq -> that yields a lazy-seq
(defn into-if-predicate
  [predicate insertion xs]
   (lazy-seq
      (if (nil? (first xs)) []
        (if (nil? (second xs)) [(first xs)]
          (if (predicate (first xs) (second xs))
            (lazy-cat [(first xs) insertion] (into-if-predicate predicate insertion (rest xs)))
            (cons (first xs) (into-if-predicate predicate insertion (rest xs))))))))

(into-if-predicate < :less [1 6 7 4 3]) ;; -> (1 :less 6 :less 7 4 3)
(class (into-if-predicate < :less [1 6 7 4 3])) ;; -> clojure.lang.LazySeq -> OK!
(into-if-predicate < :more [2]) ;; -> (2)
(into-if-predicate #(and (pos? %) (< % %2)) :x (range 5)) ;; -> (0 1 : x 2 :x 3 :x 4)

;; refactoring the function to use args-destructuring instead, of programmatically handle:
;; first + second funcs
(defn into-if-predicate
  [predicate insertion [head secnd & tail]]
   (lazy-seq
      (if (nil? head) []
        (if (nil? secnd) [head]
          (if (predicate head secnd)
            (lazy-cat [head insertion] (into-if-predicate predicate insertion (cons secnd tail)))
            (cons head (into-if-predicate predicate insertion (cons secnd tail))))))))

(take 12 (->> [0 1]
                 (iterate (fn [[a b]] [b (+ a b)]))
                 (map first) ; fibonacci numbers
                 (into-if-predicate (fn [a b] ; both even or both odd
                       (= (mod a 2) (mod b 2)))
                     :same)))
;; (0 1 :same 1 2 3 :same 5 8 13 :same 21) --> COOL :)