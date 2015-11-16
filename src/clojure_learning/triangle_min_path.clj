;;;;;;;;;;;;;;;;;;;;;;;;
;; Triangle Minimal Path
;; Difficulty:	Hard
;; Topics:	graph-theory
;; Write a function which calculates the sum of the minimal path through a triangle. 
;; The triangle is represented as a collection of vectors. 
;; The path should start at the top of the triangle and move to an adjacent number on the next row 
;; until the bottom of the triangle is reached.
(ns clojure_learning.triangle-min-path
    (use [clojure.repl])
    (require [clojure.test :refer :all]))

(defn triangle-min-path
  [points] 
  (letfn [(repeat-each-times [vov times]
            (letfn [(-repeat-each-times 
                      [[head & tail] curr times repeated]
                      (if (or (zero? times) (nil? head)) repeated
                        (if (zero? curr)
                          (recur tail times times (conj repeated head))
                          (recur (cons head tail) (dec curr) times (conj repeated head)))))]
                (-repeat-each-times vov (dec times) (dec times) [])))
          (repeat-matrix [[head-xs & tail-xs] times matrix-repeated]
            (if (nil? head-xs) matrix-repeated
              (recur tail-xs (dec times) (conj matrix-repeated (repeat-each-times head-xs times)))))
          (trim-sides [xs how-many]
            (reverse 
              (drop how-many 
                    (reverse 
                        (drop how-many xs)))))
         (trim-sides-by-first [matrix]
            (reduce 
              (fn [acc xs]
                (let [half-diff (quot (- (count xs) (count (last acc))) 2)]
                  (conj acc (trim-sides xs half-diff))))
              [(first matrix)] (rest matrix)))]

      (->>
          (-> (repeat-matrix points ((comp inc inc count) points) [])
              trim-sides-by-first)
        (apply map vector)  ;; transpose to cols: map can take as many colls: will map as vector x1 y1... x2 y2...
        (map #(reduce + %)) ;; build sum list
        (apply min))
))

(triangle-min-path '([1]
                [2 4]
               [5 1 4]
              [2 3 4 5])) ;; 7 -> NICE :

(triangle-min-path '([3]
                    [2 4]
                   [1 9 3]
                  [9 9 2 4]
                 [4 6 6 7 8]
                [5 7 3 5 1 4])) ;; 20:: 3->4->3->2->7->1 

;; let's build a function which takes 
(defn repeat-each-times [vov times]
  "func that: sequential number-of-times -> sequential of each element duplicated the number-of-times"
  (letfn [(-repeat-each-times [[head & tail] curr times repeated]
            (if (or (zero? times) (nil? head)) repeated
              (if (zero? curr)
                (recur tail times times (conj repeated head))
                (recur (cons head tail) (dec curr) times (conj repeated head)))))]
    (-repeat-each-times vov (dec times) (dec times) [])))

(deftest test-repeat-each-times
  (testing "func should duplicate the elements of a sequential the number-of-times"
    (is 
      (= [1 1 3 3] (repeat-each-times [1 3] 2)) ;; true
      (= [2 2 2 4 4 4] (repeat-each-times [2 4] 3)) ;; true
      (= [1 1 1] (repeat-each-times [1] 3)) ;; true
  )))

(defn trim-sides [xs how-many]
  "function that takes a coll and trims the l-side and r-side with the number of items"
  (reverse (drop how-many (reverse (drop how-many (vec xs))))))

(deftest test-trim-sides
  (testing "trim-sides func which drops the leading + trailing num of elements from a sequence"
    (is (= (list 3) (trim-sides [1 2 3 4 5] 2)))  ;; true
))

(defn trim-sides-by-last [matrix]
  (reduce 
    (fn [acc xs]
      (let [half-diff (quot (- (count xs) (count (last acc))) 2)]
      (conj acc (trim-sides xs half-diff))))
    [(last matrix)] (butlast matrix)))

(deftest test-trim-sides-by-last
  (testing "should trim the sides of each row from matrix by last matrix-row num of elements"
    (is (= [[1 2 3 4 5 6 7 8] [3 4 5 6 7 8 9 10]] (trim-sides-by-last [[1 2 3 4 5 6 7 8 9 10 11 12] [1 2 3 4 5 6 7 8]])))
)) ;; true

(defn trim-sides-by-first [matrix]
  (reduce 
    (fn [acc xs]
      (let [half-diff (quot (- (count xs) (count (last acc))) 2)]
      (conj acc (trim-sides xs half-diff))))
    [(first matrix)] (rest matrix)))

(deftest test-trim-sides-by-first
  (testing "should trim the sides of each row from matrix by first matrix-row num of elements"
    (is (= [[1 2 3 4 5 6 7 8] [3 4 5 6 7 8 9 10]] (trim-sides-by-first [[1 2 3 4 5 6 7 8] [1 2 3 4 5 6 7 8 9 10 11 12]])))
)) ;; true

;; ultimate abs test :)
(deftest test-triangle-min-path
  (testing "triangle-min-path to find the min path and reduce it using add func"
      (is (= 7 (triangle-min-path '([1]
                               [2 4]
                              [5 1 4]
                             [2 3 4 5])))) ;; true:: 1->2->1->3

      (is (= 20 (triangle-min-path '([3]
                                [2 4]
                               [1 9 3]
                              [9 9 2 4]
                             [4 6 6 7 8]
                            [5 7 3 5 1 4])))) ;; fail: 26 != 20:: 3->4->3->2->7->1 
))

;; other user solution:
(letfn[(minPaths [triangle, row]
                 (let [currentRow (nth triangle row)]
                   (if (= (inc row) (count triangle)) currentRow;last row is the always the minimal
                     (let [nextMinimal (map #(apply min %) (partition 2 1 (minPaths triangle (inc row))))];[2 3 4 5]->((2 3) (3 4) (4 5))->(2 3 4)
                       (map + currentRow nextMinimal)))))];get the current minimal path
  (fn [triangle]
    (first (minPaths triangle 0))));minPaths returns a sequence, but only a single value is needed

;; or:
(fn [levels]
    (first (reduce (fn [cheapest level]
                     (map +
                          level
                          (map #(apply min %) (partition 2 1 cheapest))))
                   (reverse levels))))
