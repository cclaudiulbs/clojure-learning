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

(defn find-min-path
  [points]
  (let [exp (int (Math/pow 2 (count points)))]
     (reduce
       (fn [acc v] 
          (conj acc 
              (reduce 
                  (fn [inner-acc el] (into inner-acc (repeat exp el))) 
                  [] v)))
        [] points)))


(deftest test-find-min-path
  (testing "triangle-min-path to find the min path and reduce it using add func"
      (is (= 7 (find-min-path '([1]
                               [2 4]
                              [5 1 4]
                             [2 3 4 5]))))
  ))
;; 1->2->1->3

(Math/pow 2 2)
(find-min-path '([1]
                [2 4]
               [5 1 4]
              [2 3 4 5]))
