;;;;;;;;;;;;;;;;;;;;;;;;
;; #131:: Sum Some Set Subsets
;; Difficulty:	Medium
;; Topics:	math
;; Given a variable number of sets of integers, create a function which returns true iff all 
;; of the sets have a non-empty subset with an equivalent summation. 
;; solved-times: 587
;; how i think of it: find all the reductions by applying + on each set -> combine the result of applying reductions +
;; with each source-set(from which it resulted) -> find if there are intersected elements in all of the sets

(ns clojure-learning.equiv-summation
  (use [clojure.repl])
  (require [clojure.test :refer :all]))

(defn equiv-summation [& xsets]
  (letfn [(reducted-sum-results [xsets]
            (map #(reductions + %) xsets))]
    (map (fn [& reducted-and-xset-tuple]
           (apply concat reducted-and-xset-tuple)) 
         (reducted-sum-results xsets) (map sort xsets))))

(equiv-summation #{1 3 5} #{9 11 4} #{-3 12 3} #{-3 4 -2 10})

(equiv-summation #{-1 3 -5 7 -9 11 -13 15}
                 #{1 -3 5 -7 9 -11 13 -15}
                 #{1 -1 2 -2 4 -4 8 -8})

;; at this point my func returns somethign like:
;; ((1 4 9 1 3 5) (4 15 24 9 11 4) (-3 0 12 -3 3 12) (-2 2 -1 9 -2 4 -3 10))

;; moving on -> i need to create a function which finds if there's one intersection between all the sequences
(for [x [1 2 3] y [1 2 3] z [1 2 3] :while (not= x y z)] [x y z])

(comment
(deftest test-equiv-summation
  (testing "if applying sum on some items from each subset yields a num which is in all of the sets"
    (is true (equiv-summation #{1 3 5}
                              #{9 11 4}
                              #{-3 12 3}
                              #{-3 4 -2 10}))

)))
;; -> (= (+ 1 3 5) (9..) (+ -3 12) (-3 4 -2 10))

(partition 2 1 [-1 3 -5 7 -9 11 -13 15])
(partition 2 1 (sort #{1 -1 2 -2 4 -4 8 -8}))
(partition 2 1 (sort [1 -1 2 -2 4 -4 8 -8]))
