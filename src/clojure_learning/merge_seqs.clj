(ns clojure-learning.merge-seqs
    (use clojure.repl)
    (require [clojure.test :refer :all]))

;; neat recursive function which parses two seqs and merges them into one sorted list
(defn merge-seqs 
  ([xs ys] (merge-seqs xs ys []))
  ([[head-x & tail-x] [head-y & tail-y] merged]
  (if (nil? head-x)
    (reduce conj merged (cons head-y tail-y))
    (if (nil? head-y)
      (reduce conj merged (cons head-x tail-x))
      (if (< head-x head-y)
        (recur tail-x (cons head-y tail-y) (conj merged head-x))
        (recur (cons head-x tail-x) tail-y (conj merged head-y)))))))

(deftest test-merge-seqs
  (testing "should merge two sorted seqs into one ordered list"
    (is (= [1 2 3 4 5 6 7 8] (merge-seqs [1 4 7] [2 3 5 6 8]))) ;; true
))

