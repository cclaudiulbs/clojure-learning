;; #168: Infinite Matrix
;; Difficulty:	Medium
;; Topics:	seqs recursion math
;; Solved: 418 times
;; in short: build a infinite matrix: infinite rows + infinite cols with some additional logic on generated items
(ns clojure_learning.infinite-matrix
  (use clojure.repl)
  (use clojure.test)
  (require [clojure.set :as s]))

(for [x (range 0 10) y (range 0 10)] (str x y))

;; 1st create a func which generates a lazy-seq
(defn rrange 
  "rrange start -> generates an infinite lazy-seq -> requires a take to establish the bounds"
  ([start] 
    (lazy-seq
      (cons start (rrange (inc start)))))
  ([start end] (rrange start end 1))
  ([start end step]
    (lazy-seq
      (when (< start end)
        (cons start (rrange (+ step start) end step))))))

(class (rrange 0 10 2)) ;; clojure.lang.LazySeq
(rrange 0 10 2)         ;; (0 2 4 6 8)
(rrange 0 4)            ;; (0 1 2 3)
(take 4 (rrange 0))     ;; (0 1 2 3)

(defn gen-infinite-matrix [] ())

(deftest test-gen-infinite-matrix
  (testing "should generate an infinite matrix of: infinite rows of combined 0, 1, 2...for 1, 2, 3..."
    (is
      (= (take 5 (map #(take 6 %) (gen-infinite-matrix str)))
         [["00" "01" "02" "03" "04" "05"]
          ["10" "11" "12" "13" "14" "15"]
          ["20" "21" "22" "23" "24" "25"]
          ["30" "31" "32" "33" "34" "35"]
          ["40" "41" "42" "43" "44" "45"]]))
    (is
      (= (gen-infinite-matrix * 3 5 5 7)
         [[15 18 21 24 27 30 33]
          [20 24 28 32 36 40 44]
          [25 30 35 40 45 50 55]
          [30 36 42 48 54 60 66]
          [35 42 49 56 63 70 77]])
))
