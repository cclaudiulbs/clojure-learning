;; #168: Infinite Matrix ;; Difficulty:	Medium
;; Topics:	seqs recursion math
;; Solved: 418 times
;; Special restriction:for, range, iterate, repeat, cycle, drop 
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

(defn gen-infinite-rows [func col] 
  (map (partial func col) (rrange 0)))

(defn gen-infinite-cols [func]
  (map (partial gen-infinite-rows func) (rrange 0)))

(take 3 (gen-infinite-rows str 0)) ;; ("00" "01" "02")
(take 3 (map #(take 2 %) (gen-infinite-cols str))) ;; (("00" "01") ("10" "11") ("20" "21"))

;; 1st draft of the function! working for 1st test
;; with the argument f, it returns the infinite matrix A that has the entry in the i-th row and the j-th 
;; column equal to f(i,j) for i,j = 0,1,2,...;  --> PASS
(defn gen-infinite-matrix [func]
  (letfn [(gen-range [from]
            (lazy-seq
              (cons from (gen-range (inc from)))))
          (gen-infinite-rows [func col]
            (map (partial func col) (gen-range 0)))
          (gen-infinite-cols [func]
            (map (partial gen-infinite-rows func) (gen-range 0)))]
    (gen-infinite-cols func)))

;; remember to specify take -> many
(take 4 (map (partial take 3) (gen-infinite-matrix str)))

(deftest test-gen-infinite-matrix
  (testing "should generate an infinite matrix of: infinite rows of combined 0, 1, 2...for 1, 2, 3..."
    (is (= (take 5 (map #(take 6 %) (gen-infinite-matrix str)))
         [["00" "01" "02" "03" "04" "05"]
          ["10" "11" "12" "13" "14" "15"]
          ["20" "21" "22" "23" "24" "25"]
          ["30" "31" "32" "33" "34" "35"]
          ["40" "41" "42" "43" "44" "45"]])) ;; -> ok
))

;; 2nd step is to parameterize the fn with additional args, such that:
;; when the: func fn r c -> infinite matrix starting from row: r, and coll: c (drop had been a great choice here)
(defn gen-infinite-matrix [func r c]
  (letfn [(gen-range [from]
            (lazy-seq
              (cons from (gen-range (inc from)))))
          (gen-infinite-cols [func each-col from-col]
            (map (partial func each-col) (gen-range from-col)))
          (gen-infinite-rows [func from-row from-col]
            (map (partial gen-infinite-cols func from-col) (gen-range from-row)))]
    (gen-infinite-rows func r c)))

(take 5 (map #(take 7 %) (gen-infinite-matrix * 3 5)))

;; 3rd step is to limit the matrix to by taking only x-rows and y-cols(establish boundaries)
(defn gen-infinite-matrix 
  ([func] 
   (gen-infinite-matrix func 0 0))
  ([func from-row from-col] 
    (letfn [(gen-range [from]
            (lazy-seq
              (cons from (gen-range (inc from)))))
          (gen-infinite-cols [func each-col from-col]
            (map (partial func each-col) (gen-range from-col)))
          (gen-infinite-rows [func from-row from-col]
            (map #(gen-infinite-cols func % from-col) (gen-range from-row)))]
      (gen-infinite-rows func from-row from-col)))
  ([func from-row from-col until-row until-col]
     (take until-row 
       (map #(take until-col %) (gen-infinite-matrix func from-row from-col)))))

;; absolute tests
(deftest test-gen-infinite-matrix
  (testing "should generate an infinite matrix of: infinite rows of combined 0, 1, 2...for 1, 2, 3..."
    (is (= (take 5 (map #(take 6 %) (gen-infinite-matrix str)))
         [["00" "01" "02" "03" "04" "05"]
          ["10" "11" "12" "13" "14" "15"]
          ["20" "21" "22" "23" "24" "25"]
          ["30" "31" "32" "33" "34" "35"]
          ["40" "41" "42" "43" "44" "45"]])) ;; -> ok

    (is (= (gen-infinite-matrix * 3 5 5 7)
         [[15 18 21 24 27 30 33]
          [20 24 28 32 36 40 44]
          [25 30 35 40 45 50 55]
          [30 36 42 48 54 60 66]
          [35 42 49 56 63 70 77]])) ;; -> ok

    (is (= (gen-infinite-matrix * 3 5 5 7)
         [[15 18 21 24 27 30 33]
          [20 24 28 32 36 40 44]
          [25 30 35 40 45 50 55]
          [30 36 42 48 54 60 66]
          [35 42 49 56 63 70 77]]))

    (is (= (gen-infinite-matrix #(/ % (inc %2)) 1 0 6 4)
          [[1/1 1/2 1/3 1/4]
           [2/1 2/2 2/3 1/2]
           [3/1 3/2 3/3 3/4]
           [4/1 4/2 4/3 4/4]
           [5/1 5/2 5/3 5/4]
           [6/1 6/2 6/3 6/4]])) ;; -> ok
))
