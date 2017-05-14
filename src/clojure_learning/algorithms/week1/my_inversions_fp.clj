(ns clojure-learning.algorithms.my-inversions-fp)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Count inversions in an array

;; Your task is to compute the number of inversions in the file given, where the ith row of the file indicates the ith entry of an array.
;; Because of the large size of this array, you should implement the fast divide-and-conquer algorithm covered in the video lectures.
;; The numeric answer for the given input file should be typed in the space below.

;; This version is quite faster by using some core clojure functions 
;; instead of custom-building them
;; + drops the index-based operations in favour of head-tail idioms;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn inversions [file-path]
  (letfn [(load-file-list [abs-path]
            (letfn [(parse-int [s] (int (Integer. s)))]
              (with-open [line (reader abs-path)]
                (doall (map parse-int (line-seq line))))))
          (find-inversions [xs ys inversions]
            (loop [[head-x & tail-xs] xs
                   [head-y & tail-ys] ys
                   sorted [] 
                   invs inversions]
              (if (nil? head-x) 
                {:tosort (into sorted (cons head-y tail-ys)) :inversions invs}
                (if (nil? head-y)
                  {:tosort (into sorted (cons head-x tail-xs)) :inversions invs}
                  (if (<= head-x head-y)
                    (recur tail-xs               ;; step next in left
                           (cons head-y tail-ys) ;; maintain right
                           (conj sorted head-x)  ;; add left-curr to sorted
                           invs)
                    (recur (cons head-x tail-xs) ;; maintain left
                           tail-ys               ;; step next in right
                           (conj sorted head-y)  ;; add right-curr to sorted
                           (+ invs (count (filter (fn [each-left] (< head-y each-left)) 
                                                  (cons head-x tail-xs))))))))))
          (count-inversions
            ([xs] (count-inversions xs 0))
            ([xs inversions]
              (if (> (count xs) 1) 
                (let [[l-branch r-branch] (split-at (quot (count xs) 2) xs)
                      l-mapped-res (count-inversions l-branch inversions)
                      r-mapped-res (count-inversions r-branch inversions)]
                  (find-inversions (get l-mapped-res :tosort) 
                                   (get r-mapped-res :tosort) 
                                   (+ (:inversions l-mapped-res) (:inversions r-mapped-res))))
                {:tosort xs :inversions inversions}))) ]
    (->> file-path
        load-file-list
        count-inversions
        :inversions)))

;; testing::
;; (inversions "C:/Users/I312366/MyProgramming/Clojure/tmp/clojure-project/resources/IntegerArray.txt")

;; benchmarking + result
(time
  (let [invs (inversions "src/clojure_learning/algorithms/IntegerArray.txt")]
    invs))
;; "Elapsed time: 155947.25851 msecs":: 2407905288