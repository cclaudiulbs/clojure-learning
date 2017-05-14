(ns clojure-learning.algorithms.week2
  (:require [clojure.java.io :as io]))

;;; Week 2 - Programming Assignment
;;; Your task is to compute the total number of comparisons used to sort the given input file by QuickSort. As you know, the number of comparisons depends on which elements are chosen as pivots, so we'll ask you to explore three different pivoting rules.
;;; You should not count comparisons one-by-one. Rather, when there is a recursive call on a subarray of length m, you should simply add m−1 to your running total of comparisons. (This is because the pivot element is compared to each of the other m−1 elements in the subarray in this recursive call.)

;;;;;;;;;;
;; Using in-memory array:: taking an already loaded arr
;;;;;;;;;;
(defn quick-sort-comparisons [arr]
  (let [comparisons-atom (atom 0)]
    (letfn [(arr-swap [arr at-pos with-pos]
            (let [from (get arr at-pos)
                  to (get arr with-pos)]
              (aset arr at-pos to)
              (aset arr with-pos from)
              arr))

          (calculate-pivot-index [start-pos end-pos]
            (let [subarr-counted (- end-pos start-pos)]
              (if (even? subarr-counted)
                (+ (dec (quot subarr-counted 2)) start-pos)
                (+ (quot subarr-counted 2) start-pos))))

          (quick-sort-by [arr from-index to-index]
            (letfn [(reorder-by-index [arr from-idx to-idx]
                      (let [pivot-index (calculate-pivot-index from-idx to-idx)]
                        (arr-swap arr from-idx pivot-index)
                        (loop [i (inc from-idx)
                               j (inc from-idx)]
                          (if (= i to-idx)                           ;; reached upper-bound inner-partition
                            (do 
                              (arr-swap arr from-idx (dec j))        
                              {:sorted-arr arr :next-pivot-index j})
                            (if (> (get arr i) (get arr from-idx))   ;; pivot < next
                              (do
                                (swap! comparisons-atom inc)
                                (recur (inc i) j))                      ;; step to next element
                              (do 
                                (arr-swap arr i j)                   ;; piv > next -> swap big <-> small
                                (swap! comparisons-atom inc)
                                (recur (inc i) (inc j))))
                            ))))]
              (if (= from-index to-index) {:arr arr :comparisons (deref comparisons-atom)}
                (let [{:keys [sorted-arr next-pivot-index]} (reorder-by-index arr from-index to-index)]
                  (cond (= (- to-index from-index) 1) 
                        {:arr arr :comparisons (deref comparisons-atom)}
                        :else 
                        (do
                          (quick-sort-by arr from-index next-pivot-index)
                          (quick-sort-by arr next-pivot-index to-index))
                  )))))]
    ;; final application...glue
    (quick-sort-by arr 0 (count arr)))))

(quick-sort-comparisons (into-array [1 2]))    
(quick-sort-comparisons (into-array [3 1 2]))    ;; {:arr [1, 2, 3], :comparisons 4}
(quick-sort-comparisons (into-array [1 2 3]))    ;; {:arr [1, 2, 3], :comparisons 3}
(quick-sort-comparisons (into-array [3 2 1]))    ;; {:arr [1, 2, 3], :comparisons 3}
(quick-sort-comparisons (into-array (reverse (take 10 (range))))) ;; {:arr [0, 1, 2, 3, 4, 5, 6, 7, 8, 9], :comparisons 14}

(quick-sort-comparisons (into-array [12, 10, 118, 99, 444, 5, 16, 7, 222, 1000, 28, 339, 54, 25, 666, 1])) ;; 27
(quick-sort-comparisons (into-array [10 19 16 14 28 6])) 

;;;;;;;;;;;;;;
;; Homework:: Problem III -> use a median pivot(harder but improved over choosing first el or last as pivot)
;; Adapted to load as well the homework-file from classpath...
;;;;;;;;;;;;;;
(defn quick-sort-comparisons-from-file []
  (let [comparisons-atom (atom 0)]
    (letfn [(load-quick-sort-nums [rel-path]
              (letfn [(parse-big-int [s] (BigInteger. s))]
                (with-open [line (io/reader rel-path)]
                  (doall (map parse-big-int (line-seq line))))))
            
            (arr-swap [arr at-pos with-pos]
            (let [from (get arr at-pos)
                  to (get arr with-pos)]
              (aset arr at-pos to)
              (aset arr with-pos from)
              arr))

          (calculate-pivot-index [start-pos end-pos]
            (let [subarr-counted (- end-pos start-pos)]
              (if (even? subarr-counted)
                (+ (dec (quot subarr-counted 2)) start-pos)
                (+ (quot subarr-counted 2) start-pos))))

          (quick-sort-by [arr from-index to-index]
            (letfn [(reorder-by-index [arr from-idx to-idx]
                      (let [pivot-index (calculate-pivot-index from-idx to-idx)]
                        (arr-swap arr from-idx pivot-index)
                        (loop [i (inc from-idx)
                               j (inc from-idx)]
                          (if (= i to-idx)                           ;; reached upper-bound inner-partition
                            (do 
                              (arr-swap arr from-idx (dec j))        
                              {:sorted-arr arr :next-pivot-index j})
                            (if (> (get arr i) (get arr from-idx))   ;; pivot < next
                              (do
                                (swap! comparisons-atom inc)
                                (recur (inc i) j))
                              (do 
                                (arr-swap arr i j)                   ;; piv > next -> swap big <-> small
                                (swap! comparisons-atom inc)
                                (recur (inc i) (inc j))))
                    ))))]
              (if (= from-index to-index) 
                {:arr arr :comparisons (deref comparisons-atom)}
                (let [{:keys [sorted-arr next-pivot-index]} (reorder-by-index arr from-index to-index)]
                  (cond (= (- to-index from-index) 1) 
                        {:arr arr :comparisons (deref comparisons-atom)}
                        :else 
                        (do
                           (quick-sort-by arr from-index next-pivot-index)
                           (quick-sort-by arr next-pivot-index to-index))
               )))))]
      
      (let [arr-unsorted (-> "src/clojure_learning/algorithms/week2/input.txt"
                           load-quick-sort-nums
                           into-array)]
        (quick-sort-by arr-unsorted 0 (count arr-unsorted))))))

;; run...
(time
  (let [result (quick-sort-comparisons-from-file)]
    (:comparisons result)))
;; "Elapsed time: 2256.038245 msecs":: 162918

;; should be
;; partition-1st:: => 162085
;; partition-last:: "Elapsed time: 5039.854335 msecs" => 164123
;; partition-middle:: "Elapsed time: 5094.646353 msecs" => 138382