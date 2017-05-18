(ns clojure-learning.merge-sorting)

;; cclaudiu's functional merge-sort :)

(defn merge-fn [left right]
  "In the merge-sort sorting algorithm the merge-fn is the core of the algorithm
   It will receive 2 partitions, starting with [1-element] at least in a partition
   It will attempt to traverse the partitions and recurring instead of in-place state
   mutations => each time a new version of the persistent-ds is created => kick ass GC
   i.e:: 
     (merge-fn [5] [2 9]) => [2 5 9]
     (merge-fn [1 3 4] [2 5 9]) => [1 2 3 4 5 9]"
  (loop [[x & xs :as all-left] left 
         [y & ys :as all-right] right
         sorted []]
    (if (nil? y) 
      (reduce conj sorted all-left)
      (if (nil? x) 
        (reduce conj sorted all-right)
        (if (< x y) 
          (recur xs all-right (conj sorted x)) ;; < x y:: preserve right/exhaust left
          (recur all-left ys (conj sorted y))  ;; > x y:: preserve left/exhaust right
)))))

(defn merge-sort [coll]
  "the main merge-sort function is preocupied of partitioning the initial coll
   into 2 sides, and then it will attempt to recur foreach branch/side(l/r)
   the results of recurring are then passed to the merge-fn which will receive
   from top-of-the-recur-stack the partitions in increasing size:: expansive recursion"
  (letfn [(partition-by [side-fn pivot coll]
            (map second 
              (side-fn 
                (fn [[idx x]] (< idx pivot))
                (map-indexed vector coll))))]
    (let [pivot (long (/ (count coll) 2))
          left-side (partition-by take-while pivot coll)
          right-side (partition-by drop-while pivot coll)]
      (cond 
        (<= (count coll) 1)
          coll
        :else
          (merge-fn
            (merge-sort left-side)
            (merge-sort right-side))
))))

;; coolio :)
(merge-sort [1 3 4 5 2 9]) ;; [1 2 3 4 5 9]
(merge-sort [1 3 4 5 3 2 1 9]) ;; [1 1 2 3 3 4 5 9]

(def rand-100k-nums (repeatedly 100000 (partial rand-int 100000)))
(def rand-1mil-nums (repeatedly 1000000 (partial rand-int 1000000)))

(def s-100k
  (time (merge-sort rand-100k-nums))) ;; "Elapsed time: 4111.425544 msecs"

(def s-100k-core
  (time (sort rand-100k-nums))) ;; "Elapsed time: 205.470737 msecs"

(def s-1mil
  (time (merge-sort rand-1mil-nums))) ;; "Elapsed time: 70130.117786 msecs"

(def s-1mil-core
  (time (sort rand-1mil-nums))) ;; "Elapsed time: 2719.555278 msecs"

;; Notes::
;; following those benchmarks => one can see clearly that using 
;; persistent-immutable-data-structures is again NOT the right way of
;; handling algorithms which are not parallelizable
;; however merge-sort can be...partially paralelizable when branching
;; and NOT in the merge-fn which relies on the states of both partitions