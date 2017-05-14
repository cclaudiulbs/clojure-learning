(ns clojure_learning.algorithms.merge-sort)
;; used in the 1st Week of Design and analyses of Algorithms(Tim-Coursera)
;; by the inversion-counter function

;; merge-sort base-function
(defn merge-sort [xs] 
    (letfn [(merge-fn [[xs ys]]                 "the most important func in merge-sort->destructuring the vec-tuples"
              (letfn [(reached-bound [idx xs]   "verify if index is reached the upper bound"
                        (= idx (count xs)))
                      (rest-from-index [idx xs] "take the rest of collection from where the idx left"
                        (map second 
                          (filter (fn [[rem-idx el]] (>= rem-idx idx)) (map-indexed vector xs))))]
                (loop [sorted []
                       k (count (into xs ys)) 
                       i 0 j 0]
                  (if (zero? k) sorted
                    (if (reached-bound i xs) (into sorted (rest-from-index j ys))
                      (if (reached-bound j ys) (into sorted (rest-from-index i xs))
                        (if (< (get xs i) (get ys j))
                          (recur (conj sorted (get xs i)) (dec k) (inc i) j)
                          (recur (conj sorted (get ys j)) (dec k) i (inc j) ))))))))
            (split-in-halfes [xs]
              (let [half (quot (count xs) 2)]
                (reduce 
                  (fn [[l r] [idx el]] 
                    (if (< idx half) 
                      [(conj l el) r] 
                      [l (conj r el)])) 
                  [[] []] 
                  (map-indexed vector xs))))
            (merge-sort-internal [[l r]]
              (if (empty? l) r
                (merge-fn [(merge-sort (merge-fn (split-in-halfes l)))
                           (merge-sort (merge-fn (split-in-halfes r)))])))]
      (merge-sort-internal (split-in-halfes xs))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; reading the file numbers into primitives in memory
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use 'clojure.java.io)
(defn read-file [file-path]
  (letfn [(parse-int [s] (int (Integer. s)))]
    (with-open [rdr (reader file-path)]
      (doall 
        (map parse-int (line-seq rdr))))))

(def unsorted-nums (read-file "src/clojure_learning/algorithms/IntegerArray.txt"))
(println unsorted-nums)

;; benchmarking::
(time
  (def sorted-nums (merge-sort unsorted-nums)))
;; "Elapsed time: 2095.695013 msecs"
