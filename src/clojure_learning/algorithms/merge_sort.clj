(ns clojure-learning.algorithms.merge-sort
  (use clojure.repl))

;; merge-sort:: split a collection by half, recursively until you reach the base-case
;; of empty collections; for any more than one element array apply the merge-function;
;; the merge-function should take 2 smaller collections, and would need to have 2 counters
;; each counter is increased if the corresponding element is smaller than the second element's 
;; counter
;; [5 3 1] [4 2 0] -> i,j: is 5 > 4, no -> take j[4] -> place it in the new collection [], and
;; continue with i, and j++; the accumulator is the base-counter(third counter) which keeps
;; counting from all the elements that is: 6 in this case.

(partition-all 2 [1 2 3])

;; TODO: refactor!!!
(defn merge-sort [xs]
  (letfn [(take-left [xs]
            (map second (take-while (fn [[idx el]] (< idx (quot (count xs) 2))) (map-indexed vector xs))))
          (take-right [xs]
            (map second (drop-while (fn [[idx el]] (< idx (quot (count xs) 2))) (map-indexed vector xs))))
          (merge-sort-parts [xs ys]
            (if (empty? xs) ys 
              (into [] (merge-sort-parts ( merge-fn (take-left xs) (take-right ys)))))
            )]

    (merge-sort-parts (take-left xs) (take-right xs))))

(merge-sort [5 3 1 2 4 6])

(defn merge-fn [xs ys]
  (loop [sorted []
         k (count (into xs ys)) 
         i 0 
         j 0]
    (if (zero? k) 
      sorted
      (if (> i (dec (count xs)))
        (into sorted ys)
        (if (> j (dec (count ys)))
          (into sorted xs)
          (if (< (aget (into-array xs) i) (aget (into-array ys) j))
            (recur (conj sorted (aget (into-array xs) i)) (dec k) (inc i) j)
            (recur (conj sorted (aget (into-array ys) j)) (dec k) i (inc j))))))))

(merge-fn [5 7 4] [3 2 1])
(merge-fn [5] [3]) ;; [3 5]
(merge-fn [5 1] [2 4]) ;; [2 4 5 1]


(map-indexed (fn [idx el] [idx el]) [2 4 3])
(map-indexed vector [2 4 3])

(def c [5 3 1 2 4 6 7])
(map second (take-while (fn [[idx el]] (< idx (quot (count c) 2))) (map-indexed vector c)))
(map second (drop-while (fn [[idx el]] (< idx (quot (count c) 2))) (map-indexed vector c)))


(partition (quot (count [1 2 3 4 5 6 7]) 2) [5 3 1 2 4 6 7])
(quot 7 2)
