(ns clojure-learning.algorithms.merge-sort
  (use clojure.repl))

;; merge-sort:: is one of the fastest algorithms for sorting
;; it uses basically: 6nlog2n + 6n (6 operations and log2n because it splits-in-halfes and goes recursively in each
;; branch
;; split a collection by half, recursively until you reach the base-case: [1] -> [[] [1]]
;; of empty collections; for any more than one element array apply the merge-function;
;; the merge-function should take 2 smaller collections, and would need to have 2 counters
;; each counter is increased if the corresponding element is smaller than the second element's 
;; counter; if one of the i/j counters reaches the upper bound for its iterated partition -> place into sorted the rest
;; of the other collection elements starting with where the other counter left
;; [5 3 1] [4 2 0] -> i,j: is 5 > 4, no -> take j[4] -> place it in the new collection [], and
;; continue with i, and j++; the accumulator is the base-counter(third counter) which keeps
;; counting from all the elements that is: 6 in this case.

;; and the function::
(defn merge-sort 
  ([xs] 
    (letfn [(merge-fn [[xs ys]] "the most important func in merge-sort->destructuring the vec-tuples"
              (letfn [(get-by-index [idx xs]    "convert vec into array and get it's el by index"
                        (aget (into-array xs) idx))
                      (reached-bound [idx xs]   "verify if index is reached the upper bound"
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
                        (if (< (get-by-index i xs) (get-by-index j ys))
                          (recur (conj sorted (get-by-index i xs)) (dec k) (inc i) j)
                          (recur (conj sorted (get-by-index j ys)) (dec k) i (inc j)))))))))
            (split-in-halfes [xs]
              (let [half (quot (count xs) 2)]
                (reduce 
                  (fn [[l r] [idx el]] 
                    (if (< idx half) 
                      [(conj l el) r] 
                      [l (conj r el)])) 
                  [[] []] 
                  (map-indexed vector xs))))
            (merge-sort- [[l r]]
              (if (empty? l) r
                (merge-fn [(merge-sort (merge-fn (split-in-halfes l)))
                           (merge-sort (merge-fn (split-in-halfes r)))])))]
      (merge-sort- (split-in-halfes xs)))))

;; demo::
(merge-sort [5 3 1 2 4 6])
(merge-sort (reverse (take 1000 (range))))
(sort (reverse (take 1000 (range))))

(split-in-halfes [1])           ;; [[] [1]]

(defn merge-fn [xs ys]
  (letfn [(get-by-index [idx xs]    "convert vec into array and get it's el by index"
            (aget (into-array xs) idx))
          (reached-bound [idx xs]   "verify if index is reached the upper bound"
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
            (if (< (get-by-index i xs) (get-by-index j ys))
              (recur (conj sorted (get-by-index i xs)) (dec k) (inc i) j)
              (recur (conj sorted (get-by-index j ys)) (dec k) i (inc j)))))))))

(merge-fn [5 7 4] [3 2 1]) ;; [3 2 1 5 7 4]
(merge-fn [5 1] [2 4])     ;; [2 4 5 1]
(merge-fn [5] [3])         ;; [3 5]

(defn rest-from-index [idx xs]
  (map second 
    (filter (fn [[rem-idx el]] (>= rem-idx idx)) (map-indexed vector xs))))

;; demo::
(rest-from-index 0 [1 2 3])     ;; (1 2 3)
(rest-from-index 2 [1 2 3 4 5]) ;; (3 4 5)

(defn split-in-halfes [xs]
  (let [half (quot (count xs) 2)]
    (reduce 
        (fn [[l r] [idx el]] 
              (if (< idx half) 
                [(conj l el) r] 
                [l (conj r el)])) 
        [[] []] 
        (map-indexed vector xs))))

;; demo::
(split-in-halfes [1 2 3 4 5])   ;; [[1 2] [3 4 5]]
(split-in-halfes [1 2 3 4 5 6]) ;; [[1 2 3] [3 4 5]]
(split-in-halfes [1 2])         ;; [[1] [2]]
(split-in-halfes [1])           ;; [[] [1]]


(map-indexed (fn [idx el] [idx el]) [2 4 3])
(map-indexed vector [2 4 3])

(def c [5 3 1 2 4 6 7])
(map second (take-while (fn [[idx el]] (< idx (quot (count c) 2))) (map-indexed vector c)))
(map second (drop-while (fn [[idx el]] (< idx (quot (count c) 2))) (map-indexed vector c)))

