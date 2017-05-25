(ns clojure-learning.algorithms.week1.my-inversions)

(defn my-inversions [xs]
   (letfn [(merge-fn [xs ys inversions]
             (letfn [(reached-bound [idx xs]   "verify if index is reached the upper bound"
                       (= idx (count xs)))
                     (rest-from-index [idx xs] "take the rest of collection from where the idx left"
                       (map second 
                            (filter (fn [[rem-idx el]] (>= rem-idx idx)) (map-indexed vector xs))))]
               (loop [sorted []
                      k (count (into xs ys))
                      i 0 
                      j 0
                      invs inversions]
                 (if (zero? k) 
                   {:sorted sorted :inversions invs} ;; coll is exhausted -> return and persist inversions
                   (if (reached-bound i xs) 
                     {:sorted (into sorted (rest-from-index j ys)) :inversions invs}
                     (if (reached-bound j ys) 
                       {:sorted (into sorted (rest-from-index i xs)):inversions invs}
                       (if (<= (get xs i) (get ys j))
                         (recur (conj sorted (get xs i)) (dec k) (inc i) j invs)
                         (recur (conj sorted (get ys j)) (dec k) i (inc j) 
                                (+ invs (count 
                                          (filter (fn [x] (< (get ys j) x)) xs)))) ;; take all higher-than right-j
                         )))))))
           (my-split-in-halfes [xs]
             (let [half (quot (count xs) 2)]
               (reduce (fn [[left right] [idx el]] 
                         (if (< idx half) 
                           [(conj left el) right] 
                           [left (conj right el)])) 
                       [[] []]
                       (map-indexed vector xs))))
           (my-count-inversions [coll acc]
             (if ((comp not empty? rest) coll)
               (let [[l-branch r-branch] (my-split-in-halfes coll)
                     l-sorted-res (my-count-inversions l-branch acc)
                     r-sorted-res (my-count-inversions r-branch acc)]
                 (merge-fn (:sorted l-sorted-res) 
                           (:sorted r-sorted-res) 
                           (+ acc (:inversions l-sorted-res) (:inversions r-sorted-res))))
               {:sorted coll :inversions acc}))]
     (-> xs
        (my-count-inversions 0)
        :inversions)))



;;;;;;;;;;;;
;; testing:: 
(def unsorted [37, 7, 2, 14, 35, 47, 10, 24, 44, 17, 34, 11, 16, 48, 1, 39, 6, 
               33, 43, 26, 40, 4, 28, 5, 38, 41, 42, 12, 13, 21, 29, 18, 3, 19, 0, 32,
               46, 27, 31, 25, 15, 36, 20, 8, 9, 49, 22, 23, 30, 45])

(my-inversions unsorted) ;; 590
(my-inversions [1 2 3 4 5 0]) ;; 5
(my-inversions [3 2 1 0]) ;; 6

;; benchmarking + result
(do (println (str "Start Time:: " (java.util.Date.)))
  (let [inversions (my-inversions unsorted-nums)]
    (println (str "End Time:: " (java.util.Date.)))
    (println (str "inversions:: " inversions))))

;; Start Time:: Thu Mar 24 18:39:56 EET 2016
;; End Time:: Thu Mar 24 18:49:07 EET 2016
;; inversions:: 2407905288
