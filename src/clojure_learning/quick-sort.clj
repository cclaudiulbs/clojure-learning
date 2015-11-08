(ns quick-sort
  (require [clojure.test :refer :all]
           [clojure.core])
  (use [clojure.repl]))

(defn partition-by-pivot
  ([x xs] (partition-by-pivot x xs [] []))
  ([x [head & tail] l r]
     (if (nil? head) [l x r]
       (if (< head x)
         (recur x tail (conj l head) r)
         (recur x tail l (conj r head))))))

(deftest test-partition-by-pivot
  (testing "if the partition-by-pivot func will split the containers into 3 parts: l pivot r"
    (is (= [[8 7] 10 [18 15]] (partition-by-pivot 10 [18 8 15 7]))) ;; true
  ))

;; this func implementation will throw a StackOverflow Error -> refactor using TCO
(defn quick-sort
  ([xs]
   (let [[l x r] (partition-by-pivot (first xs) (rest xs))]
     (quick-sort l x r)))
  ([l x r]
     (when-not (nil? x)
       (lazy-cat (quick-sort l) [x] (quick-sort r)))))

(deftest test-quick-sort
  (testing "quick-sort implemented in a functional style"
    (is (= [7 8 10 15 18] (quick-sort [10 18 8 15 7]))) ;; true
  ))

(quick-sort [10 18 8 15 7])

;; benchmarking
(let [nums (take 5000 (iterate dec 5000))]
  (println (str "Start: " (java.util.Date.)))
    (let [sorted (quick-sort nums)]
      (println (str "End: " (java.util.Date.)))
      (println "Nums: " sorted)))

(doc zipmap)

