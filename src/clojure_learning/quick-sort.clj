(ns quick-sort
  (require [clojure.test :refer :all])
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

(defn quick-sort
  ([xs] (quick-sort (partition-by-pivot (first xs) (rest xs)) []))
  ([[l x r] sorted]
   (when-not (nil? x)
     (list* (quick-sort l) x (quick-sort r)))))

(quick-sort [10 18 8 15 7])