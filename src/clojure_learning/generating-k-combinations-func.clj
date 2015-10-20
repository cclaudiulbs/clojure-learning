(use 'clojure.repl)
(ns combinatorics
  (use clojure.repl))
;; (= (__ 3 #{0 1 2 3 4}) #{#{0 1 2} #{0 1 3} #{0 1 4} #{0 2 3} #{0 2 4}
;;                          #{0 3 4} #{1 2 3} #{1 2 4} #{1 3 4} #{2 3 4}})

;; Step I
;; build a function which cycles the given sequence by a number of items
(defn cycle-by [x xs]
  (loop [chunks #{}
         [head & tail] xs]
    (let [chunked (take x (cons head tail))]
      (if (< (count chunked) x) chunks
        (recur (conj chunks chunked) tail)))))

(cycle-by 3 [0 1 2 3 4]) ;; #{(0 1 2) (1 2 3) (2 3 4)} -> COOL
(cycle-by 3 [0 1 2 3])   ;; #{(0 1 2) (1 2 3)} -> COOL

;; Step II: build a function which cycles all items by swapping them back and front
(defn k-combinations
  ([k xs]
   (let [n (count xs)]
     (if (or (> k n) (zero? k)) #{}    ;; combinatorics condition k should be less than n or not zero!
        (set
            (map set
              (if (= 1 k) (partition 1 xs)     ;; -> code-as-data->yaii! 1-partitioning
                (k-combinations k (seq xs) #{} (count xs) (dec (count xs))))))))) ;; recur compute the k-combinations
  ([k [head & tail :as current] combinations outer-timer cycle-timer]
   (letfn [(cycle-by [x xs]
            (loop [chunks #{}
                   [head & tail] xs]
              (let [chunked (take x (cons head tail))]
                (if (< (count chunked) x) chunks
                  (recur (conj chunks chunked) tail)))))]
       (if (zero? outer-timer) combinations
         (if (zero? cycle-timer)
           (recur k (conj (vec tail) head) ;; -> (tail head)
                  (concat combinations (cycle-by k current))
                  (dec outer-timer)        ;; -> ((first tail) (rest tail) head)
                  (dec (count current)))   ;; reset cycle-timer
           (recur k (cons head (conj (vec (rest tail)) (first tail))) ;; (head (rest tail) (first tail))
                  (concat combinations (cycle-by k current))
                  outer-timer              ;; not changed as the inner cycling is in progress
                  (dec cycle-timer)))))))  ;; -> (head (rest tail) (first tail))

;; (comment
  (= (apply hash-set
        (map set (k-combinations 3 #{0 1 2 3 4})))
     #{#{0 1 2} #{0 1 3} #{0 1 4} #{0 2 3} #{0 2 4}
       #{0 3 4} #{1 2 3} #{1 2 4} #{1 3 4} #{2 3 4}})
  ;; -> true

  (k-combinations 3 #{0 1 2 3 4})
  ;; -> #{#{0 4 3} #{0 1 4} #{4 3 2} #{0 1 2} #{0 1 3} #{1 3 2} #{0 3 2} #{1 4 3} #{1 4 2} #{0 4 2}}

  (k-combinations 2 #{0 1 2 3 4})
  ;; -> #{#{4 3} #{0 1} #{0 4} #{0 3} #{1 4} #{4 2} #{1 3} #{1 2} #{0 2} #{3 2}}

  (k-combinations 1 #{0 1 2 3 4}) ;; #{#{3} #{2} #{1} #{0} #{4}}

  (k-combinations 0 #{0 1 2 3 4}) ;; #{}

  (k-combinations 2 #{0 1 2})

  (k-combinations 1 #{4 5 6})

  (partition 1 #{1 2 3}) ;; ((1) (2) (3))
  (apply hash-set (map set (partition 1 #{1 2 3}))) ;; #{#{3} #{2} {1}}
  (conj #{1 2 3} 1)      ;; #{1 3 2}
  (drop 2 [1 2 3 4])     ;; (3 4)
  (= #{#{0 1 3}} #{#{1 0 3}}) ;; -> true -> nice clojure BY VALUE comparison
;; )
