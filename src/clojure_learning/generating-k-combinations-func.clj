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
(defn cycle-every [[head & tail :as initial]])


(= #{#{0 1 3}} #{#{1 0 3}}) ;; -> true -> nice clojure BY VALUE comparison


;; (comment
  (conj #{1 2 3} 1)
  (drop 2 [1 2 3 4])
  (= #{#{0 1 3}} #{#{1 0 3}}) ;; -> true -> nice clojure BY VALUE comparison
;; )

;; build a reverse function:
(defn reverse-rec [xs]
  (loop [rev []
         [head & tail] xs]
    (if (nil? head) rev
      (recur (cons head rev) tail))))
(reverse-rec [1 2 3 4]) ;; (4 3 2 1)

