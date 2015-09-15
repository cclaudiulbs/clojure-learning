(def foo "foo")
(use 'clojure.repl)

;; Global take-while
;; Difficulty:	Medium
;; Topics:	seqs higher-order-functions
;; implementation:
(defn global-take-while
  ([x predicate xs]
     (global-take-while x predicate xs []))
  ([x predicate [head & tail] acc]
     (if (or (nil? head) (zero? x))
       (butlast acc)
       (if (predicate head)
          (recur (dec x) predicate tail (conj acc head))
          (recur x predicate tail (conj acc head))))))

;; in action:
(global-take-while 4 #(= 2 (mod % 3))
         [2 3 5 7 11 13 17 19 23])
;; should yield: (= [2 3 5 7 11 13], and it does :) but...the class is not a lazy-seq

(class (butlast [1 2 3])) ;; persistentVector$chunkedSeq
(class (global-take-while 4 #(= 2 (mod % 3)) [2 3 5 7 11 13 17 19 23])) ;; persistentVector$chunkedSeq

;; however we do want a lazy-seq back, and not a chunked-seq:
(defn global-take-while
  [x predicate xs]
  (letfn [(lazy-take-until
            [x pred [head & tail]]
            (lazy-seq
               (let [butlast-count (dec x)
                     butlast-match #(if (predicate %) (dec %2) %2)]
                (when-not
                  (and (or (nil? head) (zero? butlast-count))
                       (and (predicate head) (zero? butlast-count)))
                   (cons
                      head
                        (lazy-take-until (butlast-match head x) predicate tail))))))]
   (lazy-take-until x predicate xs)))


(class (global-take-while 4 #(= 2 (mod % 3)) [2 3 5 7 11 13 17 19 23])) ;; clojure.lang.LazySeq !!! whohooo
(global-take-while 4 #(= 2 (mod % 3)) [2 3 5 7 11 13 17 19 23])
;; (2 3 5 7 11 13) --> NICEE

