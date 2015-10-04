(ns sequs-horribilis
  (use clojure.repl))

;; Sequs Horribilis
;; Difficulty:	Medium
;; Topics:	seqs
;; Create a function which takes an integer and a nested collection of integers as arguments.
;; Return a sequence which maintains the nested structure, and which includes all elements
;; starting from the head whose sum is less than or equal to the input integer.
;; (=  (__ 10 [1 2 [3 [4 5] 6] 7])
;;    '(1 2 (3 (4))))

;; =  (__ 9 (range))
;;    '(0 1 2 3))

;; 1st draft::
(defn take-until
  ([bound xs] (take-until bound xs []))
  ([bound [head & tail] acc]
    (letfn [(sum-higher-than? [xs bound]
              (> (apply + (flatten xs)) bound))]
         (if (sequential? head)
           (take-until bound head acc)
           (if (sum-higher-than? (conj acc head) bound)  ;; include current in comparison**
             acc
             (recur bound tail (conj acc head)))))))

;; in action:
(take-until 10 [1 2 [3 [4 5] 6] 7]) ;; [1 2 3 4]
(take-until 9 (range))              ;; [0 1 2 3]

;; ** the location of the if-predicate higher-than is important, since it includes the current head
;; in comparison, and while including head, if the conjoined head summed up yields in greather than
;; the acc is returned WITHOUT head -> [1 2], the next vector-head is ignored
;; but because we're doing the sequential? comparison first the recursivity takes precedence ->
;; and the elements are compared by each-number and NOT by conjoining vectors(several items)

;; 2nd draft: persist the nodes-structures of the initial collection/tree
;; when head resolves into a sequential? -> push on empty node in the accumulator
;; maintain also the total while going recursively
(defn take-until
  ([bound xs] (take-until bound xs [] 0))
  ([bound [head & tail] acc total]
    (if (nil? head) acc
      (if (sequential? head)                        ;; new node -> conjoin to existing and add new node to acc
        (conj acc (take-until bound head [] total)) ;; reset node & persist total across recursivity
        (if (> (+ total head) bound)
          acc
          (recur bound tail (conj acc head) (+ total head)))))))

;; in action:
(take-until 10 [1 2 [3 [4 5] 6] 7]) ;; [1 2 3 4]
(take-until 9 (range))              ;; [0 1 2 3]
(take-until 30 [1 2 [3 [4 [5 [6 [7 12 13]] 9]] 10] 11]) ;; [1 2 [3 [4 [5 [5 [7]]]]]]
