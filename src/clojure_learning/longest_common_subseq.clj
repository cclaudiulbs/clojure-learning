(ns clojure-learning.longest-common-subseq)

;; find the longest common subsequence from the given char-seq
;; aabcbbaaab
;; => (a a a)
;; the sequence should be indexed <= so that the current char can be determined
;; if it's common with the previous index char
;; a solution might be to build a map of all the chars with indexes in asc order
;; as vals
;; {\a: [0 1 6 7 8], \b: [2 4 5 9], \c: [3]}

;; the problem can be reduced(in complexity) by finding the longest sequence 
;; of numbers that are consecutive. this resolution can also be paralellizable
;; since each of the char occurences can be computed by a diff process

(defn longest-consecutive-seq [nums]
  (letfn [(consecutive? [x y] (= (dec y) x))
          (replace-last-vec-val [xs n]
            (conj (vec (butlast xs)) n))
          (inc-last-vec-val [xs]
            (inc (last xs)))]
    (apply max
      (:occurence
        (reduce (fn [prev-context num]
                  (if (consecutive? (:curr prev-context) num)
                    (assoc ;; add consec val
                      (assoc prev-context :curr num) ;; a new map is returned
                      :occurence 
                      (replace-last-vec-val 
                        (:occurence prev-context) (inc-last-vec-val (:occurence prev-context))))
                    (assoc ;; start from one as a non-consec num is encountered
                      (assoc prev-context :curr num) ;; preserve curr for prev
                      :occurence (conj (:occurence prev-context) 1))
                  ))
          {:curr (first nums) :occurence [1]} (rest nums))))))

;; exercise...
(longest-consecutive-seq [0 1 6 7 8 9])

;;;;;;;;;;;;;;;
;; There's another solution that relies on computing the longest-common-subsequence
;; using a one-time-pass through the sequence
;;;;;;;;;;;;;;;
(defn longest-common-subsequence [char-seq]
  (last
    (sort-by val
      (:occurences
        (letfn [(nil->zero [n] (if (nil? n) 0 n))
                (replace-last-in-vec [v n]
                  (conj (vec (butlast v)) n))
                (inc-or-zero-last-in-vec [v]
                  (inc (nil->zero (last v))))]
          (reduce (fn [m n]
                    (if (= n (:char m))
                      (assoc 
                        (assoc m :occurences
                          (let [char-occurences (get (:occurences m) n []) ;; ...or empty vec for new char-occurence
                                new-val-for-n-occurence (replace-last-in-vec 
                                                          char-occurences 
                                                          (inc-or-zero-last-in-vec char-occurences))]
                            (assoc (:occurences m) n new-val-for-n-occurence)))
                        :char n)
                      (assoc
                        (assoc m :occurences
                          (let [new-occurences-of-char (get (:occurences m) n [1])]
                            (assoc (:occurences m) n new-occurences-of-char)))
                        :char n)))
            {:char (first char-seq) ;; keeps the start/previous char
             :occurences {}}        ;; keeps each char together with MAX occurences
            (rest char-seq)))))))

;; exercise...
(longest-common-subsequence "aabccbbcaaab");; [\a [3]]
(longest-common-subsequence "aabccccc");; [\c [5]]
(longest-common-subsequence "aabcxxxxvl");; [\x [4]]
