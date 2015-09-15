(defn palindrome? [x]
  (loop [init x
         reversed 0]
    (if (not= 0 init)
      (recur (quot init 10) (+ (* reversed 10) (mod init 10)))
      (= x reversed))))

(palindrome? 2) ;; true
(palindrome? 101) ;; true
(palindrome? 13) ;; false

(defn gen-palindromes
  [start]
  (letfn [(palindrome? [x]
            (loop [init x
                   reversed 0]
              (if (not= 0 init)
                (recur (quot init 10) (+ (* reversed 10) (rem init 10)))
                (= x reversed))))
          (gen-palins [x]
            (if (palindrome? x)
              (cons x (lazy-seq (gen-palins (inc x))))
              (recur (inc x))))]
    (gen-palins start)))

(gen-palindromes 1234550000)
(take 26 (gen-palindromes 0))

(doc frequencies)
(frequencies [1 2 3 1 5 1 5])

(import java.util.Date)
(do
  (println (Date. (System/currentTimeMillis)))
  (take 20 (gen-palindromes 1234550000))
  (println (Date. (System/currentTimeMillis))))

(class (gen-palindromes 1234550000)) ;; -> not lazy
(= (first (gen-palindromes (* 111111111 111111111)))
   (* 111111111 111111111))

(defn gen-palindromes
  [start]
  (letfn [(palindrome? [x]
            (loop [init x
                   reversed 0]
              (if (not= 0 init)
                (recur (quot init 10) (+ (* reversed 10) (mod init 10)))
                (= x reversed))))]
    (filter palindrome? (iterate inc start))))

(filter palindrome? (range))
(gen-palindromes 1234550000)
;; 00:08:58
;; 00:10:58

(def v1 (apply vector (range 0 10)))
v1
(def v2 (pop v1))
(def v3 (pop v1))
(= v2 v3) ;; true
(identical? v2 v3) ;; false
