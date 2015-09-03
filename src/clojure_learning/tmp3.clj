(use 'clojure.repl)
(ratio? (Math/sqrt 7))
(Math/sqrt (Math/sqrt 1))
(Math/sqrt 2)
(Math/sqrt (Math/sqrt (Math/sqrt (Math/sqrt (Math/sqrt (Math/sqrt 3))))))
(double 1)

(defn happy-digit
  [dig]
  (letfn [(sqrt [x] (Math/sqrt x))]
    (if-let [happy (= (float 1) dig)]
      happy
      (recur (sqrt dig)))))

(happy-digit 7)

;; For example, 19 is happy, as the associated sequence is:
;;     1**2 + 9**2 = 82
;;     8**2 + 2**2 = 68
;;     6**2 + 8**2 = 100
;;     1**2 + 0**2 + 0**2 = 1.
(defn happy-number?
  ([x] (happy-number? x []))
  ([x init]
  (letfn [(pow-of-two [x] (reduce * [x x]))
          (to-int [c] (- (int c) (int \0)))
          (pow-and-sum
            [digits]
             (reduce +
                (map (comp pow-of-two to-int) digits)))]
    (let [curr (pow-and-sum (str x))]
      (if-let [happy (= 1 curr)]
        happy
        (if (and ((comp not nil?) (last init)) (> curr (pow-of-two (last init))))
          false
          (recur (pow-and-sum (str curr)) (conj init curr))))))))

;; notes: the init acc persists across the recursion the last pow-and-sum digit; if the digit starts
;; and powers itself -> wrong path -> it is not a happy number, since it should be reduced not increased.
;; we start from [0] to avoid nil checking and avoid incidental complexity for nil checking;
;; once the recursion processed: 3 -> 9 -> 81; we store this in the acc and break the recursion if > (last acc) curr.

(happy-number? 19) ;; true ;;  curr 100 ;; last-init 82
(happy-number? 20) ;; false ;; curr 37  ;; last-init 4
(happy-number? 7)  ;; true  ;; curr 130 ;; last-init 49
(happy-number? 3)  ;; false
(happy-number? 2)  ;; false
(happy-number? 986543210)  ;; true