(doc list)

(doc nnext)

(do-until (even? 2) (println "even 2")
          (odd? 3) (println "odd 3")
          (zero? 1) (println "not printed")
          (nil? 1) (println "not printed! nil"))

(defmacro do-until
  [& clauses]
  (list 'clojure.core/when (first clauses)
        (if (next clauses)
          (second clauses)
          (throw (IllegalArgumentException. "do-until requires an even number of forms")))
        (cons 'do-until (nnext clauses))))

(defmacro do-until
  [& clauses]
  (if (even? (count clauses))
    (let [[head-clause action-clause & tail-clauses] clauses]
         (list 'clojure.core/when head-clause action-clause)
        (if (not (empty? tail-clauses))
          (cons 'do-until tail-clauses)))
    (throw (IllegalArgumentException. "do-until requires an even number of forms"))))



