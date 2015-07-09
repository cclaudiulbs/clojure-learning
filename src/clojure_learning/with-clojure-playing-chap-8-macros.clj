(doc list)

(doc nnext)

;; introducing the custom [do-until] macro
;; how to use the do-until macro:
(do-until true (println "some") true (println "none") false (println "nil"))

(defmacro do-until [& clauses]
    (when clauses
      (list 'when (first clauses)
               (if (next clauses)  ;; check for existence? else fail-fast
                   (second clauses)
                   (throw (IllegalArgumentException. "do-until should have a pair of forms")))
            (cons 'do-until (nnext clauses)))))


