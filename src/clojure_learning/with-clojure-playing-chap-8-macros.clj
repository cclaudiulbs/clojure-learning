(doc nnext); (same as (next (next x)))

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

;; another version without checking the pair of args and using destrcturing of args as in func scenarios
(defmacro do-until
  [head-clause & tail-clauses]
  (when head-clause
    (list 'when head-clause
           (first tail-clauses)
        (cons 'do-until (next tail-clauses)))))

(do-until true (println "some") true (println "none") false (println "nil"))
(macroexpand '(do-until true (println "some") true (println "none") false (println "nil")))

;; notes:
;; there should be a check if head-clause exists(and as for clojure anything which is NOT nil or false
;; evaluates to TRUTHY; the macro body evaluates first the head-clause; then we're returning a list back
;; by quoting the core 'when to not be evaluated while the list is build, when the returned expression
;; will evaluate again the head-clause of the end-caller(as you want do-until to behave) and for truthy
;; will call the next clause.
;; Now comes the IMPORTANT part: do a recursive call, and take 'do-until itself NOT-Evaluated with the
;; next tail-clauses and call recursively. when head-clause is nil -> the evaluated built list expression
;; will return nil, and the left-most-previous clauses will be "pushed" to nil using: [cons]
;; something like:
(cons '(true (println "some")) nil) ; ((true (println "some")))
(macroexpand '(do-until true (println "some") true (println "none") false (println "nil")))
;; (if true (do (println "some") (do-until true (println "none") false (println "nil))))

(java.util.Date.)
(java.util.Date.)