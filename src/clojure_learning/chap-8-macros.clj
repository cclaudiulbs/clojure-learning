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

;; the most obvious use of macros over HOF, is that macros manipulate COMPILE time forms, transforming
;; them into runtime data-structures -> this allows my problem to be written in a more natural
;; domain specific way.
;; as we know by now, the arguments of [defmacro] are NOT evaluated before being passed to [defmacro]
;; form. imagine again the

;; taking the do-until custom macro, there's some magic happening in the last line:
;; (cons 'do-until (nnext clauses)))
;; normally clojure [cons] function takes the item and a list: cons item in-list -> the structure
;; is a valid clojure structure, however when expanded recursively it will behave as:
;; go recursively till end building the recursive structure, when there are no clauses -> nill is returned
;; and as (cons 1 nil) returns (1), it will call do-until with each pair of clauses building
;; the list

;; implement the unless macro using: syntax quoting(`), unquoting(~) + unquote-splicing(~@)
(defmacro unless
  [condition-form evaled-form]
  `(when-not ~condition-form (~@evaled-form)))

;; some neat func for macroexpansion and debugging the built macro
(require '[clojure.walk :as walk])
(walk/macroexpand-all '(unless (< 2 1) (str "indeed < 2 1 is false ...")))
;; (if (< 2 1) nil (do (str "indeed < 2 1 is false ...")))

(unless (< 2 1) (str "indeed (< 2 1) is false...going to eval this form"))

;; clojure's [defn] macro, combines several tasks, that otherwise would force us to be redundant
;; coding the same thing over and over again -> boilerplating code.
;; this boilerplating code is a fertile environment for new bugs to be born.
;; 1. creates the corresponding function using special form [fn]
;; 2. attaches the documentation after the function signature.
;; 3. binds the function name to a local schema variable.
;; 4. attaches the collected metadata

;; You could perform all these steps over and over again every time you wanted to create
;; a new function, but thanks to macros you can instead use the more convenient defn
;; form. Regardless of your application domain and its implementation, programming
;; language boilerplate code inevitably occurs and is a fertile place to hide subtle errors.
;; But identifying these repetitive tasks and writing macros to simplify and reduce or
;; eliminate the tedious copy-paste-tweak cycle can work to reduce the incidental complexities
;; inherent in a project.

(defmacro contract
  [name & body]
  `(fn ~name [~'mmap] ~@body))

(macroexpand '(contract not-nil (println "some")))
(def not-nil (contract not-nil (println "not nil contract")))
(not-nil)

