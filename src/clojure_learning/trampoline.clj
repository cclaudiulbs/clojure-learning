;;;;;;;;;;;;;;;;;;;;;;;
;; Rebuild [trampoline]
;; Category: Medium
(ns my-trampoline
  (:use clojure.repl))

;; 1st version using recursion:
(defn my-trampoline
  ([func]
    (if (fn? func)
      (recur (func))
      func))
  ([func & args]
    (my-trampoline (apply func args))))

(defn demo-trampoline
  [x stop]
  (letfn [(take-until [x stop]
            #(if (< x stop)
              (increment-until (partial < stop) x)
              x))
          (increment-until [predicate x]
              (if (predicate x) x
                (recur predicate (inc x))))]
    (my-trampoline take-until x stop)))

(demo-trampoline 10 30)

(fn? #(some [1 2 3])) ;; true

(doc recur)
(source trampoline)

;; and here's the solution using 1 stack: this is assured by using [loop] rebinding macro + [recur]:
(defn my-trampoline
  [func & args]
  (loop [applied-fn (apply func args)]
    (if (fn? applied-fn)
      (recur (applied-fn))
      applied-fn)))

