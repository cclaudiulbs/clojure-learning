(ns clojure-learning.list-monad
  (use (clojure repl test)))

(defn return [v]
  (fn [] (flatten (list v))))

(defn bind [mv func]
  (func (mv)))

(defn lift-push [mv x]
  (bind mv (fn [xs] (return (cons x xs)))))

(defn lift-pop [mv]
  (bind mv (fn [xs] (return (butlast xs)))))

(defn lift-search [mv subject]
  (bind mv (fn [xs] (return (some #{subject} xs)))))

;; in action:
((-> (return 4)
     (lift-push 5)
     (lift-push 6)
     (lift-push 7)
     lift-pop
     (lift-search 6)
     ))

(mapcat #(conj % 24) [[1] [2]]) ;; ( 1 24 2 24) 
