(ns clojure-learning.list-monad
  (use (clojure repl test)))

(defn return [v] (list v))
(defn bind [mv func]
  
  )

(defn push [xs x] (conj xs x))
(defn lift-push [mv x])
