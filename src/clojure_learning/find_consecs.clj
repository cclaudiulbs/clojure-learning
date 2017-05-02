(ns clojure-learning.find-consecs
  (:use (clojure test repl)))

(defn find-max-consecs [xs]
  (letfn [(group-by-max [[head & tail]]
            (reduce 
              (fn [vov curr]
                (if (= (dec curr) ((comp last last) vov))
                  (conj (vec (butlast vov)) (conj (last vov) curr))
                  (conj vov [curr])))
              [[head]] tail))]
    (->> xs
         group-by-max
         (sort-by count)
         last)))

(find-max-consecs [1 3 2 4 6 7 8 10 12])
;; 1 3 2 4 6 7 8 10 12 -> 6 7 8
