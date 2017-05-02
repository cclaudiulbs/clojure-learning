;;;;;;;;;;;;;;;;;;;;;
;; Transitive Closure
;; Difficulty:	Hard
;; Topics:	set-theory
;; Write a function which generates the transitive closure of a binary relation. 
;; The relation will be represented as a set of 2 item vectors.
(ns clojure-learning.transitive-clojure
  (use [clojure.repl])
  (require [clojure.test :refer :all]))

;; Note: the transitive clojure as seen -> takes the entries from the initial coll, and 
;; iteratively finds the next clojure based on the previous output and based on the input domain args

(defn find-transitive-clojure
  [xset]
   (let [xs-vec (apply vector xset)]
    (letfn [(find-acc-transitives [xs]
               (if-let [new-transitives (seq (find-new-transitives xs))]      ;; found new transitive-clojures
                  (into xs (find-new-transitives (into xs new-transitives)))  ;; recur on existing + found-new-transitives
                  xs))
           (find-new-transitives [xs]
              (reduce
                (fn [acc [h-tuple l-tuple]]
                  (into acc
                    (for [[h l] xs :when (= l-tuple h)]
                      [h-tuple l])))
                [] xs))]
      (apply hash-set 
        (find-acc-transitives xs-vec)))))

(find-transitive-clojure #{["cat" "man"] ["man" "snake"] ["spider" "cat"]})

(apply vector #{1 2 3}) ;; [1 3 2]
(= #{1 2 3} #{2 1 3})   ;; true -> participate in the value sequence abstraction

(deftest test-transitive-clojure
  (testing "transitive-clojure to find all the connections from last->first of a binary relation tuples"
    (is (= (find-transitive-clojure #{["cat" "man"] ["man" "snake"] ["spider" "cat"]})
           #{["cat" "man"] ["man" "snake"] ["cat" "snake"]
             ["spider" "cat"] ["spider" "man"] ["spider" "snake"]}))
))

(seq []) ;; nil
