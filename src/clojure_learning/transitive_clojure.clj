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
  ([xset] 
    (if-let [acc-result (seq (find-transitive-clojure xset []))]
      (into (apply vector xset) acc-result)
      xset))
  ([xset acc]
  (let [xs (apply vector xset)]
    (reduce
      (fn [acc [h-tuple l-tuple]]
        (into acc
          (for [[h l] xs :when (= l-tuple h)]
              [h-tuple l])))
      acc xs))))

(find-transitive-clojure #{["cat" "man"] ["man" "snake"] ["spider" "cat"]})

(apply vector #{1 2 3}) ;; [1 3 2]

(deftest test-transitive-clojure
  (testing "transitive-clojure to find all the connections from last->first of a binary relation tuples"
    (is (= #{["cat" "man"] ["man" "snake"] ["spider" "cat"]}

        (find-transitive-clojure
           #{["cat" "man"] ["man" "snake"] ["cat" "snake"]
             ["spider" "cat"] ["spider" "man"] ["spider" "snake"]} )))))

