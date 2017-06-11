;;Graph Connectivity(#91)
;;Difficulty:	Hard
;;Topics:	graph-theory
;;
;;Given a graph, determine whether the graph is connected. 
;;A connected graph is such that a path exists between any two given nodes.
;;-Your function must return true if the graph is connected and false otherwise.
;;-You will be given a set of tuples representing the edges of a graph. 
;;    Each member of a tuple being a vertex/node in the graph.
;;-Each edge is undirected (can be traversed either direction). 
(ns clojure-learning.graph-conectivity-hard
  (use clojure.repl)
  (use clojure.test))

(declare graph?)

(deftest test-graph?
  (testing "graph? predicate should return truthy or falsy for a pair of tuples which might connect"
    (is (= true (graph? #{[:a :a]})))
    (is (= true (graph? #{[:a :b]})))

    (is (= false (graph? #{[1 2] [2 3] [3 1]
                           [4 5] [5 6] [6 4]})))

    (is (= true (graph? #{[1 2] [2 3] [3 1]
                          [4 5] [5 6] [6 4] [3 4]})))
    )
  )

;; make the func pass for the simplest tests
(defn graph? [xset-tuples]
  (if (= 1 (count xset-tuples)) true false))

;; identify a neighbour
(defn neighbour? [[a b] [c d]]
  (or (= a c) (= a d) (= b c) (= b d)))
(neighbour? [1 2] [2 3]) ;; true
(neighbour? [1 2] [1 3]) ;; true
(neighbour? [1 2] [3 4]) ;; false
(neighbour? [1 2] [5 6]) ;; false

;; find a neighbour in a set of tuples
(defn find-neighbour [searched-tuple in-tuples]
  (filter (partial neighbour? searched-tuple) in-tuples))

(find-neighbour [1 2] [[2 3] [3 1] [4 5] [5 6] [6 4]]) ;; ([2 3] [3 1])

;; build the tuple-neighbours
(defn identify-neighbours [[head-tuple & tail-tuples]  xs-with-neighbours]
    (if (nil? head-tuple) xs-with-neighbours
      (if-let [found-neighbours (find-neighbour head-tuple tail-tuples)]
        (recur tail-tuples (conj xs-with-neighbours [head-tuple found-neighbours]))
        (recur tail-tuples (conj xs-with-neighbours nil)))))

(identify-neighbours [[1 2] [2 3] [3 1] [4 5] [5 6] [6 4]] [])
