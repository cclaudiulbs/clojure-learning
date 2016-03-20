;; Graph Tour
;; Difficulty:	Hard
;; Topics:	graph-theory
;; Starting with a graph you must write a function that returns true if it is possible to make a tour of the graph in which every edge is visited exactly once.
;; The graph is represented by a vector of tuples, where each tuple represents a single edge.
;; The rules are:
;;  - You can start at any node.
;;  - You must visit each edge ex
;;  - All edges are undirected

;; solution:: eulerian path -> is the correct definition for this type of graph
;; the intermediate nodes should all have even degrees: enter&leave degrees
;; the beggining and end nodes: must have odd degrees
;; the graph should be connected
;; the eulerian path MUST start with and END with those 2 odd-degree vertexes
;; each vertex should be consumed ONCE!!!
;; if every single edge has even-degree -> eulerian path! if ofcourse graph is connected!
;; if graph has 2 nodes with odd degree and we start traversing on the edges that have
;; even-degree -> it's impossible to tell that the graph is eulerian path, because we
;; consumed the edges in the wrong way

;; if duplicate-vertexes -> false
  ;; if connected-graph
    ;; if every edge has even-degree -> (zero? (rem (count adjacents) 2)) -> eulerian
      ;; if there's more than 2 edges with odd-degree -> false
        ;; eulerian

;; eulerian::
;; 1 --- 2
;; |   / |
;; |  /  |
;; | /   |
;; 3     4 --- 5
;;
;; -> 1 {2}, 2 {3}, 4 {2}, 5 {1} --> <= 2 odd edges! OK

;; not eulerian:
;; 1 --- 2
;;    / |
;;   /  |
;;  /   |
;; 3    4 --- 5

;; -> 1 {1}, 2 {3}, 3 {1}, 4 {2}, 5 {1} -> odd edges > 2 (is 4)

(ns clojure-learning.graph-tour
  (use (clojure repl test)))

(= false (__ [[:a :b] [:a :b] [:a :c] [:c :a]
               [:a :d] [:b :d] [:c :d]]))
(= true (__ [[:a :b] [:a :c] [:c :b] [:a :e]
              [:b :e] [:a :d] [:b :d] [:c :e]
              [:d :e] [:c :f] [:d :f]]))

(def tupless [[:a :b] [:a :b] [:a :c] [:c :a]
               [:a :d] [:b :d] [:c :d]])

;; 1. build a function which finds the edges of a graph::
(defn find-edges [tuples] (vec (set (flatten (concat tuples)))))
(find-edges tupless) ;; [:c :b :d :a]

(defn equal-by-content? [tuple1 tuple2]
  (or (= tuple1 tuple2)
      (= (sort tuple1) (sort tuple2))))

(equal-by-content? [:a :b] [:b :a]) ;; true
(equal-by-content? [:a :b] [:a :a]) ;; false
(equal-by-content? [:a :b] [:a :A]) ;; false
(equal-by-content? ["a" "b"] ["b" "a"]) ;; true

;; 2
(defn duplicate-vertexes? [tuples]
  (not= (count tuples) 
     (count
       (apply hash-set (map sort tuples)))))

(duplicate-vertexes? [[:a :b] [:b :a]])    ;; true
(duplicate-vertexes? [[:a :b] [:b :c]])    ;; false

(#{2} 2) ;; 2
(remove #(= :a %) [:a :b]) ;; (:b)

;; 3.
(defn find-adjacents [node tuples]
  (reduce (fn [adjacents-map [from to :as tuple]]
            (if (get (set tuple) node)
              (assoc adjacents-map node (set (concat (remove #(= node %) tuple) (adjacents-map node))))
              adjacents-map)) 
          {} tuples))

(find-adjacents :a [[:a :b] [:a :c]])   ;; {:a #{:c :b}}
(find-adjacents :a [[:a :b] [:a :b]])   ;; {:a #{:b}}

;; 4
(defn graph-connected? [tuples]
  (letfn [(find-nodes-with-adjacents [tuples]
            (reduce (fn [nodes-with-adjancents-map node] 
                      (conj nodes-with-adjancents-map (find-adjacents node tuples))) 
                    {} (find-edges tuples)))]
  (let [nodes-with-adjacents-map (find-nodes-with-adjacents tuples)
        root-node-with-adjacents (first nodes-with-adjacents-map)]
    nodes-with-adjacents-map)))


(graph-connected? [[:a :b] [:a :b] [:a :c] [:c :a]
                   [:a :d] [:b :d] [:c :d]])  ;; {:a #{:c :b :d}, :d #{:c :b :a}, :b #{:d :a}, :c #{:d :a}}
