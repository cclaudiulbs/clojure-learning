;; Graph Tour:: #89
;; Difficulty:	Hard
;; Topics:	graph-theory
;; Starting with a graph you must write a function that returns true if it is possible to 
;; make a tour of the graph in which every edge is visited exactly once.
;; The graph is represented by a vector of tuples, where each tuple represents a single edge.
;; The rules are:
;;   - You can start at any node.
;;   - You must visit each edge exactly once.
;;   - All edges are undirected.

(ns clojure-learning.graph-tour
  (use (clojure repl test)))

(= false (__ [ [:a :b] [:a :b] [:a :c] [:c :a]
               [:a :d] [:b :d] [:c :d]]))

(= true (__ [ [:a :b] [:a :c] [:c :b] [:a :e]
              [:b :e] [:a :d] [:b :d] [:c :e]
              [:d :e] [:c :f] [:d :f]]))

;; solution:: EULERIAN PATH
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

;; 1. build a function which finds the unique edges of a graph::
(defn find-edges [tuples] (vec (set (flatten (concat tuples)))))

;; 2. identify if some vertexes are duplicated? graph-cannot be eulerian : move on...
(defn duplicate-vertexes? [tuples]
  (not= (count tuples) 
     (count
       (apply hash-set (map sort tuples)))))

;; in action:
(duplicate-vertexes? [[:a :b] [:b :a]])    ;; true
(duplicate-vertexes? [[:a :b] [:b :c]])    ;; false
(#{2} 2) ;; 2

;; 3. find neighbours/adjacents for a given node
(defn find-adjacents [node tuples]
  (reduce (fn [adjacents-map [from to :as tuple]]
            (if (get (set tuple) node)
              (assoc adjacents-map node (set (concat (remove #(= node %) tuple) (adjacents-map node))))
              adjacents-map)) 
          {} tuples))

(find-adjacents :a [[:a :b] [:a :c]])   ;; {:a #{:c :b}}
(find-adjacents :a [[:a :b] [:a :b]])   ;; {:a #{:b}}
(find-adjacents :a [[:b :c] [:d :e]])   ;; {}
(empty? (find-adjacents :a [[:b :c] [:d :e]]))   ;; {}

;; 4. function that identifies if a given graph is connected or not:: it uses DepthFirstSearch algo'
(defn graph-connected? [tuples]
  (letfn [(vec-contains? [xs x] (some #(= % x) xs))
          (depth-first-connected? [node tuples visited]
            (cond (vec-contains? visited node) visited
                  :else
                  (reduce (fn [visited-nodes adjacent]
                            (if ((comp not empty?) (find-adjacents adjacent tuples))
                              (depth-first-connected? adjacent tuples visited-nodes)
                              (conj visited-nodes adjacent :visited)
                            )
                          )
                         (conj visited node) (last (vals (find-adjacents node tuples))))))
          (find-edges [tuples] (vec (set (flatten (concat tuples)))))]
    (-> tuples
      find-edges
      first
      (depth-first-connected? tuples [])
      count
      (= (count (find-edges tuples)))
    ))) 

;; testing:: graph-connected?
(deftest test-graph-connected?
  "graph-connected? should return truthy/falsy if a given graph has all edges connected"
  (let [connected-edges-1 [[:a :b] [:a :c] [:c :b] [:a :e]
                           [:b :e] [:a :d] [:b :d] [:c :e]
                           [:d :e] [:c :f] [:d :f]]
        connected-edges-2 [[:a :b] [:a :b] [:a :c] [:c :a]
                           [:a :d] [:b :d] [:c :d]]
        not-connected-edges [[:a :b] [:b :d] [:b :c] [:e :f]]]
    (is (= true (graph-connected? connected-edges-1)))
    (is (= true (graph-connected? connected-edges-2)))
    (is (= false (graph-connected? not-connected-edges)))
 ))

;; 5. final function:: find if a graph is - eulerian path type of graph
(defn eulerian-path-graph? [tuples]
  "a given graph is eulerian if the graph is connected and has at most 2 edges 
having odd degrees, and the rest of edges with even-degree"
  (if (duplicate-vertexes? tuples) false
    (if (graph-connected? tuples)
      (let [nodes-with-adjacents (reduce (fn [nodes-with-adjacents node]
                                           (conj nodes-with-adjacents (find-adjacents node tuples)))
                                         {} (find-edges tuples))]
        (->> (vals nodes-with-adjacents)
          (filter (comp odd? count))
          count
          (>= 2))
        ))))

(def connected-edges-1 [[:a :b] [:a :c] [:c :b] [:a :e]
                           [:b :e] [:a :d] [:b :d] [:c :e]
                           [:d :e] [:c :f] [:d :f]])
(def not-connected-edges [[:a :b] [:b :d] [:b :c] [:e :f]])

(eulerian-path-graph? connected-edges-1)
(eulerian-path-graph? not-connected-edges)

;; 6. THAT's IT THAT's ALL:: wrapping all functions into one:: eulerian-path graph::
(defn eulerian-path-graph? [tuples]
  "a given graph is eulerian if the graph is connected and has at most 2 edges 
having odd degrees, and the rest of edges with even-degree"
  (letfn [(duplicate-vertexes? [tuples]
            (not= (count tuples) 
               (count
                 (apply hash-set (map sort tuples)))))
          (find-edges [tuples] (vec (set (flatten (concat tuples)))))
          (find-adjacents [node tuples]
            (reduce (fn [adjacents-map [from to :as tuple]]
                      (if (get (set tuple) node)
                        (assoc adjacents-map node (set (concat (remove #(= node %) tuple) (adjacents-map node))))
                        adjacents-map)) 
                    {} tuples))
          (find-nodes-with-adjacents [graph]
            (reduce (fn [nodes-with-adjacents node]
                      (conj nodes-with-adjacents (find-adjacents node tuples)))
                    {} (find-edges tuples)))
          (graph-connected? [tuples]
            (letfn [(vec-contains? [xs x] (some #(= % x) xs))
                    (depth-first-connected? [node tuples visited]
                      (cond (vec-contains? visited node) visited
                            :else
                            (reduce (fn [visited-nodes adjacent]
                                      (if ((comp not empty?) (find-adjacents adjacent tuples))
                                        (depth-first-connected? adjacent tuples visited-nodes)
                                        (conj visited-nodes adjacent :visited)
                                      )
                                    )
                                   (conj visited node) (last (vals (find-adjacents node tuples))))))
                    (find-edges [tuples] (vec (set (flatten (concat tuples)))))]
              (-> tuples
                find-edges
                first
                (depth-first-connected? tuples [])
                count
                (= (count (find-edges tuples)))
                )))]
    (if (duplicate-vertexes? tuples) false
      (if ((comp not graph-connected?) tuples) false
          (->> tuples
            find-nodes-with-adjacents
            vals
            (filter (comp odd? count))
            count
            (>= 2)
          )))))

;; test stubs::
(def connected-edges-1 [[:a :b] [:a :c] [:c :b] [:a :e]
                                [:b :e] [:a :d] [:b :d] [:c :e]
                                [:d :e] [:c :f] [:d :f]])
(def connected-edges-2 [[1 2] [2 3] [3 4] [4 1]])
(def not-connected-edges-1 [[:a :b] [:a :b] [:a :c] [:c :a]
                             [:a :d] [:b :d] [:c :d]])
(def not-connected-edges-2 [[:a :b] [:b :d] [:b :c] [:e :f]])
(def not-connected-edges-3 [[:a :a] [:b :b]])
(def not-connected-edges-4 [[1 2] [2 3] [2 4] [2 5]])
(def not-connected-edges-5 [[:a :a] [:b :b]])

(deftest test-eulerian-path-graph?
  "eulerian-path-graph? should tell if a graph is can be traversed by visiting each edge exactly once"
    (is (= true (eulerian-path-graph? connected-edges-1)))
    (is (= true (eulerian-path-graph? connected-edges-2)))
    (is (= false (eulerian-path-graph? not-connected-edges-1)))
    (is (= false (eulerian-path-graph? not-connected-edges-2)))
    (is (= false (eulerian-path-graph? not-connected-edges-3)))
    (is (= false (eulerian-path-graph? not-connected-edges-4)))
    (is (= false (eulerian-path-graph? not-connected-edges-5)))
 )