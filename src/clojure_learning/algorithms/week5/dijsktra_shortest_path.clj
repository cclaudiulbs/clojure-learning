(ns clojure-learning.algorithms.week5.dijsktra-shortest-path)
(require '[clojure.string :as str])
(import '[java.util HashMap HashSet])

;; The file contains an adjacency list representation of an undirected weighted 
;; graph with 200 vertices labeled 1 to 200. Each row consists of the node 
;; tuples that are adjacent to that particular vertex along with the length of that edge. 
;; For example, the 6th row has 6 as the first entry indicating that this row 
;; corresponds to the vertex labeled 6. 
;; The next entry of this row "141,8200" indicates that there is an edge between 
;; vertex 6 and vertex 141 that has length 8200. 
;; The rest of the pairs of this row indicate the other vertices adjacent to vertex 6 
;; and the lengths of the corresponding edges.

;; Your task is to run Dijkstra's shortest-path algorithm on this graph, using 1 
;; (the first vertex) as the source vertex, and to compute the shortest-path distances 
;; between 1 and every other vertex of the graph. If there is no path between a vertex v 
;; and vertex 1, we'll define the shortest-path distance between 1 and v to be 1_000_000.
;; You should report the shortest-path distances to the following ten vertices, 
;; in order: 7,37,59,82,99,115,133,165,188,197. 
;; You should encode the distances as a comma-separated string of integers. 
;; So if you find that all ten of these vertices except 115 are at distance 1000 away 
;; from vertex 1 and 115 is 2000 distance away, then your answer should be 
;; 1000,1000,1000,1000,1000,2000,1000,1000,1000,1000. 
;; Remember the order of reporting DOES MATTER, and the string should be in the same 
;; order in which the above ten vertices are given. 
;; The string should not contain any spaces. Please type your answer in the space provided.

;; IMPLEMENTATION NOTES: This graph is small enough that the straightforward O(mn) time 
;; implementation of Dijkstra's algorithm should work fine.

;; -----
;; while(not (all-visited?))
;; get non-visited-adjacents of [Start] => [[to1 cost1] [to2 cost2]]
;; apply min on the non-visited-adjacents => to get the min => to1
;; update the costs-map {to1 [tail1 cost1], to2 [tail2 cost2]}
;; the cost1/2 updated should SUM the looked-up:: tail1 from the same costs-map
;; take min(to1) + mark From as visited and recur with to1
;; {from [[to1 cost1], 
;;        [to2 cost2]}

(defn build-graph [file-path]
  (letfn [(longify [s] (Long/valueOf s))
          (file->lines [file-path]
            (with-open [rdr (clojure.java.io/reader file-path)]
             (let [lines (doall (line-seq rdr))]
               lines)))
          (build-graph-entry [line]
            (let [[from-vertex & to-adjacents] (str/split line #"\s+")
                  adjacents-with-costs (map 
                                         (fn [csv-pair] (map longify 
                                                          (str/split csv-pair #",")))
                                         to-adjacents)]
              [(longify from-vertex) adjacents-with-costs]))
          (entries->graph [graph-entries]
            (reduce conj {} graph-entries))]
    
    (->> file-path 
         file->lines
         (map build-graph-entry)
         entries->graph)))

;; exercising
(build-graph "src/clojure_learning/algorithms/week5/dijkstra-1.txt")
;; {4 ((5 4)), 3 ((4 2)), 2 ((4 3)), 1 ((2 7) (3 1))}
;; :key 1, :val ((to-vertex cost), (to-vertex cost))

(defn sort-by-min-vertex [adjacents-with-costs]
   "second is the actual cost:: need the vertex with smaller cost to be extracted"
   (map first (sort-by second adjacents-with-costs)))

(defn update-vertices-costs [vertices-costs-m adjacent-costs from-vert]
  (letfn [(sum-costs [& costs] (apply + (map nil->zero costs)))
          (nil->zero [n] (if (nil? n) 0 n))]
    (let [[_ from-acc-cost] (.get vertices-costs-m from-vert)]
      (doseq [[adjacent adjacent-cost] adjacent-costs]
        (let [[_ old-cost] (.get vertices-costs-m adjacent)]
          (if (or (nil? old-cost) 
                  (> (nil->zero old-cost)
                     (sum-costs from-acc-cost adjacent-cost)))
            (.put vertices-costs-m adjacent
              [from-vert (sum-costs from-acc-cost adjacent-cost)]))
)))))

(defn shortest-path-dfs [vertices graph vertices-costs-map visited]
  "func will traverse only the connected vertices from the starting vertex!"
  (letfn [(adjacents-of [v graph] (get graph v))
          (all-visited? [graph visited]
            (= (count visited) (count (keys graph))))]
    (if-not (all-visited? graph visited)
      (doseq [v vertices :when (not (.contains visited v))]
        (let [adjacents (adjacents-of v graph)
              sorted-by-min-cost-verts (sort-by-min-vertex adjacents)]
          (do
            (update-vertices-costs vertices-costs-map adjacents v)
            (.add visited v)
            (shortest-path-dfs 
              sorted-by-min-cost-verts graph vertices-costs-map visited)
))))))

(defn dijkstra-shortest-path [from-vertex graph]
  "Final dijkstra function that calculates the distances from starting-vertex
   of a given graph. It will also include the unreachable vertices putting a
   big distance for them"
  (let [visited (HashSet.)
        vertices-costs (HashMap.)]
    (do
      (.put vertices-costs from-vertex [0 0])
      (shortest-path-dfs [from-vertex] graph vertices-costs visited))
    (let [calculated-vertices (keys vertices-costs)
          all-vertices (keys graph)
          unreachable-vertices (clojure.set/difference 
                                 (set all-vertices)
                                 (set calculated-vertices))]
      (doseq [unreachable-vert unreachable-vertices]
        (.put vertices-costs unreachable-vert 1000000))
      vertices-costs)))

(dijkstra-shortest-path 1 d1)
;; {1 [0 0], 2 [1 7], 3 [1 1], 4 [3 3], 5 [4 7]}

(def sample
  (build-graph "src/clojure_learning/algorithms/week5/sample-1.txt"))
sample
(dijkstra-shortest-path 1 sample)
;; {1 [0 0], 2 [1 2], 3 [2 5], 4 [3 6], 5 [4 12], 6 [3 7], 7 1000000, 8 1000000}

(def sample-2
  (build-graph "src/clojure_learning/algorithms/week5/sample-2.txt"))
(time
  (dijkstra-shortest-path 1 sample-2))
;; {1 [0 0], 2 [4 6], 3 [1 2], 4 [5 4], 5 [3 3], 6 [2 10], 7 [6 11], 8 [5 9], 9 [7 12]}

(def tim1
  (-> "src/clojure_learning/algorithms/week5/tim-1.txt"
    build-graph))
tim1
(dijkstra-shortest-path 1 tim1)
;; {1 [0 0], 2 [1 1], 3 [7 4], 4 [3 5], 5 [1 3], 6 [5 4], 7 [2 3], 8 [2 2], 9 [8 3], 10 [9 6], 11 [9 5]}

(def sample-3
  (-> "src/clojure_learning/algorithms/week5/sample-3.txt"
    build-graph))
sample-3
(dijkstra-shortest-path 1 sample-3)
;; {1 [0 0], 2 [1 3], 3 [1 2], 4 [3 4], 5 [3 5], 6 [4 5]}

(def sample-4
  (-> "src/clojure_learning/algorithms/week5/sample-4.txt"
    build-graph))
sample-4
(dijkstra-shortest-path 1 sample-4)
;; {1 [0 0], 2 [1 1], 3 [2 2], 4 [3 3], 5 [3 5]}

(def sample-5
  (-> "src/clojure_learning/algorithms/week5/sample-5.txt"
    build-graph))
sample-5
(dijkstra-shortest-path 1 sample-5)
;; {1 [0 0], 2 [1 1], 3 [2 3], 4 [1 2], 5 [4 5]}

(def r
  (build-graph "src/clojure_learning/algorithms/week5/dijkstra-real.txt"))
(time
  (def shortest-paths (dijkstra-shortest-path 1 r)))
shortest-paths

(defn req-vertices-costs [calculated-paths req-vertices]
  (str/join "," 
    (map #(second (.get calculated-paths %)) req-vertices)))

(req-vertices-costs shortest-paths [7,37,59,82,99,115,133,165,188,197])
;; 2599,3684,2947,2052,2367,2399,2029,2442,3139,5592
