(ns clojure-learning.algorithms.strongly-connected-comps-new)

(require '[clojure.string :as str])
(require '[clojure.test :as t])

;; Algorithm:: DirectedGraphs ONLY!
;; 1) G is a directed graph and S is a stack.
;; 2) While S does not contain all vertices perform step 3.
;; 3)choose a random vertex v and perform depth first search on it. Each time DFS finishes expanding vertex v, push v on to the stack S. (This guarantees that the vertex with maximum finish time will more closer to the top of the stack).
;; 4) Obtain a transpose of the G by reversing the direction of the edge.
;; 5)While S is not empty perform step 6.
;; 6) Remove v=top of S and again perform DFS on it The set of all visited vertices will give the strongly connected components containing v. Remove all visited vertices from stack.

(defn read-file [file-path]
  (slurp file-path))

(read-file "src/clojure_learning/algorithms/graph-1234.txt");

;;;;;;;;;;;
(defn to->edges [file-content]
  "primitive function that builds a list-of-list representing directed-edges"
  (map (fn [from-to-str] 
         (map #(Integer/parseInt %) 
              (str/split from-to-str #"\s"))) 
       (str/split file-content #"\n")))

(defn to->rev-edges [file-content]
  "primitive function that reverses the graph paired-vertices directions"
  (->> file-content
    to->edges
    (map reverse)))
;;;;;;;;;;;

(-> "src/clojure_learning/algorithms/graph-1234.txt"
    read-file 
    to->rev-edges)

;;;;;;;;;;;
(defn build-graph-from-edges [edges]
  "primitive function that takes the paired-edges and builds a map with vertices as keys and
   directed-adjacents as vals"
    (reduce
      (fn [graph-map[from-vertex to-vertex]]
        (if-let [adjacents (get graph-map from-vertex)]
          (assoc graph-map from-vertex (conj adjacents to-vertex))
          (assoc graph-map from-vertex [to-vertex]))) 
      {} edges)) ;; OK

;;;;;;;;;;;
(defn adjacents-of [vertex graph-map]
  (remove nil? (get graph-map vertex)))

(t/deftest testing-adjacents-of 
  "should return neighbours of a key in map that is vals for a key/vertex"
  (t/is (= [1 2] (adjacents-of :one {:one [1 2]})))
  (t/is (empty? (adjacents-of :two {:one [1]})))
  (t/is (= '() (adjacents-of :one {:one [nil]})))
)
;;;;;;;;;;;
;; func that builds the finishing-times for given Directed Graph
;; Note:: it uses iterative-recursion && expanding-recursion to create correctly
;; the finishing-times while returning from the expanding-recursion;
;; the iterative-recursion is for ensuring all vertices are visited, avoiding interruption
;; for when the given graph is Directed and depending on the starting vertex, not all verts
;; might be visited => in this case returning a forests instead of trees
;;;;;;;;;;;
(defn dfs->vertices-by-finishing-times 
  [[head-vertex & tail-vertices] visited seeds graph]
  "func that builds the finishing-times for vertices considering the last vertex found
   will have the LOWER finishing-time -> increasing the finishing-time when returning from
   recursivity"
  (if (nil? head-vertex) visited ;; finishing-time-acc
    (if (or ((set seeds) head-vertex)  ;; seeds used here only to avoid polluting the visited(and corrupting the finishing-times)
            ((set visited) head-vertex))
      (recur tail-vertices visited seeds graph)
      (recur tail-vertices 
             (conj
               (dfs->vertices-by-finishing-times (adjacents-of head-vertex graph) 
                                                 visited
                                                 (conj seeds head-vertex)
                                                 graph)
               head-vertex)
             seeds
             graph
))))

(dfs->vertices-by-finishing-times
  (shuffle (keys graph-1)) [] [] graph-1)
;;;;;;;;;;;;;
(defn forest-dfs [[vertex & tail-verts] visited graph]
  "this depth-first-search depending on the starting vertex might break for when the
   graph is NOT fully connected, or is a directional graph and the starting-vertex
   cannot reach all the vertices => might return a Tree or a Sub-Forest"
  (if (or (nil? vertex)
          ((set visited) vertex)) []
    (conj  ;; because there's NO recur for tail-verts => breaks for when FORESTs instead of TREEs
      (forest-dfs (adjacents-of vertex graph) 
                          (conj visited vertex) 
                          graph)
      vertex)))

(forest-dfs [(rand-nth (keys graph-1))] [] graph-1)
;; [4] or [4 3] or [2 0 1]
;;;;;;;;;;;;;
(defn dfs->cc [vertices graph]
  "will take all vertices and attempt a forest-dfs on each vertex, yielding small
   forests of connected-components <= used by the higher-level-algorithm func(SCC)"
  (reduce 
    (fn [visited vertex]
      (let [new-cc (forest-dfs [vertex] (flatten visited) graph)]
        (cond (empty? new-cc) visited
              :else (conj visited new-cc)
     )))
    [] 
    vertices))

;; exercising
(dfs->cc
  (reverse
    (dfs->vertices-by-finishing-times (shuffle (keys graph-1)) [] [] graph-1))
  rev-graph-1)
;; [[0 2 1] [3] [4]] -> 1st run: OK
;; [[1 0 2] [3] [4]] -> 2nd run: OK

;;;;;;;;;;;;;;
;; Final func that computes the SCC of a given graph
;;;;;;;;;;;;;;
(defn find-strongly-connected [graph-file-path]
  (letfn [(read-file [file-path]
            (slurp file-path))

          (to->edges [file-content]
            "primitive function that builds a list-of-list representing directed-edges"
            (map (fn [from-to-str] 
                   (map #(Integer/parseInt %) 
                        (str/split from-to-str #"\s"))) 
                 (str/split file-content #"\n")))

          (to->rev-edges [file-content]
            "primitive function that reverses the graph paired-vertices/edges directions"
            (->> file-content
              to->edges
              (map reverse)))

          (build-graph-from-edges [edges]
            "primitive function that takes the paired-edges and builds a map with vertices 
             as keys and directed-adjacents as vals"
              (reduce
                (fn [graph-map[from-vertex to-vertex]]
                  (if-let [adjacents (get graph-map from-vertex)]
                    (assoc graph-map from-vertex (conj adjacents to-vertex))
                    (assoc graph-map from-vertex [to-vertex]))) 
                {} edges)) ;; OK

          (adjacents-of [vertex graph-map]
            (remove nil? (get graph-map vertex)))

          (dfs->vertices-by-finishing-times 
            [[head-vertex & tail-vertices] visited seeds graph]
            "func that builds the finishing-times for vertices considering the last vertex 
             found will have the LOWER finishing-time => increasing the finishing-time 
             when returning from recursivity"
            (if (nil? head-vertex) visited ;; ~ finishing-times
              (if (or ((set seeds) head-vertex)  ;; seeds used here only to avoid polluting the visited(and corrupting the finishing-times)
                      ((set visited) head-vertex))
                (recur tail-vertices visited seeds graph)
                (recur tail-vertices 
                       (conj
                         (dfs->vertices-by-finishing-times (adjacents-of head-vertex graph) 
                                                           visited
                                                           (conj seeds head-vertex)
                                                           graph)
                         head-vertex)
                       seeds
                       graph
                ))))

          (forest-dfs [[vertex & tail-verts] visited graph]
            "this depth-first-search, depending on the starting vertex might break for 
             when the graph is NOT fully connected, or is a Directional Graph and the 
             starting-vertex cannot reach all the vertices => might return a Tree or a Sub-Forest"
            (if (or (nil? vertex)
                    ((set visited) vertex)) []
              (conj  ;; because there's NO recur for tail-verts => breaks for when FORESTs instead of TREEs
                (forest-dfs (adjacents-of vertex graph) 
                                    (conj visited vertex) 
                                    graph)
                vertex)))

          (dfs->cc [vertices graph]
            "will take all vertices and attempt a forest-dfs on each vertex, 
             yielding small forests of connected-components"
            (reduce 
              (fn [visited vertex]
                (let [new-cc (forest-dfs [vertex] (flatten visited) graph)]
                  (cond (empty? new-cc) visited
                        :else (conj visited new-cc)
               )))
              [] 
              vertices)) ]
    
    (let [loaded-raw-graph (-> graph-file-path read-file)
          normal-graph (-> loaded-raw-graph
                           to->edges
                           build-graph-from-edges)
          reversed-graph (-> loaded-raw-graph
                           to->rev-edges
                           build-graph-from-edges)

          randomized-vertices (shuffle (keys normal-graph)) ;; randomize the starting-vertex
          
          reversed-verts-by-finishing (reverse 
                                        (dfs->vertices-by-finishing-times
                                          randomized-vertices [] [] normal-graph)) ]
      (-> reversed-verts-by-finishing
        (dfs->cc reversed-graph))
)))

;; exercising...
(find-strongly-connected "src/clojure_learning/algorithms/graph-1234.txt")
;; [[0 2 1] [3] [4]] -> OK

(find-strongly-connected "src/clojure_learning/algorithms/another-graph.txt")
;; [[2 0 1] [3] [4]] -> OK

(find-strongly-connected "src/clojure_learning/algorithms/simplest-scc.txt")
;; [[0 1 3] [2]] -> OK

(find-strongly-connected "src/clojure_learning/algorithms/small-snippet.txt")
;; [[5 2 8] [9 3 6] [1 4 7]] -> OK

(find-strongly-connected "src/clojure_learning/algorithms/sample-1.txt")
;; [[2 3] [0 1 4]] -> OK

(time
  (find-strongly-connected "src/clojure_learning/algorithms/sample-2.txt"))
;; [[2] [1 4 3 0]] -> OK

;; ufff final course-file...
(time
  (find-strongly-connected "src/clojure_learning/algorithms/kosaraju-course-file.txt"))