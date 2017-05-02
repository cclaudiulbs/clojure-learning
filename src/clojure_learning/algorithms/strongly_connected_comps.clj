(ns clojure-learning.algorithms.strongly-connected-comps)
(use 'clojure.test)
(require '[clojure.string :as str])

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

(deftest testing-adjacents-of 
  "should return neighbours of a key in map that is vals for a key/vertex"
  (is (= [1 2] (adjacents-of :one {:one [1 2]})))
  (is (empty? (adjacents-of :two {:one [1]})))
  (is (= '() (adjacents-of :one {:one [nil]})))
)
;;;;;;;;;;;
(defn depth-first-search [[head-vertex & tail-vertices] visited graph]
  "depth-first-func that returnes the visited-nodes by finishing-time:: 
   top of stack is the most connected vertex"
    (if (nil? head-vertex) visited
      (if ((set visited) head-vertex)
        (recur tail-vertices visited graph)
          (let [visited-adjacents (depth-first-search ;; mark the recursion return point for adjacents(don't loose context) 
                                    (adjacents-of head-vertex graph)
                                    (cons head-vertex visited)
                                    graph)]
            (recur tail-vertices visited-adjacents graph)))))

(defn directed-graph->depth-first [[head-vertex & tail-vertices] visited graph]
  "depth-first-func that returnes the visited-nodes by finishing-time:: 
   top of stack is the most connected vertex
   Note that this function STOPS if the unlucky starting vertex does not have directed
   connections with the all/rest of vertices..."
    (if (nil? head-vertex) visited
      (if ((set visited) head-vertex)
        (recur tail-vertices visited graph)
        (if (empty? (adjacents-of head-vertex graph))
          (conj visited head-vertex)
          (let [visited-adjacents (directed-graph->depth-first ;; mark the recursion return point for adjacents(don't loose context) 
                                    (adjacents-of head-vertex graph)
                                    (conj visited head-vertex)
                                    graph)]
            (recur tail-vertices visited-adjacents graph))))))
(directed-graph->depth-first (shuffle (keys graph-01234)) [] graph-01234)
;; [4]
;; [2 1 0 3 4]

;; used by SCS component
(defn dfs->stacked-vertices-by-finishing-times [[head-vertex & tail-vertices] visited seeds graph]
  "primitive func for building the vertices-stack considering the last vertex found
   will have the LOWER finishing-time -> increasing the finishing-time when returning from
   recursivity:: defined when there's no more adjacents OR the vertices are visited"
  (if (nil? head-vertex) 
    visited;; finishing-time-acc
    (if (or ((set seeds) head-vertex) 
            ((set visited) head-vertex))
      (recur tail-vertices visited seeds graph)
      (recur tail-vertices 
             (conj
               (dfs->stacked-vertices-by-finishing-times (adjacents-of head-vertex graph) 
                                      visited
                                      (conj seeds head-vertex)
                                      graph)
               head-vertex)
             seeds
             graph
))))

graph-01234

(dfs->stacked-vertices-by-finishing-times
  (shuffle (keys graph-01234)) [] [] graph-01234)
;; some samples::
;; [2 4 3 0 1]
;; [1 2 4 3 0]
;; [4 3 2 0 1]
;; [1 2 4 3 0]
;; [4 2 3 0 1]
;; [2 4 3 0 1]
;; [2 4 3 0 1]
;; [4 2 3 0 1]

;; In stack, 3 always appears after 4, and 0 appear after both 3 and 4.
(depth-first-search (shuffle (keys graph-01234)) '() graph-01234) ;; [4 3 2 1 0]

(directed-graph->depth-first (shuffle (keys graph-01234)) [] graph-01234) ;; [3 0 1 2]
;; pushing on the stack(vector) ensures the most connected vertex is always on TOP of the stack
;; can be considered the HIGHEST finishing time
;;;;;;;;;;;;

(def graph-01234 (-> "src/clojure_learning/algorithms/graph-1234.txt"
                   read-file 
                   to->edges
                   build-graph-from-edges))
graph-01234

(def rev-graph-01234 (-> "src/clojure_learning/algorithms/graph-1234.txt"
                       read-file 
                       to->rev-edges
                       build-graph-from-edges))
rev-graph-01234

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; final function that finds the strongly-connected-components(SCC) using Kosaraju Algorithm
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn scs [graph-file-path]
  (letfn [(read-file [file-path]
            (slurp file-path))

          (to->edges [file-content]
            "primitive function that builds a list-of-list representing directed-edges"
            (map (fn [from-to-str] 
                   (map #(Integer/parseInt %) 
                        (str/split from-to-str #"\s"))) 
                 (str/split file-content #"\n")))

          (to->rev-edges [file-content]
            "primitive function that reverses the graph paired-vertices directions"
            (->> file-content
              to->edges
              (map reverse)))

          (build-graph-from-edges [edges]
            "primitive function that takes the paired-edges and builds a map with vertices as keys and
             directed-adjacents as vals"
              (reduce
                (fn [graph-map[from-vertex to-vertex]]
                  (if-let [adjacents (get graph-map from-vertex)]
                    (assoc graph-map from-vertex (conj adjacents to-vertex))
                    (assoc graph-map from-vertex [to-vertex]))) 
                {} edges))
          
          (adjacents-of [vertex graph-map]
            (remove nil? (get graph-map vertex)))

          (dfs->stacked-vertices-by-finishing-times [[head-vertex & tail-vertices] visited seeds graph]
            (if (nil? head-vertex) 
              visited;; finishing-time-acc
              (if (or ((set seeds) head-vertex) 
                      ((set (flatten visited)) head-vertex))
                (recur tail-vertices visited seeds graph)
                (recur tail-vertices (conj (dfs->stacked-vertices-by-finishing-times
                                              (adjacents-of head-vertex graph) 
                                              visited
                                              (conj seeds head-vertex)
                                              graph) 
                                           head-vertex)
                       seeds
                       graph))))

          (dfs-vertices->connected-components [finishing-time-vertices graph]
            (for [v finishing-time-vertices]
              (dfs->stacked-vertices-by-finishing-times [v] [] [] graph)))
          
          (connected->strongly-connected [[first-connected-component & tail-ccs]]
            "function that takes the output of:: [dfs-vertices->connected-components]"
            (reduce (fn [scc-m next-cc]
                      ;; if there's already a SCC => avoid adding a CC as a SCC(add only the last-vertex)
                      (if (clojure.set/intersection (set next-cc) (set (:prev-comp scc-m)))
                        (assoc
                          (assoc scc-m :sccs 
                                 ;; put only the difference vertex => since the SCC is already there
                                 (conj (:sccs scc-m) (clojure.set/difference (set next-cc)
                                                                             (set (:prev-comp scc-m)))))
                          :prev-comp next-cc)
                        ;; a new SCC => add it to our scc-map, preserving the previous-SCC
                        (assoc 
                          (assoc scc-m :sccs 
                                 ;; put only the difference vertex => since the SCC is already there
                                 (conj (:sccs scc-m) next-cc))
                          :prev-comp next-cc)))
                    ;; assume the first occurence in the connected-components is a SCC(might be ONE vertex)
                    {:prev-comp first-connected-component 
                     :sccs [(set first-connected-component)]}
                    ;; there might be multiple occurences of the same SCC(reachable from multiple vertices)
                    (sort tail-ccs)
             ))]
          
    (let [normal-graph (-> graph-file-path
                           read-file
                           to->edges
                           build-graph-from-edges)
          reversed-graph (-> graph-file-path
                   read-file
                   to->rev-edges
                   build-graph-from-edges)

          ;; randomize the choosing-start-vertex from graph => compute the finishing-times
          shuffled-vertices (shuffle (keys normal-graph))
          
          ;; performing dfs using a stack/vec as this will ensure the HIGHEST finishing time is always the most-in-depth
          ;; (that is the last vertex is the most connected)
          vertices-by-finishing-time-desc-order (reverse 
                                                  (dfs->stacked-vertices-by-finishing-times
                                                    shuffled-vertices [] [] normal-graph))

          ;; identify connected-components by doing the 2nd DFS by using the prev-computed finishing-times
          connected-components (dfs-vertices->connected-components
                                 vertices-by-finishing-time-desc-order
                                 reversed-graph)]

      ;; identify which are the SCC from the connected-components
      (-> connected-components
        connected->strongly-connected)
)))

(scs "src/clojure_learning/algorithms/graph-1234.txt")

(scs "src/clojure_learning/algorithms/another-graph.txt")

(scs "src/clojure_learning/algorithms/simplest-scc.txt")

(scs "src/clojure_learning/algorithms/small-snippet.txt")

(scs "src/clojure_learning/algorithms/sample-1.txt")

(scs "src/clojure_learning/algorithms/sample-2.txt")
