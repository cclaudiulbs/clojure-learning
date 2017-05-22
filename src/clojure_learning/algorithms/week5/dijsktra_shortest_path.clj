(ns clojure-learning.algorithms.week5.dijsktra-shortest-path)
(require '[clojure.string :as str])

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
         entries->graph)
))

;; exercising
(build-graph "src/clojure_learning/algorithms/week5/dijkstra-1.txt")
;; {4 ((5 4)), 3 ((4 2)), 2 ((4 3)), 1 ((2 7) (3 1))}
;; :key 1, :val ((to-vertex cost), (to-vertex cost))

(defn non-visited-adjacents [adjacents-with-costs visited-by-now]
  (seq ;; ensures nil instead of empty-list
    (remove
      (fn [[adjacent cost]] (.contains visited-by-now adjacent)) 
      adjacents-with-costs)))

(non-visited-adjacents [[1 2] [3 4]] #{1}) ;; ([3 4])
(non-visited-adjacents [[1 2] [3 4]] #{1 3}) ;; nil

(defn extract-min-vertex [adjacents-with-costs]
  "second is the actual cost:: need the vertex with smaller cost to be extracted"
  (first (sort-by second adjacents-with-costs)))

;; exercising
(extract-min-vertex [[5 2] [3 1] [7 5]]) ;; [3 1] ;; tuple:: [3 vertex, 1 cost]

(defn nil->zero [n] (if (nil? n) 0 n))
(nil->zero nil) ;; 0
(apply + (map nil->zero [nil 0 1 nil 3])) ;; 4

;; one of the most important func that keeps the costs in sync updated to the latest value
(defn update-vertices-costs [vertices-costs-m adjacent-costs from-vert]
  (let [[_ accumulated-cost] (get vertices-costs-m from-vert)]
    (reduce 
      (fn [verts-costs-map [adjacent adj-cost]] ;; destructuring
        (if-let [[old-from-vert old-cost] (get verts-costs-map adjacent)]
          (if (> (nil->zero old-cost)
                 (apply + (map nil->zero [adj-cost accumulated-cost])))
            (assoc verts-costs-map adjacent
              [from-vert (apply + (map nil->zero [adj-cost accumulated-cost]))])
            verts-costs-map) ;; else old cost is still smaller
          (assoc verts-costs-map adjacent 
            [from-vert (apply + (map nil->zero [adj-cost accumulated-cost]))])
      ))
      vertices-costs-m 
      adjacent-costs
)))

;; exercising
(update-vertices-costs 
  {2 [1 7]}     ;; costs-map:: should be kept in sync
  [[2 3] [3 1]] ;; adjacency lists
  1             ;; from-vertex => is a tail for vertex 2:: 1->2 cost 7
)
;; output::
;; {3 [1 1]    ;; (=> new tail-with-cost)
;;  2 [1 3]}   ;; (=> update the cost for vertex 2 with new cost:3 and the new tail 1)

(defn adjacents-of [vert graph]
  (get graph vert))

;; exercising
(def d1
  (build-graph "src/clojure_learning/algorithms/week5/dijkstra-1.txt"))
(adjacents-of 1 d1) ;; ((2 7) (3 1))

(defn all-visited? [graph visited]
  (= (count (keys graph)) 
     (count visited)))

;; exercising
(all-visited? d1 #{1 2}) ;; false
(all-visited? d1 #{1 2 3 4}) ;; true

;; dropping the tail-vertices:: ensure that everytime the from-vert will NOT have a tail
(defn dijkstra-shortest-path
  [from-vert [head-rem & tail-rems :as remainder-verts] graph tail-verts-costs-m visited]
  (if (all-visited? graph visited)
    tail-verts-costs-m
    (if (or (nil? from-vert) 
            (visited from-vert))
      (recur head-rem tail-rems graph tail-verts-costs-m visited)
      (let [non-visited-adjacents (non-visited-adjacents 
                                     (adjacents-of from-vert graph) visited)
            calculated-vertices-costs (update-vertices-costs
                                         tail-verts-costs-m ;; preserve previous costs
                                         non-visited-adjacents
                                         from-vert)    ;; from vertex( = tail-vertex in costs-map)
            [next-vertex _] (extract-min-vertex non-visited-adjacents)]
        (recur next-vertex
               remainder-verts 
               graph 
               calculated-vertices-costs
               (conj visited from-vert))
))))

;; exercising
(dijkstra-shortest-path 1 (keys d1) d1 {} #{})
;; {5 [4 7], 4 [3 3], 3 [1 1], 2 [1 7]}

(def sample
  (build-graph "src/clojure_learning/algorithms/week5/sample-1.txt"))
(dijkstra-shortest-path 1 (keys sample) sample {} #{})
;; 1->3:: 5, 1->4:: 6, 1->5:: 12, 1->6:: 7
;; out:: {7 [8 5], 5 [4 12], 6 [3 7], 4 [3 6], 3 [2 5], 2 [1 2]}

(def r
  (build-graph "src/clojure_learning/algorithms/week5/dijkstra-real.txt"))
(time
  (def shortest-paths
    (dijkstra-shortest-path 1 (keys r) r {} #{})))

;; homework output:: taking into consideration the requested vertices
(def req-vertices #{7,37,59,82,99,115,133,165,188,197})

;; output::
(def req-vertices-costs
  (sort-by first 
    (filter 
      (fn [[key [tail cost :as weight]]] (req-vertices key)) 
      shortest-paths)))
req-vertices-costs
#_([7 [27 6110]]
   [37 [107 3684]]
   [59 [162 2947]]
   [82 [135 2052]]
   [99 [1 2367]]
   [115 [80 2399]]
   [133 [85 2029]]
   [165 [153 5543]]
   [188 [125 3139]]
   [197 [54 5592]])

(str/join ","
  (map 
    (comp #(* 1000 (quot % 1000)) second second) 
    req-vertices-costs))
;; 6000,3000,2000,2000,2000,2000,2000,5000,3000,5000