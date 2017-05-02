(ns clojure-learning.algorithms.draft-scc-first-vs)

;; ####################################################
;; previous(1st draft attempt to resolve the algorithm)
;; deprecated:: what comes after this line..... !!!!!!!!!
(defn get-by-last-finishing-time [paired-vertices] 
  "primitive func that takes the accumulated paired-vertices from the top of the stack and returns the finishing-time"
  ((comp last last) paired-vertices))

;;;;;;;;;;
(defn all-vertices-visited? [visited vertices]
  "primitive function that checks if vertices has a single-remained-vertex 
   and was/not visited"
  (and (empty? (rest vertices))
      (empty? (remove visited vertices))))

(deftest testing-all-vertices-visited? "should test whether all vertices are visited"
  (is (= true (all-vertices-visited? #{1 2 3} [1])))
  (is (= false (all-vertices-visited? #{1 2} [3])))
)
;;;;;;;;;;
(defn depth-first-expansive-adjacents-of
  "mutually recursive function that goes in expansion that is: back-tracking-recursion"
  [current-vertex visited graph vertices-labeled finishing-time]
    ;; base-case predicate that returns last vertex from which the back-tracking begins
    (if (all-vertices-visited? visited (adjacents-of current-vertex graph))
      ;; types should allign -> both mut-recur-functions return the same type!
      [[current-vertex (inc finishing-time)]]
      (let [adjacents-verts-with-times-rec 
                             (depth-first-iterative-with-finishing-times 
                                   (adjacents-of current-vertex graph)
                                   visited
                                   graph
                                   vertices-labeled
                                   finishing-time)]
        ;; dups appear since the recursivity builds from the top of the recur-stack
        (conj adjacents-verts-with-times-rec
             [current-vertex (inc (get-by-last-finishing-time adjacents-verts-with-times-rec))])
)))

(defn depth-first-iterative-with-finishing-times
  "mutually recursive function with its counterpart the tree-expansion-recursion
   Note:: every state should be persisted across recursions!!!"
 [[head-vertex & tail-vertices] visited graph vertices-labeled finishing-time]
 (letfn [(visited? [vertex vertices-labeled] 
           ((set (map first vertices-labeled)) vertex))
         (visited-vertices [vertices-labeled] (set (map first vertices-labeled)))
         (join-colls [col1 col2] (vec (concat col1 col2)))]
 (if (nil? head-vertex) ;; base-case for exit-recursivity(non domain) 
   vertices-labeled
   (if (visited? head-vertex vertices-labeled)
     (recur tail-vertices visited graph vertices-labeled finishing-time)
     (let [acc-verts (depth-first-expansive-adjacents-of 
                       head-vertex 
                       (conj visited head-vertex)
                       graph
                       vertices-labeled
                       finishing-time)]
      (recur tail-vertices
              (conj visited head-vertex) 
              graph
              (vec (concat vertices-labeled acc-verts)) ;; accumulate the entire result -> there might be dups because a vertex is reachable from many adjacents!
              (get-by-last-finishing-time acc-verts)))
))))

(defn keys-in-desc [graph] 
  "primitive func that sequence the keys of graph and returns them in reversed order"
  (-> graph keys sort reverse))

(deftest func-keys-in-desc "should return the keys of the graph-map sorted in reverse order"
  (is (= '(3 2 1) (keys-in-desc {1 1, 2 2, 3 3})))
)

(defn backtrack-vertices->map [graph-map]
  "high-order function which glues together the mutual recursive functions and outputs the
   vertices-backtracked in a map structure"
  (letfn [(edges->map [edges]
              "primitive func which takes the labeled vertices with finishing times and builds a
               map with only the relevant/unique vertices discarding any dups"
            (reduce (fn [acc-labeled-verts [vertex finishing-time :as edge]]
                      (if (get acc-labeled-verts vertex)
                        acc-labeled-verts
                        (conj acc-labeled-verts edge))) 
                    {} edges))
          
          (keys-in-desc [graph] (-> graph keys sort reverse))]
    
    (edges->map
      (depth-first-iterative-with-finishing-times 
        (keys-in-desc graph-map)
        #{} 
        graph-map 
        [] 
        0)
)))

(defn edges->map [edges]
  "primitive func which takes the labeled vertices with finishing times and builds a
   map with only the relevant/unique vertices discarding any dups, if present -> discard semantic"
  (reduce (fn [acc-labeled-verts [vertex finishing-time]]
            (if (get acc-labeled-verts vertex) acc-labeled-verts
              (assoc acc-labeled-verts vertex finishing-time))) 
          {} edges))

(deftest func-edges->map "should take a list of edges(pairs of points) and translate them to a map"
  (is (= {:one 1 :two 2} (edges->map [[:one 1] [:two 2]])))
 )

g-small-1
(backtrack-vertices->map small-graph)
;; OK!
(backtrack-vertices->map reversed-small)
;; OK!

(defn map->desc-sorted-tups [vertices-with-finishing-times]
  (reverse (sort-by second (map identity vertices-with-finishing-times))))

(deftest func-map->desc-sorted-tups 
  "primitive func that should take a map with key-val, and output a seq of tuples in reversed order based on descendent values"
  (is (= '([1 8] [4 7] [7 1]) 
         (map->desc-sorted-tups {7 1, 4 7, 1 8}))) ;; true
)
(defn map->keys-by-desc-val [vertices-with-finishing-times]
  (letfn [(map->desc-sorted-tups [vertices-with-finishing-times]
              (reverse (sort-by second (map identity vertices-with-finishing-times))))]
  (map first (map->desc-sorted-tups vertices-with-finishing-times))))

(deftest func-map->keys-by-desc-val "should return a sequence with only the nodes sorted in reversed order"
  (is (= '(1 4 7) (map->keys-by-desc-val {7 1, 4 7, 1 8}))) ;; true
)  

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; an idiomatic alternative to the DFS algorithm(of above more readable version)
;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn main-dfs [graph [head-vertex & tail-vertices] visited acc]
  (if (nil? head-vertex) acc
    (if (visited head-vertex)
    (recur graph tail-vertices visited acc)
    (recur graph tail-vertices 
           (conj visited head-vertex)
           (conj acc
             (main-dfs 
               graph
               (adjacents-of head-vertex graph) 
               (conj visited head-vertex) 
               [head-vertex]))
))))

;; demo::
(let [vertices-with-finishing-times (backtrack-vertices->map g-small-2)
      vertices-in-desc (map->keys-by-desc-val vertices-with-finishing-times)]
  (map flatten (main-dfs g-small-1 vertices-in-desc #{} []))
)

;; and the main function for finding the strongly-connected-components of a given graph
(defn find-scc-of [graph]
  (letfn [(main-dfs [graph [head-vertex & tail-vertices] visited acc]
            (if (nil? head-vertex) acc
              (if (visited head-vertex)
              (recur graph tail-vertices visited acc)
              (recur graph tail-vertices 
                     (conj visited head-vertex)
                     (conj acc
                       (main-dfs 
                         graph
                         (adjacents-of head-vertex graph) 
                         (conj visited head-vertex) 
                         [head-vertex]))))))
          
          (map->keys-by-desc-val [vertices-with-finishing-times]
            (letfn [(map->desc-sorted-tups [vertices-with-finishing-times]
                        (reverse (sort-by second (map identity vertices-with-finishing-times))))]
            (map first (map->desc-sorted-tups vertices-with-finishing-times))))

          (get-by-last-finishing-time [paired-vertices] 
            "primitive func that takes the accumulated paired-vertices from the top 
             of the stack and returns the finishing-time"
            ((comp last last) paired-vertices))

          (depth-first-expansive-adjacents-of
            [current-vertex visited graph vertices-labeled finishing-time]
              ;; base-case predicate that returns last vertex from which the back-tracking begins
              (if (all-vertices-visited? visited (adjacents-of current-vertex graph))
                ;; types should allign -> both mut-recur-functions return the same type!
                [[current-vertex (inc finishing-time)]]
                (let [adjacents-verts-with-times-rec 
                                       (depth-first-iterative-with-finishing-times 
                                             (adjacents-of current-vertex graph)
                                             visited
                                             graph
                                             vertices-labeled
                                             finishing-time)]
                  ;; dups appear since the recursivity builds from the top of the recur-stack
                  (conj adjacents-verts-with-times-rec
                       [current-vertex (inc (get-by-last-finishing-time adjacents-verts-with-times-rec))])
                  )))

          (depth-first-iterative-with-finishing-times
           [[head-vertex & tail-vertices] visited graph vertices-labeled finishing-time]
           (letfn [(visited? [vertex vertices-labeled] 
                     ((set (map first vertices-labeled)) vertex))
                   (visited-vertices [vertices-labeled] (set (map first vertices-labeled)))
                   (join-colls [col1 col2] (vec (concat col1 col2)))]
           (if (nil? head-vertex) ;; base-case for exit-recursivity(non domain) 
             vertices-labeled
             (if (visited? head-vertex vertices-labeled)
               (recur tail-vertices visited graph vertices-labeled finishing-time)
               (let [acc-verts (depth-first-expansive-adjacents-of 
                                 head-vertex 
                                 (conj visited head-vertex)
                                 graph
                                 vertices-labeled
                                 finishing-time)]
                (recur tail-vertices
                        (conj visited head-vertex) 
                        graph
                        (vec (concat vertices-labeled acc-verts)) ;; accumulate the entire result -> there might be dups because a vertex is reachable from many adjacents!
                        (get-by-last-finishing-time acc-verts)))
               ))))

          (backtrack-vertices->map [graph-map]
            "high-order function which glues together the mutual recursive functions and outputs the
             vertices-backtracked in a map structure"
            (letfn [(edges->map [edges]
                        "primitive func which takes the labeled vertices with finishing times and builds a
               map with only the relevant/unique vertices discarding any dups"
                        (reduce (fn [acc-labeled-verts [vertex finishing-time :as edge]]
                                  (if (get acc-labeled-verts vertex)
                                    acc-labeled-verts
                                    (conj acc-labeled-verts edge))) 
                                {} edges))
                    (keys-in-desc [graph] (-> graph keys sort reverse))]
              (edges->map
                (depth-first-iterative-with-finishing-times 
                     (keys-in-desc graph-map)
                     #{} 
                     graph-map 
                     [] 
                     0))))]
    
    (let [vertices-with-finishing-times (backtrack-vertices->map graph)
          vertices-in-desc (map->keys-by-desc-val vertices-with-finishing-times)]
  (map flatten (main-dfs graph vertices-in-desc #{} []))
)))

(find-scc-of graph-01234)
;; Answer: 3,3,3,0,0

(def simplest-file "src/clojure_learning/algorithms/simplest-scc.txt")
(def simplest-graph (-> simplest-file read-file listify build-graph-from-edges))
(def simplest-graph-rev (-> simplest-file read-file rev-listify build-graph-from-edges))
simplest-graph
simplest-graph-rev

(let [vertices-with-finishing-times (backtrack-vertices->map simplest-graph-rev)
      vertices-in-desc (map->keys-by-desc-val vertices-with-finishing-times)]
 (main-dfs simplest-graph vertices-in-desc #{} [])
  ;;vertices-with-finishing-times
  ;;vertices-in-desc
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; build a master map from submaps
(defn build-master-map [& maps]
  (reduce (partial conj {}) (concat maps)))

(deftest build-master-map "should build a master map from various submaps"
  (is (= {:one 1 :two 2 :three 3}
         (build-master-map {:one 1} {:two 2} {:three 3})))
  (is (= {:one 1 :two 1}
         (build-master-map {:one 2} {:one 1} {:two 2} {:two 1}))) ;; order counts!!!
)

(build-master-map {:one 1} {:two 2} {:three 3})
(reduce (partial conj {}) (concat {"one" 1} {"two" 2}))
