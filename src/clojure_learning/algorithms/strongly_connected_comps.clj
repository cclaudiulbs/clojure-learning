(ns clojure-learning.algorithms.strongly-connected-comps)

(set! *warn-on-reflection* true)

(require '[clojure.string :as str])
(require '[clojure.test :as t])

(defn read-file [file-path]
  (slurp file-path))

(read-file "src/clojure_learning/algorithms/test-sample-1.txt");

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

(-> "src/clojure_learning/algorithms/sample-1.txt"
    read-file 
    to->edges)

;;;;;;;;;;;
(defn build-graph-from-edges [edges]
  "primitive function that takes the paired-edges and builds a map with vertices 
   as keys and directed-adjacents as vals"
  (reduce (fn [graph [vertex adjacent-edges]]
            (assoc graph vertex (map second adjacent-edges)))
          {} (group-by first edges)))

(-> "src/clojure_learning/algorithms/sample-1.txt"
    read-file 
    to->edges
    build-graph-from-edges)

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
(defn dfs->adj-finishings-expansive
  [[head-vertex & tail-vertices] finishings visited graph]
  (if (nil? head-vertex) 
    finishings;; finishing-time-acc
    (if (or (visited head-vertex)  ;; seeds used here only to avoid polluting the visited(and corrupting the finishing-times)
            ((set finishings) head-vertex))
      (recur tail-vertices finishings visited graph)
      (recur tail-vertices 
             (conj
               (dfs->adj-finishings-expansive
                 (adjacents-of head-vertex graph) 
                    finishings
                    (conj visited head-vertex)
                    graph)
               head-vertex)
             visited
             graph
))))

(defn non-visited [vertices visited-by-now]
  (seq (remove visited-by-now vertices)))

;; exercising
(non-visited [1 2] #{1}) ;; (2)
(non-visited [1 2] #{1 2}) ;; nil
(non-visited [8] #{7 8}) ;; nil
(non-visited (adjacents-of 8 a-graph) (clojure.set/union #{6 7} (set '(7 8))))
(non-visited '(6 8) (set '(8)))
(reduce (fn [rems rem] (cons rem rems)) '(8) [7])

(defn dfs->adj-finishings-iterative
  [[head-vertex & tail-vertices :as vertices] remainings finishings visited graph]
  (if (and (nil? head-vertex) (empty? remainings))
    finishings;; finishing-time-acc
    (if (nil? head-vertex)
      (recur [(first remainings)] (rest remainings) finishings visited graph)
      (if (visited head-vertex)
        (recur tail-vertices remainings finishings visited graph)
        (if-let [not-visited (non-visited 
                               (adjacents-of head-vertex graph)
                               (clojure.set/union visited (set remainings)))]
          (recur not-visited 
             (reduce #(cons %2 %1) remainings (reverse vertices)) ;; cons -> 1st
             finishings
             visited
             graph)
          (recur tail-vertices 
                 remainings 
                 (conj finishings head-vertex)
                 (conj visited head-vertex)
                 graph))))))


(def a-graph
  (-> "src/clojure_learning/algorithms/sample-1.txt"
      read-file 
      to->rev-edges
      build-graph-from-edges))
a-graph

(dfs->adj-finishings-iterative [8] '() [] #{} a-graph)

(dfs->adj-finishings-expansive [8] [] #{} a-graph)

;;;;;;;;;;;;;
(defn dfs->by-func-type [leader-vertices graph dfs-func-type]
  "parameterized function by type:: if flatten -> finishing-times, if identity -> SCCs"
  (dfs-func-type
    (:finishing-times
      (reduce (fn [ctx leader-vertex]
                (if ((:visited ctx) leader-vertex) ctx
                  (let [adjacents-traversal-m
                            (->> (adjacents-of leader-vertex graph)
                                 (reduce
                                   (fn [adjacents-ctx each-adjacent] 
                                     (let [adjacent-finishings (dfs->adj-finishings-iterative
                                                                      [each-adjacent] 
                                                                      '()
                                                                      []
                                                                      (:adjacents-visited adjacents-ctx) 
                                                                      graph)]
                                       (assoc  ;; preserve visited across adjacents
                                         (assoc adjacents-ctx :adjacents-connected ;; stack the founded component of adjacents
                                                (conj (:adjacents-connected adjacents-ctx) ;; preserve previous
                                                      adjacent-finishings)) ;; add new
                                         :adjacents-visited
                                         ;; preserve each visited vertex by taking the stacked-vertices from finishing-times
                                         (reduce conj (:adjacents-visited adjacents-ctx) adjacent-finishings)
                                   )))
                                   ;; preserve already acummulated visiteds
                                   {:adjacents-connected [] :adjacents-visited (conj (:visited ctx) leader-vertex)}
                                   ;; [[4 1 7] [6 3]]
                            ))
                  
                        ;; the need to use: reduce conj => because i'm reducing a nested-vec-of-vecs
                        ;; !!! && make sure preserving the previous finishing-times!!!
                        leader->adjacents-finishing-times (conj (:finishing-times ctx)
                                                                  (flatten
                                                                    (conj (:adjacents-connected adjacents-traversal-m)
                                                                          leader-vertex)))

                        all-visiteds (clojure.set/union 
                                               (:visited ctx)
                                               (:adjacents-visited adjacents-traversal-m))]
                
                    (assoc ;; preserve the visiteds during iterations
                       ;; put the [[1-finishing-times] [2nd-finishing-times] leader-vertex]
                       (assoc ctx :finishing-times leader->adjacents-finishing-times)
                       :visited all-visiteds))))
                                                     
          {:finishing-times [] :visited #{}}
          leader-vertices
))))

(flatten [[] [1 2]]) ;; successfully removes empty-colls <= no need to filter empties
(reduce conj [[5]] [[1 2] [3]]) ;; [[5] [1 2] [3]] => avoid creating a separated container for reduced

(def ts1
  (-> "src/clojure_learning/algorithms/sample-1.txt"
      read-file 
      to->edges
      build-graph-from-edges))
(def rev-ts1
  (-> "src/clojure_learning/algorithms/sample-1.txt"
      read-file 
      to->rev-edges
      build-graph-from-edges))

;; will result in finishing-times:: (6 7 8 1 2 3 5 4)
(dfs->by-func-type (reverse (sort (keys rev-ts1))) rev-ts1 flatten)
  
;; will result in StronglyConnectedComponents:: [[4] [5] [2 1 3] [7 6 8]]
(dfs->by-func-type (reverse '(6 7 8 1 2 3 5 4)) ts1 identity)

;;;;;;;;;;;;;
;; Final func that computes the SCCs via Kosaraju-Algo of a given graph
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
            (reduce (fn [graph [vertex adjacent-edges]]
                      (assoc graph vertex (map second adjacent-edges)
                    ))
                    {} (group-by first edges)))

          (adjacents-of [vertex graph-map]
            (remove nil? (get graph-map vertex)))
          
          (non-visited [vertices visited-by-now]
            (seq (remove visited-by-now vertices)))

        (dfs->adj-finishings-iterative
          [[head-vertex & tail-vertices :as vertices] 
           [head-rem & tail-rems :as remainings] 
           finishings 
           visited 
           graph]
          (if (and (nil? head-vertex) (empty? remainings))
            finishings;; finishing-time-acc
            (if (nil? head-vertex)
              (recur [head-rem] tail-rems finishings visited graph)
              (if (visited head-vertex)
                (recur tail-vertices remainings finishings visited graph)
                (if-let [not-visited (non-visited 
                                       (adjacents-of head-vertex graph)
                                       (clojure.set/union visited (set remainings)))]
                  (recur not-visited 
                     (reduce #(cons %2 %1) remainings (reverse vertices)) ;; cons -> 1st
                     ;; (lazy-cat vertices remainings)
                     finishings
                     visited
                     graph)
                  (recur tail-vertices 
                         remainings 
                         (conj finishings head-vertex)
                         (conj visited head-vertex)
                         graph)
          )))))

          (dfs->by-func-type [leader-vertices graph dfs-func-type]
            "parameterized-by-type func that can be used to compute finishing-times or 
             in DFS for finding the strongly connected-components of a directed graph"
            (dfs-func-type
              (:finishing-times
                (reduce (fn [ctx leader-vertex]
                          (if ((:visited ctx) leader-vertex) ctx
                            (let [adjacents-traversal-m
                                  (->> (adjacents-of leader-vertex graph)
                                    (reduce
                                      (fn [adjacents-ctx each-adjacent] 
                                        (let [adjacent-finishings (dfs->adj-finishings-iterative
                                                                    [each-adjacent] 
                                                                    '() ;; remainings
                                                                    [] ;; finishing-times
                                                                    (:adjacents-visited adjacents-ctx) ;;visited
                                                                    graph)]
                                          (assoc  ;; preserve visited across adjacents
                                            (assoc adjacents-ctx :adjacents-connected ;; stack the founded component of adjacents
                                                   (conj (:adjacents-connected adjacents-ctx) ;; preserve previous
                                                         adjacent-finishings)) ;; add new
                                            :adjacents-visited
                                            ;; preserve each visited vertex by taking the stacked-vertices from finishing-times
                                            (reduce conj (:adjacents-visited adjacents-ctx) adjacent-finishings)
                                          )))
                                      ;; preserve already acummulated visiteds
                                      {:adjacents-connected [] :adjacents-visited (conj (:visited ctx) leader-vertex)}
                                      ;; [[4 1 7] [6 3]]
                              ))
                  
                                  ;; the need to use: reduce conj => because i'm reducing a nested-vec-of-vecs
                                  ;; !!! && make sure preserving the previous finishing-times!!!
                                  leader->adjacents-finishing-times (conj (:finishing-times ctx)
                                                                            (flatten
                                                                              (conj (:adjacents-connected adjacents-traversal-m)
                                                                                    leader-vertex)))

                                  all-visiteds (clojure.set/union 
                                                         (:visited ctx)
                                                         (:adjacents-visited adjacents-traversal-m))]
                
                              (assoc ;; preserve the visiteds during iterations
                                 ;; put the [[1-finishing-times] [2nd-finishing-times] leader-vertex]
                                 (assoc ctx :finishing-times leader->adjacents-finishing-times)
                                 :visited all-visiteds))))
                                                     
                    {:finishing-times [] :visited #{}}
                    leader-vertices))))
          
          (find-finishing-times [vertices orig-graph] 
            (dfs->by-func-type vertices orig-graph flatten))
          
          (find-strongly-connected-comps [vertices graph] 
            (dfs->by-func-type vertices graph identity)) ]

    (let [loaded-raw-graph (-> graph-file-path read-file)
          normal-graph (-> loaded-raw-graph
                           to->edges
                           build-graph-from-edges)
          reversed-graph (-> loaded-raw-graph
                           to->rev-edges
                           build-graph-from-edges)

          desc-sorted-vertices (reverse (sort (keys reversed-graph)))
          
          reversed-verts-by-finishing (reverse 
                                        (find-finishing-times desc-sorted-vertices reversed-graph)) ]
      (-> reversed-verts-by-finishing
        (find-strongly-connected-comps normal-graph))
)))

;; consider the vertices sorted descendent
;; compute finishing-times taking the reversed-graph as input(taking the desc vertices)
;; find-scc by sorting the finishing-times in descendent-order on the NORMAL-GRAPH
;; 
;; exercising...
(time
  (find-strongly-connected "src/clojure_learning/algorithms/sample-1.txt"))
;; [[4] [5] [2 1 3] [7 6 8]] -> OK

(time
  (find-strongly-connected "src/clojure_learning/algorithms/sample-2.txt"))
;; [[1 0 3 4] [2]] -> OK

(time
  (find-strongly-connected "src/clojure_learning/algorithms/test-sample-1.txt"))
;; Answer: 3,3,3,0,0 -> OK:: [(4 1 7) (6 3 9) (2 5 8)]

(time
  (find-strongly-connected "src/clojure_learning/algorithms/test-sample-2.txt"))
;; Answer: 3,3,2,0,0 -> OK:: [(4 5) (6 7 8) (1 3 2)]

(time
  (find-strongly-connected "src/clojure_learning/algorithms/test-sample-3.txt"))
;; Answer: 3,3,1,1,0 -> OK:: [(4) (5) (2 1 3) (7 6 8)]

(time
  (find-strongly-connected "src/clojure_learning/algorithms/test-sample-4.txt"))
;; Answer: 7,1,0,0,0 -> OK:: [(2 1 3 4 7 6 8) (5)]

(time
  (find-strongly-connected "src/clojure_learning/algorithms/test-sample-5.txt"))
;; Answer: 6,3,2,1,0 -> OK:: [(8 7 9 11 10 12) (3 6) (4 2 5) (1)]

;; and the last one...assignment test-case...
;; which are the five last bigger SCC(number); e.g  ...8 5 4 2 1
(time
  (take 5
     (reverse
       (sort
         (map count
             (find-strongly-connected 
               "src/clojure_learning/algorithms/kosaraju-course-file.txt"))))))
;; "Elapsed time: 51337.885541 msecs":: (152 126 114 108 97)

(clojure.set/union #{1 2} [3])
(remove (clojure.set/union #{1 2} (cons 3 nil)) [1 2])
