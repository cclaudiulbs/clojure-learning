(ns clojure-learning.algorithms.scc)

(in-ns 'clojure-learning.algorithms.scc)

(import '[java.util Stack HashSet])
(require '[clojure.string :as str])
(set! *warn-on-reflection* false)

(defn find-strongly-connected [^String graph-file-path]
  (letfn [(read-file->lazy-edges [file-path]
            "func:: -> String file-path -> LazySeq((Long vert1, Long vert2))..)
             reads the file via a buffered-reader into a lazy-sequence"
            (with-open [^java.io.BufferedReader rdr (clojure.java.io/reader file-path)]
              (letfn [(read->edges [[head-line-str & tail-lines]]
                        (lazy-seq
                          (if (nil? head-line-str) nil
                            (let [edge (map #(Long/valueOf ^String %) (str/split head-line-str #"\s"))]
                              (cons edge (read->edges tail-lines))))))]
                (let [realized-lines (doall (line-seq rdr))]
                  (read->edges realized-lines)))))
          
          (to->lazy-rev-edges [[head-edge & tail-edges]]
            "func:: [(v1 v2) (v3 v4)] -> LazySeq(() ()..) 
             Lazy-seq function that reverses the direction of the previously built graph-edges
             Note: based on the input the func::read-file->lazy-edges"
            (lazy-seq
              (when-let [r (seq (reverse head-edge))]
                (cons r
                    (to->lazy-rev-edges tail-edges)))))

          (build-graph-from-edges [edges]
            "primitive function that takes the paired-edges and builds a map with vertices 
             as keys and directed-adjacents as vals"
            (reduce (fn [graph [vertex adjacent-edges]]
                      (assoc graph vertex (map second adjacent-edges)
                    ))
                    {} (group-by first edges)))

          (adjacents-of [vertex graph-map]
            (remove nil? (get graph-map vertex)))
          
          (non-visited-adjs-of [vert graph visited]
                      "algorithm assers the vertices are traversed in desc order higher->lower
                       => always ensure the higher vertices come first"
                      (->> graph
                          (adjacents-of vert)
                          (remove #(.contains visited %))
                          sort
                          reverse
                          seq)) ;; => ensures nil(easier branching::falsy)
          
          (dfs-4-vertex->finishings [vert graph finishings visited-by-now]
            "side-effect operation:: will populate the finishings for given vertex"
            (letfn [(dfs [verts graph roots finishings already-visited]
                      (doseq [v verts :when (not (.contains already-visited v))]
                        (.add already-visited v)
                        (if-let [non-visited-adjs (non-visited-adjs-of v graph already-visited)]
                          (do
                            (.push roots v)
                            (dfs non-visited-adjs graph roots finishings already-visited))
                          (.push finishings v)) ;; else => just push it to finishings(no-adjs for him)
                   ))]
              (let [roots (Stack.)]
                ;; @side-effect:: finishings will be partial-populated & already-visited with vertices
                ;; that compose a connected-component
                (dfs [vert] graph roots finishings visited-by-now)
                ;; then make sure the roots -> come on TOP of the finishings(they are ALREADY ordered
                ;; as they are pushed on the Stack)
                (loop []
                   (if (.empty roots) nil
                     (do
                       (.push finishings (.pop roots))
                       (recur))))
                )))
          
          (dfs->all-finishings [vertices graph]
            ;; optimize:: prefer passing the visited -> skipping any leader if already visited
            (let [visited-by-now (HashSet.)
                  by-component-finishings (Stack.)]
              (doseq [vert vertices :when (not (.contains visited-by-now vert))]
                  (dfs-4-vertex->finishings 
                    vert graph by-component-finishings visited-by-now))
              by-component-finishings
            ))

          (find-sccs [finishings graph]
            (let [visited (HashSet.)
                  sccs (Stack.)]
              (doseq [f finishings :when (not (.contains visited f))]
                (let [scc (Stack.)]
                  (do
                    (find-scc-for [f] graph visited scc)
                    (.push sccs scc))))
              sccs
          ))

          (find-scc-for [vertices graph visited scc]
            (doseq [f vertices :when (not (.contains visited f))]
              (.add visited f) ;; mark as visited
              (if-let [not-visited (non-visited-adjs-of f graph visited)] ;; find adjacents for it
                (do 
                  (.push scc f)
                  (find-scc-for not-visited graph visited scc))
                (.push scc f)
           )))]

    (let [directed-edges (-> graph-file-path 
                             read-file->lazy-edges)
          normal-graph (-> directed-edges
                           build-graph-from-edges)
          reversed-graph (->> directed-edges
                              to->lazy-rev-edges
                              build-graph-from-edges)
          desc-sorted-vertices (-> reversed-graph 
                                   keys 
                                   sort 
                                   reverse)
          rev-verts-by-finishing-times (reverse 
                                         (dfs->all-finishings
                                           desc-sorted-vertices reversed-graph)) ]
      (do 
        ;; (println (str "finishing times:: " reversed-verts-by-finishing))
        (-> 
          rev-verts-by-finishing-times
          (find-sccs normal-graph)))
)))

;; exercising...
(comment
  (time
    (find-strongly-connected "src/clojure_learning/algorithms/sample-1.txt"))
;; "Elapsed time: 0.527944 msecs" [[4] [3 1 2] [5] [8 7 6] [10] [9]]

  (time
    (find-strongly-connected "src/clojure_learning/algorithms/sample-2.txt"))
;; "Elapsed time: 0.482718 msecs" [[4 3 1 0] [2]]

  (time
    (find-strongly-connected "src/clojure_learning/algorithms/sample-real.txt"))
;; "Elapsed time: 0.569494 msecs" [[4] [3 1 2] [5] [8 7 6] [10] [9]]

  (time
    (find-strongly-connected "src/clojure_learning/algorithms/test-sample-1.txt"))
;; Answer: 3,3,3,0,0 -> "Elapsed time: 0.634245 msecs" [[7 1 4] [9 3 6] [8 5 2]]

  (time
    (find-strongly-connected "src/clojure_learning/algorithms/test-sample-2.txt"))
;; Answer: 3,3,2,0,0 -> "Elapsed time: 0.542796 msecs" [[5 4] [8 7 6] [2 3 1]]

  (time
    (find-strongly-connected "src/clojure_learning/algorithms/test-sample-3.txt"))
;; Answer: 3,3,1,1,0 -> "Elapsed time: 0.559667 msecs" [[4] [3 1 2] [5] [8 6 7]]

  (time
    (find-strongly-connected "src/clojure_learning/algorithms/test-sample-4.txt"))
;; Answer: 7,1,0,0,0 -> "Elapsed time: 0.537795 msecs" [[8 6 7 4 3 1 2] [5]]

  (time
    (find-strongly-connected "src/clojure_learning/algorithms/test-sample-5.txt"))
;; Answer: 6,3,2,1,0 -> "Elapsed time: 0.588697 msecs" [[12 10 11 9 7 8] [6 3] [5 2 4] [1]]

  (time
    (find-strongly-connected "src/clojure_learning/algorithms/test-sample-6.txt"))

  (time
    (find-strongly-connected "src/clojure_learning/algorithms/test-sample-12.txt"))
  ;; [[2] [1] [3]]
  
    (time
      (find-strongly-connected "src/clojure_learning/algorithms/test-sample-7.txt"))
    ;; [[401] [402] [400] [673] [672]]
)

;; and the monstrosity file...
(time
   (println
     (take 5
       (reverse
         (sort
           (map count
             (find-strongly-connected 
               "./SCC.txt")))))))

;; correct-result:: 434821, 968, 459, 313, 211

#_(println
   (sort-by count 
     (find-strongly-connected 
       "/Users/i312366/SCC.txt")))
