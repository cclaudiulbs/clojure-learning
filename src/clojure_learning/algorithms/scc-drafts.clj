(ns clojure-learning.algorithms.scc-drafts
  (:import (clojure.utils.StronglyConnected)))

;; TODO:: to be cleaned!!!! -> or take "scc.clj" which is already cleaner
(in-ns 'clojure-learning.algorithms.strongly-connected-comps)

(set! *warn-on-reflection* false)
(require '[clojure.core.reducers :as r])
(require '[clojure.string :as str])
(require '[clojure.test :as t])

;;;;;;;;;;
;; non optimized read in memory whole file content
(defn read-file [file-path]
  (slurp file-path))

;;;;;;;;;;
;; reading file "line-by-line" that is through the BufferedReader interface
;; prefer [line-seq] which returns a lazy-seq of strings => less memory
(defn read-file->edges [^String file-path]
  "func:: -> String file-path -> [(Long vert1, Long vert2)...]
   reads the file via a buffered-reader line by line"
  (with-open [^java.io.BufferedReader r (clojure.java.io/reader file-path)]
    (loop [[head-line-str & tail-lines-str] (line-seq r)
           edges []]
      (if (nil? head-line-str) 
        edges
        (recur tail-lines-str (conj edges
                                    (map #(Long/valueOf ^String %) 
                                         (str/split head-line-str #"\s"))))))))

;; i can do better...saving memory...using lazy-seqs
(defn read-file->lazy-edges [^String file-path]
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

(read-file->lazy-edges "src/clojure_learning/algorithms/test-sample-1.txt")
;; LazySeq ((1 4) (2 8) (3 6) ...)

;;;;;;;;;;;
(defn to->edges [^String file-content]
  "func:: String all-file -> (()), function that builds a list-of-list representing directed-edges"
  (map (fn [from-to-str] 
         (map #(Long/valueOf ^String %) (str/split from-to-str #"\s"))) 
       (str/split file-content #"\n")))

(defn to->rev-edges [^String file-content]
  "func:: String all-file -> (()), function that reverses the graph paired-vertices directions"
  (->> file-content
    to->edges
    (map reverse)))

(defn to->lazy-rev-edges [[head-edge & tail-edges]]
  "func:: [(v1 v2) (v3 v4)] -> LazySeq(() ()..) 
   Lazy-seq function that reverses the direction of the previously built graph-edges
   Note: based on the input the func::read-file->edges"
  (lazy-seq
    (when-let [r (seq (reverse head-edge))]
      (cons r
          (to->lazy-rev-edges tail-edges)))))

(-> "src/clojure_learning/algorithms/test-sample-1.txt"
  read-file->lazy-edges
  to->lazy-rev-edges)

;;;;;;;;;;;
(defn build-graph-from-edges [^clojure.lang.PersistentVector edges]
  "function that takes the paired-edges and builds a map with vertices 
   as keys and directed-adjacents as vals"
  (reduce (fn [graph [^long vertex adjacent-edges]]
            (assoc graph vertex (map second adjacent-edges)))
          {} (group-by first edges)))

;; exercising:: 
(-> "src/clojure_learning/algorithms/sample-1.txt"
    read-file->edges
    build-graph-from-edges)

(def sample-1-graph
  (-> "src/clojure_learning/algorithms/sample-1.txt"
      read-file->lazy-edges
      build-graph-from-edges))
sample-1-graph

(def r-sample-1-graph
  (-> "src/clojure_learning/algorithms/sample-1.txt"
      read-file->lazy-edges
      to->lazy-rev-edges
      build-graph-from-edges))
r-sample-1-graph


;; 1st time:: "Elapsed time: 18207.621234 msecs"
;; 2nd time:: OutOfMemoryError GC overhead limit exceeded  java.util.regex.Matcher
;; 3rd time:: "Elapsed time: 16115.838716 msecs"
;; 4th time:: "Elapsed time: 45534.071925 msecs"
;; 5th time:: "Elapsed time: 49748.786821 msecs"
#_(time
   (def r-opt
     (-> "src/clojure_learning/algorithms/SCC.txt"
       read-file
       to->edges
       build-graph-from-edges
)))

;; 1st time:: "Elapsed time: 12873.814537 msecs" => 13 secs
;; 2nd time:: "Elapsed time: 123005.510974 msecs" => ~ 2mins:: what the heck???
;; 3rd time:: "Elapsed time: 125207.964107 msecs" =? ~ 2 mins
;; 4th time:: "Elapsed time: 13160.188122 msecs"
#_(time
    (def r-rev-opt
      (-> "src/clojure_learning/algorithms/SCC.txt"
        read-file
        to->rev-edges
        build-graph-from-edges
)))

;; 1st time:: "Elapsed time: 45337.763811 msecs":: using Buffered-Reader takes longer...
;; 2nd time:: OutOfMemoryError GC overhead limit exceeded  java.util.regex.Matcher
;; 3rd time:: OutOfMemoryError GC overhead limit exceeded  clojure.lang.LazilyPersistentVector.createOwning 
#_(time
     (def r-opt
       (-> "src/clojure_learning/algorithms/SCC.txt"
         read-file->edges
         build-graph-from-edges
)))

;; "Elapsed time: 24070.549295 msecs"
;; "Elapsed time: 32696.817832 msecs"
;; "Elapsed time: 22888.813997 msecs"
#_(time
       (def normal
         (-> "src/clojure_learning/algorithms/SCC.txt"
           read-file->lazy-edges
           build-graph-from-edges
)))

;; 1st time:: "Elapsed time: 27582.181524 msecs"
;; 2nd time:: "Elapsed time: 30123.501427 msecs"
;; 3rd time:: "Elapsed time: 29978.564801 msecs"
#_(time
      (def reversed
        (-> "src/clojure_learning/algorithms/SCC.txt"
          read-file->lazy-edges
          to->lazy-rev-edges
          build-graph-from-edges
)))

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
;; this function computes the finishing-times in correct order, however being an expansive
;; type of recursive function will blow the stack eventually::
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

;; exercising::
(dfs->adj-finishings-expansive [6] [] #{} sample-1-graph) ;; [4 8 7 6]

;; function that retrieves the non-visited already vertices::it returns nil or non-visited-verts
(defn non-visited [vertices visited-by-now]
  (seq (remove visited-by-now vertices)))

;; exercising
(non-visited [1 2] #{1})   ;; (2)
(non-visited [1 2] #{1 2}) ;; nil
(non-visited [8] #{7 8})   ;; nil
(non-visited [8] #{6 7})   ;; (8)

;;;;;;;;;;;;;
;; replacing expansive-recursion with iterative-recursion for computing the finishing-times
;; Note: this goes quite head-blowing i have to mention...but the good side is that this
;; version using iterative recursion can be tuned to use the TCO mechanism
;;;;;;;;;;;;;
(defn dfs->adj-finishings-iterative [verts visited-by-now graph]
   (loop [[head-vertex & tail-vertices :as vertices] verts
          [head-rem & tail-rems :as remainings] '()
          finishings []
          visited visited-by-now]
     (if (and (nil? head-vertex) (empty? remainings))
         finishings
       (if (nil? head-vertex)
         (recur [head-rem] tail-rems finishings visited)
         (if (visited head-vertex)
           (recur tail-vertices remainings finishings visited)
           (if-let [not-visited (non-visited 
                                  (adjacents-of head-vertex graph)
                                  (r/reduce conj visited remainings))] ;; go only in 1 direction
               (recur not-visited ;; go for adjacents
                  ;; but put the vertices to head in the remaining container => on return just pop it
                  (r/reduce (fn [rems x] (cons x rems)) remainings (reverse vertices))
                  finishings
                  visited)
               (recur tail-vertices ;; no adjacents to visit -> start nnexting and mark as visited
                      remainings 
                      (conj finishings head-vertex) ;; because adjacents are exhausted -> the leaders will come after
                      (conj visited head-vertex))) ;; marking as visited
          )))))

(dfs->adj-finishings-iterative [8] #{} sample-1-graph) ;; [7 4 6 8]
(dfs->adj-finishings-iterative [6] #{} sample-1-graph) ;; [4 8 7 6]
(dfs->adj-finishings-iterative [9] #{} sample-1-graph) ;; [10 9]

;; iterative-recur finishing-times
(dfs->adj-finishings-iterative [8] '() [] #{} sample-1-graph) ;; [7 4 6 8]
(dfs->adj-finishings-iterative [6] '() [] #{} sample-1-graph) ;; [4 8 7 6]
(dfs->adj-finishings-iterative [9] '() [] #{} sample-1-graph) ;; [10 9]

;; expansive-recur finishing-times
(dfs->adj-finishings-expansive [8] [] #{} sample-1-graph)     ;; [7 4 6 8]
(dfs->adj-finishings-expansive [6] [] #{} sample-1-graph)     ;; [4 8 7 6]

;;;;;;;;;;;;;
(defn dfs->by-func-type [leader-vertices graph dfs-func-type]
  "parameterized function by type:: if flatten -> finishing-times, if identity -> SCCs"
  (dfs-func-type
    (:finishing-times
      (r/reduce (fn [ctx leader-vertex]
                  (if ((:visited ctx) leader-vertex) ctx
                    (let [adjacents-traversal-m
                              (->> (non-visited  ;; reduce only for non-already-visited adjacents of leader
                                     (adjacents-of leader-vertex graph) 
                                     (:visited ctx))
                                   (r/reduce
                                     (fn [adjacents-ctx each-adjacent] 
                                       (let [adjacent-finishings (dfs->adj-finishings-expansive
                                                                        [each-adjacent]
                                                                        ;; '() ;; remaining-adjacents(preserved during tails-recur)
                                                                        []  ;; finishing-times
                                                                        (:adjacents-visited adjacents-ctx) ;; already-visited
                                                                        graph)]
                                         (assoc  ;; preserve visited across adjacents
                                           (assoc adjacents-ctx :adjacents-connected ;; stack the founded component of adjacents
                                                  (conj (:adjacents-connected adjacents-ctx) ;; preserve previous
                                                        adjacent-finishings)) ;; add new
                                           :adjacents-visited
                                           ;; preserve each visited vertex by taking the stacked-vertices from finishing-times
                                           (r/reduce conj (:adjacents-visited adjacents-ctx) adjacent-finishings)
                                     )))
                                     {:adjacents-connected [] 
                                      :adjacents-visited (conj (:visited ctx) leader-vertex)} 
                                     ;; preserve already acummulated visiteds & add the leader vertex
                                     ;; [[4 1 7] [6 3]]
                              ))
                  
                          leader->adjacents-finishing-times 
                                    (conj 
                                      (:finishing-times ctx)
                                      (into [] (r/flatten
                                                 (conj (:adjacents-connected adjacents-traversal-m)
                                                       leader-vertex))))

                          all-visiteds (clojure.set/union 
                                                 (:visited ctx)
                                                 (:adjacents-visited adjacents-traversal-m))]
                
                      (let [m (assoc ;; preserve the visiteds during iterations
                                 ;; put the [[1-finishing-times] [2nd-finishing-times] leader-vertex]
                                 (assoc ctx :finishing-times leader->adjacents-finishing-times)
                                 :visited all-visiteds)]
                      
                          m))))
                                                     
          {:finishing-times [] :visited #{}}
          leader-vertices
))))

;; will result in finishing-times:: (9 10 6 7 8 1 2 3 5 4)
(dfs->by-func-type 
  (reverse (sort (keys r-sample-1-graph))) 
  r-sample-1-graph 
  flatten)
  
;; will result in StronglyConnectedComponents:: [(4) (5) (2 1 3) (7 6 8) (9 10)]
(dfs->by-func-type (reverse '(9 10 6 7 8 1 2 3 5 4)) sample-1-graph identity)

;;;;;;;;;;;;;
;; Final func that computes the SCCs via Kosaraju-Algo of a given graph
;;;;;;;;;;;;;;
(defn find-strongly-connected [^String graph-file-path]
  (letfn [(read-file->lazy-edges [^String file-path]
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

          (build-graph-from-edges [^clojure.lang.PersistentVector edges]
            "primitive function that takes the paired-edges and builds a map with vertices 
             as keys and directed-adjacents as vals"
            (reduce (fn [graph [vertex adjacent-edges]]
                      (assoc graph vertex (map second adjacent-edges)
                    ))
                    {} (group-by first edges)))

          (adjacents-of [^long vertex ^clojure.lang.PersistentHashMap graph-map]
            (remove nil? (get graph-map vertex)))
          
          (non-visited [^clojure.lang.PersistentVector vertices ^clojure.lang.PersistentHashSet visited-by-now]
            (seq (remove visited-by-now vertices)))
          
          (dfs->adj-finishings-iterative [verts visited-by-now graph]
            (loop [[head-vertex & tail-vertices :as vertices] verts
                   [head-rem & tail-rems :as remainings] '()
                   finishings []
                   visited visited-by-now]
              (if (and (nil? head-vertex) (empty? remainings))
                  finishings
                (if (nil? head-vertex)
                  (recur [head-rem] tail-rems finishings visited)
                  (if (visited head-vertex)
                    (recur tail-vertices remainings finishings visited)
                    (if-let [not-visited (non-visited 
                                           (adjacents-of head-vertex graph)
                                           (r/reduce conj visited remainings))] ;; go only in 1 direction
                        (recur not-visited ;; go for adjacents
                           ;; but put the vertices to head in the remaining container => on return just pop it
                           (r/reduce (fn [rems x] (cons x rems)) remainings (reverse vertices))
                           finishings
                           visited)
                        (recur tail-vertices ;; no adjacents to visit -> start nnexting and mark as visited
                               remainings 
                               (conj finishings head-vertex) ;; because adjacents are exhausted -> the leaders will come after
                               (conj visited head-vertex))) ;; marking as visited
                   )))))
          
          (dfs->adj-finishings-expansive
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

          (dfs->by-func-type [leader-vertices graph dfs-func-type]
            "parameterized function by type:: if flatten -> finishing-times, if identity -> SCCs"
            (dfs-func-type
              (:finishing-times
                (reduce (fn [ctx leader-vertex]
                          (if ((:visited ctx) leader-vertex) ctx
                            (let [adjacents-traversal-m
                                      (->> (non-visited 
                                             (adjacents-of leader-vertex graph) (:visited ctx))
                                           (reduce
                                             (fn [adjacents-ctx each-adjacent] 
                                               (let [adjacent-finishings (dfs->adj-finishings-expansive
                                                                                [each-adjacent]
                                                                                []
                                                                                (:adjacents-visited adjacents-ctx) ;; already-visited
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
                                             ;; [(4 1 7) (6 3)]
                                      ))
                  
                                  leader->adjacents-finishing-times 
                                                   (conj (:finishing-times ctx)
                                                            (flatten
                                                              (conj (:adjacents-connected adjacents-traversal-m)
                                                                    leader-vertex)))

                                  all-visiteds (clojure.set/union 
                                                         (:visited ctx)
                                                         (:adjacents-visited adjacents-traversal-m))]
                
                              (let [m (assoc ;; preserve the visiteds during iterations
                                         ;; put the [[1-finishing-times] [2nd-finishing-times] leader-vertex]
                                         (assoc ctx :finishing-times leader->adjacents-finishing-times)
                                         :visited all-visiteds)]
                                  m))))
                    {:finishing-times [] :visited #{}}
                    leader-vertices))))

          (find-finishing-times [vertices orig-graph] 
            (dfs->by-func-type vertices orig-graph flatten))
          
          (find-strongly-connected-comps [vertices graph] 
            (dfs->by-func-type vertices graph identity)) ]

    (let [directed-edges (-> graph-file-path read-file->lazy-edges)

          normal-graph (-> directed-edges
                           build-graph-from-edges)
          
          reversed-graph (->> directed-edges
                              to->lazy-rev-edges
                              build-graph-from-edges)

          desc-sorted-vertices (-> reversed-graph 
                                   keys 
                                   sort 
                                   reverse)
          
          reversed-verts-by-finishing (reverse 
                                        (find-finishing-times 
                                          desc-sorted-vertices reversed-graph)) ]
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
;; [(4) (5) (2 1 3) (7 6 8) (9 10)] -> OK

(time
  (find-strongly-connected "src/clojure_learning/algorithms/sample-2.txt"))
;; [(1 0 3 4) (2)] -> OK

(time
  (find-strongly-connected "src/clojure_learning/algorithms/sample-real.txt"))
;; [(4) (5) (2 1 3) (7 6 8) (9) (10)] -> OK

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
#_(time
   (take 5
      (reverse
        (sort
          (map count
              (find-strongly-connected 
                "src/clojure_learning/algorithms/kosaraju-course-file.txt"))))))
;; Previously NON-optimized:: "Elapsed time: 51337.885541 msecs":: (152 126 114 108 97)
;; Current OPTIMIZED:: "Elapsed time: 18596.257383 msecs":: (152 126 114 108 97)

;; real-graph-file ~ 900K entries
(time
   (take 5
      (reverse
        (sort
          (map count
              (find-strongly-connected 
                "src/clojure_learning/algorithms/SCC.txt"))))))

;;;;;;;;;;;;;;;
;; DEBUGGING:: 9 -> 10 decoupled component
;; "Elapsed time: 778.820454 msecs"
#_(time (def r (reverse (sort (keys reversed)))))

#_(time
    (def finishing-times 
      (dfs->by-func-type r reversed flatten)))

#_(time
   (def finishing-times 
     (dfs->adj-finishings-iterative r reversed)))


;; and freezes here @[874938]:: -> then fails with::
;; StackOverflowError   clojure.lang.Util.hasheq (Util.java:170)
;; --> grep 874938 SCC.txt 
;; 874938 874939
;; --> grep 874939 SCC.txt 
;; 874938 874939 ==> a component ISOLATED!!!! not connected to anything...
;; handle that case!!!

;; .....
;; finishings:: [874953]
;; finishings:: [874950]
;; finishings:: []
;; finishings:: [874938]
;; 
#_(time
   (def sccs
     (dfs->by-func-type (reverse (keys reversed)) normal identity)))

(defn non-visited-adjs-of [vert graph visited]
  "doesn't treats the vertices in desc order...!!!"
  (seq (remove visited (adjacents-of vert graph))))

;; => treat vertices in descendent order && convert to a more human readable version::
(defn non-visited-adjs-of [vert graph visited]
            "algorithm assers the vertices are traversed in desc order higher->lower
             => always ensure the higher vertices come first"
            (->> graph
                (adjacents-of vert)
                (remove visited)
                sort
                reverse
                seq)) ;; => ensures nil(easier branching::falsy)

(non-visited-adjs-of 6 simple #{7 8 9 10 11 12})

(def t (transient {0 '()}))
(assoc! t 1 (cons 23 (get t 1)))
(assoc! t 2 (cons 24 (get t 2)))
(persistent! t)


(time
  (def f1
    (dfs (reverse (sort (keys r-sample-1-graph)))
         #{} r-sample-1-graph flatten)))
f1 ;; (9 10 6 7 8 5 1 2 3 4) -> OK
 
(time
  (def scc
    (dfs->adj-finishings-iterative 
      (reverse f1)
      #{} sample-1-graph identity)))
scc ;; ((4) (2 1 3) (5) (6 7 8) (10)) -> OK

#_(time
   (def f1
     (dfs
       (reverse (sort (keys reversed)))
       #{} reversed flatten)))
f1 ;; 
 
#_(time
   (def scc
     (dfs
       (reverse f1)
       #{} normal identity)))
#_(time
   (def five-most-bigger 
     (take 5 
        (reverse (sort (map count scc))))))

(defn dfs->finishings [leader-verts graph]
  (letfn [(non-visited-adjs-of [vert graph visited]
            "algorithm assers the vertices are traversed in desc order higher->lower
             => always ensure the higher vertices come first"
            (->> graph
                (adjacents-of vert)
                (remove visited)
                sort
                reverse
                seq))] ;; => ensures nil(easier branching::falsy)
    (let [fs (transient {0 nil})] ;; the STATE that makes things faster...
      (loop [next-leaders (rest leader-verts)
             visited #{}
             [x & xs] [(first leader-verts)]
             recorded #{}
             finishings []]
        ;; base-case:: returning finishings:: after exhausted ALL leaders not before!
        (if (nil? x) ;; exhausted leader
          (recur (rest next-leaders)
                 visited
                 [(first next-leaders)]
                 #{}
                 finishings) ;; reset recordings
          (if (empty? next-leaders) 
            finishings
            (if (visited x)
              (recur next-leaders visited xs recorded finishings)
              (if-let [not-yet-visited (non-visited-adjs-of x graph 
                                         (clojure.set/union visited recorded))]
                (recur next-leaders;; leader still has adjacents
                       visited    ;; don't mark it as visited yet
                       (concat not-yet-visited (cons x xs)) ;; build the RIGHT order
                       (conj recorded x) ;; mark it as already recorded
                       finishings)
                (recur next-leaders
                       (conj visited x) 
                       xs
                       recorded
                       (conj finishings x)))
            )))))))

(defn non-visited-adjs-of [vert graph visited]
  "algorithm assers the vertices are traversed in desc order higher->lower
   => always ensure the higher vertices come first
   => [visited] is an instance of java.util.HashSet. here"
  (->> graph
      (adjacents-of vert)
      (remove #(.contains visited %))
      sort
      reverse
      seq))

(non-visited-adjs-of 4 rsimple #{})
rsimple

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(import '[java.util Stack HashSet])

(defn dfs-4-vertex->finishings [vert graph finishings visited-by-now]
  "side-effect operation:: will populate the finishings for given vertex"
  (letfn [(dfs [verts graph roots finishings already-visited]
            (doseq [v verts :when (not (.contains already-visited v))]
              (do
                (println "adding visited: " v)
                (.add already-visited v)
                (let [non-visited-adjs (non-visited-adjs-of v graph already-visited)]
                  (if (nil? non-visited-adjs)
                    (do
                      (println (str "pushing " v " to finishings"))
                      (.push finishings v))
                    (do
                      (println (str "pushing: " v " on stack"))
                      (.push roots v)
                      (println (str "recurring with adjacents of " v ": " non-visited-adjs))
                      (dfs non-visited-adjs graph roots finishings already-visited))
               )))))]
    (let [roots (Stack.)]
      ;; @side-effect:: finishings will be partial-populated & already-visited with vertices
      ;; that compose a connected-component
      (dfs [vert] graph roots finishings visited-by-now)
      ;; then make sure the roots -> come on TOP of the finishings(they are ALREADY ordered
      ;; as they are pushed on the Stack)
      (loop []
         (if (.empty roots) nil
           (do
             (println (str "adding to finishings from stack: " (.peek roots)))
             (.push finishings (.pop roots))
             (recur))))
)))

;; and non-verbose(removing println debugging...)
(defn dfs-4-vertex->finishings [vert graph finishings visited-by-now]
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

;; exercise::
(let [finishings (Stack.)
      visited (HashSet.)]
  (do
    (dfs-4-vertex->finishings 10 rsimple finishings visited)
    finishings)) 
;; -> OK!!! sample-2 reversed finishings:: [3 2 0 1 4]
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn dfs->all-finishings [vertices graph]
  ;; optimize:: prefer passing the visited -> skipping any leader if already visited
  (let [visited-by-now (HashSet.)
        by-component-finishings (Stack.)]
  (doseq [vert vertices :when (not (.contains visited-by-now vert))]
      (dfs-4-vertex->finishings 
        vert graph by-component-finishings visited-by-now)
    )
  by-component-finishings
))

;; exercise::
(dfs->all-finishings rverts rsimple);

(dfs->all-finishings [8] sample-1-graph) ;; [7 4 6 8]
(dfs->all-finishings [6] sample-1-graph) ;; [4 8 7 6]
(dfs->all-finishings (reverse (sort (keys r-sample-1-graph))) r-sample-1-graph) 
;; [10 9 7 4 6 8 5 2 1 3]

;; expansive-recur finishing-times
(dfs->adj-finishings-expansive [8] [] #{} sample-1-graph)     ;; [7 4 6 8]
(dfs->adj-finishings-expansive [6] [] #{} sample-1-graph)     ;; [4 8 7 6]
(dfs->adj-finishings-expansive (reverse (sort (keys r-sample-1-graph))) [] #{} r-sample-1-graph)
;; [10 9 7 4 6 8 5 2 1 3]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def simple
  (-> "src/clojure_learning/algorithms/sample-1.txt"
    read-file->lazy-edges
    build-graph-from-edges))
simple

(def rsimple
  (-> "src/clojure_learning/algorithms/sample-1.txt"
    read-file->lazy-edges
    to->lazy-rev-edges
    build-graph-from-edges))
rsimple

(def rverts (reverse (sort (keys rsimple)))) 
rverts ;; (12 11 10 9 8 7 6 5 4 3 2)

(dfs->adj-finishings-expansive rverts [] #{} rsimple)
;; [1 5 2 4 3 6 8 9 7 10 11 12]

;;;;;;;;;;;
(defn find-sccs [finishings graph]
  (let [visited (HashSet.)
        sccs (Stack.)]
    (doseq [f finishings :when (not (.contains visited f))]
      (let [scc (Stack.)]
        (do
          (find-scc-for [f] graph visited scc)
          (.push sccs scc))))
    sccs
))

(defn find-scc-for [vertices graph visited scc]
  (doseq [f vertices :when (not (.contains visited f))]
    (.add visited f) ;; mark as visited
    (if-let [not-visited (non-visited-adjs-of f graph visited)] ;; find adjacents for it
      (do 
        (.push scc f)
        (find-scc-for not-visited graph visited scc))
      (.push scc f)
)))

;; exercise::
(dfs->all-finishings (reverse (sort (keys r-sample-1-graph))) r-sample-1-graph) 
;; [9 10 6 7 8 5 1 2 3 4]
(def reversed-finishings (reverse [9 10 6 7 8 5 1 2 3 4]))
reversed-finishings

(find-sccs reversed-finishings sample-1-graph)

;;;;;;;;;;;