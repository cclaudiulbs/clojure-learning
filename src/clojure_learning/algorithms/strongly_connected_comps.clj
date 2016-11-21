(ns clojure-learning.algorithms.strongly-connected-comps)
(use 'clojure.test)
(require '[clojure.string :as str])

(defn read-file [file-path]
  (slurp file-path))

(read-file "src/clojure_learning/algorithms/scc-week4-algos-input-file.txt")

(defn listify [file-content]
  "primitive function that builds a list-of-list representing directed-edges"
  (map (fn [from-to-str] (str/split from-to-str #"\s")) 
       (str/split file-content #"\n")))

(defn rev-listify [file-content]
  "primitive function that reverses the graph paired-vertices directions"
  (->> file-content
    listify
    (map reverse)))

(def f-path "src/clojure_learning/algorithms/scc-week4-algos-input-file.txt")
(-> f-path read-file rev-listify) ;; OK

(defn build-graph-from-edges [edges]
  "primitive function that takes the paired-edges and builds a map with vertices as keys and
   directed-adjacents as vals"
    (reduce
      (fn [graph-map[from-vertex to-vertex]]
        (if-let [adjacents (get graph-map from-vertex)]
          (assoc graph-map from-vertex (conj adjacents to-vertex))
          (assoc graph-map from-vertex [to-vertex]))) 
      {} edges)) ;; OK

(deftest assert-built-graph "testing"
  (is (= ["1" "2" "5" "6" "7" "3" "8" "4"]
         (get (-> f-path read-file listify build-graph-from-edges) "1")))
)

(for [x (range 1 10) :when (not (< x 5))] x) ;; (5..9)

(defn all-vertices-visited? [visited vertices]
  (and (empty? (rest vertices))
      (empty? (remove visited vertices))))

(deftest testing-all-vertices-visited? "should test whether all vertices are visited"
  (is (= true (all-vertices-visited? #{1 2 3} [1])))
  (is (= false (all-vertices-visited? #{1 2} [3])))
)

(defn adjacents-of [vertex graph-map]
  (get graph-map vertex))

(deftest testing-adjacents-of 
  "should return neighbours of a key in map that is vals for a key/vertex"
  (is (= [1 2] (adjacents-of :one {:one [1 2]})))
  (is (nil? (adjacents-of :two {:one [1]})))
)

;; depth-first in clojure is a litlle bit more trickier since there's no mutation only recursions
(defn depth-first [[head-vertex & tail-vertices] visited graph]
  "base-depth-first function that returnes the visited-nodes"
    (if (nil? head-vertex) visited
      (if (visited head-vertex)
        (recur tail-vertices visited graph)
        (let [visited-adjacents (depth-first ;; mark the recursion return point for adjacents(don't loose context) 
                                  (remove visited (adjacents-of head-vertex graph)) 
                                  (conj visited head-vertex) 
                                  graph)]
          (recur (remove visited tail-vertices) (set (concat visited visited-adjacents)) graph))
)))

(def small-snippet-path-1 "src/clojure_learning/algorithms/small-snippet.txt")
(def g-small-1 (-> small-snippet-path-1 read-file listify build-graph-from-edges))
g-small-1
(depth-first (keys g-small-1) #{} g-small-1)

(def g-small-2 (-> small-snippet-path-1 
                 read-file 
                 rev-listify 
                 build-graph-from-edges))
g-small-2

(defn get-by-last-finishing-time [paired-vertices] 
  ((comp last last) paired-vertices))

;;;;;;;;;;
(defn depth-first-iterative-with-finishing-times
  "mutually recursive function with its counterpart the tree-expansion-recursion
   Note:: every state should be persisted across recursions!!!"
 [[head-vertex & tail-vertices] visited graph vertices-labeled finishing-time]
 (letfn [(visited? [vertex vertices-labeled] 
           ((set (map first vertices-labeled)) vertex))]
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
       (recur (remove visited tail-vertices)
              (conj visited head-vertex) 
              graph
              (vec (concat vertices-labeled acc-verts)) ;; accumulate the entire result -> there might be dups because a vertex is reachable from many adjacents!
              (get-by-last-finishing-time acc-verts)))
))))
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
       (conj adjacents-verts-with-times-rec 
             [current-vertex (inc (get-by-last-finishing-time adjacents-verts-with-times-rec))])
)))

(defn keys-in-desc [graph] 
  (-> graph keys sort reverse))

(deftest func-keys-in-desc "should return the keys of the graph-map sorted in reverse order"
  (is (= '("3" "2" "1") (keys-in-desc {"1" 1 "2" 2 "3" 3})))
)

(depth-first-iterative-with-finishing-times 
                (keys-in-desc g-small-1) 
                #{}
                g-small-1
                []
                0)

(defn backtrack-vertices->map [graph-map]
  "high-order function which glues together the mutual recursive functions and outputs the
   vertices-backtracked in a map structure"
  (letfn [(edges->map [edges]
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
  (reduce (fn [acc-labeled-verts [vertex finishing-time]]
            (if (get acc-labeled-verts vertex) acc-labeled-verts
              (assoc acc-labeled-verts vertex finishing-time))) 
          {} edges))

(edges->map [["2" 3] ["2" 4] ["5" 6]])

(backtrack-vertices->map g-small-1)
;; OK!
(backtrack-vertices->map g-small-2) 
;; OK!

;; NEXT STEP::
;; TODO:: drop "visited" as it's computed differently!!!
;; once the finishing-times have been computed! -> run the main DFS loop in the descendent order
;; of the finishing-times: for instance start

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
