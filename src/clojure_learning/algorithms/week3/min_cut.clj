(ns clojure-learning.algorithms.week3.min-cut
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

;;;;The file contains the adjacency list representation of a simple undirected graph.
;;;; There are 200 vertices labeled 1 to 200. The first column in the file represents the vertex label, and the
;;;; particular row (other entries except the first column) tells all the vertices that the vertex is adjacent to.
;;;; So for example, the 6th row looks like : "6	155	56	52	120	......". This just means that the vertex with label
;;;; 6 is adjacent to (i.e., shares an edge with) the vertices with labels 155,56,52,120,......,etc
;;;;
;;;;Your task is to code up and run the randomized contraction algorithm for the min cut problem and use it on the
;;;; above graph to compute the min cut (i.e., the minimum-possible number of crossing edges). (HINT: Note that you'll
;;;; have to figure out an implementation of edge contractions. Initially, you might want to do this naively, creating
;;;; a new graph from the old every time there's an edge contraction. But you should also think about more efficient
;;;; implementations.) (WARNING: As per the video lectures, please make sure to run the algorithm many times with
;;;; different random seeds, and remember the smallest cut that you ever find.) Write your numeric answer in the space
;;;; provided. So e.g., if your answer is 5, just type 5 in the space provided.

;; I will represent the graph as a list of edges, sorted by vertex value [[1 2] [1 3] ...]

;; NOT-used currently...
(defn read-file-by-line [from-file]
    (with-open [loaded-content (io/reader from-file)]
      (doseq [line (line-seq loaded-content)]
        (let [line-splitted (str/split line #"\\s+")]
          (println (first line-splitted) (rest line-splitted))
    ))))
(println (read-file-by-line file))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ------ entrypoint ------
;;;;;;;;; read file-in-lines
(defn stringify-file-in-lines [file]
  (str/split (slurp file) #"\n"))
;; OK!

(stringify-file-in-lines file)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;; structure file content in a map: { :node [adjacents] ... }
(defn structure-nodes-from [lines]
  (letfn [(build-nodes [[head & tail] mapped-nodes]
            (if (nil? head) mapped-nodes
              (let [nodes (str/split head #"\s+")]
                (recur tail (assoc mapped-nodes (first nodes) (rest nodes))))))]
    (build-nodes lines {})
))
;; OK!
(structure-nodes-from (stringify-file-in-lines file))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn remove-self-loops-from [adjacents contracted-node into-node]
  (vec (remove #{contracted-node into-node} adjacents)))
;; OK!

(remove-self-loops-from [1 2 4 5] 2 1) ;; [4 5]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn update-converged-node [graph-nodes-map contracted-node converged-node]
    (let [contracted-node-adj (get graph-nodes-map contracted-node)
        curr-converged-node-adj (get graph-nodes-map converged-node)]
      (dissoc 
        (assoc graph-nodes-map 
               converged-node 
               (remove-self-loops-from 
                 (concat contracted-node-adj curr-converged-node-adj)
                 contracted-node converged-node))
        contracted-node
        )))

(update-converged-node {1 [1 2 3] 2 [4 5 1], 3 [2 10]} 2 1)
;; {1 [4 5 3], 3 [2 10]}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn update-adjacents [adjacents contracted-node converged-node]
  (let [contracted-occurences (count (filter (partial = contracted-node) adjacents))
        but-contracted-adj (remove (partial = contracted-node) adjacents)]
    (apply vector 
           (concat but-contracted-adj
                   (repeat contracted-occurences converged-node)))))
;; now-OK!
(update-adjacents [1 2 3 4] 2 3) ;; [1 3 4 3]
(update-adjacents [4 4 3 3] 4 3) ;; [3 3 3 3]

(defn nodes-but [contracted-node all-nodes-map]
  (keys (dissoc all-nodes-map contracted-node)))
;; OK!
  
(nodes-but :one {:one "one" :two "two"}) ;; '(:two)
(nodes-but 1 {1 [1, 2] 3 [3 4] 4 [4 5]}) ;; '(4 3)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn update-graph-nodes [graph-nodes-map contracted-node converged-node]
  "function will first update the graph by contracting the contracted-node-adj into converged-node
   and will then attempt to update all other nodes to point to the new converged-node instead"
  (letfn [(exists-contracted? [contracted adjacents]
            (some (partial = contracted) adjacents))
          (adjacents-of [node graph-map] (get graph-map node))]
    (loop [contracted-graph  (update-converged-node graph-nodes-map contracted-node converged-node)
           [current-node & tail-nodes] (nodes-but converged-node contracted-graph)]
      (if (nil? current-node) contracted-graph   ;; exit recursivity base-case
        (if (exists-contracted? contracted-node (adjacents-of current-node contracted-graph))
          (recur 
            (assoc contracted-graph current-node 
                   (update-adjacents 
                     (adjacents-of current-node contracted-graph) contracted-node converged-node))
            tail-nodes)
          (recur contracted-graph tail-nodes)
          )))))
 
;; OK!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; function for computing min-cuts
(defn min-cuts [contracted-graph]
  (if (= 2 (count (keys contracted-graph)))
      (count (second (vals contracted-graph)))
  (let [rand-picked-contracted-node (rand-nth (keys contracted-graph))
        rand-picked-into-node (rand-nth (get contracted-graph rand-picked-contracted-node))]
      (recur 
        (update-graph-nodes contracted-graph 
                            rand-picked-contracted-node 
                            rand-picked-into-node)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; exercise.....
(def file "src/clojure_learning/algorithms/week3/karger.txt")
;(def file "resources/simple_graph_0.txt")
;(def file "resources/simple_graph_2.txt")
(def nodes-graph (structure-nodes-from (stringify-file-in-lines file)))
nodes-graph
(min-cuts nodes-graph) 
;; yields randomly a result => being a randomized type of algorithm we should
;; run it many times
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; randomly....
(time
  (loop [x 0 
         cuts []]
    (if (> x 400) (take 1 (sort (set cuts)))
      (recur (inc x) (conj cuts (min-cuts nodes-graph))))))
;; "Elapsed time: 90960.02621 msecs":: 17 => OK(correct)
