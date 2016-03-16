(ns clojure-learning.graph-connectivity
  (use 
    clojure.repl 
    clojure.test
    clojure.set
    ))

;; Graph Connectivity
;; Difficulty:	Hard
;; Topics:	graph-theory
;; Given a graph, determine whether the graph is connected. A connected graph is such that a path exists between any two given nodes.
;; -Your function must return true if the graph is connected and false otherwise.
;; -You will be given a set of tuples representing the edges of a graph. Each member of a tuple being a vertex/node in the graph.
;; -Each edge is undirected (can be traversed either direction).

;; first:: identify the nodes
(defn find-nodes [set-of-tuples]
  (reduce (fn [acc [from to]] (conj acc from to)) #{} set-of-tuples))

;; demo::
(find-nodes #{[:a :b] [:b :a]}) ;; #{:b :a}

;; NOT-USED:: second:: create a function which tells that two tuples are linked
(defn linked? [[a b :as head-tup] [c d :as tail-tup]]
  (cond (not= head-tup tail-tup) ;; a tuple cannot be linked with itself
    (or (= a c) (= a d) (= b c) (= b d))))

;; demo::
(linked? [:a :b] [:a :b]) ;; nil
(linked? [:a :b] [:a :d]) ;; true
(contains? #{:a :b} :b)

;; third:: given the set-of-tuples find partial-graph(islands) for each unique node in the graph
(defn find-direct-links [nodes set-of-tuples]
  (reduce 
    (fn [acc-m node] 
      (assoc acc-m node (vec (filter #(contains? (set %) node) set-of-tuples)))) 
    {} nodes))

(find-direct-links [:a :b :r :v] #{[:a :c] [:a :d] [:b :d] [:b :y] [:r :t] [:t :v]})
;; {:b ([:b :d] [:b :y])  :a ([:a :c] [:a :d])}

(vals (find-direct-links [:a :b] #{[:a :c] [:a :d] [:b :d] [:b :y]}))
(vals (find-direct-links [:a :b :r :v] #{[:a :c] [:a :d] [:b :d] [:b :y] [:r :t] [:t :v]}))
(find-direct-links [:a :b :r :v] #{[:a :c] [:a :d] [:b :d] [:b :y] [:r :t] [:t :v]})

(defn linked-islands? [l-island r-island]
  ((comp not empty?) (clojure.set/intersection (set (flatten l-island)) (set (flatten r-island)))))

(linked-islands? '([:a :b] [:b :d]) '([:d :e] [:d :f])) ;; true
(linked-islands? '([:a :b] [:b :d]) '([:f :e] [:g :h])) ;; true
(linked-islands? '([:t :v]) '([:r :t])) ;; true
(linked-islands? '([:t :v]) '([:r :y])) ;; false

;; forth:: take each values from the direct-linked-map (small island) and check with the next
;; if island is linked -> merge it one a bigger island forall
;; each linked-tuples-of-each-node can be flatten and checked against the next linked-tuples of the next node
(defn merge-direct-links [[head-island next-island & tail]]
  (if (nil? next-island) head-island
    (if (linked-islands? head-island next-island)
      (recur (cons (concat head-island next-island) tail))
      (recur (cons head-island tail)))))

(set (merge-direct-links (vals (find-direct-links [:a :b :r :v] #{[:a :c] [:a :d] [:b :d] [:b :y] [:r :t] [:t :v]}))))
;; -> NOT LINKED!!! -> an un-linked island as result

(merge-direct-links (vals (find-direct-links [:a :b :c :d :y :t :r] #{[:a :c] [:a :d] [:b :d] [:b :y] [:r :t] [:t :a]})))
(set (merge-direct-links (vals (find-direct-links [:a :b :c :d :y :t :r] #{[:a :c] [:a :d] [:b :d] [:b :y] [:r :t] [:t :a]}))))
;; -> ALL LINKED!!! -> initial set of tuples as result

;; fifth:: compose all the function into one from the previous created smaller functions
(defn connected-graph? [set-of-links]
  (letfn [(find-nodes [set-of-tuples]
            (reduce (fn [acc [from to]] (conj acc from to)) #{} set-of-tuples))
          (find-direct-links [nodes set-of-tuples]
            (vals 
              (reduce 
                (fn [acc-m node] 
                  (assoc acc-m node (vec (filter #(contains? (set %) node) (vec set-of-tuples))))) 
                {} nodes)))
          (linked-islands? [l-island r-island]
            ((comp not empty?) (clojure.set/intersection (set (flatten l-island)) (set (flatten r-island)))))
          (merge-direct-links [[head-island next-island & tail]]
            (if (nil? next-island) (set head-island)
              (if (linked-islands? head-island next-island)
                (recur (cons (concat head-island next-island) tail))
                (recur (cons head-island tail)))))]
    (-> set-of-links
        find-nodes
        (find-direct-links set-of-links)
        merge-direct-links
        (= set-of-links)
    )
))

(connected-graph? #{[:a :b] [:b :c] [:c :d]
                    [:x :y] [:d :a] [:b :e] [:x :a]})  ;; true -> OK

(connected-graph? #{[:a :b] [:b :c] [:c :d]
                    [:x :y] [:d :a] [:b :e]})          ;; false -> OK

(connected-graph? #{[1 2] [2 3] [3 1]
                    [4 5] [5 6] [6 4]})                ;; false -> OK

(connected-graph? #{[1 2] [2 3] [3 1]
                    [4 5] [5 6] [6 4] [3 4]})          ;; true -> OK

(deftest testing-graph? 
  (testing "graph? predicate should return true if all the nodes are linked and form a valid graph"
    (is
      (= true (connected-graph? #{[:a :b] [:b :c] [:c :d]
                                  [:x :y] [:d :a] [:b :e] [:x :a]})))
    (is 
      (= false (connected-graph? #{[:a :b] [:b :c] [:c :d]
                                   [:x :y] [:d :a] [:b :e]})))
    (is
      (= false (connected-graph? #{[1 2] [2 3] [3 1]
                                   [4 5] [5 6] [6 4]})))
    (is
      (= true (connected-graph? #{[1 2] [2 3] [3 1]
                                  [4 5] [5 6] [6 4] [3 4]})))
))
