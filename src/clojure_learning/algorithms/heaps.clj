(ns clojure-learning.algorithms.heaps)
;;;;;;;;;
;; HEAP and some notes about HEAP-SORT...
;;;;;;;;;
;;;;;;;;;
;; Selection sort usually has the worst running time of O(n^2), however,
;; using a Heap-Sort we can improve it to: O(nlogn), the same running time
;; that merge-sort or quick-sort has
;; O(n log n) time to build a heap, because we need
;; to apply Heapify roughly n/2 times (to each of the internal nodes), and (2) that it takes O(n log n)
;; time to extract each of the maximum elements, since we need to extract roughly n elements and each
;; extraction involves a constant amount of work and one Heapify. Therefore the total running time of
;; HeapSort is O(n log n).
;; it's NOT possible via comparisons to produce a running time for sorting algorithms,
;; better than O(nlogn)

;; Heap Analysing::
;; With this assumption, level 0 of the tree has 1 node, level 1 has 2 nodes, and up to level h, which has
;; 2h nodes. All the leaves reside on level h.

;; as an improvement to my prev implementation of the "heapify()" function, i can
;; drop the level-h since, there's no way to try to shift them down(bubble-down),
;; as these nodes are leaves, therefore the very next level (h-1) is worth taken
;; into consideration::
;; At the bottommost level there are 2h nodes, but we do not call Heapify on any of these so the work is
;; 0. At the next to bottommost level there are 2h−1 nodes, and each might sift down 1 level. At the 3rd
;; level from the bottom there are 2h−2 nodes, and each might sift down 2 levels.
;; Clearly the algorithm takes at least
;; Ω(n) time (since it must access every element of the array at least once) so the total running time for
;; BuildHeap is Θ(n).

;; operations that a Heap data structure should define::
(declare heapify)
(declare extract-min)
(declare insert)

;; some container:: [3 4 1 2 6 9 5 7 8]
(defn left-child-of [h idx]
  "primitive func that takes an array and an index and retrieves the logical
   representation for the passed-index left-child"
  (let [left-idx #(dec (* 2 (inc %)))]
    [(left-idx idx) (get h (left-idx idx))]))

(defn right-child-of [h idx]
  "primitive func that takes an array and an index and retrieves the logical
   representation for the passed-index right-child"
  (let [right-idx #(dec (inc (* 2 (inc %))))]
    [(right-idx idx) (get h (right-idx idx))]))

;; exercising::
(left-child-of [3 4 1 2 6 9 5 7 8] 1)  ;; [3 2] => idx 3, val 2
(right-child-of [3 4 1 2 6 9 5 7 8] 1) ;; [4 6] => idx 4, val 6
(left-child-of [3 4 1 2 6 9 5 7 8] -1) ;; [-1 nil] => not existent val
(left-child-of (to-array [3 4 1 2 6 9 5 7 8]) 1) ;; [3 2] => being associative works for primitive arrays

;; quick repl
(alength (into-array [1 2 3])) ;; 3
(dotimes [x (alength (into-array [1 2 3]))]
  (println x)) ;; prints 0, 1, 2  nil
(Math/min 5 3) ;; 3

;;;;;;;;;;;;;
;; core Heap operation
;;;;;;;;;;;;;
(defn heapify [xs]
  "heapify takes a vector and transforms it into a heapified data-structure
   the operations performs in O(nlogn) just like any sorting operation
   Note:: there's no better sorting operation that performs faster than
          worst case running time of O(nlogn)"
 (let [heapified-arr (into-array xs)
       last-idx (dec (alength heapified-arr))
       reverse-indexing #(take (inc %) (iterate dec %))]
   (loop [[idx & first-idxs] (reverse-indexing last-idx)]
     (cond (nil? idx) heapified-arr
       :else
         (let [node-val (aget heapified-arr idx)
               [left-idx left-val] (left-child-of heapified-arr idx)
               [right-idx right-val] (right-child-of heapified-arr idx)]
           ;; curr-node higher than both left & right children?
           (if (and ((comp not nil?) left-val)
                    ((comp not nil?) right-val)
                    (> node-val left-val) (> node-val right-val))
             ;; compare the children nodes
             (if (> right-val left-val)
               (do ;; swap left
                 (aset heapified-arr idx left-val)
                 (aset heapified-arr left-idx node-val)
                 (recur (cons left-idx first-idxs))) ;; bubble-down(remember swaped indx)
               (do ;; swap right
                 (aset heapified-arr idx right-val)
                 (aset heapified-arr right-idx node-val)
                 (recur (cons right-idx first-idxs)))) ;; bubble-down(remember swaped indx)
             ;; curr-node higher only than right-child
             (if (and ((comp not nil?) right-val)
                      (> node-val right-val)) ;; swap right
               (do
                 (aset heapified-arr idx right-val)
                 (aset heapified-arr right-idx node-val)
                 (recur (cons right-idx first-idxs))) ;; bubble-down(remember swaped indx)
               ;; curr-node higher only than left-child
               (if (and ((comp not nil?) left-val)
                        (> node-val left-val))
                 (do
                   (aset heapified-arr idx left-val)
                   (aset heapified-arr left-idx node-val)
                   (recur (cons left-idx first-idxs))) ;; bubble-down(remember swaped indx)
                 ;; no attention required
                 (recur first-idxs)
))))))))

(heapify [3 4 1 2 6 9 5 7 8]) ;; [1, 2, 3, 4, 6, 9, 5, 7, 8]
(type (heapify t)) ;; [Long] array

(heapify [12 8 7 6 5 11 4 2 3 1 10]) ;; [1, 2, 4, 3, 5, 11, 7, 6, 12, 8, 10]
;;right is ok, bubbling down with left-idx:: 9
;;bubbling down with left-idx:: 7
;;left is ok, bubbling down with right-idx:: 5
;;bubbling down with right-idx:: 3
;;right is ok, bubbling down with left-idx:: 9
;;bubbling down with left-idx:: 1
;;bubbling down with left-idx:: 3
;;bubbling down with right-idx:: 7


(import java.util.ArrayList)
;;;;;;;;;;;;;;;;;;;;;;;;;
;; odd? idx/2 - 1 => root
;; even? idx/2 => root
(defn heap-insert [arr x]
  "core Heap function that inserts one by one elements in a given array-list
   that preserves the heap-ordering!!!"
  (letfn [(last-idx [list]
            (dec (.size list)))

          (root-of-idx [idx array]
            (let [root-idx (if (odd? idx) 
                             (quot idx 2)
                             (dec (quot idx 2)))]
              (if (< root-idx 0) 
                [-1 nil]
                [root-idx (.get array root-idx)])))

          (cas-bubble-to-root [curr-idx node-val heapified-list]
            (when (>= curr-idx 0)
              (let [[root-idx root-val] (root-of-idx curr-idx heapified-list)]
                (when (and (>= root-idx 0) 
                           (> root-val node-val))
                  (do
                    (.set heapified-list root-idx node-val)
                    (.set heapified-list curr-idx root-val)
                    (recur root-idx node-val heapified-list))))))]
      ;; 1. set the newly x as last node in array
      (.add arr x)
      ;; 2. check value comparison with root:: extract to function and go recursive till root
      (cas-bubble-to-root (last-idx arr) x arr)
))

;; exercising...
(def h (ArrayList.))
(doseq [x [3 4 1 2 6 9 5 7 8]] 
  (heap-insert h x)) ;; OK!
h ;; [1 2 3 4 6 9 5 7 8]

(def h (ArrayList.))
(doseq [x [12 8 7 6 5 11 4 2 3 1 10]] 
  (heap-insert h x))
h ;; [1 2 5 4 3 11 8 12 6 7 10] => OK (although is bit different than the heapify when it comes to but-minium root val)

;;;;;;;;;;;;
;; core Heap operation:: extract-min
;;;;;;;;;;;;
(defn heap-extract-min [heapified-list])
