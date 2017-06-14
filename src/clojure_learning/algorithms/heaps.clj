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
          worst case running time of O(nlogn) VIA value-comparison!!!"
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
;; core Heap operation:: extract-min:: performs in O(logn) running time/worst case of course... :)
;;;;;;;;;;;;
(defn heap-extract-min [heapified-list]
  "extract-min should perform in O(logn) time complexity
   the steps required are::
   1. assuming the root is the min -> heap sorted by min-nodes(root is always the min)
      extract it, by removing it from the heap-structure
   2. eventually the heap(logical binary searched tree will remain inconsistent without a root)
      so pick the LAST leaf node from the heap structure and PROMOTE it as ROOT.
      because being the last node(in a sorted heap) will 'FORCE' the mechanics to
      downgrade it to a valid heap-level, this operation is called the:: bubble-down operation
   3. as in the heapify operation:: compare new-promoted-root with both left-right logical children
      and (because the root is the min) choose the smaller between those children.
   4. perform the swap with new-promoted-root hence bubbling-down the promoted-root, and bubbling-up
      the child node. Recursivelly bubble down until (here's the base-case) there's NO more children!"
  (letfn [(arraylist-remove [#^ArrayList list idx]
            "FIX for clojure<=>java .remove ArrayList by index!!! instead of Object"
            (.remove list (int idx)))
          
          (promote-as-root! [h] 
            "side effectful operation:: will return old min root and perform the promotion of last leaf-node to new root"
            (cond
              (.isEmpty h) 
                nil
              (= (.size h) 1)
                (let [x (.get h 0)]
                  (arraylist-remove h 0)
                  x)
              :else
                (let [last-node-idx (dec (.size h))
                      last-node (.get h last-node-idx)
                      root-min-node (.get h 0)]
                  (arraylist-remove h last-node-idx)
                  (.set h 0 last-node)
                  root-min-node)))
          
          (left-child-of [h idx]
            "primitive func that takes an array and an index and retrieves the logical
             representation for the passed-index left-child"
            (let [left-idx (dec (* 2 (inc idx)))]
              (if (> left-idx (dec (.size h)))
                [-1 nil] ;; reached the upper bound
                [left-idx (.get h left-idx)]
          )))

          (right-child-of [h idx]
            "primitive func that takes an array and an index and retrieves the logical
             representation for the passed-index right-child"
            (let [right-idx (dec (inc (* 2 (inc idx))))]
              (if (> right-idx (dec (.size h)))
                [-1 nil]
                [right-idx (.get h right-idx)]
          )))

          (cas-bubble-down! [start-idx [lidx lval] [ridx rval] h]
            (let [startval (.get h start-idx)]
              (cond
                (and (< 0 lidx) (< 0 ridx)
                     (> startval lval) (> startval rval) 
                     (> lval rval)) ;; take min children
                  ;; swap with rval -> as it's the smallest
                  (do
                    (.set h start-idx rval)
                    (.set h ridx startval)
                    ridx) ;; new start-idx
                (and (< 0 lidx) (< 0 ridx)
                     (> startval lval) (> startval rval) (> rval lval))
                  ;; swap with lval -> as it's the smallest
                  (do
                    (.set h start-idx lval)
                    (.set h lidx startval)
                    lidx)
                (and (< 0 ridx) (> startval rval))
                  ;; swap with rval -> as it's the smallest
                  (do
                    (.set h start-idx rval)
                    (.set h ridx startval)
                    ridx) ;; new start-idx

                (and (< 0 lidx) (> startval lval))
                  ;; swap with lval -> as it's the smallest
                  (do
                    (.set h start-idx lval)
                    (.set h lidx startval)
                    lidx)
                
                :else (inc start-idx)
          )))
          
          (restructure-heap-recur! [h start-idx]
            (when (< start-idx (dec (.size h)))
              (let [[l-idx l :as lnode] (left-child-of h start-idx)
                    [r-idx r :as rnode] (right-child-of h start-idx)]
                (let [bubbled-down-idx (cas-bubble-down! start-idx lnode rnode h)]
                  (recur h bubbled-down-idx)))))]
    
    (cond (.isEmpty heapified-list) 
      heapified-list
    :else
      (let [min-node (promote-as-root! heapified-list)
            start-node 0]
        (restructure-heap-recur! heapified-list start-node)
        min-node)) ;; return min-node
))

;; exercising::
(def h (ArrayList.))
(doseq [x [12 8 7 6 5 11 4 2 3 1 10]] 
  (heap-insert h x))
h ;; [1 2 5 4 3 11 8 12 6 7 10] => OK (although is bit different than the heapify when it comes to but-minium root val)
(heap-extract-min h) ;; 1..12..[] -> eventually an empty list

;; this fixes the BUG in clojure while intercommunicating with java's ArrayList
;; .remove(by idx) -> in clojure all number instances being promoted as primitive wrappers
;; hence the .remove(by idx) is NEVER CALLED, the .remove(Object) overloaded method being called!!!
(defn arraylist-remove [#^ArrayList list idx]
  (.remove list (int idx)))

;;;;;;;;
;; with this HEAP-SORT becomes a pie @implementation
;;;;;;;;
(set! *warn-on-reflection* true)
(def nums (shuffle (range 0 100000)))
nums

(time
  (def core-sorted (sort nums)))
;; core sorting:: "Elapsed time: 50.575168 msecs"

;; an example of heap-sort, however the running time is not quite good...
(time
  (def s
    (let [heapified (ArrayList.)
          sorted (ArrayList.)] ;; mutable auto-shrinkable list
      (doseq [x nums] (heap-insert heapified x)) ;; O(n) however i initially thought of:: O(nlogn)
      (loop []
        (let [min (heap-extract-min heapified)]
          (when (number? min)
            (do 
              (.add sorted min)
              (recur)))))
  sorted

;; my merge-sort from previous implementation-studies computed
;; shuffled 100K nums in:: "Elapsed time: 1785.201451 msecs"
;; no way the heap-sort will compute faster than merge-sort...
;; "Elapsed time: 92,053.150033 msecs" for heap-sort

;; an example of heap-sort
(defn heap-sort-by-heapify [xs]
  (let [heapified (ArrayList.)
        sorted (ArrayList.)]
    (doseq [x (heapify xs)] (.add heapified x))
    (loop []
      (let [extracted-min (heap-extract-min heapified)]
        (when (number? extracted-min)
          (do 
            (.add sorted extracted-min)
            (recur)))))
    sorted
    ))
(time
  (def s (heap-sort-by-heapify nums)))
s

;;;;;;;;;;;;;;;
;; ANALYSING & IMPROVING the running times.....
;;;;;;;;;;;;;;;
(time
  (def a (reduce (fn [acc x] (.add acc x) acc) (ArrayList.) (range 0 100000))))
;; producing a new ArrayList with 100K items requires:: "Elapsed time: 142.279817 msecs"
;; kind of a lot when the core "sort" requires only:: "Elapsed time: 50.9928 msecs"
;; => therefore only building the array-list requires with a factor of 3 x times than
;; to sort it.
;; i've used ArrayList in my heap-insert/heap-extract implementations relying on
;; dynamic shrinking the array-list while removing items from it, however behind
;; the scenes, the entire array is copied with System.arraycopy...which is NOT
;; performant-wise at all...

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; OWN ideas...on how to improve the running time ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; To optimize the heapify operation, i should discard ALL the ((2^height - 1) level) items
;; bubbling-up, because those are leaves and shouldn't be heap-ordered!

;; Therefore:: based on the (alength) of the array, i should find the 
;; correspondent:: 2^x factor. i'm interested in finding the "x" here...
;; where the x stands for the height/level => if i know the height => 
;; i know which items should be discarded based on the total items.

;; Applying a rudimental math here, i'm interested in finding the (log2 x) 
;; by applying the formula:: (log10 x) / (log10 2) => (log2 x) gives the log base 2
(/ (Math/log10 8) (Math/log10 2)) ;; => (log2 x) -> where x = 3 => x = pow of 2:: 2^3
(/ (Math/log10 11) (Math/log10 2)) ;; => (log2 x) -> where x = 3.4594316186372978 = pow of 2:: 2^3
;; => so flooring the number and i got the height
;; If the items in an array size is: 11 -> then i can apply the math formula,
;; and get the height, then applying the power of 2 using that computed height
;; i know the range of the items that SHOULD BE included in the heap operation.

(defn heapify-opt [xs]
  "heapify takes a vector and transforms it into a heapified data-structure
   the operations performs in O(nlogn) just like any sorting operation
   Note:: there's no better sorting operation that performs faster than
          worst case running time of O(nlogn) VIA value-comparison!!!"
 (let [heapified-arr (into-array xs)
       height (int (/ (Math/log10 (alength heapified-arr)) (Math/log10 2)))
       rev-idxs (reverse (range 0 (Math/pow 2 height)))]

   ;; optimized to heapify ONLY the nodes which have children:: bubble-up
   (loop [[idx & first-idxs] rev-idxs]
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
(heapify-opt [3 4 1 2 6 9 5 7 8])        ;; [1, 2, 3, 4, 6, 9, 5, 7, 8] => OK
(heapify-opt [12 8 7 6 5 11 4 2 3 1 10]) ;; [1, 2, 4, 3, 5, 11, 7, 6, 12, 8, 10] => OK

;; with optimisation => discarding all the leaves::
(time (def h (heapify-opt nums))) ;; => "Elapsed time: 2597.754177 msecs"
;; without optimisation::
(time (def h (heapify nums))) ;; => "Elapsed time: 2945.936282 msecs"
;; => with optimisation i saved ~ 400 millis => which is quite good!
(time (def h 
        (let [a (ArrayList.)] (doseq [x nums] (heap-insert a x)) a )))
;; => iterating through all the nums and BUILDING the HEAP WHILE iterating
;; takes MUCH less time:: "Elapsed time: 918.576557 msecs"

;; optimizing the heap-extract-min to work with primitive-arrays, and 
;; avoiding the dumb copying that ArrayList does...
(defn heap-extract-min-opt [heapified-arr last-idx]
  "extract-min should perform in O(logn) time complexity
   the steps required are::
   1. assuming the root is the min -> heap sorted by min-nodes(root is always the min)
      extract it, by removing it from the heap-structure
   2. eventually the heap(logical binary searched tree will remain inconsistent without a root)
      so pick the LAST leaf node from the heap structure and PROMOTE it as ROOT.
      because being the last node(in a sorted heap) will 'FORCE' the mechanics to
      downgrade it to a valid heap-level, this operation is called the:: bubble-down operation
   3. as in the heapify operation:: compare new-promoted-root with both left-right logical children
      and (because the root is the min) choose the smaller between those children.
   4. perform the swap with new-promoted-root hence bubbling-down the promoted-root, and bubbling-up
      the child node. Recursivelly bubble down until (here's the base-case) there's NO more children!"
  (letfn [(promote-as-root! [h last-idx] 
            "side effectful operation:: will return old min root and perform the promotion of last leaf-node to new root"
            (cond
              (< last-idx 0) nil
              (zero? last-idx)
                (let [single (aget h last-idx)]
                  (aset h last-idx nil)
                  single)
              :else
                (let [last-node (aget h last-idx)
                      root-node-idx 0
                      root-min-node (aget h root-node-idx)]
                  (aset h last-idx nil)
                  (aset h root-node-idx last-node)
                  root-min-node)))
          
          (left-child-of [h curr-idx end-idx]
            "primitive func that takes an array and an index and retrieves the logical
             representation for the passed-index left-child"
            (let [left-idx (dec (* 2 (inc curr-idx)))]
              (if (> left-idx end-idx) 
                [-1 nil]
                [left-idx (aget h left-idx)]
          )))

          (right-child-of [h curr-idx end-idx]
            "primitive func that takes an array and an index and retrieves the logical
             representation for the passed-index right-child"
            (let [right-idx (dec (inc (* 2 (inc curr-idx))))]
              (if (> right-idx end-idx)
                [-1 nil]
                [right-idx (aget h right-idx)]
          )))

          (cas-bubble-down! [start-idx [lidx lval] [ridx rval] h]
            (let [startval (aget h start-idx)]
              (cond
                (and (< 0 lidx) (< 0 ridx)
                     (> startval lval) (> startval rval) 
                     (> lval rval)) ;; take min children
                  ;; swap with rval -> as it's the smallest
                  (do
                    (aset h start-idx rval)
                    (aset h ridx startval)
                    ridx) ;; new start-idx
                (and (< 0 lidx) (< 0 ridx)
                     (> startval lval) (> startval rval) (> rval lval))
                  ;; swap with lval -> as it's the smallest
                  (do
                    (aset h start-idx lval)
                    (aset h lidx startval)
                    lidx)
                (and (< 0 ridx) (> startval rval))
                  ;; swap with rval -> as it's the smallest
                  (do
                    (aset h start-idx rval)
                    (aset h ridx startval)
                    ridx) ;; new start-idx

                (and (< 0 lidx) (> startval lval))
                  ;; swap with lval -> as it's the smallest
                  (do
                    (aset h start-idx lval)
                    (aset h lidx startval)
                    lidx)
                
                :else (inc start-idx)
          )))
          
          (restructure-heap-recur! [h start-idx end-idx]
            (when (< start-idx end-idx)
              (let [[l-idx l :as lnode] (left-child-of h start-idx end-idx)
                    [r-idx r :as rnode] (right-child-of h start-idx end-idx)]
                (let [bubbled-down-idx (cas-bubble-down! start-idx lnode rnode h)]
                  (recur h bubbled-down-idx end-idx)))))]
    
    ;; the control of passing the last-idx is externalized...
    (let [min-node (promote-as-root! heapified-arr last-idx)
          start-idx 0]
      ;; after promotion -> the array is shrinked -> last-idx is less with one
      (restructure-heap-recur! heapified-arr start-idx (dec last-idx))
      min-node)) ;; return min-node
)
(def h (heapify-opt [12 8 7 6 5 11 4 2 3 1 10])) 
;; [1, 2, 4, 3, 5, 11, 7, 6, 12, 8, 10] => OK

;; second step ...extracting each min in the RIGHT reversed order...
(for [rev-idx (reverse (range 0 (alength h)))] 
  (heap-extract-min h rev-idx))
;; => (1 2 3 4 5 6 7 8 10 11 12) => niceeeeee

;;;;;;;;;;;;;;;
;; optimized heap-sort:: wooohoooo :) what a nice and neat function...
;;;;;;;;;;;;;;;
(defn heap-sort-opt [xs]
  (let [heapified-arr (heapify-opt xs)]
    (for [rev-idx (reverse (range 0 (count xs)))] 
      (heap-extract-min-opt heapified-arr rev-idx))
))

(def nums (shuffle (range 0 100000)))
(time 
  (def s (heap-sort-opt nums))) ;; "Elapsed time: 2427.288922 msecs"
;; => MUCH BETTER than the NON-optimised solution...comparable with merge-sort func
;; now compare this running time(not a pure running time, but...) with the 
;; initial NON optimised versions where time to perform:: 92,053.150033 msecs
;; ORDER OF MAGNITUDE ....well done!

(time
  (def core-sorted (sort nums))) ;; "Elapsed time: 40.887231 msecs"