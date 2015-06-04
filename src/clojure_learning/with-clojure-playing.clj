(ns cclaudiu)

*clojure-version*

(use 'clojure.repl)
(use 'clojure.java.javadoc)

(source defn)

(def a-vector [1 2 3])
(conj a-vector (peek [4 5 6]))

(doc conj)

(source defn)

(def sum-of-nums (fn [x] (fn [y] (+ x y))))
(let [result ((sum-of-nums 1) 5)] result)

(defn traverse-collection
  [collection]
  (if (not (nil? (first collection)))
      (do
        (println (str "Iterating each num: " (first collection)))
        (recur (rest collection)))))
;; invoking::
(traverse-collection [1 2 3 4 5 6])

(defn traverse-container
  [[head-arg & tail-args]]
  (if (not (nil? head-arg))
      (do
        (println (str "Iterating through the container: " head-arg))
        (recur tail-args )
      )
  )
)
;; test the destructuring of args
(traverse-container ["cclaudiu" "gigi" "coca"])

(rest [1 2 3 4])
(first [1 2 3 4])
(peek [1 2 3 4])
(take 3 [1 2 3 4])

(def a-test {:some-test "some-test-val"})
(defn check-nil
  [a-map]
  (if (not (nil? (:some-test a-map)))
    (println "NOT NIL"))
)
(check-nil a-test)

;; using [loop] + [recur] to build a map function in rudimental forms
(defn map-rec
  "map-rec is a function that simulates the power of built-in map"
  [func & [collection]]                          ; 1) destructure the function args into the first arg-func + vector-container
  (loop [acc-coll []                             ; 2) start with an empty accumulator, that will be populated and referenced in the next recur call as arg
         current-coll collection]                ; 3) current-coll takes the initial passed collection
    (if (empty? current-coll)                    ; when we reach the end of popped container -> return the accumulator
        acc-coll
        (recur (conj acc-coll (func (first current-coll))) (rest current-coll)))) ; else put an item with func applied in the accumulator
)                                                ; and the rest of container -> recur will rebind the loop local vars to these
                                                 ; as current collection is popped the applied container is accumulated

;; imagine loop as a function that establishes the next point of recursion.
;; in order to be "recur" while traversing the collection, the "accumulator", which
;; do NOT mutates, will be passed as the loop rebound argument, while the collection
;; is being popped by each recursive invocation
;; 4) populate the accumulator with the first element mapped by func + use this accumulator with the
;;    first element to invoke the next time the recursion, and pass the rest of collection except first element
;; as known, vector operations are fast when working from the top of the stack: remove the most rightmost element is fast
;; [conj] applied on vector will push in the top of the stack, while [pop] removes from the rightmost stack
;; on lists the operation [conj] starts from the leftmost element.
;; demo::
(doc map-rec)
(def mapped-items (map-rec #(str "func applied: " %1) ["cclaudiu" "mary"]))
(println mapped-items)

(def mapped-items-2 (map-rec #(* %1 20) (take 20 (range)))); 0 20 40 60 ...
(def mapped-items-3 (map-rec (fn [each] (* each 20)) (take 20 (range))))
;; personally i like better the last version where the "fn" is explicitly stated

;; subvec: start-included-pos -> end-excluded-pos
(def a-sub-vec (subvec mapped-items 0 1))

(source map)
(source doseq)
(doseq [[some-key some-value] {:name "cclaudiu" :job "clojure developer"}]
  (println (str "key: " some-key ", value: " some-value)))
;; doseq is a macro that uses destructure to bind each pair of the data-structure to the bounded locals
;; the map when iterated using "seq" returns a MapEntry -> this MapEntry is a vector: [key value]
;; using destructuring we bind our local custom to the pair of the map -> to each map-entry vector

;; doseq is built upon "let" binding at least a pair of 2 elements together
(doseq [each [1 2 3 4]]
  (str "each number: " each))

(seq [1 2 3 4])

;; simple traverse function that uses the primitive-loop + recur to traverse recursively the given container
(defn traverse
  [container]
  (loop [curr-container container]
    (if(not (empty? curr-container))
      (do
        (println (str "each item from container" (first curr-container)))
        (recur (rest curr-container)))
      )))
(traverse [1 2 3 4])

(contains? ["one" "two"] "one"); false? wtf -> ah it's because contains? check for KEYs NOT values -> vector key is the index
(contains? ["one" "two"] 1); true now
(doc contains?)

;; dont use vectors as queues!!! using [subvec] to "pop" from the start of the queue & [conj] to push at the top of the queue
;; because [subvec] keeps a reference to the entire collection -> so the popped element is NOT garbage collected since its still in memory
;; dont use vectors as queues!!! using [rest] func since it returns a sequence back + [conj] behaves differently on sequences than vectors
;; vectors are NOT sparsed --> cannot remove from a vector or add in the middle of collection -> since all the keys/indexes would have
;; to be re-calculated and Clojure don't do this. when removing always from the top of the stack!
;; [assoc] + [dissoc] dont work for vectors!!!

(cons 1 [2 3 4]); 1 2 3 4

(require '[clojure.string :as str])
(->> "a space delimited string"
     (.split #" ")
     (map (fn[each] (str "--" each)))
     (filter (fn[each] (re-find #"d|s" each)))
     (reduce str))

;; thread-last operator: takes an input and binds it as the LAST argument in the FIRST-FORM
;; if there are many forms, the result of first form will be binded as the LAST argument to the next form

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; take this:
(def format-token (comp reduce str
                       (partial filter (fn[each] (re-find #"d|s" each)))
                       (partial map (fn[each] (str "--" each)))
                       str/split))
;; clojure.lang.ArityException: Wrong number of args (1) passed to: core/reduce
;; ah!!! it fails for: (comp (reduce str)), because the expression is evaluated first by the compiler and
;; this is NOT a function to compose because it misses anyway the last argument for reduce;
;; if no parans -> then how the bind func "str" is passed??? that's why we need partial
;; to return a function back + fixing the first callback-func arg to "str" of reduce, inside of the closure
;; because every func in this context is NOT alone -> and takes ~ 1 argument AND does NOT return a function to compose BACK!!!
(def format-token (comp (partial reduce str)
                       (partial filter (fn[each] (re-find #"d|s" each)))
                       (partial map (fn[each] (str "--" each)))
                       (partial str/split)
                  ))

(format-token "a space delimited string" #" ")

(def partial-add (partial +))
(partial-add 1 2)
