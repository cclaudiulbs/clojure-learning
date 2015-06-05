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
;; Anything you type in here will be executed
;; immediately with the results shown on the
;; right.

(defn foo [x]  (* x x))
(def squared (foo 4))

(def a-vec-1 (vec (take 20(range 40 65))))
(get a-vec-1 0)
(nth a-vec-1 0)
(a-vec-1 0)

(def nested-vec [[1 2 3]
                 [4 5 6]
                 [7 8 9]])
(get-in nested-vec [1 2])


(require 'clojure.repl)
(assoc-in nested-vec [1 2] 81)
(doc update-in)
(update-in nested-vec [1 2] * 44)
; take the old-value -> itentified by the position of keys -> feed its corresponding
; value as argument to the function

(defn my-map
  "recursively traverses the collection applying the provided function returning a new collection of transformed items"
  [a-func & container]
  (loop [tail-items (vec container)
         acc-items []]
      (if (empty? tail-items)
        acc-items

        (recur (pop tail-items) (conj acc-items (a-func (peek tail-items)))))
  )

)

(doc my-map)

(def other-nums (my-map #(str "prefix: " %1) "cc" "dd"))
(println other-nums)

(next [1 2 3])
(pop [1 2 3])


;; Lists and all about them :)
;; a persistentList is a LINKED LIST internally, where each node knows its distance from the end.
;; any element CAN ONLY be found in a clojure-list starting from the begg of the list and walking fw till end.
;; any element can ONLY be removed or inserted at the start of the list(the leftmost position)
;; rarely provide any value added over vectors -> hence are not so used.

;; both: [cons] + [conj] add something to the begg of the list.

(cons 1 [2 3 4]); (1 2 3 4)
(conj [2 3 4] 1); [2 3 4 1]
;; on vectors as seen [cons] + [conj] both add one element but: [cons] acts like [unshift] from JS
;; in that it ADDS the element on the start and returns a SEQUENCE instead of the vector itself
;; [conj] works as expected: for vectors it adds on top of the stack, for lists at the begg of the list
;; explain: [cons] on vector adding on the begg of the data-structure??? maybe because clojure evals first the
;; arguments and the operand-in this case the function cons -> and it knows the function will pre-generate
;; a sequence on the eval of args step; that sequence is used instead to "now normally" prepend the element
;; on the begg of the list.


(conj '(2 3 4) 1); (1 2 3 4)
(cons 1 '(2 3 4)); (1 2 3 4)
;; therefore [conj] & [cons] work the SAME on PersistentLists, however they diff in the order of args passed

(doc cons)

;; USE [cons] only FOR SEQUENCES, lazy sequences, ranges! do not use [cons] for vectors(at it returns a sequence back)
;; and do not use [cons] for persistentLists because the result is not guaranteed that it will behave consistently

(pop []); throws IllegalStateException: cannot pop empty vector
(pop '()); throws IllegalStateException: cannot pop empty list
(next []); nil
(next '()); nil
(rest []); ()
(rest '()); ()


(contains? [1 2 3] 2)
(contains? '(1 2 3) 2); IllegalArgumentException: contains? not supported on persistentLists

;; dont use lists to find items by index! because clojure starts from the begg in finding the item -> O(n)
;; practical reason why:
((list 1 2 3) 0); ; classCastException

(cons 1 nil)

(cons 1 '(2 3 4)); (1 2 3 4)
(pop '(1 2 3 4)); (2 3 4)??? wtf": pop poppes an element from the start of the list
(peek '(1 2 3 4)); 1
;; [pop] + [peek] of PersistentStack funcs operate on lists from the begginging!!!


;; because any persistentList implements the PersistentStack which provides the: [peek] + [pop] funcs
(peek '(1 2 3)); 1
(pop '(1 2 3)); (2 3)
;; as seen the peek is the same as: [first]; while [next] & [rest] is the same as [pop]

(next '(1 2 3)); (2 3)
;; same as [rest]
(rest '(1 2 3)); (2 3)
;; the differences come when the collection is empty, or has 1 element
(pop '(1)); ()
(rest '(1)); ()
(next '(1)); nil

(pop '()); illegalStateException:: cannot pop empty list
(next '()); nil
(rest '()); ()

(pop []); illegalStateException:: cannot pop an empty vector
(next []); nil
(rest []); ()

;; its OK that the vectors + lists share the same behavior, for these 3 funcs, however the vectors works from top of the stack
;; while the lists work this funcs from the start of the list(leftmost)

(cons 1 [2 3 4]); (1 2 3 4) --> returns a seq back, and as seq work by adding items to the start of the list

(into [] (rest [1 2 3])); [2 3]

(pop [1]); []
;; IMPORTANT: for vectors! use always ONLY: [conj] + [pop];
;; DON'T USE [cons] + [next] + [rest] as they all return a sequence back -> and the results are based on the SEQ
;; NOT based on the initial vector passed; there's a "cast" into based on the func type: cons + next + rest ->
;; the type of collection is identified(seq, hashes, vectors) -> and then converted to certain type
;; then the operation is performed

(first {:one 1 :two 2}); [:one 1]
(vector? (first {:one "one" :two "two"})); true
(seq {:one 1 :two 2}); ([:one 1] [:two 2])

(doseq [[each-key each-val] {:one 1 :two 2}]
  (println (str "each key: " each-key ", each val: " each-val)) )

(source doseq)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; QUEUEs: FIFO as oposed to vectors which behave MORE like STACKs(LIFO)
;; peek -> peeks the first element from queue without removing
;; conj -> adds on element at the top of the queue
;; pop -> return a new queue with the leftmost(first) element removed from queue
(def my-queue (conj clojure.lang.PersistentQueue/EMPTY "one" "two" "three"))
(peek my-queue); "one"
(def new-queue (conj my-queue "four"))
(count my-queue); 3
(count new-queue); 4
(sequential? my-queue); true

(def init-queue (pop new-queue))
(count init-queue); 3
(= my-queue init-queue); false
(identical? my-queue init-queue); false
eval my-queue
(doc mapcat)

;; queues are not a common behavior thus clojure did NOT incorporate naturally in its language, but provides a simple way
;; to start build one from the [clojure.lang.PersistentQueue/EMPTY];

(= [1 2 3] [1 2 3]); true
(= '(1 2 3) '(1 2 3)); true


;; DO NOT BE TEMPTED TO use [rest] for QUEUES!!! instead of [pop]; always use POP.
;; why? because [rest] indeed removes the first element from queue -> BUT IT is a list/seq like operation
;; and hence the data-structure(queue) is translated to a 'seq' before -> and in return of [rest] -> we get
;; a seq back NOT a queue.
