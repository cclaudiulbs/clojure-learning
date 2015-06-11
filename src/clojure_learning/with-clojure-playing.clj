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

;;;;;;;;;;; Clojure SETS ;;;;;;;;;;;;;
;; immutable unsorted collections
;;
;; lookup by using set-coll as a function
(#{:a :b :c :d} :a); :a

;; lookup by using :key of the set-coll as a function
(:a #{:a :b :c}); :a

;; lookup by using the abstract get passing the :key
(get #{:a :c :d} :a); :a

;; lookup for not-found, and specify a default :key
(get #{:a :c :d} :z :not-found); :not-found

(get #{:a :c :d} :z); :nil

([1 2 3] 0); 1

;; as said earlier, trying to find an element within a vector using the [contains?] function
;; will not produce the expect result, contains takes a :key/index instead of the value;
;; however clojure provides the [some predicate coll] function that returns the first
;; element that evaluates to true;
(doc some)
(some #{3} [1 2 3]); 3
;; a common clojure idiom for replicating the [contains?] behavior for vectors
;; the set-predicate in this case, finds if any of truthy values in the set are contained in the collection

;; clojure-sorted-sets have one thing to mention about:
;; as in java they should all be of the same type!!! if mixing the :key with numbers, say -> ClassCastException is
;; being threw!
(def my-sorted-set (sorted-set :c :a :b)); #{:a :b :c}
(conj my-sorted-set "not keyword"); ClassCastException is thrown!

;; because the internal comparator is comparing by 1 type, and they are not the same type
;; however we can provide the comparator to use
(def my-sorted-set-comp (sorted-set-by #((comp > str) %1 %2) "ab" "cc" "dd" :d))
(println my-sorted-set-comp); #{:d dd cc ab} --> as you see there's no ClassCastException
                            ; for diff types when a comparator is provided in place

(doc sorted-set-by)
;; internally set behaves as the hash, but values are bound to the corresponding :keys; each :key = each value
;; however a cross check is made for each :key while insertion.

;; import a function into the current namespace via:
(ns other
  (:require clojure.set))

;; or require the function on the fly:
(require 'clojure.set)

;; clojure purely inherits the mathematical principles of sets: Intersection, Union, Difference
(clojure.set/intersection #{1 2 3 4}
                          #{3 4 5 6}); #{4 3}
(clojure.set/union #{1 2 3}
                   #{3 4 5 6}); #{1 4 6 3 2 5}
(clojure.set/difference #{1 2 3 4}
                        #{3 4 5 6}); #{1 2}
;; one would expect that [clojure.set/difference] would give a result: #{1 2 5 6}
;; however [difference] func returns something like the collection A - collection B;

;;;;;;;;;;;;;;;;; Hashes ;;;;;;;;;;;;;;;;;;
;; many of the clojure collection act as FUNCTIONS of their KEYS.
;; --> hash-map collection can be invoked as a function passing as arg the KEY to lookup
;; --> Keys can be used as well as functions that lookup themselvs in the hash-map container passed as argument.
;; strange but nice :)
(def my-hash-map {:a 2
                  :b 4
                  :c 5})
;; hash-map collection as afunction
(my-hash-map :a); 2

;; hash-map key as a function
(:c my-hash-map); 5

(get my-hash-map :b); 4
(get my-hash-map :z :not-found); returns nil or default: :not-found

;; hash-map uses as we would expect the [seq] function as "iterator", which returns a sequence of MapEntrys tuples(in a vector)
(seq my-hash-map); ([:a 2] [:b 4]...)

;; ' means do not evalute!!! use as a string literal
(use 'clojure.repl)
(use 'clojure.java.javadoc)
;; using the doseq sequence abstraction, we can iterate through, by destructuring each MapEntry into a bound key-value synonyms
;;
(doseq [[mk mv] my-hash-map]
  (println (str "my map with key: " mk ", value: " mv))); as expected...
;; internally the [doseq] function abstraction starts iterating through the hash-map-entries
;; and we destructure EACH map-entry into a valuable synonyms
(doc doseq)

(into {} (seq my-hash-map)); {:a 2, :b 4, :c 5}

;; even if our pairs are NOT vectors we can transform them to be:
(def a-vec-of-lists '[(:a 1) (:b 2)])
(into {} (map vec a-vec-of-lists)); {:a 1, :b 2}
;; hey [map] func, take the [vec] function which accepts 1 arg, and while each iteration
;; apply me the [vec] function over the iterated-list-pair

(apply hash-map [:a 1 :b 2]); {:b 2, :a 1}
(zipmap [:a :b] [1 2]); {:b 2, :a 1}
;; zipmap takes exactly 2 arguments and pairs them to return a map back

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise_1: build a function that given an element -> would return the positional index of it in the container
;; should work a a sequence abstraction, disregarding the container type: if hash-map || hash-set -> :key
;; else if sequence -> index. <<---- DONE!
(defn pos
  "for the given item-arg -> pos returns the index for sequences || unsorted sets/hash-maps containers
     pos :a [:b :c :a] -> 2
     pos :z #{:a :b: :z} -> :z
     pos :foo {:bar 2 :foo 3} -> :foo"
  [item container]
    (cond
       (set? container)
          (item container)
        (map? container)
          (when (not (nil? (some #(identical? item %1) (keys container))))
            item)
        :else
          (let [seq-container (seq container)]      ; one-time transformation
              (loop [init-ret-val 0
                     popped-container seq-container]
                (cond (empty? popped-container)
                        nil
                      (= item (first popped-container))
                        init-ret-val
                :else
                      (recur (inc init-ret-val) (rest popped-container)))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TESTING the [pos] custom built function:
(pos :a {:b 2, :z 1, :a 3}); :a
(pos :a #{:b 2 :a 3}); :a
(pos 2 [1 2 3 4]); 1
(pos "cc" '("aa" "bb" "cc" "dd")); 2

(pos :m {:b 2, :z 1, :a 3}); nil
(pos :c {}); nil
(pos :c #{}); nil
(pos 333 [1 2 3]); nil
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cond (set? #{:b 2 :a 3}) "set"); set
(get #{:b :a 3} :a); :a

(:a #{:a}); :a
(some #(identical? :h %1) (keys {:a 2 :z 22 :b 3})); nil
(when (zero? (some #(compare :a %1) (keys {:a 2 :b 3})))
  :z)

(cond
   (zero? (some #(compare :b %1) (keys {:a 2 :b 3})))
      :b
   :else
     (cond (nil? nil)
           nil)
      :else :z); nil

(some #(= :a %1) [1 2 3])
(= :a "b")

(vector? [1])
(keys {:a 2 :v 3})
(map? {})
(zero? (some #(compare :z %1) (keys {:a 2 :b 3})))

;;;;;;;;;;;;;;; Shared Structures:: Concepts + Implementation ;;;;;;;;;;;;;;;;
(def base-list (list "cclaudiu" "cosar"))
(def child-list-1 (cons "petra" base-list))
(def child-list-2 (cons "clojure" base-list))
(= (next child-list-1) (next child-list-2)); true --> equal by comparing the list-elements by their values
(identical? (next child-list-1) (next child-list-2)); true --> the same shared structure(base-list) is also identical

;; we can think of the base-list as a historical version of both childs, but it's also the shared part of both childs.


;; to illustrate how the shared-data-structures are working internally we'll put another exercise on our TODO list
;; we'll build a shared-tree-data-structure.
;; requirements:
;;   + each node of the Tree will have 3 fields(a value, a left-branch, a right-branch)
;;   + expose the functionality through a "tree-conj" function having the interface:
;;     defn tree-conj tree-node tree-value -> tree-node
;;        - if the given value of the new node is smaller than current value -> go to :left-branch, :else :right-branch
;;   + expose a flatten function, that will flatten the tree-data-structure:
;;     defn tree-flatten tree-node -> [&tree-node]

;;                       node
;;       node-val   left-link      right-link

;; each node will be constructed as a map: {:node-val :left-branch :right-branch}

;; start from small: this is the most-leaf node -> if the tree-node is nil? then this node has NO left/right linking
(defn tree-conj
  [tree-node tree-value]
  (cond
     (nil? tree-node)
       {:node-val tree-value
        :left-branch nil
        :right-branch nil}
   ))

;; test:
(def tree-node-1 (tree-conj nil 5)); {:node-val 5, :left-branch nil, :right-branch nil}

;; if the value of the current-tree-node is higher than the new-value -> go to :left-branch
;; this corresponds to the following cond-branch:
;; (< tree-value (:node-val tree-node) )
(defn tree-conj
  [tree-node tree-value]
  (cond
     (nil? tree-node)
       {:node-val tree-value
        :left-branch nil
        :right-branch nil}

     (< tree-value (:node-val tree-node) )  ; new-tree-value < current node's tree-value -> go to leftbranch, by calling recursively
       {:node-val (:node-val tree-node)     ; copy current value
        :left-branch (tree-conj (:left-branch tree-node) tree-value)
        :right-branch nil
        }
     ))

;; test:
(def tree-node-1 (tree-conj tree-node-1 3))
; {:node-val 5, :left-branch {:node-val 3, :left-branch nil, :right-branch nil}, :right-branch nil} --> WHOOOOOHOOOOO :)

;; adding more nodes to inital tree-node-1
(def tree-node-1 (tree-conj tree-node-1 2))
; {:node-val 5, :left-branch {:node-val 3, :left-branch {:node-val 2, :left-branch nil, :right-branch nil}, :right-branch nil}, :right-branch nil}

;; now that we have a working solution for adding tree-nodes and sharing their structure we can handle the :right-branch as awell

(defn tree-conj
  [tree-node tree-value]
  (cond
     (nil? tree-node)
       {:node-val tree-value
        :left-branch nil
        :right-branch nil}

     (< tree-value (:node-val tree-node) )      ; new-tree-value < current node's tree-value -> go to leftbranch, by calling recursively
       {:node-val (:node-val tree-node)         ; copy current value
        :left-branch (tree-conj (:left-branch tree-node) tree-value)   ; go recursively to the tree-node referenced by :left-branch!!!
        :right-branch (:right-branch tree-node) ; copy current right-branch
        }

     :else
       {:node-val (:node-val tree-node)                                 ; copy existing :node-val
        :left-branch (:left-branch tree-node)                           ; copy existing :left-branch
        :right-branch (tree-conj (:right-branch tree-node) tree-value)} ; go recursively to the tree-node referenced by :right-branch!!!
     ))

;; testing the function:
(def tree-node-1 (tree-conj nil 5)); {:node-val 5, :left-branch nil, :right-branch nil}
(def tree-node-1 (tree-conj tree-node-1 3))
(def tree-node-1 (tree-conj tree-node-1 2))
(def tree-node-2 (tree-conj tree-node-1 29))
; {:node-val 5, :left-branch {:node-val 3, :left-branch {:node-val 2, :left-branch nil, :right-branch nil}, :right-branch nil}, :right-branch {:node-val 29, :left-branch nil, :right-branch nil}}
; here the node-val with 2 is the most inner node -> at the bottom of the tree, whereas node-val 29 identifies another tree node that is linked
; from the initial-node-val 5

;;                     tree-node-1
;;                     node-val 5
;;               :L 3              :R tree-node-2
;;         :L 2       :R nil         node-val 29
;;   :L nil   :R nil             :L nil          :R nil


;; and this summarizes the tree structural sharing example

;;;;;;;;;;;;;;;;;;;;;; Being lazy ;;;;;;;;;;;;;;;;;;
;; taking a collection as argument -> destructure the collection marking first & rest items of the collection
(defn step-coll [[head-arg & tail-args]]
  (if head-arg
    [head-arg, (step-coll tail-args)]
    []
))
(step-coll [1 2 3 4 5]); [1 [2 [3 [4 [5 []]]]]]

(step-coll (range 200000))

(doc range)

;; to implement the step-coll as a lazy-seq, we need to place the [lazy-seq] macro AT THE TOP of the function
;; providing as its body the sequence needed to work on:
(defn lazy-step-coll
  [coll]
  (lazy-seq                        ; this is the macro: lazy recipes reccommend: place this macro at the top!
     (if (seq coll)                ; the inner-sequence to work on; returns nil for empty sequence; our break-out recursion cond
       [ (first coll) (lazy-step-coll (rest coll)) ] )))    ; instead of returning a list back -> work on the vector data type
                                   ; [item1 (item2...)] -> using the sequence to "store" the lazy sequence inside this structure

(seq '()); nil
(seq []); nil

(lazy-step-coll [1 2 3 4 5]); (1 (2 (3 (4 (5)))))

;;;;;;;;;;;;;;; Lazy Recipes recommend the following 2 points(for the moment): ;;;;;;;;;;;;;;;;;
;; always use the [lazy-seq] macro at the TOP of the function body, when working on a sequence.
;; always use [rest] instead of [next] to process lazy sequences:
;;   + because [next] will eagerly check if the container is empty ? nil, while [rest] will return an empty list back
;; Watch out that DEPENDING on the function types -> the arguments ARE CONVERTED to sequences(lists) during evaluation
(rest []); ()
(next []); nil

;; some examples of thread-first + thread-last macros
(require '[clojure.string :as str])

(-> "cclaudiu"); "cclaudiu"

(str/split "cclaudiu" #""); ["c" "c"...]


;; combining thread-first with thread-last macros for buidling powerful expresive process-chains
(-> "claudiu"
    (str/split #"")
    rest
    (->> (filter (fn[each] (re-find #"\w+" each))); pass the result from [rest] as the LAST item int the FIRST FORM
         (map #(str "char: " %1))                 ; ->> passes the result of the first form as the LAST arg in the next form, for map(the ds in this case)
    ))
; ("char: c" "char: l"...)


(doc str/upper-case)
(doc filter)
(re-find #"\w" ".")

(defn lazy-step-coll-1
  [coll]
  (lazy-seq
     (if (seq coll)
       [(first coll) (lazy-step-coll-1 (rest coll))])
      ))
(lazy-step-coll-1 '(1 2 3 4 5))

;; how about implementing a simple range -> using lazy-seq, a rudimental impl of the [range]
(range 10)
(defn lazy-range
  [upper-bound]
  (lazy-seq
     (loop [start upper-bound      ;; start with upper-bound -> decreasing since its recursive
            acc-coll []]           ;; cons translates the [] into a sequential/list first
       (cond
          (< start 0)
            acc-coll               ;; exit recursion condition
        :else
          (recur (dec start) (cons start acc-coll))
        )
      )
   ))
;; we're using the decremental approach, since "cons" translates the vector to list + list cons pushes each element from first-leftmost position
;; 10 9 8 -> would become: (10) 9 8 -> (9 10) 8 -> (8 9 10)
(lazy-range 10); (0 1 2 .. 10)
(class (lazy-range 10)); clojure.lang.LazySeq

(doc while)
(doc for)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; some practice & training :) how to returns the same number again and again using diff approaches
; (solutions to exercises from 4clojure.com)
(->> (range)
    (take 4)
     last); 3

((comp last (partial take 4) range) 100); 3
(peek (reverse (list 1 2 3)))

(loop [start 10
      coll-acc []]
  (cond
   		(< 40 start)
   		coll-acc
   :else
   		(recur (* (inc 1) start) (conj coll-acc start)))); [10 20 40]

(rest (into [] (range 10 49 10))); [20 30 40]
(use 'clojure.repl)
(doc range)
(cons 1 [])
(conj nil 1 2); (2 1)
(conj [] 1 2); [1 2]

;; functions...
(let [[_ & tail-args] (range 9)]
  (-> tail-args
      last)); 8

;; how to build a lazy iteration using recursion:: simulating the [range] func again
(defn lazy-range
  [start-bound upper-bound]
  (lazy-seq
     (when (< start-bound upper-bound)
       (cons start-bound (lazy-range (inc start-bound) upper-bound)))))

(lazy-range 0 10); (0 1 2 .. 9)
(class (lazy-range 0 10)); clojure.lang.LazySeq
;; how to read this in clojure: mark the returned sequence using the top-level lazy-seq macro, providing
;; it's body; [when] returns [nil] when the cond fails when the function is called.
;; taking 0 10 -> (...cons 9 (nil))
;; cons 9 is the last condition that mets the [when] clause, while [nil] is the returned expression of the
;; failing-when condition for start-bound being equal to upper-bound;
;; this opens the gate that in clojure: cons 1 nil -> will treat nil as an empty container: and return (1)
(cons 1 nil); 1
;; therefore: recursive invocation... (cons 1 (...cons 9 (nil))) -> (cons 1 (...(cons 8 (9))))
;; cons -> will push on the start of the sequence each number; the recursivity is BUILDING the
;; actual sequence

#_(let [r (range 1e9)]
  (first r)
  (last r))

#_(let [r (range 1e9)]
  (last r)
  (first r))

;; if those sequences are executed one after the other -> then OutOfMemoryError is triggered, since the compiler
;; do NOT rearrange the code in Clojure, because the ORDER MIGHT MATTER!
;; we can keep a sequence in memory if we BIND THE HEAD OF THAT SEQUENCE TO A LOCAL VARIABLE.
;; The current example shows that the reference to the sequence is lost right after the [last] invocation completes
;; the compiler is smart enough to notice that [last] was invoked -> the in-memory obejcts are cleared aggresively
;; by the compiler. While the second binding, again creates that in memory ds, but when the [first] func returns
;; the compiler thinks the sequence is still needed, and hence DO NOT CLEARS it, since the head of the sequence is kept.

(doc delay)
(doc force)
(defn defer-expensive
  [cheap expensive]
  (if-let [result (force cheap)]; if cheap is a Delay -> returns cached value, else returns cheap-value
    result
    (force expensive)))

;; demo:
(def exp (defer-expensive
  (delay "some delayed object")
  (delay
     (do
       (Thread/sleep 3000)
       "an expensive lazy object NOT returned because first-arg yields in a truthy!")))); "some delayed object"

(delay false); Delay Object returned
(force (delay false)); false
(defer-expensive
  (delay false)
  (delay
     (do
       (Thread/sleep 3000)
       "this is executed eagerly")));

;; or another example might be:
(defn build-container-w-range
  [upper-bound]
  (delay
     (list (range 1 upper-bound))))
(def not-container-yet (build-container-w-range 10))
(def now-a-container (force not-container-yet))

;; setup for the quick-sort in clojure :) ;;
(doc partition)
(def a-vec [4 3 1 5 6 2 8])
(ratio? (/ (count a-vec) 2)); true
(int (/ (count a-vec) 2)); (/ 7 2) -> 3
(partition (int (/ (count a-vec) 2)) a-vec)

(doc if-let)

;; book example(joy of clojure...) --> does throw a runtime exception: cannot convert ISeq from Long!
(defn quick-sort
  [container]
  (lazy-seq
     (loop [[head & tail] container]
         (if-let [ [head-pivot & tail-items] (seq head)]
            (let [smaller? #(< %1 head-pivot)]
                (recur (list*
                          (filter smaller? tail-items)
                          head-pivot
                          (remove smaller? tail-items)
                          tail))) ;; -> concat also the tail-parts
             (when-let [[head-sec & tail-sec] tail]
                 (cons head-sec (quick-sort tail-sec)))
          )
    )))
(quick-sort a-vec)
(first a-vec)


;;               #_(loop [part-1   (first partitioned-colls)
;;                      part-2   (-> partitioned-colls
;;                                  rest
;;                                  flatten)
;;                      each-from-first   first-from-1
;;                      each-from-sec     (first (rest part-2))]
;;                   (cond
;;                      (> each-from-first pivot)
;;                        (do
;;                           (println (str "higher: " each-from-first))
;;                           (recur
;;                             (rest part-1)
;;                             (conj (vec part-2) each-from-first)
;;                             (first (rest part-1))
;;                             (first part-2))
;;                          )

;;                      (< each-from-sec pivot)
;;                        (do
;;                          (println (str "smaller: " each-from-sec))
;;                          (recur
;;                             (cons (vec part-1) (first part-2))
;;                             (rest part-2)
;;                             (first part-1)
;;                             (first (rest part-2)); dont take it twice -> set to the next of part-2
;;                           )
;;                          )
;;                       (nil? each-from-first)
;;                        part-2
;;                       (nil? each-from-sec)
;;                        part-1
;;                    :else
;;                      [part-1 part-2]
;;                     )
;;                 )
;;           )
;;     ))

(def a-vec [4 3 1 5 6 2 8])
(quick-sort (pop a-vec))
(conj (vec (list 3 4 5)) 1)
(conj (list 1 2 3) 4)
(doc list*)

(defn qsort
  [container]
  (lazy-seq
     (let [ [x & partitioned :as init-container] container
            pivot (first partitioned)
            smaller? #(< % pivot)
            pivot-less (remove #(= pivot %) init-container)]


         (filter smaller? pivot-less)
         pivot-less
         (remove smaller? pivot-less)


       )
   ))
(qsort [4 8 6 9 5 3 10])
(doc remove); -> the oponent of filter true -> returns items for which the pred returned falsy
(doc list*)
(rest '())

(list* 1 3 [32 3])
