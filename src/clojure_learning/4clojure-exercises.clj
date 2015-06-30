;; Exercises from 4clojure.com
;; @author cclaudiu
;; My solutions come first, followed (possibly) by other users solutions(which are explicitly stated)
(use 'clojure.repl)

;; Write a function which returns the Nth element from a sequence.
;; 1. using the collection in place as a function <- standard clojure provides
(defn nth-item-fn
  [coll item-idx]
  ((vec coll) item-idx))

(nth-item-fn '(1 2 3 4) 3) ; 4

(defn nth-item-fn-1
  [coll idx]
  (loop [[head & tail] coll
         pos 0]
    (if (= pos idx)
      head
      (recur tail (inc pos)))))

(nth-item-fn-1 ["aa" "bb" "cc" "dd"] 2); "cc"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Write a function which returns the total number of elements in a sequence.
(defn count-items
  [coll]
  (loop [total 0
         [head & tail] coll]
    (if (nil? head)
      total
      (recur (inc total) tail))))

;; demo:
(count-items [1 2 3 4 5])

;; or one-liner:: inspired from other fellas:
(#(reduce + (map (fn [_] 1) %))  [1 2 3])
;; in steps:
;; 1. map a new data-structure of only 1's foreach element in the init-coll
;; 2. apply reduce on the returned collection of ones: [1 1 1], by passing the [+] function

(def countt (fn[coll] (reduce (fn[c n] (inc c)) 0 coll)))
(countt [1 2 3]); 3

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Write a function which reverses a sequence;
(reduce #(cons %2 %1) () [1 2 3])

(reduce conj nil [1 2 3]) ; (3 2 1)
;; conj nil 1 -> (1); conj (1) 2 -> (2 1); conj (2 1) 3 -> (3 2 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Write a function which returns only the odd numbers from a sequence.
(filter #(= 1 (mod % 2)) [1 2 3 4])
; (1 3)

(filter odd? [1 2 3 4])
; (1 3)

(doc mod)
(mod 11 2); 1
(mod 10 2); 0

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Write a function which returns the first X fibonacci numbers.
;; if n < 2 n else f(n-1) + f(n-2)
(#(map (fn fib
   [some-num]
    (if (< some-num 2)
      some-num
      (+ (fib (- some-num 1)) (fib (- some-num 2)))))
(range 1 (inc %))) 8)

(doc letfn)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Write a function which returns true if the given sequence is a palindrome.
;; preparing:
(clojure.core/reverse "RACE c ar")
(= "racecar" (apply str (map str/lower-case (remove #(re-seq #"\s" %) (map str (clojure.core/reverse "RACE c ar")))))); true
(= "113311" (apply str (map str/lower-case (remove #(re-seq #"\s" %) (map str (clojure.core/reverse [1 1 3 3 1 1])))))); true
(= ":foo:bar:foo" (apply str (map str/lower-case (remove #(re-seq #"\s" %) (map str (clojure.core/reverse [:foo :bar :foo])))))); true

(remove #(re-seq #"\s" %) (map str (clojure.core/reverse [:foo :bar :foo])))

;; my palindrome function(ignoring upper/lower-case & *whitespaces) follows :)
(defn palindrome? [subject]
  (let [post-subj (-> subject
                      ((partial apply str))
                      (clojure.string/replace #"\s+" "")
                      (clojure.string/lower-case))
        processed (->> subject
                       clojure.core/reverse
                       (map str)
                       (remove #(re-seq #"\s" %))
                       (map clojure.string/lower-case)
                       ((partial apply str))
                       )]

    (= post-subj processed)
    ))

;; testing::
(palindrome? "racecar")
(palindrome? "RACE c ar")
(palindrome? '(1 2 3 4))
(palindrome? '(1 1 3 3 1 1))
(palindrome? [:foo :bar :foo])
(palindrome? '(:a :b :c))

;; others responded with:
(true? (#(= (seq %) (clojure.core/reverse (seq %))) "racecar")) ;true
(true? (#(= (seq %) (clojure.core/reverse (seq %))) "RACE c ar"))
; But here: false, as opposed to my palindrome function which ignores: upper/lowercase & *whitespaces

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Create a function which flattens any sequence or nested container;
;; my version:
(defn flatten-coll
  [[head & tail]]
  (when-not (nil? head)
     (lazy-cat
        (if (sequential? head)
          (flatten-coll head)
          [head])
        (flatten-coll tail))))

(flatten-coll [1 [2 [3] 4] 5]); (1 2 3 4 5)
(sequential? 3); false
;; in detail: destructure the arguments into a collection: head & tail
;; then lazy-cat on top without loosing the results from previous recursion
;; do a check if/not sequential, if yes -> recurse into head, else get the head
;; THEN move FORWARD recursive into tail (rest args of container)

;; Note: look at mapcat...
;; Alternative from other clojurits would be:
(fn my-flatten
    [xs]
	(mapcat #(if (coll? %)
              (my-flatten %)
              (list %)) xs))

;; simply elegant :)

(doc mapcat); ([f & colls])
;; -> Returns the result of applying concat to the result of applying map
;; to f and one-level-nested-colls.  Thus function f should return a collection.
(mapcat #(if (coll? %) % (list %)) [[1 2 3] [4 5 6]]); (1 2 3 4 5 6)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Write a function which takes a string and returns a new string containing only the capital letters.
#(apply str (re-seq #"[A-Z]+" %))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Write a function which removes consecutive duplicates from a sequence.
;; "Leeeeeerrroyyy" -> "Leroy"
(defn discard-dups
  [subject]
    (loop [[head & tail] (vec subject)
            uniques []]
      (if (nil? head)
        uniques
        (if-not (= head (peek uniques))
          (recur tail (conj uniques head))
          (recur tail uniques)))))

(require 'clojure.string)
(clojure.string/split "Leeeeeerrroyyy" #"")
(vec "Leeeeeerrroyyy")
(seq [[1 3] [1 3]])
(discard-dups "Leeeeerooooyyyy")

(= (last [\A \B]) \B)
(peek [1 2 3])

;; another one of my versions, might be using the HOF [reduce]:
(reduce (fn [f s] (if (= s (peek f)) f (conj f s))) [] (vec "Leeeeerrrrooooy"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Write a function which packs consecutive duplicates into sub-lists.
;; [:a :a :b :b :c]) -> '((:a :a) (:b :b) (:c))
(partition-by identity [:a :a :b :b :c])


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Write a function which duplicates each element of a sequence.
;; [1 2 3]) -> [1 1 2 2 3 3]
(reduce (fn [f n] (conj (conj f n) n)) [] [1 2 3])

;; others solution
(mapcat #(vector % %) [1 2 3])
; (1 1 2 2 3 3)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Write a function which replicates each element of a sequence a variable number of times
;; [1 2 3] 2) -> '(1 1 2 2 3 3)
(#(mapcat (fn [each] (repeat %2 each)) %) [1 2 3] 2)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Write a function which creates a list of all integers in a given range
;; 1 4 -> '(1 2 3)
(
   (fn range-within [lower higher] (take (- higher lower) (iterate #(inc %) lower)))
-2 2); (-2 -1 0 1)
;; take handles negative numbers as well now

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Write a function which takes a variable number of parameters and returns
;; the maximum value
(reduce min [1 8 3 4])
(defn find-max
  [head & tail]
  (loop [biggest head
         remained tail]
    (cond
       (empty? remained)
          biggest
       :else
         (if (< biggest (first remained))
            (recur (first remained) (rest remained))
            (recur biggest (rest remained))))))

;; soon a master of recursivity :)
(find-max 1 8 3 4); 8

;; alin's solution is to use [sort] + [last] instead of the recursivity solution
((fn [& args]
  (last (sort args))) 1 8 3 4) ; -> 8

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Write a function which takes two sequences and returns the first item from each,
;; then the second item from each, then the third, etc.
;; (= (__ [1 2] [3 4 5 6]) '(1 3 2 4)) -> true; restrictions: [interleave]

(mapcat #(vector % %2) [1 2] [3 4 5 6])
;; (1 3 2 4)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Write a function which separates the items of a sequence by an arbitrary value.
;; (= (__ 0 [1 2 3]) [1 0 2 0 3]) -> true ;; restrictions: [interpose]

(doc interpose)
(->> [1 2 3]
    (interpose 0)); (1 0 2 0 3)

(defn interpose-in
  [sep coll]
  (loop [[head & tail] coll
          curr-coll []]
    (cond
       (empty? tail)
         (conj curr-coll head)
      :else
           (recur tail (conj curr-coll head sep)))))

(interpose-in 0 [1 2 3]); [1 0 2 0 3]

;; alin's solution is:
(fn [sep coll]
  (drop-last (mapcat #(vector % sep) coll)))

;; mapcat: take a collection coll, apply the function on each element -> returning a vector of tuples(val sep)
;; then concatenate, or flatten that vector of tuples together, and last -> drop the last element since it's
;; obviously the separator

;; another solution of mine would be:
((fn [sep coll]
   (drop-last (flatten (map #(vector % sep) coll)))
   ) 0 [1 2 3])
;; (1 0 2 0 3)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Write a function which drops every Nth item from a sequence.
;; (= (__ [:a :b :c :d :e :f] 2) [:a :c :e])
(defn drop-every
  [coll pos]
      (let [head (take (dec pos) coll)
            tail (drop pos coll)]
        (when-not (empty? head)
          (lazy-cat head (drop-every tail pos)))))

(drop-every [1 2 3 4 5] 2) ; (1 3 5)

;; inspired by [partition-all] func -> here's another version using HOFs solving the drop-every problem
(doc partition-all)
;; Returns a lazy sequence of lists like partition, but may include
;; partitions with fewer than n items at the end.
;;   (partition-all 2 [1 2 3 4 5]) -> ((1 2) (3 4) (5))
(defn drop-every-nth
  [coll pos]
  (mapcat #(take (dec pos) %) (partition-all pos coll)))

(drop-every-nth [1 2 3 4 5] 2) ; (1 3 5)

(lazy-cat [1 2 3] [4 5 6]); (1 2 3 4 5 6)
(take-while #(> % 0) [1 2 4 -1 3 -9]); (1 2 4)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Write a function which calculates factorials
;; (= (__ 5) 120) -> true
;; 1st solution using "mundane" recursion
(defn fact [x]
  (if (zero? x)
    1
    (* x (fact (dec x)))))

;; demo:
(fact 5); 120

;; 2nd solution using TCO -> and avoiding the stack limitations
(defn big-fact [x]
  (loop [decremented x
         fact-num 1N]
    (if (zero? decremented)
       fact-num
       (recur (dec decremented) (* fact-num decremented)))))

;; demo:
(big-fact 99999)

;; others solution might be: using [reduce] & [range] to get a sequence from 1 -> upper-bound
(#(range 1 (inc %)) 5) ; -> (1 2 3 4 5)
( (fn [x] (reduce * (#(range 1 (inc x))))) 5) ; 120

;; build the seq from 1..arg-num; [range] starts def from 0 -> so explicitly pass 1, while
;; [range] until-exclusive arg-num -> increase it by 1;
;; then pass this seq to the reduce by defering the multiplication func


;; ................. and we reached the MEDIUM dificulty level :)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Write a function which reverses the interleave process into x number of subsequences
;; (= (__ (range 10) 5) '((0 5) (1 6) (2 7) (3 8) (4 9)))
;; (= (__ [1 2 3 4 5 6] 2) '((1 3 5) (2 4 6))) -> true

(defn rev-interleave
  [coll pos]
    (letfn [(take-items [func xs]
            (loop [acc []
                   [head & tail] xs]
              (if (empty? head)
                acc
                (recur (conj acc (func head)) tail))))]
          (loop [new-coll []
                 curr-coll (partition-all pos coll)]
            (if ((comp empty? flatten) curr-coll)
              new-coll
              (recur (conj new-coll (take-items first curr-coll)) (take-items rest curr-coll))))))


;; testing:
(rev-interleave (range 10) 5) ; ([0 5] [1 6] [2 7] [3 8] [4 9])
(rev-interleave [1 2 3 4 5 6] 2) ; [[1 3 5] [2 4 6]]

;; work:
(doc interleave); returns a lazy seq of the first item in each coll, then the second...
(empty? (flatten [[]])); true
(empty? [[]]); false
(map #(vector %1 %2) [1 2 3] [4 5 6]); ([1 4] [2 5] [3 6])

;; other users solutions:
(fn reverse-interleave [xs n]
	(let [ys (partition-all n xs)]
		(partition-all (count ys) (apply interleave ys))))

;; or:
(fn [coll num-subseqs]
    (let [ps (partition num-subseqs coll)]
      (for [i (range num-subseqs)]
        (map #(nth % i) ps))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; here's another neat func -> allowing to remove only the poss positions from the coll
(defn remove-items
  [poss coll]
  (loop [rem coll
         [head & tail] poss]
    (if (nil? head)
      rem
      (recur (remove #(= head %) rem) tail))))

;; testing:
(remove-items [1 3] [1 2 3 4 5]) ; (2 4 5)

;; notes:
(remove #(= 0 %) (range 10)); (1 2 3 ... 9)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Write a function which can rotate a sequence in either direction
;; (= (__ 2 [1 2 3 4 5]) '(3 4 5 1 2))
;; (= (__ -2 [1 2 3 4 5]) '(4 5 1 2 3))
;; (= (__ 6 [1 2 3 4 5]) '(2 3 4 5 1))
;; (= (__ -4 '(:a :b :c)) '(:c :a :b))

(cycle (range 1 6))
(reduce (fn [acc each]
          (if (< (count acc) 6)
              (conj acc each)
            ))
        [] (take 6 (cycle (range 1 6)))); [1 2 3 4 5 1]

(defn cyclic-buffer
  [rotator coll]
  (let [smaller-coll-than-rot? (< (count coll) rotator)
        make-pos #(if (neg? %) (- %) %)
        gen-cyclic-coll #(lazy-cat (take-last (make-pos rotator) coll) coll)]
      (if smaller-coll-than-rot?
        (->> (take rotator (cycle coll))
             (drop (- rotator (count coll))))
        (if (neg? rotator)
            (if (> (make-pos rotator) (count coll))
              (take (count coll) (take-last (make-pos rotator) (gen-cyclic-coll)))
              (take (count coll) (gen-cyclic-coll)))
            (->> (take (+ rotator (count coll)) (cycle coll))
                 (drop rotator))))))

;; testing:
(cyclic-buffer 2 [1 2 3 4 5]) ; OK
(cyclic-buffer 6 [1 2 3 4 5]) ; OK
(cyclic-buffer -2 [1 2 3 4 5]) ; OK
(cyclic-buffer -4 '(:a :b :c)) ; OK (:c :a :b)

(drop-last 4 [1 2 3 4 5 6])
(take (count [1 2 3 4 5]) (lazy-cat (take-last 2 [1 2 3 4 5]) [1 2 3 4 5]))
(lazy-cat (take-last 2 [1 2 3 4 5]) [1 2 3 4 5])

(take (count [:a :b :c]) (take-last 4 (lazy-cat (take-last 4 [:a :b :c]) [:a :b :c]))) ; (:c :a :b) -> OK
(take (count [1 2 3 4 5]) (lazy-cat (take-last 2 [1 2 3 4 5]) [1 2 3 4 5]))


;;;;;;;;;;;;;;;;;;;;;;
;; Difficulty:	Medium
;; Topics:	higher-order-functions
;; Write a higher-order function which flips the order of the arguments of an input function.
;; (= [1 2 3] ((__ take) [1 2 3 4 5] 3))

;; impl: returns a closure which captures the "func"tion and the closure returned takes other 2 args
;; that will flip as position
(defn flip-args [func]
    (fn [head-arg tail-arg]
        (func tail-arg head-arg)))

;; in action:
(((fn flip [func] (fn[head-arg tail-arg] (func tail-arg head-arg))) take) [1 2 3 4 5] 3) ;(1 2 3)

;; other user solutions:
(fn [f]
    (fn [& args] (apply f (reverse args))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The contains? function checks if a KEY is present in a given collection.
;; This often leads beginner clojurians to use it incorrectly with numerically indexed collections like vectors and lists.
;; for which case, the KEY used to lookup via [contains?] returns thruthy if there's a value on that KEY numeric position.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(contains? #{:a :b 4} 4) ; true
(contains? [1 1 1 1 1] 4) ; true --> we say: WTF -> but at position 4 there's 1 -> hence contains? returns true


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The some function takes a predicate function and a collection:
;;      fn some [predicate coll] -> 1st logical true where (= true (predicate x))
;; It returns the first logical true value of (predicate x) where x is an item in the collection.
(#(some (fn [x] (< x 2)) [1 2 3 4])) ; true

;; we can use a collection which is also a function to simulate the predicate
(#(some #{4 7} [2 5 6 7])) ; 7
;; take #{4 7} hash-set and use it as a function to lookup by each item from coll -> returning
;; instead of predicate-true -> the value from the hash-set-func.
;; #{4 7} 2 -> nil; #{4 7} 5 -> nil; #{4 7} 6 -> nil; #{4 7} 7 -> 7

;; a pretty nice english phrase might look like:
(some #(when (even? %) %) [1 3 5 6 8]) ; 6
;; when first even? number is found -> then return it via the second "%"


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Write a function which will split a sequence into two parts.
;; (= (__ 3 [1 2 3 4 5 6])         [[1 2 3] [4 5 6]])
;; (= (__ 2 [[1 2] [3 4] [5 6]])   [[[1 2] [3 4]]  [[5 6]]])
(defn split-at-pos
  [pos coll]
    (letfn [(take-frst [] (take pos coll))
            (take-lst [] (drop pos coll))]
      [(take-frst) (take-lst)]))

(split-at-pos 3 [1 2 3 4 5 6]); [(1 2 3) (4 5 6)]
(split-at-pos 2 [[1 2] [3 4] [5 6]]); [([1 2] [3 4]) ([5 6])]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implement the quick-sort using rec & TCO:: bad algorithm!!!
(defn bubble-sort
  [coll]
  (letfn [(swap-items [[pivot & tail]]
            (cond
               (empty? tail)
                 [pivot]
               (> pivot (first tail))
                 (lazy-cat [(first tail)] (bubble-sort (conj (rest tail) pivot)))
            :else
               (lazy-cat [pivot] (bubble-sort tail))))]
      (loop [n ((comp dec dec count) coll)
             xs coll]
        (if (= 0 n) xs
          (recur (dec n) (swap-items xs))))
    ))

;; second version using stricly recursion: Note that this is the WORST CASE SCENARIO sorting-algorithm!!!
;; and was done by me...yeah...stupid right...
(defn bubble-sort
  [coll]
  (letfn [(swap-items [[pivot & tail]]
            (cond
               (empty? tail)
                 [pivot]
               (> pivot (first tail))
                 (lazy-cat [(first tail)] (bubble-sort (conj (rest tail) pivot)))
            :else
               (lazy-cat [pivot] (bubble-sort tail))))
          (apply-sort-recur [coll counter]
              (if (= counter (- (count coll) 2))
                  coll
                  (apply-sort-recur (swap-items coll) (inc counter))))]
    (apply-sort-recur coll 0)))


(bubble-sort [3 1 4 2 10 8 9 5]); (1 2 3 4 5 8 9 10)

(defn best-quick-sort
  [coll]
  (let [pivot (first coll)
        smaller? #(< % pivot)
        higher? #(> % pivot)]
  (when pivot
    (lazy-cat
       (best-quick-sort (filter smaller? coll))
       (list pivot)
       (best-quick-sort (filter higher? coll))))))

(best-quick-sort [31 2 1 212 3 4 9 8 6 7])
(doc complement)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Write a function which simulates the [comp] core-function-composition
;;
;; Difficulty: Medium
;; This function should be a HOF internally, that takes a seq of functions -> returning a
;; function which takes some args and uses the right-most function to call on those args
;; passing each result to the left-next function
;;
;; examples:
;; ( (comp-> str - +) [1 2 3]) -> (str (- (+ 1 2 3))) -> "-6"
;; (str (- ((partial +) 1 2 3))) -> "-6"

;; my version of [comp]::
(defn comp->
  [& comp-funcs]
    (let [rev-funcs (reverse comp-funcs)]
      (fn closure-recur [& args]
        (loop [[head-func & tail-funcs] rev-funcs
               result (apply head-func args)]
            (if (nil? head-func)
              result
              (recur tail-funcs (head-func result)))))))

((comp-> str - +) 1 2 3)

;; another option of implementing function composition [comp] is more concise:
(defn comp->
  [& comp-funcs]
  (let [rev-funcs (reverse comp-funcs)]
      (fn ret-closure [& closure-args]
        (let [[rightmost-func & leftmost-funcs] rev-funcs
              init-result (apply rightmost-func closure-args)]
            (reduce (fn [acc each-fn] (each-fn acc)) init-result leftmost-funcs)))))

;; demo:
((comp-> str - +) 1 2 3)

;; explanation:
;; take a seq of functions, and create a new local binding of those functions by reversing the functions
;; sequence -> so that ops start from left-> right. The "ret-closure" is the returned closure which
;; takes the actual operands. A local binding using seq-destructuring is used to take the 1st function
;; that is the initial right-most func and [apply] it on the sequence of args passed. With this init-result
;; using [reduce] and the "tail-funcs" remained and the initial accumulator -> reduce the result by
;; calling the rest funcs on that init-result.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
