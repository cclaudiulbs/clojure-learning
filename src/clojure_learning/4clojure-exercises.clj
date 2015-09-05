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
            (if (nil? tail-funcs)
              result
              (recur tail-funcs ((first tail-funcs) result)))))))

((comp-> str - +) 1 2 3)
((comp-> rest reverse) [1 2 3 4]) ; (3 2 1)

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

;; TRICK to learn:
;; Why on earth this application of [reverse] does NOT throw a compile exception?
((#(fn [& args] (apply reverse args))) [1 2 3]) ; (3 2 1)
;; while this throws:
(apply reverse [1 2 3]) ;; let me tell you why:
;; it's because the vector is converted as "& args" into another list having ONE element, hence inside looks like:
(apply reverse '([1 2 3])) ; (3 2 1)


;; The THREAD FIRST -> macro::
;; The [->] macro threads an expression x through a variable number of forms.
;; First, x is inserted as the second item in the first form, making a list of it if it is not a list already.
;; Then the first form is inserted as the second item in the second form, making a list of that form if necessary.
;; This process continues for all the forms. Using -> can sometimes make your code more readable.
(-> [1 2 3]
    (conj 4)) ; [1 2 3 4]

((comp peek vec) (sort (rest (reverse [2 5 4 1 3 6])))) ; 5
(-> [2 5 4 1 3 6] (reverse) (rest) (sort) ((comp peek vec))) ; 5


;; [recur]
;; Clojure only has 1 non-stack-consuming looping construct: recur.
;; Either a function or a loop can be used as the recursion point.
;; Either way, recur REBINDS the bindings of the recursion point to the values it is passed.
;; Recur MUST be called from the TAIL-POSITION, and calling it elsewhere will result in an error.


;; THREAD-LAST macro:
;; The [->>] macro threads an expression x through a variable number of forms.
;; First, x is inserted as the last item in the first form, making a list of it if it is not a list already.
;; Then the first form is inserted as the last item in the second form, making a list of that form if necessary.
;; This process continues for all the forms. Using ->> can sometimes make your code more readable.
(->> [1 2 3]
     (lazy-cat [0])) ; (0 1 2 3)

(reduce + [1 23])

;; taken this expression:
(= (__ (map inc (take 3 (drop 2 [2 5 4 1 3 6]))))
   (->> [2 5 4 1 3 6] (drop 2) (take 3) (map inc) (__))
   11)
;; fill the empty-space with a function:
(= (apply + (map inc (take 3 (drop 2 [2 5 4 1 3 6]))))
   (->> [2 5 4 1 3 6] (drop 2) (take 3) (map inc) (apply +) )
   11) ; true

(= ((partial apply +) (map inc (take 3 (drop 2 [2 5 4 1 3 6]))))
   (->> [2 5 4 1 3 6] (drop 2) (take 3) (map inc) ((partial apply +)) ) ;; notice the 2 parans' needed to invoke the partial
   11) ; true

;; or:
(= (reduce + (map inc (take 3 (drop 2 [2 5 4 1 3 6]))))
   (->> [2 5 4 1 3 6] (drop 2) (take 3) (map inc) (reduce +))
   11) ; true


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Write a function which, given a key and map, returns true iff the map contains
;; an entry with that key and its value is nil.
;; (true?  (__ :a {:a nil :b 2}))
;; (false? (__ :b {:a nil :b 2}))
(#(if (contains? %2 %) (nil? (get %2 %)) false) :c {:a nil :b 2})

;; or translated inot a more readable version:
(fn [k m]
  (if (contains? m k)
    (nil? (k m))
    false))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; the following [for] macro form:
(for [x (range 40) :when (= (rem x 4) 1)] x)
;; it can be translated to:
(take (/ 40 4) (iterate #(+ 4 %) 1))
;; or:
(range 1 40 4) ; (1 5 9 13 ... 37)


;; FALSY:
;; In Clojure, only nil and false represent the values of logical falsity in conditional tests - anything else is logical truth.


;; map and default values:
;; When retrieving values from a map, you can specify default values in case the key is not found:
;; However, what if you want the map itself to contain the default values? Write a function which takes a default value and a sequence of keys and constructs a map.
;; (= (__ 0 [:a :b :c]) {:a 0 :b 0 :c 0})
(
 #(reduce (fn [m k] (assoc m k %)) {} %2)  ;; the function
  0 [:a :b :c]) ; {:c 0 :b 0 :a 0}

(
 #(apply hash-map (interleave %2 (repeat (count %2) %))) ;; the function
  0 [:a :b :c]) ; {:c 0 :b 0 :a 0}

((fn default-map-vals
  [default-val coll]
  (apply hash-map (mapcat (fn [k v] [k v]) coll (repeat (count coll) default-val))))
  0 [:a :b :c])

(repeat 3 0) ; (0 0 0)

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Intro to destructuring
;; Let bindings and function parameter lists support destructuring.
;; (= [2 4] (let [[a b c d e f g] (range)] __))
(let [{:keys (a b c d)} (apply hash-map [:a 1 :b 2 :c 3 :d 4])] [b d])


;;;;;;;;;;;;;;;;;;;;;;;;
;; Reimplement [iterate]
;; Given a side-effect free function f and an initial value x write a function which returns an
;; infinite lazy sequence of x, (f x), (f (f x)), (f (f (f x))), etc.
;; (= (take 5 (__ #(* 2 %) 1)) [1 2 4 8 16])
;; (= (take 100 (__ inc 0)) (take 100 (range)))
(iterate #(+ 2 %) 0)
(map #(* % %) (range))

(reduce #(lazy-seq (conj % %2)) [] (range))
(for [x (range) y (drop 1 (range))] [x y])
(take 5 (foo #(* 2 %) 1))

(defn iterate-recur
  [func start]
    (lazy-cat (list start)
              (lazy-seq (iterate-recur func (func start)))))

(take 5 (iterate-recur #(* 2 %) 1))


(repeatedly (partial rand-int 50))

(defn iter-recur
  [func start]
  (lazy-seq
     (let [computed (func start)]
       (cons start (iter-recur func computed)))))

(take 5 (iter-recur #(* 2 %) 1)) ; (1 2 4 8 16)
;; the [lazy-seq] construction helps build an infinite sequence by prefixing the data-structure
;; that encloses. the [cons] function is used to push at the head of the lazy-seq built list
;; it goes recursively AND lazily till the indicated (take 5) upper bound, and
;; then the recursive call will start building the list:
;; (cons 1 (cons 2 (cons 4 (cons 8 lazy-seq-which-continues))))
;; 8 (and cons 4, 2, 1)
;; 4 8 (and cons 2, 1)
;; 2 4 8 (and cons 1)
;; 1 2 4 8...
;; apply the given func on the start operand + put start on top of the lazy-seq and
;; apply again the same func on the already computed result passing it to the recur-call


;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sequential Destructuring:
;; Sequential destructuring allows you to bind symbols to parts of sequential things
;; like (vectors, lists, seqs, etc.):
;;   (let [bindings* ] exprs*)

;; (= [1 2 [3 4 5] [1 2 3 4 5]] (let [[a b & c :as d] __] [a b c d]))
(apply vector (range 1 6))
(into [] [1 2 [3 4 5]])


;; Indexing Sequences:
;; Transform a sequence into a sequence of pairs containing the original elements along with their index.
;; (= (__ [:a :b :c])  [[:a 0] [:b 1] [:c 2]])
;; (= (__ [0 1 3])     '((0 0) (1 1) (3 2)))

;; two-liner:
( #(let [xs (range (count %))]
    ((comp (partial partition 2) interleave) % xs))
[:a :b :c] )
;; ((:a 0) (:b 1) (:c 2))

;; using func composition and [interleave]
(defn index-coll
  [coll]
  (let [xs (range (count coll))]
    ((comp (partial partition 2) interleave) coll xs)))

;; demo:
(index-coll [:a :b :c]) ; ((:a 0) (:b 1) (:c 2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; or the map like version <- most succint :)
(defn index-coll
  [coll]
  (let [idxs ((comp range count) coll)]
    (map #(vector % %2) coll idxs)))

;; demo:
(index-coll [:a :b :c]) ; ([:a 0] [:b 1] [:c 2])

;; or the low-level TCO recursive version:
(defn index-coll
  [coll]
  (loop [[head & tail] coll
         newxs []
         idx (count newxs)]
    (if (nil? head)
      newxs
      (recur tail (conj newxs [head idx]) (inc idx)))))

;; demo:
(index-coll [:a :b :c]) ; [[:a 0] [:b 1] [:c 2]]

;; or without TCO optimization, but with "overloading" func
;; -> going recur till end, & pushing on top of the head-list, while the stack decreases
(defn index-coll
  ([coll] (index-coll coll 0))
  ([[head & tail] idx]
     (if head
       (conj (index-coll tail (inc idx)) [head idx])
       (list))))

;; demo:
(index-coll [:a :b :c]) ; ([:a 0] [:b 1] [:c 2])


(reverse nil) ; ()


;;;;;;;;;;;;;;;;;;;
;; Map Construction
;; Write a function which takes a vector of keys and a vector of values and constructs a map from them.
;; (= (__ [:a :b :c] [1 2 3]) {:a 1, :b 2, :c 3})
(#(apply hash-map (interleave % %2)) [:a :b :c] [1 2 3]) ; {:c 3, :b 2, :a 1}

;;;;;;;;;;;;;;;;;;;
;; and the more intuitive version(same but written more nicely):
(defn zipmapp
  [xs ys]
  (apply hash-map (interleave xs ys)))

;; demo:
(zipmapp [:a :b :c] [1 2 3]) ; {:c 3, :b 2, :a 1}

;;;;;;;;;;;;;;;;;;
;; or mapping into as another version:
(defn zipmapp
  [xs ys]
  (into {} (map #(vector % %2) xs ys)))


;;;;;;;;;;;;;;;;;;
;; or put the thread-last-english macro in action on the returned flattened + mapped cols
;; flatten because there's NO way to: apply hash-map ([] []), to work, since there should
;; all be flatten and must yield in pairs(<- even number of entire collection)
;; for pairs the: "into {}" works like a charm
(defn zipmapp
  [xs ys]
  (->> (map #(vector % %2) xs ys)
       flatten
       (apply hash-map)))

;; demo:
(zipmapp [:a :b :c] [1 2 3]) ; {:c 3, :b 2, :a 1}


;;;;;;;;;;;;;;;;;;;
;; find the sum of all numbers from 0->1000 divisible with 3 and 5, and which are not duplicated
(defn sum-divisibles
  [x y bound]
  (letfn [(module-nums [x] (filter #(= 0 (mod % x)) (range (inc bound))) )]
    (apply + (apply hash-set (concat (module-nums x) (module-nums y))))))

;; tip: (inc bound) is used to take the last inclusive

;; demo:
(sum-divisibles 3 5 12)

(apply hash-set (concat [1 2] [1 3])) ; #{1 3 2}
(range 2) ; (0 1)

;; or another version:: using [iterate] and thread-last macro
(defn sum-divisibles
  [x y bound]
  (letfn [(build-divisibles [n] (take (int (/ bound n)) (iterate #(+ % n) n)))]
    (->> (build-divisibles x)
         (concat (build-divisibles y))
         (apply hash-set)
         (apply +))))

;; demo:
(sum-divisibles 3 5 12)

(take 3 (iterate #(+ 3 %) 3)) ; (3 6 9)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; For any orderable data type it's possible to derive all of the
;; basic comparison operations (<, ≤, =, ≠, ≥, and >) from a single operation
;; (any operator but = or ≠ will work).
;; Write a function that takes three arguments, a less than operator for the
;; data and two items to compare.
;; The function should return a keyword describing the relationship between the two items.
;; The keywords for the relationship between x and y are as follows:
;; x = y → :eq
;; x > y → :gt
;; x < y → :lt
;; (= :gt (__ < 5 1))
;; (= :eq (__ (fn [x y] (< (count x) (count y))) "pear" "plum"))
(defn comparison
  [lower-fn & ops]
 (let [lower-compl-map {[true false] :lt
                        [false true] :gt
                        [false false] :eq}
       lower? (apply lower-fn ops)
       higher? (apply lower-fn (reverse ops))]
     (lower-compl-map [lower? higher?])))

;; demo:
(= :gt (comparison < 5 1)) ; true
(comparison < 1 4) ; :lt
(comparison < 5 5) ; :eq

((complement <) 3 3) ; true --> WTF???

;; or the more idiomatic version
(defn comparison
  [lower-fn & ops]
 (let [lower-compl-map {[true false] :lt
                        [false true] :gt
                        [false false] :eq}]
     (lower-compl-map (map #(lower-fn (first %) (second %)) (list ops (reverse ops))))))



;; or the more idiomatic version
(defn comparison
  [lower-fn & ops]
 (let [lower-compl-map {[true false] :lt
                        [false true] :gt
                        [false false] :eq}]
     (lower-compl-map (map #(lower-fn (first %) (second %)) (list ops (reverse ops))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Write a function which takes a variable number of booleans.
;; Your function should return true if some of the parameters are true,
;; but not all of the parameters are true. Otherwise your function should return false
;; (= false (__ false false))
;; (= false (__ true))
;; (= true (__ true false))
(defn find-half-true
  [& bools]
  (let [truthies (filter true? bools)
        falsies (filter false? bools)]
    (and (not (empty? truthies)) (not (empty? falsies)))))

;; demo:
(= false (find-half-true false false))
(= false (find-half-true true))
(= true (find-half-true true false))

(doc remove)
(doc true?)
(doc if-let)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; create a function which given some numbers will get the greatest common divisor.
;; recall that gcd is the common greatest number to which all others are divisible.
;; my-logic:
;; -> get the min
;; -> build a list of its divisible nums including itself
;; -> build a recursive func which takes this list + the rest of the list given as args
;; -> check that each head from args is divisible with the LAST most higher from the divisible-nums
;;    of the first one. if yes
;; -> call recur to next tail-arg; if no -> call recur popping the
;; last-most-higher num from the smallest-divisible nums with itself

;; first variant:: using HOFs instead of low-level recursion :)
(defn gcd
  [& nums]
  (let [smallest (reduce min nums)
        divisible? #(zero? (mod % %2))
        smallest-divisibles (filter (partial divisible? smallest) (range 1 (inc smallest)))]
     (letfn [(find-gcds
                 [possible-divisibles xs]
                   (reduce (fn rec-find [sm-div each]
                              (if (divisible? each (last sm-div))
                                    sm-div
                                   (recur (butlast sm-div) each)))
                    possible-divisibles xs))]
       (last (find-gcds smallest-divisibles nums)))))

;; demo:
(gcd 3 5 9) ;1
(gcd 8 12) ;4
(gcd 1023 858) ; 33

;; tries:
(seq [1]) ;(1)
(last [1 2]) ;2
(last (list 1 2)) ;2

(defn gcd
  [& nums]
  (let [divisible? #(zero? (mod % %2))
        min-num ((comp first sort) nums)
        min-num-divisibles (filter (partial divisible? min-num) (take min-num (iterate (partial inc) 1)))]
      (letfn [(find-gcd-rec
                 [min-nums init-nums]
                  (if-let [tail-nums (seq init-nums)]
                    (if (divisible? (first tail-nums) (last min-nums))
                        (find-gcd-rec min-nums (rest tail-nums))
                        (find-gcd-rec (butlast min-nums) tail-nums))
                    (last min-nums)))]
    ;; invoke recursive func
    (find-gcd-rec min-num-divisibles nums))))

;; demo:
(gcd 3 5 9) ;1
(gcd 8 12) ;4
(gcd 1023 858) ; 33


;; tries:
(sort [4 2 5 1 3]) ; (1 2 3 4 5)

;; one way of writing core [iterate fn foo]
(iterate #(+ 1 %) 1) ; (1 2 3 4 ..) -> f (f (f x))...

;; another way of writing core [iterate] without the literal func, but using partial
;; that returns the curried back having the curried take one argument.
(iterate (partial inc) 1) ; (1 2 3 4 ...)

;; alin's solution to this:
(fn gcd [a b]
	(if (< a b)
		(recur b a)
		(first (filter #(= 0 (rem a %) (rem b %)) (iterate dec b)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Difficulty:	Easy
;; Topics:	higher-order-functions math
;; Lexical scope and first-class functions are two of the most basic building blocks of a
;; functional language like Clojure. When you combine the two together,
;; you get something very powerful called lexical closures.
;; With these, you can exercise a great deal of control over the lifetime of your local bindings,
;; saving their values for use later, long after the code you're running now has finished.
;; write a function which returns a lexical-closure back and which computes x pow n.
(defn pow
  [power]
    (fn [num] (reduce * (repeat power num))))

((pow 2) 16)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Write a function which multiplies two numbers and returns the result as a sequence of its digits.

(require 'clojure.string)
(defn mult-and-seq
  [& nums]
  (map #(. java.lang.Integer valueOf %)
       (-> (apply * nums)
            str
           (clojure.string/split #""))))

;; FOR SOME REASONS, THIS FUNCTION THROWS a number-format-exception on 4clojure...
;; however on my station the results are compiled successfully:
(mult-and-seq 20 3) ; (6 0)
(mult-and-seq 1 1) ; (1)
(mult-and-seq 99 9) ; 8 9 1


;; work:
(< 1/5 1) ; true
(apply * '(1 2 3)) ; 6
(. java.lang.Integer valueOf "2") ; 2


;; so, here's a function that passes the tests...
(defn mult-and-seq
  [& nums]
  (map #(- (int %) 48) ((comp vec str) (apply * nums))))

;; here's the refactoring of this func:
(defn mult-and-seq
  [& nums]
  (map #(- (int %) 48) (str (apply * nums))))

;; we dropped the composition of [vec] + [str] functions, since the sequence abstraction
;; [map] function knows how to iterate over a string-sequence, internally it does the same
;; it calls: [seq] on the string...

;; alin's solution would be using recursive funcs:
(defn mult-and-seq
  ([x y]
     (mult-and-seq (apply * (list x y))))
  ([product]
     (if (zero? (int (/ product 10)))   ;; / 6 10 -> 0
       [(mod product 10)]               ;; mod 6 10 -> 6
       (conj (mult-and-seq (int (/ product 10)))  (mod product 10))))) ;; / 30 10 -> 3

(mod 30 10) ; 0


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Easy
;; Group a Sequence:: Special Restrictions: [group-by]
;; Given a function f and a sequence s, write a function which returns a map.
;; The keys should be the values OF f applied to each item in s.
;; The value at each key should be a vector of corresponding items in the order they appear in s.
;; In fact we're implementing the [group-by] function
;; (= (__ #(> % 5) [1 3 6 8]) {false [1 3], true [6 8]})

;; (= (__ count [[1] [1 2] [3] [1 2 3] [2 3]])
;;    {1 [[1] [3]],  2 [[1 2] [2 3]],  3 [[1 2 3]]})

(defn group-by-fun
  [func coll]
  (reduce
     (fn [m k]
       (if (contains? m (func k))       ;; contains: true | false applied func key
         (assoc m (func k) (conj (get m (func k)) k))
         (assoc m (func k) [k]) ))
  {} coll))

;; demo::
(group-by-fun #(> % 5) [1 3 6 8]) ;; {false [1 3], true [6 8]})

(contains? {:true [1 2]} :true) ; true
(doc get) ;; get map key -> value

;; and using the recursion implementation:
(defn group-by-fun
  [func coll]
  (letfn [(add-to-map
             [m k v]
           (if (contains? m k)
              (assoc m k (conj (get m k) v))
              (assoc m k [v])))]
  (loop [acc {}
        [head & tail] coll]
    (if (nil? head)
      acc
      (recur (add-to-map acc (func head) head) tail)))))

(group-by-fun #(> % 5) [1 3 6 8]) ;; {false [1 3], true [6 8]})

;; both functions can be refactored to drop the [if] conditional by using the default
;; if a key within the map is not found:
(defn group-by-fun
  [func coll]
  (reduce
     (fn [m k]
         (assoc m (func k) (conj (get m (func k) []) k)))
  {} coll))

;; [conj] puts on top of the stack an element k, while "get m k []" is saying
;; get value corresponding to the key k from the map, and if not found -> get me an empty vector.
;; where the [conj] will push

;; while the recursive function looks more simplified:
(defn group-by-fun
  [func coll]
  (letfn [(add-to-map
             [m k v]
              (assoc m k (conj (m k []) v)))]
  (loop [acc {}
        [head & tail] coll]
    (if (nil? head)
      acc
      (recur (add-to-map acc (func head) head) tail)))))

;; small refactor and the improvement is seen. the [get] core-function is a litlle verbose
;; but as we know we can use: m k -> to get the corresponding value, idiomatic clojurist way :)
;; yes the add-to-map func is just for readability -> clean code ;)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Easy
;; Reimplement [map]
;; Map is one of the core elements of a functional programming language.
;; Given a function f and an input sequence s,
;; RETURN A LAZY SEQUENCE of (f x) for each element x in s.
(defn mmap
  [func coll]
  (lazy-seq
    (when-let [head (first coll)]
      (cons (func head) (mmap func (rest coll)) ))))

;; neat :)
(cons 2 (cons 3 (cons 4 (lazy-seq nil)))) ; (2 3 4)
(lazy-seq nil) ; ()

;; Note1: recursion with lists: go till end & start from the end -> use [cons]
;; to build the list in order recursively.
;; While with vectors -> start recursively from beginning and push on top of the stack.
;; Note2: any lazy-sequences in clojure, must be prepended with lazy-seq

;; demo:
(mmap inc [1 2 3]) ; (2 3 4)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Infix HOF
;; Write a HOF that takes inline math operations, one level nested and produces a result
;; (infix 2 + 3 / 5) -> 1
(defn infix
  [& ops]
  (loop [[x op y & tail] ops]
    (if (nil? tail)
      (op x y)
      (recur (conj tail (op x y))))))

;; demo:
(infix 2 + 3)
(infix 2 + 3 / 5)

;; Note on impl:
;; constant scalling -> the in-list of infix-operations is reduced by 3, on each recursive iteration
;; if only 3 ops -> give me the result
;; else -> using the same stack(TCO) produce the result of each pair x op y and conj it on
;; top of the list, until there's no more tail. each computed result is used to calculate the
;; next result
(conj '(1 2 3) 0) ; (0 1 2 3)

;; using [reduce]::
(defn infix
  [head & tail]
  (reduce
     (fn [acc each]
       ((first each) acc (second each)))
     head
     (partition-all 2 tail)))

(infix 1 + 3 / 4 + 1)
;; [partition-all] yields: (+ 3) (/ 4) (+ 1)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Easy:
;; Convert a binary number, provided in the form of a string, to its numerical value.
;; (= 7     (__ "111"))
;; (= 8     (__ "1000"))
;; (= 65535 (__ "1111111111111111"))

;; we need the pow func first: 1st version using HOFs
(defn pow [x n] (reduce * 1 (repeat n x)))

;; 2nd version using low-level recursion
(defn pow
  ([x n] (pow x x (dec n)))
  ([initial acc n] (if (zero? n) acc (pow initial (* initial acc) (dec n)))))

(pow 2 10) ; 8

;; here's the func:
(reduce + (map (fn [p x] (if (= x \1) (pow 2 p) 0)) (range 0 3) (vec "111")))

(defn binary->int
  [stream]
  (letfn [(pow [x n] (reduce * 1 (repeat n x)))]
    (apply +
      (map (fn [b p]
             (if (= b \1)
               (pow 2 p)
               0))
           (vec stream) (reverse (range 0 (count stream)))))))

(binary->int "111") ;7
(binary->int "1000") ;8

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sum of square of digits
;; Write a function which takes a collection of integers as an argument.
;; Return the count of how many elements are smaller than the sum of their squared component digits.
;; For example: 10 is larger than 1 squared plus 0 squared;
;; whereas 15 is smaller than 1 squared plus 5 squared.
;; (= 8 (__ (range 10)))
;; (= 50 (__ (range 1000)))

;; 1st thing first: build the split num func -> able to calculate the sum of square digits
(defn split-num-by-digits [x]
  (if (< x 10) (list x)
  (map (fn [each] (- (int each) (int \0))) ((comp vec str) x))))

;; the pow function we already have -> anyway for squared we don't need it, since we're using the num itself
(split-num-by-digits 123) ; (1 2 3)
(split-num-by-digits 9) ; (9)

(defn smaller-than-sum-of-squared-digs
  [coll]
  (letfn [(split-num-by-digits [x]
              (if (< x 10)
                (list x)
                (map (fn [v] (- (int v) (int \0))) ((comp vec str) x))))
          (sum-of-squared [x]
              (reduce + (map #(* % %) (split-num-by-digits x))))]
      (count (filter #(< % (sum-of-squared %)) coll))))

;; demo:
(smaller-than-sum-of-squared-digs (range 10)) ;8
(smaller-than-sum-of-squared-digs (range 100)) ;50
(smaller-than-sum-of-squared-digs (range 1000)) ;50


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; To Tree, or not to Tree
;; Trees: easy
;; Write a predicate which checks whether or not a given sequence represents a binary tree.
;; Each node in the tree must have a value, a left child, and a right child.
;; in short: binary trees are the trees which have NO MORE than 3 nodes:
;; -> 1 value, 1 left-branch, 1 right-branch
(defn binary-tree?
  [xs]
    (let [valid-node? #(= 3 (count %))
          node? #(and (or (seq? %) (vector? %)))]
    (->> xs
       (reduce (fn [acc composite]
              (if (node? composite)
                (conj acc ((comp not empty?) composite) (binary-tree? composite))
                (conj acc (and (valid-node? xs) composite))))  [])
       flatten
       (filter false?)
       empty?)))


;; demo:
(binary-tree? '(:a (:b nil nil) nil))                  ; true
(binary-tree? [1 [2 [3 [4 false nil] nil] nil] nil])   ; false
(binary-tree? '(:a nil ()))                            ; false

;; api-learning:
(doc seq?) ; -> returns true if x implements ISeq
(map sequential? [[] '()])  ; (true true)
(class ()) ; EmptyList
(seq? ()) ; true
(if-let [c (seq ())] c) ; nil

;; the recursive version follows...interesting approach dude :)
(defn binary-tree? [xs]
  (letfn [(bin-node? [node]
           (and (sequential? node) (= 3 (count (remove false? node)))))
          (nested-nodes? [node]
           ((comp not empty?) (filter sequential? node)))]
  (if (bin-node? xs)
    (if (nested-nodes? xs)
      (if (sequential? (second xs))
        (binary-tree? (second xs))
        (binary-tree? (last xs)))                      ; handle right branch as well
      true)
    false)))


(binary-tree? '(:a (:b nil nil) nil))                  ; true
(binary-tree? [1 [2 [3 [4 false nil] nil] nil] nil])   ; false
(binary-tree? [1 [2 nil nil] [3 nil nil] [4 nil nil]]) ; false
(binary-tree? [ 1 nil  [2 [3 nil nil] [4 nil nil]]  ]) ; true -> handle right-branch as well
(binary-tree? '(:a nil ()))                            ; false

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Difficulty:	Medium
;; Topics:	strings
;; Create a function which takes lower-case and converts to java lowerCase camelCase
(defn camel->case
  [that]
  (let [[head & tail] (clojure.string/split that #"-")]
    (apply str
      (reduce
         (fn [xs x]
            (conj xs (apply str (conj (rest x) (clojure.string/upper-case (first x))))))
       [head] tail))))

(camel->case "lower-case") ; "lowerCase"


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Difficulty: Medium
;; Write a function that splits a sentence up into a sorted list of words.
;; Capitalization should not affect sort order and punctuation should be ignored.
;; (= (__  "Have a nice day.") ["a" "day" "Have" "nice"])
(defn sort-words
  [xs]
  (-> xs
      (clojure.string/replace #"[^\s|\w]" "")
      (clojure.string/split #"\s")
      ((partial sort-by (comp clojure.string/lower-case)))))

(sort-words "Have a nice day.")
(doc sort-by)
(sort-by (comp str first) ["have" "a" "nice"])


;; brean teaser:
(class (class (class []))) ; (class Class) java.lang.Class

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Difficulty: Easy? -> really i don't see this quite easy ;) as it's implying an algorithm...
;; Pascal's triangle:
;; - The first row is 1.
;; - Each successive row is computed by adding together adjacent numbers in the row above,
;;   and adding a 1 to the beginning and end of the row.

(defn pascal-triangle [x]
   (letfn [(make-row-until [x] (->> (repeat 1) (take x) vec))
           (sum-adiacents [xs]
              (loop [acc []
                    [head & tail] xs]
                (if (nil? tail) acc
                (recur (conj acc (+ head (first tail))) tail))))]

     (loop [acc (make-row-until 2)]
       (if (= x (count acc))
         acc
         (if (< x 3)
           (make-row-until x)
           (recur ((comp vec flatten) [1 (sum-adiacents acc) 1])))))))

;; demo:
(map pascal-triangle (range 1 5))
;;      ([1]
;;      [1 1]
;;     [1 2 1]
;;    [1 3 3 1])

;; helper func
(defn sum-adiacents
  [xs]
  (loop [acc []
        [head & tail] xs]
    (if (nil? tail)
      acc
      (recur (conj acc (+ head (first tail))) tail))))

;; nil? tail NOT head -> because we're accessing the tail using [first] function; :else NPE
(sum-adiacents [1 3 3 1]) ; [4 6 4] -> OK :)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; the dot product
;; The dot product of two vectors A = [A1, A2, ..., An] and B = [B1, B2, ..., Bn] is defined as:[1]
;;     [A1 A2...] dot [B1 B2...] = sum{i=1}^n AiBi = A1B1 + A2B2 + ...
(defn dot-product
  [xs ys]
  (reduce + (map #(* % %2) xs ys)))

(dot-product [2 5 6] [100 10 1]) ; 256

(defn dot-product
  [xs ys]
  (reduce
     (fn [acc tuple] (+ (apply * tuple) acc))
     0 (partition 2 (interleave xs ys))))

(defn dot-product
  ([xs ys] (apply + (dot-product xs ys [])))
  ([[head1 & tail1] [head2 & tail2] acc]
  (if (or (nil? head1) (nil? head2))
      acc
      (dot-product tail1 tail2 (conj acc (* head1 head2))))))

(dot-product [2 5 6] [100 10 1]) ; 256
;; the second function is used to accumulate the results of the first multiplication op
;; while the invoker function is wrapped by the [apply] which applies the + func to the
;; returned accumulator.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Count Occurrences
;;  Difficulty:	Medium
;; Topics:	seqs core-functions
;; Restrictions: [frequencies]
;; (= (__ [1 1 2 3 2 1 1]) {1 4, 2 2, 3 1})
(defn frequencies-c
  [xs]
  (reduce
     (fn [m k]
       (if-let [[k v] (find m k)]
         (assoc m k (inc v))
         (assoc m k 1)))
     {} xs))

(frequencies-c [1 1 2 3 2 1 1])

(find {1 2, 3 2} 1) ; [1 2]
(get {1 2, 3 2} 1)  ; 2
(assoc {1 2} 3 2)   ; {3 2, 1 2}
(find {} 1) ; nil

;; an interesting other user solution is:
(defn count-occurs [xs]
  (reduce
    (fn [m k]
      (update-in m [k]
               (fn [x]
                 (if
                   (nil? x) 1
                   (inc x)))))
    {} xs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Parantheses Again
;; Difficulty:	Medium
;;
;; Topics:	math combinatorics
;; It is not a difficult exercise to find all the combinations of well-formed parentheses
;; if we only have N pairs to work with. For instance, if we only have 2 pairs,
;; we only have two possible combinations: "()()" and "(())".
;; Any other combination of length 4 is ill-formed.
;; (= #{"((()))" "()()()" "()(())" "(())()" "(()())"} (__ 3))
(require 'clojure.string)
(defn generate-parans
  ([x]
   (letfn [(to-binary
              [x]
              (Integer/toBinaryString x))
           (left-padding
              [bin-num]
              (apply str (conj (repeat (inc (count bin-num)) 0) bin-num)))
           (pow [x n] (reduce #(* % %2) (repeat n x)))]

  (let [raw-mapped-bins (map #(left-padding (to-binary %)) (range (pow 2 x)))
        mapped-bins raw-mapped-bins]
    mapped-bins ))))

;; WIP: not ready
(generate-parans 3)
(class (Integer. 2))
(Integer/toBinaryString 3)

;; here's the working solution:
(defn gen-parentheses [x]
  (letfn [(pow [x n]
              (reduce * (repeat n x)))
          (to-binary [x]
              (Integer/toBinaryString x))
          (left-padding [bin-num]
             (apply str (conj (vec (repeat (- x (count bin-num)) 0) ) bin-num)))
          (binary-nums-padded [exp]
              (if (zero? exp) nil
                (map (comp left-padding to-binary) (range 0 (pow 2 exp)))))
          (nest-op [acc]
              (list acc ))
          (nest-op? [each-exp]
              (= 1 (Integer. (str each-exp))))
          (append-right [acc]
              (conj acc ()))
          (gen-parens-recur
             [binary-seq]
             (loop [[head & tail] binary-seq
                     acc ()]
                 (if (nil? head)
                    acc
                   (if (nest-op? head)
                     (recur tail (nest-op acc))
                     (recur tail (append-right acc))))))
           (drop-leading-trailing-parens
              [grouping-pars]
              (apply str (butlast (rest grouping-pars))))]

    (let [r-appended-gen-parens (map gen-parens-recur (binary-nums-padded x))
          l-appended-gen-parens (map reverse r-appended-gen-parens)]
      (apply hash-set
        (map
           (comp drop-leading-trailing-parens str)
           (concat l-appended-gen-parens r-appended-gen-parens))))))

;; demo:
(gen-parentheses 2) ; #{"() ()" "(())"}
(gen-parentheses 3) ; #{"((()))" "() (())" "(()) ()" "() () ()" "(() ())"}

(gen-parentheses 4) ;

(count (gen-parentheses 10))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; notes: each r-l gen parens generate a similar structure:
;; (
;;    (() () ()), ((() ())), ((()) ()), (((()))), (() () ()), ((() ())), ((()) ()), (((())))
;; )

;; the solution is to:
;;    build function to drop the left-most + right-most parentheses
(apply str (butlast (rest "(() () ())")))

;; now define a nice function:
(defn drop-leading-pars
  [grouping-pars]
  (apply str (butlast (rest grouping-pars))))

;; demo:
(drop-leading-pars "(() () ())") ; "() () ()"

(apply str (map str (reverse "cclaudiu"))) ; "uidualcc"

;; the current solution uses two operations: adding + nesting the grouping parentheses, that's why
;; we can group them as a binary operation, and using the custom "pow" function we generate all
;; the possible aggregations. having duplicates we now discard them using the hash-set features.
;; The thing is that we have a left-add + right-add is because to handle the reverse situation
;; in which case the acc is reversed by the adder.

;; binary-nums-padded:
;; "000"
;; "001"
;; "010"
;; "100"
;; "110"
;; "011"
;; "101"
;; "111"

;; work:
(defn append-op [each-exp]
    (format "%1s()" (clojure.string/replace each-exp #"0|1" "()")))
(append-op \0);  ()()

(let [[head & tail] "010"] [head tail]) ; [\0 (\1 \0)]
(let [[head & tail] "010"] (clojure.string/replace head "0" "()")) ; "()"

(map #(Integer. (str %)) (vec "123")) ; (1 2 3)

(defn wrap-op [str-token]
    (format "(%1s)" (clojure.string/replace str-token #"0|1" "()")))
(wrap-op (str \0)) ; (())
(clojure.string/replace "0" #"0|1" "()") ; "()"


;; my version of generate-parentheses was a litlle bit simpler :) in that
;; i start with the repeated sets of l-parens + r-parens, and combine them
;; by a "step"; "step" here is to say: combine ( or (( or ((( with the reflexive version of right-hand parens
;; we will take an iterative progress of implementing this neat function :)
;; WIP:
(defn gen-parens
  ([x]
    (let [l-parens (vec (repeat x "("))
          r-parens (vec (repeat x ")"))]
       (apply str (gen-parens 1 l-parens r-parens []))))

  ([step l-parens r-parens acc]
      (if (nil? (first l-parens))
            acc
            (if (= 1 (count l-parens))
              (conj acc (first l-parens) (first r-parens))
              (concat (gen-parens step (subvec l-parens step) (subvec r-parens step) acc)
                      (take step l-parens) (take step r-parens))))))

;; for instance this function taking a step of 1, and 2(as 2 pairs) produces:
(gen-parens 2) ;;   "()()"

;; if we modify the step to 2, it will produce a set of nested parentheses:
(gen-parens 2) ;;   "(())"

;; NOTE:
;; for a step higher than the number x provided to combine the parens, it throws an IndexOutOfBoundsException
;; normally

;; ofcourse we need all the possible valid combinations, so we should [map] through all the possibles
;; "step"s from 1..total of repeated set of parens
;; WIP still:
(defn gen-parens
  ([x]
    (let [l-parens (vec (repeat x "("))
          r-parens (vec (repeat x ")"))]
       (map
          (fn [each-step] (gen-parens each-step l-parens r-parens []))
          (range 1 (inc x)))))

  ([step l-parens r-parens acc]
      (if (nil? (first l-parens))
            acc
            (if (zero? (dec (count l-parens)))
              (conj acc (first l-parens) (first r-parens))
              (concat (gen-parens step (subvec l-parens step) (subvec r-parens step) acc)
                      (take step l-parens) (take step r-parens))))))

;; the change, here, is mapping through all the steps starting from 1 to (inc x) to include total,
;; because [range] excludes the last by default;
;; the function produces:
(gen-parens 2) ;;   ( "(" ")" "(" ")" ), ( "(" "(" ")" ")" )

;; we also need to reverse the possible combinations, because for 3 the function produces:
(gen-parens 3) ;;
;; one-("(" ")" "(" ")" "(" ")"), two-("(" ")" "(" "(" ")" ")"),  last-("(" "(" "(" ")" ")" ")")

;; as we see the second need to produce also the reversed version -> final version:

;;;;;;;;;;;;;;;;;;;;;;
;; Split by Type
;; Difficulty:	Medium
;; Topics:	seqs
;; Write a function which takes a sequence consisting of items with different types and splits them up
;; into a set of homogeneous sub-sequences. The internal order of each sub-sequence should be maintained,
;; but the sub-sequences themselves can be returned in any order (this is why 'set' is used in the test cases).
;; (= (set (__ [1 :a 2 :b 3 :c])) #{[1 2 3] [:a :b :c]})
(sort-by #() [1 :a 2 :b 3 :c])
(reduce
   (fn [xs-map each]
     (if-let [m-vals (get xs-map (class each))]
       (assoc xs-map (class each) (conj m-vals each))
       (assoc xs-map (class each) (conj [] each)))) {} [1 :a 2 :b 3 :c])
;; --> {clojure.lang.Keyword [:a :b :c] [1 2 3]}

;; make a nice function:
(defn split-by-type
  [xs]
  (letfn [(map-marshall [xs]
             (reduce
               (fn [xs-map x]
                 (if-let [m-vals (get xs-map (class x))]
                   (assoc xs-map (class x) (conj m-vals x))
                   (assoc xs-map (class x) (conj [] x)))) {} xs))]
    (apply hash-set (vals (map-marshall xs)))))

;; demo:
(split-by-type [1 :a 2 :b 3 :c]) ;; #{[:a :b :c] [1 2 3]}

;; another neat solution is:
(#(vals (group-by type %)) [1 :a 2 :b 3 :c]) ([1 2 3] [:a :b :c])


;; work:
(vals {:a [1 2 3] :b [4 5 6]}) ;; ([1 2 3] [4 5 6]) -> a list of vals
(get {(class 1) 2} (class 1)) ;; 2


;;;;;;;;;;;;;;;;;;;;;;
;; Find Distinct Items
;; Difficulty:	Medium
;; Topics:	seqs core-functions
;; Special Restrictions: [clojure.core/distinct]
;; Write a function which removes the duplicates from a sequence. Order of the items must be maintained.
;; (= (__ [1 2 1 3 1 2 4]) [1 2 3 4])

;; using recursion:
(defn find-distinct
  ([xs] (find-distinct (reverse xs) [])) ;; reverse for going recur's till end -> build from end
  ([[head & tail] uniques]
     (if (nil? head)
        uniques
        (if ((comp not empty?) (filter #(= head %) tail))
          (recur tail uniques)
          (conj (find-distinct tail uniques) head)))))

;; demo:
(find-distinct '([2 4] [1 2] [1 3] [1 3])) ; [[2 4] [1 2] [1 3]]
(find-distinct [1 2 1 3 1 2 4]) ; [1 2 3 4]

;; using HOFs:
(defn find-distinct [xs]
  (reduce
     (fn [uniques x]
       (if (empty? (filter (partial = x) uniques))
         (conj uniques x)
         uniques))
     [] xs))


(compare [1 2] [1 2])     ;; 0
(identical? [1 2] [1 2])  ;; false
(identical? 2 2)          ;; true

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set Intersection
;; Difficulty:	Easy
;; Topics:	set-theory
;; Write a function which returns the intersection of two sets.
;; Special Restriction: [clojure.set/intersection]
;; (= (__ #{0 1 2 3} #{2 3 4 5}) #{2 3})
(defn find-intersect
  [xs ys] (reduce #(if (get ys %2) (conj % %2) %) #{} xs))

;; and the readable version :)
(defn find-intersect
  [xs ys]
  (reduce
     (fn [intersects x]
       (if (get ys x)
         (conj intersects x)
         intersects) )
      #{} xs))


(find-intersect #{0 1 2 3} #{2 3 4 5})     ;; #{3 2}

;; !!!!!!! others solution !!!!!!!
(filter #{0 1 2 3} #{2 3 4 5})             ;; (3 2)
((comp set filter) #{0 1 2 3} #{2 3 4 5})  ;; #{3 2}

;; because filter takes a predicate, and the xs-coll acts as the function predicate
;; while each item from ys is filtered against the xs-predicate function -> which
;; returns the number or nil if its in the predicate-xs collection....NEAT!!! clojure style

;;;;;;;;;;;;;;;;;;;;
;; Cartesian Product
;; Set-theory: easy
;; (= (__ #{1 2 3} #{4 5})  #{[1 4] [2 4] [3 4] [1 5] [2 5] [3 5]})

;; using HOFs
(defn cartesian-prod
  [xs ys]
  ((comp (partial apply hash-set)
         (partial partition 2)
         flatten)
      (reduce
         (fn [acc x]
           (conj acc (map (fn [y] [x y]) ys)))
         [] xs)))


(cartesian-prod #{1 2 3} #{4 5}) ;; #{(2 5) (3 4) (1 4) (1 5) (2 4) (3 5)}

;; using imperative style:
(defn imperative-cart-prod
  [xs ys]
  (apply hash-set (for [x xs
                        y ys :while (not (nil? x))]
                    [x y])))

(imperative-cart-prod #{1 2 3} #{4 5}) ;; #{[2 5] [3 4] [1 4] [1 5] [2 4] [3 5]}

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Symmetric Difference
;; Set-theory: Easy
;; Write a function which returns the symmetric difference of two sets.
;; The symmetric difference is the set of items belonging to one but not both of the two sets.
;; (= (__ #{1 2 3 4 5 6} #{1 3 5 7}) #{2 4 6 7})
(doc clojure.set/difference)
(clojure.set/difference #{1 2 3 4 5 6} #{1 3 5 7})
(clojure.set/difference #{1 3 5 7} #{1 2 3 4 5 6})
(defn diff
  [xs ys]
  (merge (clojure.set/difference xs ys) (clojure.set/difference ys xs)))

(diff #{1 2 3 4 5 6} #{1 3 5 7}) ;; #{4 #{7} 6 2}

(defn diff
  [xs ys]
  (apply hash-set (lazy-cat (remove xs ys) (remove ys xs))))

;;  #{1 2 3 4 5 6} #{1 3 5 7}

;; remember that: [hash-set] returns the wrapped data-structure with a set
;; while [set] converts the data-structure to a set
;; [apply hash-set] behaves in this case like [set], converting the data-structure to a set.
;; ex:
(set [1 2 3])            ;; #{1 3 2}
(hash-set [1 2 3])       ;; #{[1 2 3]}
(apply hash-set [1 2 3]) ;; #{1 3 2}

;; another solution(other user) is using core-sets funcs:
;; #(clojure.set/union (clojure.set/difference %1 %2) (clojure.set/difference %2 %1))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Recognize Playing Cards
;; Difficulty:	Easy
;; Topics:	strings game
;; http://www.4clojure.com/problem/128 ....long story...
;; suits - Spades, Hearts, Diamonds, and Clubs
;; (= {:suit :diamond :rank 10} (__ "DQ"))     ;; DQ = :diamond queen
;; (= {:suit :heart :rank 3} (__ "H5"))        ;; H5 = :heart 5

;; For purposes of determining rank, we will define the cards to be valued from 0 (the two) to 12 (the ace)

(defn decode-card
  [card-code]
  (let [cards-suits {\D :diamond \H :heart \S :spade \C :club}
        cards-schema ((comp (partial apply hash-map) reverse)
                        (interleave (range 0 13)
                                    (concat (map (comp first str) (range 2 10))
                                            (list \T \J \Q \K \A))))
        card-suit (first card-code)
        card-rank (second card-code)]
    {:suit (get cards-suits card-suit) :rank (get cards-schema card-rank)}))

;; demo:
(decode-card "H4")  ;;  {:suit :heart, :rank 2}

;; work:
;; generate cards mappings: 0..12 + map the 10 to T, + J + Q + K + A
(apply hash-map (interleave (range 0 13) (concat (range 2 10) (list \T \J \Q \K \A))))
;; {0 2, 7 9, 1 3, 4 6, 6 8, 3 5, 12 \A, 2 4, 11 \K, 9 \J, 5 7, 10 \Q, 8 \T}

;; reverse them, as we get the human readable version: DA -> diamond-ace (for a simpler lookup ONLY)
((comp (partial apply hash-map) reverse) (interleave (map (comp first str) (range 0 13)) (concat (range 2 10) (list \T \J \Q \K \A))))
;; {\A \1, 7 \5, 4 \2, \J \9, \K \1, \Q \1, 6 \4, 3 \1, \T \8, 2 \0, 9 \7, 5 \3, 8 \6}


(get (into {} (reverse {\D :diamond \S :spade})) \S) ;; :spade
(get {\D :diamond \S :spade} \S) ;; :spade

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Pairwise Disjoint Sets
;; Difficulty:	Easy
;; Topics:	set-theory
;; Given a set of sets, create a function which returns true if no two of those sets have
;; any elements in common1 and false otherwise

(defn not-intersected?
  ([set-of-sets]
     (empty? (filter false? (not-intersected? (vec set-of-sets) (vec set-of-sets) []))))

  ([[head-set & tail-sets] checked-sets commons]
   (if (empty? checked-sets)  ;; all sets are checkd for intersect items -> return commons
     commons
     (if (nil? tail-sets)     ;; first set is checked with all others->step to next, popping the checked-sets
       (recur checked-sets (rest checked-sets) commons)
       (let [found? (empty? (filter #(contains? head-set %) (first tail-sets)))] ;; *** will use the head-set as the fn-predicate
          (recur (cons head-set (rest tail-sets)) checked-sets (conj commons found?)))))))

(not-intersected? #{#{:a :b :c :d :e} #{:a :b :c :d} #{:a :b :c} #{:a :b} #{:a}})
;; [false false false false false false false false false false false false false false]

(not-intersected? #{#{\U} #{\s} #{\e \R \E} #{\P \L} #{\.}}) ;
;; [true true true true true true true true true true true true true true]

;; but hey! pretty much the impl is done...because we have something like:
;; [true true true true true true true true true true true true true true]

;; ...in the output of the overloaded-func we only need to make a [filter] based on the "false"
;; so that we know neither of the sets are having some common elements.

;; !!!! upsssss.... 1 test is failing:
(not-intersected? #{
      #{ (#(-> *)) + (quote mapcat) #_ nil}
      #{ '+ '* mapcat (comment mapcat)}
      #{ (do) set contains? nil?}
      #{ , , , #_, , empty?}}) ;; false ---> OKKKK, treating nils explictly using [contains?]

;; nil is there -> and [filter] by using the head-set as a predicate-func discards nils
(str #{1 2 (comment mapcat)}) ;; nil 1 2
(str #{(do) nil?}) ;; nil nil-func
(str #{ , , ,}) ;; #{}

(filter #{nil 1 2} #{nil 3}) ; () -> this is the cause, present nils as elements

(= nil nil) ;; true
(contains? #{nil 1 2} nil) ;; true
(filter #(contains? #{nil 1 2} %) #{nil 3}) ;; (nil)
(empty? #{nil}) ;; false

;; *** because the predicate func head-set is using [get] internally, is like:
;; get an element from the collection: #{1 2 3} 2 -> 2
;; and the [filter] func is seing 2? then not nil -> is taken
;; but: #{1 2 nil} nil ---->>> nil, and filter works on truthy -> in clojure nil + false -> falsy


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Least Common Multiple <-> the opposite of GCD
;; Difficulty:	Easy
;; Topics:	math
;; Write a function which calculates the least common multiple. Your function should accept a
;; variable number of positive integers or ratios.

;; LCM(a, b), IS THE SMALLEST POSITIVE INTEGER THAT IS DIVISIBLE BY(BY not WITH) BOTH(inclusive) a AND b.
;; (== (__ 2 3) 6)
;; (== (__ 5 3 7) 105)
;; (== (__ 3/4 1/6) 3/2)
;; (== (__ 1/3 2/5) 2)
;; solution:
;;;;;;; real numbers:
;; find the sum of each number -> yielding many sum of nums lists -> find least common number
;;;;;;; rational:
;; (= LCM(a b) (/ (* a b) gcd(a b)))

(defn lcm [& xs]
  (letfn [(gen-step-seq [bound step]
            (take bound (iterate #(+ % step) step)))
          (real-nums-lcm [xs]
            (apply min
                (reduce clojure.set/intersection
                    (map (comp (partial apply hash-set)
                               (partial gen-step-seq (reduce * xs)))
                         xs))))
          (find-numerators [xs] (map (fn [x] (if (ratio? x) (numerator x) x)) xs))
          (find-denominators [xs] (map (fn [x] (if (ratio? x) (denominator x) 1)) xs))
          (find-divisibles [x] (filter #(integer? (/ x %)) (gen-step-seq x 1)))
          (gcd [xs]
            (apply max
              (reduce clojure.set/intersection
                      (map (comp (partial apply hash-set) find-divisibles) xs))))]
    (/ (real-nums-lcm (find-numerators xs)) (gcd (find-denominators xs)))))

;; demo:
(lcm 2 3)         ;; 6 -> OK
(lcm 1/3 2/5)     ;; 2 -> OK
(lcm 3/4 1/6)     ;; 3/2 -> NOK 3
(lcm 7 5/7 2 3/5) ;; 210 -> OK
(lcm 5 3 7) ;; 105 -> OK

;; most elegant solution:
(defn lcm [& nums]
    (letfn [(gcd [a b] (if (zero? b) a (gcd b (mod a b))))]
      (/ (reduce * nums) (reduce gcd nums))))


(doc every?) ;; every pred coll -> boolean: returns true if predicate(x) is true for every x in coll
(numerator 3/2)   ;; 3
(denominator 3/2) ;; 2
(take 40 (iterate #(+ % 5) 5)) ;; 5 10 15...105 110
(apply min [1 2 3]) ;; 1
(min 1 2 3) ;; 1

;; using euclid algorithm:
(defn gcd [a b]
  (if (zero? b) a
    (gcd b (rem a b))))
;; for any a>=b
(gcd 8 5) ;; 1
(gcd 121 11) ;; 11
(gcd 72 27) ;; 9
;; 8 5 -> 5 3 -> 3 2 -> 2 1 -> 1 0 -> 0 1 -> 1

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Pascal's Trapezoid
;; Difficulty:	Easy
;; Topics:	seqs
;; Write a function that, for any given input vector of numbers, returns an infinite lazy sequence
;; of vectors, where each next one is constructed from the previous following the rules used in
;; Pascal's Triangle. For example, for [3 1 2], the next row is [3 4 3 2].
;; (= (take 5 (__ [1]))       [[1] [1 1] [1 2 1] [1 3 3 1] [1 4 6 4 1]])
;; (= (take 2 (__ [3 1 2]))   [[3 1 2] [3 4 3 2]])

;; some warm-up :)
(defn iterate-lazy [start]
  (lazy-seq
     (cons start (iterate-lazy (inc start)))))

;; demo of the iterate-lazy func:
(take 2 (iterate-lazy 1)) ;; (1 2)

;; real-impl:
(defn lazy-pascal [xs]
  (letfn [(new-row-recur
            [[head secnd & tail]]
             (lazy-seq
                (if (nil? secnd)
                  (list)
                  (conj (new-row-recur (cons secnd tail)) (+' head secnd)))))] ;; don't loose the secnd!
    (lazy-seq
       (cons xs (lazy-pascal (lazy-cat [(first xs)] (new-row-recur xs) [(last xs)]))))))

;; demo
(take 2 (lazy-pascal [3 1 2])) ;; ([3 1 2] (3 4 3 2))
(class (take 5 (lazy-pascal [1]))) ;; clojure.lang.LazySeq
(lazy-pascal [1]) ;; IntegerOverflow -> use: +' to defer a BigInteger instance

(defn new-row-recur
            [[head secnd & tail]]
             (lazy-seq
                (if (nil? secnd)
                  (lazy-seq)
                  (conj (new-row-recur (cons secnd tail)) (+ head secnd)))))

(class (new-row-recur [3 4 5 6 9])) ;; clojure.lang.LazySeq

;; extracting for quicker feedback in the repl:
(defn new-row [[head secnd & tail]]
    (if (nil? secnd)
      (list)
      (conj (new-row (cons secnd tail)) (+ head secnd))))

(new-row [3 4 3 2]) ;; (7 7 5)
(new-row [3 1 2])   ;; (4 3)
(class (new-row [3 1 2])) ;; clojure.lang.PersistentList

;; IMPORTANT: on Arithmetic operations
;; Beware of arithmetic overflow! In clojure (since version 1.3 in 2011), if you use an
;; arithmetic operator like [+] and the result is too large to fit into a 64-bit integer,
;; an exception is thrown.
;; You can use +' to indicate that you would rather overflow into Clojure's slower, arbitrary-precision bigint.

;; case study on this exercise. reading others solutions:
;; (fn [v0] (iterate (fn [v] (mapv +' (conj v 0) (cons 0 v))) v0))
(doc mapv)
;; or:
(partial iterate
           (fn [nums]
             (vec (map +' (conj nums 0) (cons 0 nums)))))


;;;;;;;;;;;;;;;;
;; Juxtaposition
;; Difficulty:	Medium
;; Topics:	higher-order-functions core-functions
;; Take a set of functions and return a new function that takes a variable number of arguments
;; and returns a sequence containing the result of applying each function
;; left-to-right to the argument list.
;; (= [21 6 1] ((__ + max min) 2 3 5 1 6 4))
(defn juxt-disguised
  [& funcs]
  (fn [& args] (map #(apply % args) funcs)))

;; demo:
((juxt-disguised + max min) 2 3 5 1 6 4)  ;; (21 6 1)

;; now the recursive solution:
(defn juxt-disguised [& funcs]
  (fn [& xs]
    (loop [[head-fn & tail-fns] funcs
           acc []]
      (if (nil? head-fn)
        acc
        (recur tail-fns (conj acc (apply head-fn xs)))))))


;;;;;;;;;;;;;;;;;;;;;;;
;; Partition a Sequence
;; Difficulty:	Medium
;; Topics:	seqs core-functions
;; Special Restrictions: partition + partition-all

;; Write a function which returns a sequence of lists of x items each.
;; Lists of less than x items should not be returned.
;; (= (__ 3 (range 9)) '((0 1 2) (3 4 5) (6 7 8)))
;; (= (__ 3 (range 8)) '((0 1 2) (3 4 5)))
(defn partition-disguised
  [parted xs]
  (let [some-part (take parted xs)]
    (if (< (count some-part) parted) ;; -> replace this with empty? check -> take the rem items
      (list)
      (cons some-part (partition-disguised parted (drop (count some-part) xs))))))

(partition-disguised 3 (range 9)) ;; ((0 1 2) (3 4 5) (6 7 8))
(partition-disguised 3 (range 8)) ;; ((0 1 2) (3 4 5))

(doc drop) ;; drop n xs -> all but n items

;; here's another version now, including the rem items using recursivity:
(defn my-partition-all
  [parted xs]
  (loop [acc [] part [] [head & tail] xs]
    (if (nil? head)
        (conj acc part)
      (if (= (count part) parted)
        (recur (conj acc part) [] (cons head tail))
        (recur acc (conj part head) tail)))))

(my-partition-all 3 (range 9)) ;; [[0 1 2] [3 4 5] [6 7 8]]
(my-partition-all 3 (range 8)) ;; [[0 1 2] [3 4 5] [6 7]]

;; and a version using lazy sequence would be neat:
(defn lazy-partition
  [parted xs]
  (lazy-seq
    (let [some-part (take parted xs)]
      (if (< (count some-part) parted) ;; -> replace this with empty? check -> take the rem items
        (list)
        (cons some-part (lazy-partition parted (drop (count some-part) xs)))))))

(class (lazy-partition 3 (range 8))) ;; clojure.lang.LazySeq
(lazy-partition 3 (range 100000000000)) ;; niceeeeeeeeee


;;;;;;;;;;;;;;;;;;;;;;;;
;; Longest Increasing Sub-Seq

;; Difficulty:	Hard
;; Topics:	seqs
;; Given a vector of integers, find the longest consecutive sub-sequence of increasing numbers.
;; If two sub-sequences have the same length, use the one that occurs first.
;; An increasing sub-sequence must have a length of 2 or greater to qualify.
;; (= (__ [1 0 1 2 3 0 4 5]) [0 1 2 3])
;; time-complexity: O(n) <- as it requires to scale all the collection
(ns hard)
(defn find-greater-consecs
  ([[head & tail]] (find-greater-consecs tail [[head]]))
  ([[head & tail] xs-of-xs]
   (letfn [(greater-coll [xs ys]
              (if (< (count xs) (count ys)) ys xs))
           (find-first-greatest-coll
              [xs-of-xs]
              (reduce greater-coll [] xs-of-xs))
           (apply-min-length-rules [xs-of-xs]
              (if (> (count xs-of-xs) 1) xs-of-xs []))]

     (if (nil? head)
       (-> (find-first-greatest-coll xs-of-xs)
           apply-min-length-rules)
       (if (= (inc (last (last xs-of-xs))) head)
           (recur tail (conj xs-of-xs (conj (last xs-of-xs) head)))
           (recur tail (conj xs-of-xs [head])))))))


(find-greater-consecs [1 0 1 2 3 0 4 5 6 7])  ;; [0 1 2 3]
(find-greater-consecs [7 6 5 4]) ;; [0 1 2 3] ;; []

;;;;;;;;;;;;;;;;;;;;;
;; Beauty is Symmetry
;; Difficulty:	Easy
;; Topics:	trees
;; Let us define a binary tree as "symmetric" if the left half of the tree is the mirror
;; image of the right half of the tree. Write a predicate to determine whether or not a
;; given binary tree is symmetric.
;; (= (__ '(:a (:b nil nil) (:b nil nil))) true)
;; (= (__ [1 [2 nil [3 [4 [5 nil nil] [6 nil nil]] nil]]
;;           [2 [3 nil [4 [6 nil nil] [5 nil nil]]] nil]])
;;        true)
;; how i think of? it's quite hard to go recursively and compare both trees from the inner-most level
;; using clojure's support for values over identities, denormalize the trees into sequences, inversing
;; the left->to->right branches and compare the yield sequence-values.
;; assume first it's a valid binary tree! then create a function which validates-first the binary-tree?

;; DEPTH-FIRST-SEARCH -> in depth
;; Step1: reverse the so-called-binary-tree
(defn reverse-tree-rec
  ([[node l-branch r-branch]]
     (when-not (nil? node)
        [node (reverse-tree-rec r-branch) (reverse-tree-rec l-branch)])))

;; demo:
(reverse-tree-rec [2 nil [3 [4 [5 nil nil] [6 nil nil]] nil]])
;;                [2 [3 nil [4 [6 nil nil] [5 nil nil]]] nil]

;; go recursively INSIDE the binary tree reversing the branches: left-with-right, but start

;; Step2: going further on validating the nested-binary-tree-like-data-structures
(defn validate-binary-tree
  [[node l-branch r-branch :as tree]]
  (letfn [(node? [xs] (= 3 (count xs)))]
    (if (nil? node)
      []
      (lazy-cat
        (conj (validate-binary-tree l-branch) (node? tree))
        (conj (validate-binary-tree r-branch) (node? tree))))))

(validate-binary-tree [2 [3 nil [4 [6 nil nil] [5 nil nil]]] nil])
;; (true true true true true true true true true true)

(validate-binary-tree [[3 nil [4 [6 nil nil] [5 nil nil]]] nil])
;; (false false) --> OK

;; Step3: implement the real solution using these function composed
(defn symetric-bin-tree?
  [[node l-branch r-branch :as tree]]
  (letfn [(bin-tree-nodes?
            [[node l-branch r-branch :as tree]]         ;; destructing the so called tree:bind names to coll items
              (letfn [(node? [xs] (= 3 (count xs)))]
                (if (nil? node)
                  []
                  (lazy-cat
                    (conj (bin-tree-nodes? l-branch) (node? tree))
                    (conj (bin-tree-nodes? r-branch) (node? tree))))))

          (binary-tree? [tree] (every? true? (bin-tree-nodes? tree)))

          (reverse-tree-recur
             [[node l-branch r-branch]]
             (when-not (nil? node)
                [node (reverse-tree-recur r-branch) (reverse-tree-recur l-branch)]))]
    (and (binary-tree? tree)
         (= l-branch (reverse-tree-recur r-branch)))))

;; work:
(= [1 [1 2] nil] [1 [1 2] nil]) ;; comparing values!!! true

(= (balanced-bin-tree? [1 [2 nil [3 [4 [5 nil nil] [6 nil nil]] nil]] [2 [3 nil [4 [6 nil nil] [5 nil nil]]] nil]])
       true)

(doc contains?) ;; -> contains? coll key -> returns true if key present in the collection
(every? true? [true false]) ;; false
(every? true? [true true])  ;; true
(empty? (filter false? [true false])) ;; same as using filter + empty
((comp empty? (partial filter false?)) [true false]) ;; false -> using func composition and partial application
((comp empty? (partial filter false?)) [true true]) ;; true

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Prime Numbers
;; Difficulty:	Medium
;; Topics:	primes
;; Write a function which returns the first x number of prime numbers.
;; (= (__ 2) [2 3])
;; (= (__ 5) [2 3 5 7 11])
(take 5 (range)) ; (0 1 2 3 4)
(filter #(zero? (rem 5 %)) (range 2 5)) ;; (5)

(defn prime-nums
  [bound]
  (letfn [(prime? [x]
            (empty? (filter #(zero? (rem x %)) (range 2 x))))]
    (filter prime? (range 2 bound))))

;; demo:
(prime-nums 15) ;; (2 3 5 7 11 13)

;; well we should change the impl to return the actually prime-nums primes? x n
(defn first-x-prime-nums
  [bound]
  (letfn [(prime? [x]
            (empty? (filter #(zero? (rem x %)) (range 2 x))))]
    (take bound (nnext (filter prime? (range))))))

(first-x-prime-nums 5) ;; (2 3 5 7 11)
(first-x-prime-nums 2) ;; (2 3)

;; work:
(filter prime? (nnext (range))) ;; (2 3 5 ... lazyseq to infinite)

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Filter Perfect Squares
;; Difficulty:	Medium
;; Topics:
;; Given a string of comma separated integers, write a function which returns a new comma
;; separated string that ONLY CONTAINS the numbers which are perfect squares.
;; (= (__ "4,5,6,7,8,9") "4,9")
;; how i think? create a lazy-pow-seq of each num started from 2. then extract the string-seq
;; into workable real numbers seq. then found a way to check lazily(as we started with a lazy-solution)
;; to iterate and check each realified-number in the lazy-pow-seq

;; Step1:
(defn lazy-pows-start-with [x]
  (letfn [(pow [x] (reduce * [x x]))]
  (lazy-seq
     (cons (pow x) (lazy-pows-start-with (inc x))))))

;; demo:
(reduce * [8 8]) ;; 64

(class (lazy-pows-start-with 2))  ;; clojure.lang.LazySeq
(lazy-pows-start-with 2)          ;; (4 9 16 25 36 ...)

;; Step2: impl function to tranform the comma-sep string-seq into a seq of numbers
(defn to-num-seq [str-seq reg]
  (map read-string (clojure.string/split str-seq reg)))

;; demo:
(to-num-seq "4,5,6,7,8,9" #",") ;; (4,5,6,7,8,9)

;; Step3: using a lazy-sequence + recursivity version, over filter which blocks when comparing using predicate
;; and the produced lazy-pows-seq
(defn contained-nums
  [lazy-xs nums acc]
  (if (empty? nums)
    acc
    (if (> (first lazy-xs) (apply max nums))
      (recur (rest lazy-xs) (rest nums) acc)
      (if-let [contained-x ((set nums) (first lazy-xs))]
        (recur (rest lazy-xs) (rest nums) (conj acc contained-x))
        (recur (rest lazy-xs) nums acc)))))

;; demo:
(if-let [lazy-pows-seq (lazy-seq [4 9 16 25 36 42 100])]
  (contained-nums lazy-pows-seq [4 5 6 7 8 9 16] [])) ;; [4 9 16]

(if-let [lazy-pows-seq (lazy-seq [4 9 16 25 36 49 64 81])]
  (contained-nums lazy-pows-seq [15,16,25,36,37] [])) ;; [16 25 36]


;; Real implementation composed of the 3 implemented funcs:
(defn filter-perf-squares [seq-str]
  (letfn [(pow-x-start-with
             [start]
             (letfn [(pow [x] (reduce * [x x]))]
               (lazy-seq
                 (cons (pow start) (pow-x-start-with (inc start))))))
          (split-by-comma
            [str-seq]
             (map read-string (clojure.string/split str-seq #",")))
          (find-squares
            [lazy-pows-seq xs acc]
            (if (empty? xs)
              (apply str (interpose "," acc))
              (if (> (first lazy-pows-seq) (apply max xs))
                (recur (rest lazy-pows-seq) (rest xs) acc)
                (if-let [contained-x ((set xs) (first lazy-pows-seq))]
                  (recur (rest lazy-pows-seq) (rest xs) (conj acc contained-x))
                  (recur (rest lazy-pows-seq) xs acc)))))]

    (find-squares (pow-x-start-with 2) (split-by-comma seq-str) [])))

;; demo:
(filter-perf-squares "4,5,6,7,8,9")  ;; "4,9"
(filter-perf-squares "15,16,25,36,37")  ;; "16,25,36"

;; some clojure-doc
(doc interpose)             ;; interpose sep coll -> lazy-seq items separated by sep
(interpose "," [1 2 3])             ;; (1 "," 2 "," 3)
(apply str (interpose "," [1 2 3])) ;; "1,2,3"

;;;;;;; another user interesting solution ;;;;;;;
(fn [s]
  (let [xs (map #(Integer. %) (clojure.string/split s #","))
        p? (fn [x] (some #(= x (* % %)) (range 2 x)))]
    (clojure.string/join "," (filter p? xs))))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Perfect Numbers
;; Difficulty:	Medium
;; A number is "perfect" if the sum of its divisors equal the number itself. 6 is a perfect number because 1+2+3=6
;; (= (__ 6) true)
;; (= (__ 7) false)
;; (= (__ 496) true)
;; 1) find divisors of a number, including 1. 2) apply sum of them 3) check reduced sum with the number itself
(defn find-divisors
  [x]
  (let [possible-divs (take (quot x 2) (iterate inc 1))]
    (filter (fn [each] (zero? (rem x each))) possible-divs)))

(find-divisors 6) ;; (1 2 3)
(find-divisors 11) ;; (1)
(find-divisors 10) ;; (1 2 5)
(quot 13 2) ;; 6

(defn perf-number?
  [x]
  (letfn [(find-divisors [x]
            (let [possible-divs (take (quot x 2) (iterate inc 1))]
              (filter #(zero? (rem x %)) possible-divs)))]
    (= x (reduce + (find-divisors x)))))

(perf-number? 6) ;; true
(perf-number? 7) ;; false
(perf-number? 496) ;; true

;;;;;;;;;;;;;;;;;;;;;;;;
;; Power Set
;; Difficulty:	Medium
;; Topics:	set-theory
;; Write a function which generates the power set of a given set.
;; The power set of a set x is the set of all subsets of x, including the empty set and x itself.
;; (= (__ #{1 :a}) #{#{1 :a} #{:a} #{} #{1}})
;; (= (__ #{1 2 3})
;;    #{#{} #{1} #{2} #{3} #{1 2} #{1 3} #{2 3} #{1 2 3}}
;; 1) build the binary representation of all the possible numbers: 2 power(count set)
;; 2) bind: x y z -> to 0 1 1; whereas x = 0, y = z = 1 -> #{y z} -> yields one set
(defn binary-representation
  [exp]
  (letfn [(left-padding-seq
            [how-many padded]
             (take how-many (lazy-cat (reverse (seq padded)) (repeat how-many 0))))
          (to-binary
             [x]
             (map #(Integer/valueOf (str %)) (seq (Integer/toBinaryString x))))
          (gen-bit-combinations
             [x]
             (let [bin-val (Math/pow 2 exp)]
                (map
                   (fn [x] (->> x
                               to-binary
                               (left-padding-seq exp)))
                   (range 1 (inc bin-val)))))]

    (gen-bit-combinations exp)))

(binary-representation 3)
;; (1 0 0) (0 1 0) (1 1 0) (0 0 1) (1 0 1) (0 1 1) (1 1 1) (0 0 0)

(binary-representation 2)
;; ((1 0) (0 1) (1 1) (0 0))

(count (binary-representation 10)) ;; 1024 -> OK 2 power 10 -> 1024

;; stepping fw to how to replace the actual positional-1-s with the actual bound values
;; (1 0 1) -> (:a 0 :b)
;; Step2:
(defn assoc-entries
  [x-set]
  (letfn [(left-padding-seq
            [how-many padded]
             (take how-many (lazy-cat (reverse (seq padded)) (repeat how-many 0))))
          (to-binary
             [x]
             (map #(Integer/valueOf (str %)) (seq (Integer/toBinaryString x))))
          (gen-bit-combinations
             [x]
             (let [bin-val (Math/pow 2 (count x-set))]
                (map
                   (fn [x] (->> x
                               to-binary
                               (left-padding-seq (count x-set))))
                   (range 1 (inc bin-val)))))
          (map-to-vals
             [x-set]
             (map (fn [bit-pair]
                    (apply hash-map (interleave (seq x-set) bit-pair)))
                  (gen-bit-combinations (count x-set))))
          (filter-maps-by-val
             [m-val m]
              (map
                 (fn [each-map]
                   (reduce
                      (fn [m entry]
                        (if (zero? (val entry)) m
                          (conj m entry)))
                      {} each-map))
                 m))]

    ((comp (partial filter-maps-by-val 0)
            map-to-vals) x-set)))

;; demo:
(assoc-entries #{:a :b :c})
;; ({:c 1} {:b 1} {:b 1, :c 1} {:a 1} {:a 1, :c 1} {:a 1, :b 1} {:a 1, :b 1, :c 1} {})

;; before applying the filter-maps-by-val func, we had this result:
;; {:c 1, :b 0, :a 0}
;; {:c 0, :b 1, :a 0}
;; {:c 1, :b 1, :a 0}
;; {:c 0, :b 0, :a 1} .....
;; next step: filter 0-keys + map the keys into sets and we're done :)

(defn filter-maps-by-val
  [m-val m]
  (map
     (fn [each-map]
       (reduce
          (fn [m entry] (if (zero? (val entry)) m (conj m entry))) {} each-map)) m))

;; demo:
(filter-maps-by-val 0 [{:foo 0 :bar 1 :zip 0}])  ;; ({:bar 1}) --> cool :)
(doc val) ;; -> returns the value in the map-entry

;; next step: extract all the keys of each map into sets
(defn keys-to-sets [maps]
  (map (comp set keys) maps))

;; demo:
(keys-to-sets '({:c 1} {:b 1} {:b 1, :c 1} {:a 1} {:a 1, :c 1} {:a 1, :b 1} {:a 1, :b 1, :c 1} {}))
;; (#{:c} #{:b} #{:c :b} #{:a} #{:c :a} #{:b :a} #{:c :b :a} #{})


;; Note: (comp set keys) partial application can be translated into: (comp (partial apply hash-set) keys)
;; but because [set] function transforms the given structure into a set is more less-to-write + readable
(keys {:c 1 :a 1}) ;; (:c :a)

;; final function which yields power-sets algorithm:
(defn power-sets
  [x-set]
  (letfn [(left-padding-seq
            [how-many padded]
             (take how-many (lazy-cat (reverse (seq padded)) (repeat how-many 0))))
          (to-binary
             [x]
             (map #(Integer/valueOf (str %)) (seq (Integer/toBinaryString x))))
          (gen-bit-combinations
             [x]
             (let [bin-val (Math/pow 2 (count x-set))]
                (map
                   (fn [x] (->> x
                               to-binary
                               (left-padding-seq (count x-set))))
                   (range 1 (inc bin-val)))))
          (map-to-vals
             [x-set]
             (map (fn [bit-pair]
                    (apply hash-map (interleave (seq x-set) bit-pair)))
                  (gen-bit-combinations (count x-set))))
          (filter-maps-by-val
             [m-val m]
              (map
                 (fn [each-map]
                   (reduce
                      (fn [m entry]
                        (if (zero? (val entry)) m
                          (conj m entry)))
                      {} each-map))
                 m))
          (keys-to-sets [maps]
            (map (comp set keys) maps))]
    ((comp
         set
         keys-to-sets
        (partial filter-maps-by-val 0)
         map-to-vals)
       x-set)))

;; demo....niceeeeeeeee:
(power-sets #{:a :b :c})
;; #{#{:c} #{:b} #{:c :b} #{:a} #{:c :a} #{:b :a} #{:c :b :a} #{}}

;; Study function:
(fn power-set [xs]
	(letfn [(gen-next [xs acc]
				(if (contains? acc xs)
					(conj acc #{})
					(recur xs (set (for [x xs ys acc] (conj ys x))))))]
	(gen-next xs #{#{}})))


;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Black Box Testing
;; Difficulty:	Medium
;; Topics:	seqs testing
;; Special Restrictions: class, instance?, type, vector?, sequential?, list?, seq?, map?, set?, getClass
;; Clojure has many sequence types, which act in subtly different ways. The core functions
;; typically convert them into a uniform "sequence" type and work with them that way,
;; but it can be important to understand the behavioral and performance differences so that
;; you know which kind is appropriate for your application.

;; Write a function which takes a collection and returns one of :map, :set, :list, or :vector
;; - describing the type of collection it was given.
;; You won't be allowed to inspect their class or use the built-in predicates like list?
;; - the point is to poke at them and understand their behavior.
;; (= :map (__ {:a 1, :b 2}))
;; (= :vector (__ [1 2 3 4 5 6]))
(defn type-of
  [some-struct]
  (let [foo [:some :foo]
        bar [:some :bar]
        is-vector? #(and (identical? (last (conj % foo bar)) bar)   ;; lifo behavior from last
                         (identical? (last (butlast (conj % foo bar))) foo)
                         (nil? (get (conj % foo) :some)))           ;; associative by index
        is-list? #(and (identical? (first (conj % foo bar foo)) foo)
                       (identical? (second (conj % foo bar foo)) bar) ;; lifo behavior from first
                       (nil? (get (conj % foo) :some)))
        is-set? #(and (= 2 (count (conj (empty %) foo bar foo))))   ;; uniques
        is-map? #(and (= :bar ((conj % foo bar) :some))             ;; override map-entry
                      (= 1 (count (conj (empty %) foo bar))))]      ;; only one-map-entry added
    (cond (is-set? some-struct) :set
      	  (is-vector? some-struct) :vector
          (is-list? some-struct) :list
          (is-map? some-struct) :map)))

;; refactored function to take in control the properties associated to each ds: associative? + reversible?
(defn type-of [ds]
  (if (associative? ds)
    (if (reversible? ds) :vector :map)
    (if (= 1 (count (conj (empty ds) :foo :foo))) :set :list)))

(associative? '(1 2))  ;; false
(associative? #{1 2})  ;; false
(associative? [1 2])   ;; true
(associative? {1 2})   ;; true

(reversible? '(1 2))   ;; false
(reversible? [1 2])    ;; true
(reversible? {1 2})    ;; false
(reversible? #{1 2})   ;; false

;; demo:
(map type-of [{} #{} [] ()])  ;; (:map :set :vector :list)
(= [:map :set :vector :list] (map type-of [{} #{} [] ()])) ;; true
(= :set (type-of #{10 (rand-int 5)})) ;; true

;; Note: TWO sets are NOT identical, even if they have all the items the same, and use the same "given" order
(identical? #{1 2 3} #{1 3 2}) ;; false
(identical? #{1 2 3} #{1 2 3}) ;; false


;;;;;;;;;;;;;;;;;
;; Anagram Finder
;; Difficulty: Medium
;; GIven a vector of strings, output a set of subsets(where each subset contains the
;; possible permutations of chars foreach item)
;; Each subset should have at least 2 words. A set that does not contain any anagrams is not included.
;; (= (__ ["veer" "lake" "item" "kale" "mite" "ever"])
;;    #{#{"veer" "ever"} #{"lake" "kale"} #{"mite" "item"}})

;; How i think: map on initial vector of words, passing a reduce function which operates on outer-map-item and initial vector
;; checking using [every?] if the outer-map-item contains all the chars reduced by the next function

(defn anagram-finder
  [words]
    (set
      (filter #(> (count %) 1)
        (map (fn [outer-word]
          (reduce
            (fn [acc inner-word]
              (if (= (sort outer-word) (sort inner-word))
                (conj acc inner-word)
                acc))
            #{} words)) words))))

(anagram-finder ["meat" "mat" "team" "mate" "eat"])  ;; #{#{"meat" "mate" "team"}}

(doc every?) ;; returns true if predicate x is true forall the x in coll, else false
(= ["a" "b" "c"] (sort ["b" "c" "a"])) ;; true
(= [\a \b \c] (sort "bca"))

;; TODO: now impl anagram-finder using recursion
(defn anagram-finder
  ([words] (anagram-finder words words words [#{}]))
  ([[head & tail] next-comp-words init-words words-acc]
     (if (empty? next-comp-words)  ;; rest returns a lazyseq and empty else(don't check for nil?)
       (apply hash-set
         (filter #(> (count %) 1) words-acc))
       (if (nil? head)
         (recur init-words (rest next-comp-words) init-words (conj words-acc #{}))  ;; rebind initial words for next checked word
         (if (= (sort head) (sort (first next-comp-words)))
           (recur tail next-comp-words init-words (conj (vec (butlast words-acc)) (conj (last words-acc) head)))
           (recur tail next-comp-words init-words words-acc))))))

(anagram-finder ["meat" "mat" "team" "mate" "eat"])  ;; COOL dude :)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Build [reductions] a similar version of [reduce], that takes either 2 or 3 args and returns
;; a lazy sequence of all the intermediate values that reduce computes.
;; Difficulty: Medium
;; Category: seqs-core-funcs
;; Special Restrictions: [reduction]
(take 5 (reductions + (range)))   ;; (0 1 3 6 10)
(take 5 (reductions + 2 (range))) ;; (2 2 3 5 8)

;; currently works for lazy-infinite-seqs
(defn reduction
  [func init xs]
  (lazy-seq
     (let [intermediate-result (apply func (list init (first xs)))]
       (cons init (reduction func intermediate-result (rest xs))))))

(take 5 (reduction + 2 (range))) ;; (2 2 3 5 8)
(take 5 (reduction * 2 (iterate inc 1))) ;; (2 2 4 12 48)

;; now make it work with finite-number of elements + handle init/acc as optional
(defn reduction
  ([func xs] (reduction func (first xs) (rest xs)))
  ([func init xs]
      (lazy-seq
         (if (empty? xs)
           (list init)
           (let [inter-result (func init (first xs))]
             (cons init (reduction func inter-result (rest xs))))))))

;; demo:
(take 5 (reduction + 2 (range))) ;; (2 2 3 5 8)
(reduction + 2 (range))          ;; (2 2 3 5 8 ... 120 ... 122...infinite)
(reduction * 2 [3 4 5])          ;; (2 6 24 120)

(class (reduction + 1 (range)))  ;; clojure.lang.LazySeq

(take 5 (reduction + 2 (range))) ;; (2 2 3 5 8)
(reduction conj [1] [2 3 4])     ;; ([1] [1 2] [1 2 3] [1 2 3 4])
(reduction + 0 '(1 2 3))

;;;;;;;;;;;;;;;;;;;;;
;; Happy Numbers
;; Difficulty: Medium
;; A Happy number is any number which when: split by digits, applying square on each digit
;; and then sum the numbers and recursive going the next cycle again yields 1.
;; A sad number is one that loops endesly.
;; For example, 19 is happy, as the associated sequence is:
;;     1**2 + 9**2 = 82
;;     8**2 + 2**2 = 68
;;     6**2 + 8**2 = 100
;;     1**2 + 0**2 + 0**2 = 1.
(defn happy-number?
  ([x] (happy-number? x []))
  ([x init]
  (letfn [(pow-of-two [x] (reduce * [x x]))
          (to-int [c] (- (int c) (int \0)))
          (pow-and-sum
            [digits]
             (reduce +
                (map (comp pow-of-two to-int) digits)))]
    (let [curr (pow-and-sum (str x))]
      (if-let [happy (= 1 curr)]
        happy
        (if (and ((comp not nil?) (last init)) (> curr (pow-of-two (last init))))
          false
          (recur (pow-and-sum (str curr)) (conj init curr))))))))

;; notes: the init acc persists across the recursion the last pow-and-sum digit; if the digit starts
;; and powers itself -> wrong path -> it is not a happy number, since it should be reduced not increased.
;; we start from [0] to avoid nil checking and avoid incidental complexity for nil checking;
;; once the recursion processed: 3 -> 9 -> 81; we store this in the acc and break the recursion if > (last acc) curr.

(happy-number? 19) ;; true ;;  curr 100 ;; last-init 82
(happy-number? 20) ;; false ;; curr 37  ;; last-init 4
(happy-number? 7)  ;; true  ;; curr 130 ;; last-init 49
(happy-number? 3)  ;; false
(happy-number? 2)  ;; false
(happy-number? 986543210)  ;; true

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Trees into tables
;; Difficulty:	Easy
;; Topics:	seqs maps
;; Because Clojure's for macro allows you to "walk" over multiple sequences in a nested fashion,
;; it is excellent for transforming all sorts of sequences.
;; If you don't want a sequence as your final output (say you want a map), you are often
;; still best-off using for, because you can produce a sequence and feed it into a map, for example.
(for [x (range 1 3) y (range 5 7)] [x y]) ;; ([1 5] [1 6] [2 5] [2 6])

;; (= (__ '{a {p 1, q 2}
;;          b {m 3, n 4}})
;;    '{[a p] 1, [a q] 2
;;      [b m] 3, [b n] 4})

;; solution:
(defn trees-to-tables
  [big-hash]
  (letfn [(get-path [[key-entry m]]
             (map (fn [x y z]
                    [[x y] z])
                  (repeat (count m) key-entry)
                  (keys m)
                  (vals m)))]
    (into {} (apply
                concat
                  (map get-path big-hash)))))

;; demo:
(trees-to-tables '{a {p 1, q 2} b {m 3, n 4}})
;; {[a p] 1, [a q] 2, [b m] 3, [b n] 4}

;; refactoring -> using mapcat instead of [concat] + [map]
(defn trees-to-tables
  [big-hash]
  (letfn [(get-path [[key-entry m]]
             (map (fn [x y z]
                    [[x y] z])
                  (repeat (count m) key-entry)
                  (keys m)
                  (vals m)))]
    (into {} (mapcat get-path big-hash))))


(into {} '([[a p] 1] [[a q] 2])) ;; {[a p] 1, [a q] 2}
