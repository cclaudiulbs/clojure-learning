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
(defn gen-parans [x]
  (letfn [(pow [x n]
              (reduce * (repeat n x)))
          (to-binary [x]
              (Integer/toBinaryString x))
          (left-padding [bin-num]
             (apply str (conj (vec (repeat (- x (count bin-num)) 0) ) bin-num)))
          (binary-nums-padded [exp]
              (map (comp left-padding to-binary) (range 0 (pow 2 exp))))
          (nest-op [acc]
              (str "(" acc ")" ))
          (nest-op? [each-exp]
              (= 1 (Integer. (str each-exp))))
          (append-op [acc]
              (str acc "()"))
          (gen-parans-recur
             [binary-seq]
             (loop [[head & tail] binary-seq
                     acc ""]
                 (if (nil? head)
                    acc
                   (if (nest-op? head)
                     (recur tail (nest-op acc))
                     (recur tail (append-op acc))))))]

    (apply hash-set (map gen-parans-recur (binary-nums-padded x)))))

;; 010
(gen-parans 3)

;; "(()())" "((()))" "()()()" "(())()"

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

(let [[head & tail] "010"] [head tail]) ; [\0 \1 \0]
(let [[head & tail] "010"] (clojure.string/replace head "0" "()")) ; "()"

(map #(Integer. (str %)) (vec "123")) ; (1 2 3)

(defn wrap-op [str-token]
    (format "(%1s)" (clojure.string/replace str-token #"0|1" "()")))
(wrap-op (str \0)) ; (())
(clojure.string/replace "0" #"0|1" "()") ; "()"
