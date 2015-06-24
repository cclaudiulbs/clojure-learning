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

