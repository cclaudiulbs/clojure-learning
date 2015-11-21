;;;;;;;;;;;;;;;;;;;;;;;;
;; #131:: Sum Some Set Subsets
;; Difficulty:	Medium
;; Topics:	math
;; Given a variable number of sets of integers, create a function which returns true iff all 
;; of the sets have a non-empty subset with an equivalent summation. 
;; solved-times: 587
;; in practical terms: the problem is that applying an operation(say +) on a domain of sets elements, would result some combinations of
;; of numbers yielded by the application of that operation. find if the resulted subsets share some intersected elements.
(ns clojure-learning.subsets-sum-intersection
  (use [clojure.repl])
  (require [clojure.test :refer :all]))

;; first draft is not covering all the corner cases, as it uses the core [reductions] func and will rely on the order from Left->Right
;; of each elements, hence not covering the first..and...last elements for example.
(defn subsets-sum-intersection [& xsets]
  (letfn [(reducted-sum-results [xsets]
            (map #(reductions + %) xsets))]
    (map (fn [& reducted-and-xset-tuple]
           (apply concat reducted-and-xset-tuple)) 
         (reducted-sum-results xsets) (map sort xsets))))

(subsets-sum-intersection #{1 3 5} #{9 11 4} #{-3 12 3} #{-3 4 -2 10})
;; --> NOT GOOD ENOUGH, because 1 + 5 is not covered, and neither 3 + 5, because using reductions: 1 + 0 -> 1 -> 1 + 3 -> 4 -> 4 + 5 -> 9

;; moving on -> i need to create a function which finds if there's one intersection between all the sequences
(for [x [1 2 3] y [1 2 3] z [1 2 3] :while (not= x y z)] [x y z])
;; would result in all the possible combinations of 1 2 3 elements <-- using [for] comprehension

;; for instance there might be a chance using partition, and a step of 1, but still will not compute all the possible combinations of elements!
(partition 2 1 [-1 3 -5 7 -9 11 -13 15]) ;; (-1 3), (3 -5), (-5 7) ...

;; this function takes a set -> reduces over this domain arg each element, and while reducing over each one -> will apply a for-comprehension
;; of itself AND the initial set-reduced to find all the combinations, while the element is NOT equal to itself!
(defn generate-all-tuple-combs 
  [xset]
    (reduce 
      (fn [acc x]
            (conj acc (for [y xset :when (not= x y)] [x y])))
      [] xset))

(generate-all-tuple-combs #{-1 3 -5 7 -9 11 -13 15}) ;; -> yields a lot of tuples, with the combinations of nums

;; last attempt is finally the function which handles all the combinations of applying an operation to each set-elements
(defn subsets-sum-intersection
  [& xsets]
  (letfn [(pair-and-add [x ys]
            (for [y ys :when (not= x y)] (+ x y)))
          (generate-sum-of-each [xs]
            (loop [[head & tail :as init] (vec xs)
                   [h s & tail-init] init
                   acc []]
              (if (nil? head)
                (recur (cons (+ h s) tail-init) (cons (+ h s) tail-init) acc)
                (if (nil? s) acc
                  (recur tail (list* h s tail-init) (conj acc (pair-and-add head tail))))))) ;; keep head as well as sum
          (generate-sum-recur [xs-vecs]
            (map (comp flatten generate-sum-of-each) xs-vecs))]
    (->> xsets 
         generate-sum-recur)
))

(subsets-sum-intersection #{1 3 5}) ;; (4 6 8 9)...you got the point :)
(subsets-sum-intersection #{-1 3 -5 7 -9 11 -13 15}) ;; long sequence of ALL combinations
(subsets-sum-intersection #{1 3 5}
                          #{9 11 4}
                          #{-3 12 3}
                          #{-3 4 -2 10});; (4 6 8 9), (15 13 20 24), (0 9 15 12), (2 -5 8 1 14 7 -1 12 7 9)
;; as we see -> there's a single element which might form a subset -> 9 -> that is found across the three subsets

;; the last step in solving the problem, because we found all the combinations, but the problem should build a predicate-func which
;; returns true of there's a intersection of subsets in the set-domain-of-args. i can use the core: clojure.set/intersection for this
(clojure.set/intersection #{1 2} #{2 3}) ;; #{2} --> EXACTLY WHAT I WANT :) but this is too trivial :) therefore 

;; NEXT: i'll build a recursive solution of finding the intersection
(defn map-by-min-and-tail [xs]
  (let [mapped-and-sorted (sort-by first (map #(apply vector [(count %) %]) xs))
        smallest          ((comp last first) mapped-and-sorted)
        tail              (map last (rest mapped-and-sorted))]
      [smallest tail]))

;; my func of grouping min-xs-and-rest in action:
(map-by-min-and-tail [[1 2 3] [1 2] [1 2 3 4]])  ;; [[1 2] ([1 2 3] [1 2 3 4])]

;; Once i found and grouped the min and rest 
;; -> i'm ensuring that i will take the smallest collection in order to find correspoding intersection in other colls

;; now i'm building the recursive function to check for intersections into a unnormalized-matrix
(defn has-intersections? [xs]
  (letfn [(vec-contains? [xs subject]
            (some #{subject} xs))]
    (loop [[smaller-head & smaller-tails] (first (map-by-min-and-tail xs))
           reseted-next-colls (second (map-by-min-and-tail xs))  ;; keep the state and reset when every rest-coll is exhausted
           [next-coll & tail-colls] reseted-next-colls
           intersections []]
    (if (or (nil? smaller-head) (nil? next-coll))
      ((comp not empty?) intersections)   ;; we're done -> as the smallest collection is exhausted :) -> check for non-emptyness!
      (if (and (nil? tail-colls) (vec-contains? next-coll smaller-head)) ;; we reached the last collection -> push into intersections
        (recur smaller-tails reseted-next-colls reseted-next-colls (conj intersections smaller-head))
        (if (vec-contains? next-coll smaller-head)  ;; one intermediate coll contains the smaller-head el -> recur/check in next-colls
          (recur (cons smaller-head smaller-tails) reseted-next-colls tail-colls intersections)
          (recur smaller-tails reseted-next-colls (cons next-coll tail-colls) intersections) ;; no match -> step to next el from smaller-coll
    ))))))
;; that was quite an effort :) ... now in action:
(has-intersections? [[1 2] [3 4 2]]) ;; true -->>> WHOOOOOHOOOO true! it contains 2
(has-intersections? [[1 2] [3 4]])   ;; false
(has-intersections? [[]])            ;; false -->>> NICE bravo dude'

;; some play-around...
(for [x [1 2 3]] (+ 1 x)) ;; (2 3 4)

(let [subject 3
      coll [1 2 3]]
  (some #{subject} coll)) ;; 3 -> NICE

;; Implementing the final version of the function, by augmenting it with the map-by-min-and-tail + has-intersections?
;; yeah...making things harder i implemented the intersection-func using TCO myself(i could have just use the: clojure.set/intersection)
(defn subsets-sum-intersection
  [& xsets]
  (letfn [(generate-sum-recur [xs-vecs]
            (map (comp flatten generate-sum-of-each) xs-vecs))
          (generate-sum-of-each [xs]
            (loop [[head & tail :as init] (vec xs)
                   [h s & tail-init] init
                   acc []]
              (if (nil? head)
                (recur (cons (+ h s) tail-init) (cons (+ h s) tail-init) acc)
                (if (nil? s) acc
                  (recur tail (list* h s tail-init) (conj acc (pair-and-add head tail))))))) ;; keep head as well as sum
          (pair-and-add [x ys]
            (for [y ys :when (not= x y)] (+ x y)))
          (map-by-min-and-tail [xs]
            (let [mapped-and-sorted (sort-by first (map #(apply vector [(count %) %]) xs))
                  smallest          ((comp last first) mapped-and-sorted)
                  tail              (map last (rest mapped-and-sorted))]
              [smallest tail]))
          (has-intersections? [xs]
            (letfn [(vec-contains? [xs subject] 
                      (some #{subject} xs))]
              (loop [[smaller-head & smaller-tails] (first (map-by-min-and-tail xs))
                     reseted-next-colls (second (map-by-min-and-tail xs))  ;; keep the state and reset when every rest-coll is exhausted
                     [next-coll & tail-colls] reseted-next-colls
                     intersections []]
                (if (or (nil? smaller-head) (nil? next-coll))
                  ((comp not empty?) intersections)   ;; we're done -> as the smallest collection is exhausted :) -> check for non-emptyness!
                  (if (and (nil? tail-colls) (vec-contains? next-coll smaller-head)) ;; we reached the last collection -> push into intersections
                    (recur smaller-tails reseted-next-colls reseted-next-colls (conj intersections smaller-head))
                    (if (vec-contains? next-coll smaller-head)  ;; one intermediate coll contains the smaller-head el -> recur/check in next-colls
                      (recur (cons smaller-head smaller-tails) reseted-next-colls tail-colls intersections)
                      (recur smaller-tails reseted-next-colls (cons next-coll tail-colls) intersections) ;; no match -> step to next el from smaller-coll
                ))))))
          (add-init-entries-to-combinations [init-entries all-combs]
            (map 
              (fn [each-combs-coll each-init-entries]
                (into each-combs-coll each-init-entries)) all-combs init-entries))]
    (->> xsets 
         generate-sum-recur
         (add-init-entries-to-combinations xsets)
         has-intersections?)))

(deftest test-subsets-sum-intersection
(testing "if applying sum on some items from each subset yields a num which is in all of the sets"
  (is (= true (subsets-sum-intersection #{1 3 5}
                                        #{9 11 4}
                                        #{-3 12 3}
                                        #{-3 4 -2 10}))) ;; true -> NICE dude'

  (is (= true (subsets-sum-intersection #{-1 3 -5 7 -9 11 -13 15}
                                        #{1 -3 5 -7 9 -11 13 -15}
                                        #{1 -1 2 -2 4 -4 8 -8} ))) ;; true

  (is (= false (subsets-sum-intersection #{-1 -2 -3 -4 -5 -6}
                                         #{1 2 3 4 5 6 7 8 9}))) ;; true -> normally it returns false

  (is (= true (subsets-sum-intersection #{1})))

  (is (= true (subsets-sum-intersection #{-1 1 99} 
                                        #{-2 2 888}
                                        #{-3 3 7777})))

))

(subsets-sum-intersection #{1}) ;; -yeah it fails for this corner case -> however it's NOT normal to return TRUE if there's
;; no other intersection-comparisons sets -> 1 is intersected with WHAT??? dude DUMB 4clojure author's scenario....

(subsets-sum-intersection #{1 3 5}
                          #{9 11 4}
                          #{-3 12 3}
                          #{-3 4 -2 10})

(map (fn [combs-coll initial-xset] (into combs-coll initial-xset)) [[1 2 3] [4 5]] '(#{-1 0} #{6 7})) ;; [[1 2 3 0 -1] [4 5 7 6]]
