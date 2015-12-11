;;Word Chains
;;Difficulty:	Hard
;;Topics:	seqs
;;A word chain consists of a set of words ordered so that each word differs by only one letter 
;;from the words directly before and after it. 
;;The one letter difference can be either an insertion, a deletion, or a substitution. 
;;Here is an example word chain:
;;cat -> cot -> coat -> oat -> hat -> hot -> hog -> dog
;;
;;Write a function which takes a sequence of words, and returns true if they can be arranged 
;;into one continous word chain, and false if they cannot.
;;test not run	
(ns clojure-learning.word-chains
  (use (clojure [repl] test)))

;; #{"hat" "coat" "dog" "cat" "oat" "cot" "hot" "hog"}

;; thinking: take each element and reduce with rest to find if there's a matching word
;; the matching func should take two words -> and return truthy if the words seem to match
(defn words-chain? [source-str target-str]
  (let [intersected (clojure.set/intersection (set source-str) (set target-str))
        identify-by #(apply % [(count %2) (count %3)])
        smallest (identify-by min source-str target-str)
        greatest (identify-by max source-str target-str)]
     (and (<= (- greatest smallest) 1) 
         (or (= intersected (set source-str)) 
             (= intersected (set target-str))))))

(words-chain? "tackli" "tackl") ;; true
(words-chain? "clau" "claud")   ;; true
(words-chain? "clau" "clua")    ;; true
(words-chain? "cos" "cosar")    ;; false

(defn all-words-chain? 
  ([xset] 
   (all-words-chain? (vec xset) (rest (vec xset)) []))
  ([[head-inner sec-inner & tail-inner] [head-rest & tail-rest]  matches-vec]
   (if (nil? head-rest) matches-vec 
    (if (nil? sec-inner)
     (recur tail-rest tail-rest matches-vec)
     (if (words-chain? head-inner sec-inner)
       (recur (cons sec-inner tail-inner) (cons head-rest tail-rest) (conj matches-vec :link))
       (recur (cons sec-inner tail-inner) (cons head-rest tail-rest) matches-vec))))))

(all-words-chain? #{"hat" "coat" "dog" "cat" "oat" "cot" "hot" "hog"}) ;; true
(all-words-chain? #{"cot" "hot" "bat" "fat"})                 ;; false
(all-words-chain? #{"to" "top" "stop" "tops" "toss"})         ;; false
(all-words-chain? #{"spout" "do" "pot" "pout" "spot" "dot"})  ;; true
(all-words-chain? #{"share" "hares" "shares" "hare" "are"})   ;; true
(all-words-chain? #{"share" "hares" "hare" "are"})            ;; false

(defn words-chain? [source-str target-str]
  (let [intersected (clojure.set/intersection (set source-str) (set target-str))
        identify-by #(apply % [(count %2) (count %3)])
        smallest (identify-by min source-str target-str)
        greatest (identify-by max source-str target-str)]
     (when (and (<= (- greatest smallest) 1) 
                (or (= intersected (set source-str)) 
                    (= intersected (set target-str))))
       source-str)))
;; unfortunatelly this function is not helping too much as it's using sets as values and not words stricly comparisons

(words-chain? "share" "hares") ;; -> fix this func and the algorithm is ok!
(words-chain? "tops" "toss")

;; if count a - count b = 1 -> then leading-padding & trailing padding of-smaller with 0
(interleave "tops" "toss")
(interleave "toss" "to")
(map (fn [[a b]] (= a b)) (partition 2 (interleave "0hare" "hares"))) ;; (false false false false false)
(map (fn [[a b]] (= a b)) (partition 2 (interleave "hare0" "hares"))) ;; (true true true true false)

(map (fn [[a b]] (= a b)) (partition 2 (interleave "0top" "stop")))   ;; (false true true true)
(map (fn [[a b]] (= a b)) (partition 2 (interleave "top0" "stop")))   ;; (false false false false)

(map (fn [[a b]] (= a b)) (partition 2 (interleave "0pout" "spout"))) ;; (false true true true true)
(map (fn [[a b]] (= a b)) (partition 2 (interleave "pout0" "spout"))) ;; (false false false false false)

;; check for false in both vec-results -> if a vec contains only one false -> words-chaining-match!
(defn words-chain? [source-str target-str]
    (letfn [(difference [& words]
              (let [[smallest greatest] (sort-by count words)]
                (- (count greatest) (count smallest))))
            (pair-compared-chars [& words]
              (let [[smallest greatest] (sort-by count words)]
                 (assoc
                    (assoc {} :left-padded-vals (map (fn [[a b]] (= a b)) (partition 2 (interleave (apply str (cons "$" smallest)) greatest))))
                      :right-padded-vals (map (fn [[a b]] (= a b)) (partition 2 (interleave (apply str (conj (vec smallest) "$")) greatest))))))]
      (if (<= (difference source-str target-str) 1)
        (if-let [{:keys [left-padded-vals right-padded-vals]} (pair-compared-chars source-str target-str)]
          (if (or (= 1 (count (filter false? left-padded-vals)))
                  (= 1 (count (filter false? right-padded-vals))))
            :linked
            :not-linked)))))

("abcde" "abvde")
(map (fn [[a b]] (= a b)) (partition 2 (interleave "abcde" "abvde"))) ;; (true true false true true)
(map (fn [[a b]] (= a b)) (partition 2 (interleave "abcdef" "abvde0"))) ;; (true true false true true false)

(sort-by count ["clojure" "this"])
(apply str  (cons "$" "this"))
(filter false? [true true false])

(words-chain? "share" "hares") ;; -> fix this func and the algorithm is ok!
(words-chain? "tops" "toss")
(words-chain? "top" "tops")

(assoc {} :left-padded (map (fn [[a b]] (= a b)) (partition 2 (interleave (apply str (cons "$" "thi")) "cthi"))))
(conj "$" (vec "this"))
