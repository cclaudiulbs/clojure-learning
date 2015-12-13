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

;; if count a - count b = 1 -> then leading-padding & trailing padding of-smaller with 0
(map (fn [[a b]] (= a b)) (partition 2 (interleave "0hare" "hares"))) ;; (false false false false false)
(map (fn [[a b]] (= a b)) (partition 2 (interleave "hare0" "hares"))) ;; (true true true true false)

(map (fn [[a b]] (= a b)) (partition 2 (interleave "0top" "stop")))   ;; (false true true true)
(map (fn [[a b]] (= a b)) (partition 2 (interleave "top0" "stop")))   ;; (false false false false)

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
;; {:left-padded (false true true true)}

;; refactor this func to work with tuples as domain-arguments and not with fixed-args <- because find-all-unique-tuples
;; outputs a set of set-tuples -> set of sets for discarding duplicates! by value semantics
(defn words-chain? [[source-str target-str]]
    (letfn [(difference [& words]
              (let [[smallest greatest] (sort-by count words)]
                (- (count greatest) (count smallest))))
            (pair-compared-chars [& words]
              (let [[smallest greatest] (sort-by count words)]
                (if (not= (count smallest) (count greatest))
                  (assoc
                    (assoc {} :left-padded-vals (map (fn [[a b]] (= a b)) (partition 2 (interleave (apply str (cons "$" smallest)) greatest))))
                      :right-padded-vals (map (fn [[a b]] (= a b)) (partition 2 (interleave (apply str (conj (vec smallest) "$")) greatest))))
                  (assoc {} :same-width (map (fn [[a b]] (= a b)) (partition 2 (interleave smallest greatest))))
                 )))]
      (if (<= (difference source-str target-str) 1)
        (if-let [{:keys [left-padded-vals right-padded-vals same-width]} (pair-compared-chars source-str target-str)]
          (when (or (= 1 (count (filter false? left-padded-vals)))
                  (= 1 (count (filter false? right-padded-vals)))
                  (= 1 (count (filter false? same-width))))
            {:linked [source-str target-str]})))))

(defn all-words-chain? [xset]
  (letfn [(find-all-unique-words-tuples [words-set]
            
                 (for [x words-set
                       y words-set
                       :when (not= x y)] 
                          [x y]) )
          (find-linked-words [xset]
            (reduce (fn [links words-tuple]
                      (if-let [linked-map ((comp words-chain? vec) words-tuple)]
                        (conj links linked-map)
                        links))
                    [] (find-all-unique-words-tuples xset)))
          (filter-dups-of-word-refs [linked-maps] 
            (reduce (fn [uniques-m each-m]
                      (if (some (fn [searched-m] 
                                  (= (first (:linked searched-m)) (first (:linked each-m)))) uniques-m)
                        uniques-m
                        (conj uniques-m each-m)))
                [(first linked-maps)] (rest linked-maps)))
          ]
    (->> xset
         find-linked-words
         (map (fn [{:keys [linked]}] linked))
         ;; filter-dups-of-word-refs
         ;; count
         ;; (= (dec (count xset)))
      )))

(some #{:linked}  (keys {:linked "linked" :not-linked "not-linked"}))  ;; :linked

(= {:linked "l"} {:linked "l"}) ;; true
(let [{:keys [linked]} {:linked ["some" "none"]}]
  (val linked))

;; currently the above implementation outputs a results as:
(all-words-chain? #{"cot" "hot" "bat" "fat"})                 
;; [{:linked ["bat" "fat"]} {:linked ["cot" "hot"]}] (should be false)

(all-words-chain? #{"to" "top" "stop" "tops" "toss"})         
;; [{:linked ["top" "tops]} {:linked ["stop" "top"]} {:linked ["top" "to"]} {:linked ["tops" "toss"]}]

;; how i think?
;; start with the smallest word -> and build the transitive-clojure to the next word, accumulating the
;; the results into a flat-vec -> if one word does not have any transitive-clojure && rest words is not null?
;; then the chain breaks and predicate yields false, else continue till rest is null in which case the
;; sequence is a words-chain
(all-words-chain? #{"hat" "coat" "dog" "cat" "oat" "cot" "hot" "hog"})  ;; true
(all-words-chain? #{"spout" "do" "pot" "pout" "spot" "dot"})  ;; true
(all-words-chain? #{"share" "hares" "shares" "hare" "are"})   ;; true
(all-words-chain? #{"share" "hares" "hare" "are"})            ;; false

(words-chain? ["hares" "share"])
