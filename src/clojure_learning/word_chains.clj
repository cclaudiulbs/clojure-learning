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

(sort-by count ["clojure" "this"])  ;; ("this" "clojure")
(apply str  (cons "$" "this"))      ;; "$this"

(assoc {} :left-padded (map (fn [[a b]] (= a b)) (partition 2 (interleave (apply str (cons "$" "thi")) "cthi"))))
;; {:left-padded (false true true true)}

;; fails for [pot pout]::
#_(defn words-chain? [source-str target-str]
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
            target-str)))))

#_(defn all-words-chain? [xset]
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
                    [] (find-all-unique-words-tuples xset)))]
    (->> xset
         find-linked-words
      )))

(some #{:linked}  (keys {:linked "linked" :not-linked "not-linked"}))  ;; :linked

(= {:linked "l"} {:linked "l"}) ;; true

;; currently the above implementation outputs a results as:
(all-words-chain? #{"cot" "hot" "bat" "fat"})                 
;; [{:linked ["bat" "fat"]} {:linked ["cot" "hot"]}] (should be false)

(all-words-chain? #{"to" "top" "stop" "tops" "toss"})         
;; [{:linked ["top" "tops]} {:linked ["stop" "top"]} {:linked ["top" "to"]} {:linked ["tops" "toss"]}]

(defn but-word [words word] (remove #{word} words))
(defn find-matching [words word] (some (partial words-chain? word) words))

;; in action the two functions::
(find-matching (but-word ["hares" "hare" "are" "share"] "share") "share")  ;; "hare"
(but-word ["hares" "hare" "are" "share"] "share") ;; ("hares" "hare" "are")
(but-word ["cclaudiu"] "cclaudiu") ;; () -> empty


;; nice clojure prefix notation!!!
(>= 2 1 0) ;; true

(conj #{} 1)          ;; #{1}
(some #{"this"} #{})  ;; nil
(empty? #{})          ;; true
(remove #{"this"} ["this"]) ;; () -> empty

(first #{1 2})

(sort-by count #{"this" "cc"}) ;; ("cc" "this")

(words-chain? "pot" "pout") ;; nil -> the focking function: words-chain? from above FAILS miserably....fix it!!!

;; fix::
(defn words-chain? [some-word other-word]
  (loop [[head-char & tail-chars] some-word
         target other-word
         acc []]
    (if (nil? head-char) 
      (<= (- (count other-word) (count acc)) 1)  ;; domain-logic-rule -> for identifying if the word-chain contract OK!
      (if (some #{head-char} target)
        (recur tail-chars (remove #{head-char} target) (conj acc head-char))
        (recur tail-chars target acc)))))

(words-chain? "pot" "pout") ;; true 
(words-chain? "are" "hare") ;; true
(words-chain? "share" "hare") ;; true
(words-chain? "hares" "share") ;; true
(words-chain? "share" "hares") ;; true
(words-chain? "cot" "fat")    ;; false
(words-chain? "rare" "are") ;; true
(words-chain? "hat" "coat")  ;; false
(words-chain? "oat" "hat")  ;; true
(words-chain? "dog" "hog") ;; true
(words-chain? "ler" "leru") ;; leru
(words-chain? "lar" "leru") ;; false
(words-chain? "lr" "leru") ;; false

;; adaptation for 4clojure all-words-chain? function -> should retrieve the linked-target so that the check for
;; words-chain work -> rename it since it's not a predicate anymore :)
(defn find-link-of [some-word other-word]
  (letfn [(get-index-from [some-str some-char]
            (some 
              (fn [indexed-tuple] 
                (when (= (last indexed-tuple) some-char) 
                  (first indexed-tuple)))
              (map-indexed (fn [idx each-char] [idx each-char]) some-str)))
          (words-chain? [a b]
            (loop [[head-char & tail-chars] some-word
                   target other-word
                   acc []]
              (if (nil? head-char) 
                (<= (- (count other-word) (count acc)) 1)  ;; domain-logic-rule -> for identifying if the word-chain contract OK!
                (if (and (some #{head-char} target) (= (get-index-from a head-char) (get-index-from b head-char)))
                  (recur tail-chars (remove #{head-char} target) (conj acc head-char))
                  (recur tail-chars target acc)))))]
    (when (words-chain? some-word other-word) other-word)))

(defn get-index-from [some-str some-char]
    (some 
        (fn [indexed-tuple] 
            (when (= (last indexed-tuple) some-char) (first indexed-tuple)))
          (map-indexed (fn [idx each-char] [idx each-char]) some-str)))

;; in action
(get-index-from "this" \h) ;; 1

;; in action:: map-indexed
(map-indexed (fn [idx c] [idx c]) "this") ;; ([0 t] [1 h] [2 i] [3 s])

;; in action
(find-link-of "pot" "pout") ;; "pout" -> currently nil, if get-index-of is included
(find-link-of "hare" "share") ;; "share" -> currently nil, if get-index-of is included
(find-link-of "cot" "fat")    ;; nil
(find-link-of "are" "rare")

;; thinking...take the transitive approach in checking for a char in a range:
;; if get-index-of-char-a = 1 -> valid-range can ONLY be: 0 1 2, since the char-a can vary in the target with only
;; 1 character-position; then based on this range -> check the get-index-of-char-a from target -> if the index is 
;; in the range that 0 1 2 -> the word is valid. there should be 2 conditions: if same-index? -> skip to next comparison
;; if 0 || 2 -> mark a dirty flag that the change occured, and ALL other chars should be on the same index as the
;; source. steping to \r -> index 1 -> (while checking the occurence if changed? -> drop first from target -> because
;; all other indexes SHOULD match!
(defn linked-words?
  ([a b] 
   (let [[greatest smallest] (reverse (sort-by count [a b]))]
     (when (<= (- (count greatest) (count smallest)) 1)
        (linked-words? (seq greatest) (seq smallest) 0))))
  ([[head-a & tail-a :as source] [head-b & tail-b :as target] dirty-counter]
   (letfn [(get-index-of [ch word]
            (some (fn [indexed-tuple] 
                    (when (= (second indexed-tuple) ch) 
                      (first indexed-tuple)))
                  (map-indexed (fn [idx each-char] [idx each-char]) word)))]
     (if (nil? head-a) (<= dirty-counter 1)
       (if (and (= (get-index-of head-a source) (get-index-of head-a target)) 
                (<= dirty-counter 1)) ;; same char-positions -> skip to next
           (recur tail-a tail-b dirty-counter)
           (if (and (nil? (get-index-of head-a target)) 
                    (<= dirty-counter 1))
              (recur tail-a target (inc dirty-counter))

                    (= (inc (get-index-of head-a source)) (get-index-of head-a target))
              (recur tail-a tail-b (inc dirty-counter)))
           )))))

(map-indexed (fn [idx c] [idx c]) "share")
(map-indexed (fn [idx c] [idx c]) "hares")

(defn find-matching [words links] 
    (remove nil? (map #(some (partial linked-words? %) words) links)))

(defn but-word [words word] 
  (remove #{word} words))

;; [to] [top stop tops toss]
(but-word [] "rod")
(seq [])
(find-matching ["top" "stop" "tops" "toss"] ["top" "topsss"])
(but-word ["top" "stop" "tops" "toss"] "top") ;; -> (stop tops toss)
;; [to top] [stop tops toss]
(find-matching ["stop" "tops" "toss"] "top")
(but-word ["stop" "tops" "toss"] "stop") ;; -> (tops toss)
(find-matching ["tops" "toss"] "stop")  ;; -> tops -> should fail! since all chars are OK, but there are many replaces of chars from the words!

(defn get-index-of [c word]
            (some 
              (fn [indexed-tuple] 
                (when (= (last indexed-tuple) c) 
                  (first indexed-tuple)))
              (map-indexed 
                (fn [idx each-char] [idx each-char]) 
                  word)))
(get-index-of \h nil) ;; nil
(get-index-of nil nil) ;; nil

(defn find-matching [words links] 
  (last (remove nil? (map #(some (partial linked-words? %) words) links))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; final version!!!!
;;;;;;;;;;;;;; reshaped function ;;;;;;;;;;;;;;;;;;;;
;; find correct linked words + the final function should have a linked-list of words ->
;; all elements are linked -> each element has at least 1 link to another: 1 -> 2 -> 2 -> 1
(defn linked-words? [source target]
  (let [[smaller greater] (sort-by count [source target])
        greater-indexed (map-indexed (fn [idx c] [idx c]) greater)
        smaller-indexed (map-indexed (fn [idx c] [idx c]) smaller)]
    (letfn [(combine-matches-or-nil [greater-tuple smaller-tuple]
              (map (fn [[out-idx out-ch]] 
                (some (fn [[in-idx in-ch]] 
                        (when (and (= out-ch in-ch) 
                                   (or (= out-idx in-idx)
                                       (= out-idx (inc in-idx))
                                       (= out-idx (dec in-idx)))) out-ch)) 
                     smaller-tuple)) 
             greater-tuple))]
      (when (<= (- (count greater) (count smaller)) 1)
        (when-let [greater-combined-matches (combine-matches-or-nil greater-indexed smaller-indexed)]
          (when-let [smaller-combined-matches (combine-matches-or-nil smaller-indexed greater-indexed)]
            (let [greater-matches-indexed (map-indexed (fn [idx x] [idx x]) greater-combined-matches)
                  smaller-matches-indexed (map-indexed (fn [idx x] [idx x]) smaller-combined-matches)]
              (>= 1
                (count
                    (filter (fn [matched-indexed] (nil? (last matched-indexed)))
                        (apply hash-set 
                            (apply conj 
                                greater-matches-indexed smaller-matches-indexed))))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; this is the corner-case: dropping + insertion at the end
(linked-words? "share" "hares") ;; ([0 nil] [4 nil]) --> works nice -> there are 2 OPERATIONS hence NOT linked word!
;; currently the linked-words function outputs: FALSE!!! whoooohoooo

(map-indexed (fn [idx c] [idx c]) "share") ;; ([0 s] [1 h] [2 a] [3 r] [4 e])
(map-indexed (fn [idx c] [idx c]) "hares") ;; ([0 h] [1 a] [2 r] [3 e] [4 s])
(sort-by count ["share" "hares"]) ;; ("share" "hares")

(linked-words? "pout" "pot") ;; true
(linked-words? "hare" "hares") ;; true
(linked-words? "hare" "share") ;; true
(linked-words? "share" "hares") ;; !!!!!!!!!!!!!!!!!!!  should be false!!! drop + insert at the end! AND NOW IT IS! false
(linked-words? "share" "shares") ;; true
(linked-words? "cot" "fat")    ;; false
(linked-words? "rare" "are") ;; true
(linked-words? "rare" "ar") ;; nil
(linked-words? "hat" "coat")  ;; nil
(linked-words? "oat" "hat")   ;; hat
(linked-words? "dog" "hog") ;; hog
(linked-words? "claudiu" "clodius") ;; nil
(linked-words? "claudiu" "claudius") ;; claudius
(linked-words? "mary" "mari") ;; mari
(linked-words? "leru" "ler") ;; leru
(linked-words? "leru" "lar") ;; nil
(linked-words? "leru" "lr") ;; nil

;; ;;;;;;;;;;;;;;;; reshaped ;;;;;;;;;;;;;;;;;;;;
(defn all-words-chain? [words-set]
  (map (fn [word]
         [word (reduce (fn [acc compared-word]
                   (if (= word compared-word) acc
                     (if (linked-words? word compared-word)
                       (inc acc)
                       acc))) 
                 0 words-set)]) 
       words-set))

;; tests:
(all-words-chain? #{"hat" "coat" "dog" "cat" "oat" "cot" "hot" "hog"})  ;; true 
;; current output:
;; (["dog" 1] ["oat" 3] ["cot" 3] ["coat" 3] ["hot" 3] ["hat" 3] ["hog" 2] ["cat" 4])

(all-words-chain? #{"share" "hares" "hare" "are"})            ;; false -> OK
;; current output::
;; (["are" 1] ["hare" 3] ["hares" 1] ["share"] 1)

;; --> if the resulted collection have more than 2 tuples with 1-link -> NOT CHAINED!
;; --> upgrading the function

;; final function for finding all the links::
(defn all-words-chain? [words-set]
  (letfn [(find-links-foreach-word [words]
            (map (fn [word]
                    (reduce (fn [acc compared-word]
                              (if (= word compared-word) acc
                                (if (linked-words? word compared-word)
                                  (inc acc)
                                  acc)))
                            0 words)) 
                 words))]
    (->> words-set
        find-links-foreach-word
        (filter #(= 1 %))
        count
        (>= 2))))

(all-words-chain? #{"spout" "do" "pot" "pout" "spot" "dot"})  ;; true  -> OK
(all-words-chain? #{"share" "hares" "shares" "hare" "are"})   ;; true  -> OK
(all-words-chain? #{"share" "hares" "hare" "are"})            ;; false -> OK
(all-words-chain? #{"cot" "hot" "bat" "fat"})                 ;; false -> OK
(all-words-chain? #{"to" "top" "stop" "tops" "toss"})         ;; false -> OK

;;;;;;;;;;;;;;;; submited to 4clojure::
(defn all-words-chain? [words-set]
  (letfn [(linked-words? [source target]
            (let [[smaller greater] (sort-by count [source target])
                  greater-indexed (map-indexed (fn [idx c] [idx c]) greater)
                  smaller-indexed (map-indexed (fn [idx c] [idx c]) smaller)]
              (letfn [(combine-matches-or-nil [greater-tuple smaller-tuple]
                        (map (fn [[out-idx out-ch]] 
                                (some (fn [[in-idx in-ch]] 
                                        (when (and (= out-ch in-ch) 
                                                   (or (= out-idx in-idx)
                                                   (= out-idx (inc in-idx))
                                                   (= out-idx (dec in-idx)))) out-ch)) 
                                smaller-tuple)) 
                          greater-tuple))]
                (when (<= (- (count greater) (count smaller)) 1)
                  (when-let [greater-combined-matches (combine-matches-or-nil greater-indexed smaller-indexed)]
                    (when-let [smaller-combined-matches (combine-matches-or-nil smaller-indexed greater-indexed)]
                      (let [greater-matches-indexed (map-indexed (fn [idx x] [idx x]) greater-combined-matches)
                            smaller-matches-indexed (map-indexed (fn [idx x] [idx x]) smaller-combined-matches)]
                        (>= 1
                          (count
                              (filter (fn [matched-indexed] (nil? (last matched-indexed)))
                                  (set
                                      (apply conj 
                                          greater-matches-indexed smaller-matches-indexed))))))))))))
          (find-links-foreach-word [words]
            (map (fn [word]
                    (reduce (fn [acc compared-word]
                              (if (= word compared-word) acc
                                (if (linked-words? word compared-word)
                                  (inc acc)
                                  acc)))
                            0 words)) 
                 words))]
    (->> words-set
        find-links-foreach-word
        (filter #(= 1 %))
        count
        (>= 2))))
