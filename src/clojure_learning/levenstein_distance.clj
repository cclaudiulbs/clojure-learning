(ns clojure-learning.levenstein-distance)
(require '[clojure.set :as s])

(defn str-remove-first-occurence [target-str subject]
  (let [string->coll #(map str %)
        contained? #(some (partial = %1) (string->coll %2))
        remove-from #(reduce
                      (fn [changes-map x]
                        (if (and (= %2 x)
                                 (not (:already-seen changes-map)))
                          (assoc changes-map :already-seen true)
                          (assoc changes-map :changes
                              (conj (:changes changes-map) x))))
                      {:already-seen false :changes []}
                      %1)]
    (if (contained? subject target-str)
      [subject (apply str
                  (:changes (remove-from (string->coll target-str) subject)))]
      [nil target-str])))

(str-remove-first-occurence "claudiu" "c") ;; ["c" "laudiu"]
(str-remove-first-occurence "bmw" "i") ;; [nil "bmw"]
(str-remove-first-occurence "xyyyx" "x") ;; ["x" "yyyx"]
(str-remove-first-occurence "yyyx" "y") ;; ["x" "yyyx"]
;; that is to remove only the first-occurence, not all!

(defn remove-first-occurence [target-tups subject-tup]
  (letfn [(contained? [target-tups [subj-idx subj]]
           ((comp not nil?)
            (some
              (fn [[idx x]] (and (= subj x) (= subj-idx idx)))
              target-tups)))

          (remove-from [target-tups [subj-idx subj]]
            (reduce
              (fn [changes-map [target-idx target :as target-tup]]
                  (if (and (= subj target)
                           (= subj-idx target-idx)
                           (not (:already-seen changes-map)))
                    (assoc changes-map :already-seen true)
                    (assoc changes-map :but-removed
                      (conj (:but-removed changes-map) target-tup))))
             {:already-seen false :but-removed []}
             target-tups))]
    (if (contained? target-tups subject-tup)
      [subject-tup (:but-removed (remove-from target-tups subject-tup))]
      [nil target-tups])))

(remove-first-occurence [[0 "c"] [1 "l"] [2 "a"]]  [0 "l"])
; [nil, [[0 "c"] [1 "l"] [2 "a"]]]

(remove-first-occurence [[0 "c"] [1 "l"] [2 "a"]]  [1 "l"])
; [[1 "l"], [[0 "c"] [2 "a"]]]


(defn count-diffs [this-word that-word]
  (letfn [(str->indexed-tuple [this]
           (map-indexed (fn [idx x] [idx (str x)]) this))

          (compute-insertions [this-word that-word]
                  (if (>= (count this-word) (count that-word)) 0
                    (- (count that-word) (count this-word))))

          (compute-diffs
            [[[idx c :as this-char-tup] & tail-words] that-tuples changes]
            "destructuring coll of froms, and also each-tuple[idx c]"
            (if (nil? this-char-tup) changes
              (let [[removed-subj remained-words-tups] (remove-first-occurence
                                                          that-tuples
                                                          this-char-tup)]
                (if (nil? removed-subj)
                  (recur tail-words remained-words-tups (inc changes))
                  (recur tail-words remained-words-tups changes)))))]

    ; (+ (compute-insertions this-word that-word)
      ;  (count-diffs- this-word that-word 0))
   {:insertions (compute-insertions this-word that-word)
    :diffs (compute-diffs
                (str->indexed-tuple this-word)
                (str->indexed-tuple that-word)
            0)}))


(map-indexed (fn [i x] [i (str x)]) "claudiu")

(count-diffs "closure" "clojure") ;; ["j"] => 1 OK
(count-diffs "xyx" "xyyyx")      ;; 2 NOK-3
(count-diffs "kitten" "sitting") ;; 3 OK
(count-diffs "" "123456")        ;; 6 OK
(count-diffs "Clojure" "Clojure") ;; 0 OK
(count-diffs [1 2 3 4] [0 2 3 4 5]) ;; 2 OK
(count-diffs "ttttattttctg"
             "tcaaccctaccat") ;; 10 OK
(count-diffs "gaattctaatctc"
             "caaacaaaaaattt") ;; 9 OK
(count-diffs "abcd" "ad")   ;; 2 NOK-3

(count-diffs "cclaudiu" "ccaudiu") ;; 1 insertion

;;;;;;;;;;;;
;; new version following the wiki-Levenstein-distance
;; https://en.wikipedia.org/wiki/Levenshtein_distance
;;;;;;;;;;;;
(def sit (map str "sitting"))
(def kit (map str "kitten"))
(defn lev [froms tos]
  ())

(defn structure-asc [this that]
  (if (> (count this) (count that)) [that this] [this that]))

(defn pad-minimized->to [this that]
  (letfn [(str->coll [some] (vec (map str some)))
          (structure-asc [this that]
                  (if (> (count this) (count that)) [that this] [this that]))
          (diff-sizes? [this that]
            (> 0 (apply - (map count (structure-asc this that)))))
          (matching-heads? [[head-x & _] [head-y & _]] (= head-x head-y))
          (matching-tails? [[head-x & tail-x] [head-y & tail-y]]
            (let [[last-x & _](reverse tail-x)
                  [last-y & _] (reverse tail-y)]
                 (= last-x last-y)))
          (identify-necessary-padding [this that]
            (apply - (map count (reverse (structure-asc this that)))))
          (pad-> [this that position]
            (let [[smaller bigger] (structure-asc this that)
                  paddings (repeat (identify-necessary-padding this that) nil)]
             (if (:to-head position)
                (conj []
                  (vec (concat paddings smaller))
                  bigger)
                (conj []
                  (vec (concat smaller paddings))
                  bigger))))]
    (let [this-coll (str->coll this)
          that-coll (str->coll that)]
         (if (and (matching-heads? this-coll that-coll)
                  (matching-tails? this-coll that-coll)
                  (diff-sizes? this-coll that-coll))
             ;; this is the place to return the same consistent data-structure and not only the vector!
             (vec
               (concat [(first this-coll)]
                     (first (pad-minimized->to (vec (butlast (rest this-coll)))
                                               (vec (butlast (rest that-coll)))))
                     [(last this-coll)]))
          (if (and (matching-heads? this-coll that-coll)
                   (diff-sizes? this-coll that-coll))
              (pad-> this-coll that-coll {:to-head false})
              (if (and (matching-tails? this-coll that-coll)
                       (diff-sizes? this-coll that-coll))
                  (pad-> this-coll that-coll {:to-head true})
                  (if (diff-sizes? this-coll that-coll)
                    (pad-> this-coll that-coll {:to-head false})
                    [this-coll that-coll])))))))

(pad-minimized->to "cosar" "claudiu")
;; [["c" "o" "s" "a" "r" :missing :missing] ["c" "l" "a" "u" "d" "i" "u"]]

(pad-minimized->to "bmw" "claudiu")
; [["b" "m" "w" :missing :missing :missing :missing] ["c" "l" "a" "u" "d" "i" "u"]]

(pad-minimized->to "acel" "lacel")
;; [[:missing "a" "c" "e" "l"]  ["l" "a" "c" "e" "l"]]

(pad-minimized->to "xyx" "xyyyx")
;; ["x" "y" null null "x"]

(pad-minimized->to "y" "yyy")
;; [["y" nil nil] ["y" "y" "y"]]

;; and the function :D
(defn levenstein-distance [from to]
  (filter
        (fn [[each-from each-to]]
          (not= each-from each-to))
        (partition 2 (apply interleave
                        (pad-minimized->to from to)))))

(levenstein-distance "closure" "clojure") ;; ["j"] => 1 OK
(levenstein-distance "xyx" "xyyyx")      ;; 2 NOK-3
(levenstein-distance "kitten" "sitting") ;; 3 OK
(levenstein-distance "" "123456")        ;; 6 OK
(levenstein-distance "Clojure" "Clojure") ;; 0 OK
(levenstein-distance [1 2 3 4] [0 2 3 4 5]) ;; 2 OK
(levenstein-distance "ttttattttctg"
                     "tcaaccctaccat") ;; 10 OK
(levenstein-distance "gaattctaatctc"
                     "caaacaaaaaattt") ;; 9 OK
(levenstein-distance "abcd" "ad")   ;; 2 NOK-3

(defn structure-asc [this that]
        (if (> (count this) (count that)) [that this] [this that]))
(defn diff-sizes? [this that]
  (> 0 (apply - (map count (structure-asc this that)))))
(defn matching-heads? [[head-x & _] [head-y & _]] (= head-x head-y))
(defn identify-necessary-padding [this that]
  (apply - (map count (reverse (structure-asc this that)))))
(defn pad-> [this that position]
  (let [[smaller bigger] (structure-asc this that)
        paddings (repeat (identify-necessary-padding this that) :missing)]
   (if (:to-head position)
      (conj []
        (vec (concat paddings smaller))
        bigger)
      (conj []
        (vec (concat smaller paddings))
        bigger))))

(pad-> ["y"] ["y" "y" "y"] {:to-head false})
(last ["y"])
(reverse nil)

(defn matching-tails? [[head-x & tail-x] [head-y & tail-y]]
  (let [[last-x & _](reverse tail-x)
        [last-y & _] (reverse tail-y)]
       (= last-x last-y)))
(matching-tails? "y" "yyy")
(matching-heads? "y" "yyy")
