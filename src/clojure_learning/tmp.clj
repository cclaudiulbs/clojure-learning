(defn gen-parentheses
  ([x]
   (letfn [(nest-child-parens
              [step l-parens r-parens acc]
              (lazy-cat (take step l-parens) (take step r-parens)))
           (lazy-subvec-fn
              [step l-parens r-parens acc]
              (lazy-cat (gen-parentheses step
                              (take (- (count l-parens) step) l-parens)
                              (take (- (count r-parens) step) r-parens)
                              nest-child-parens
                              lazy-subvec-fn acc)))
           (gen-side-parens [x side-paren] (vec (repeat x side-paren)))
           (map-over
              [func1 func2 x acc]
              (let [l-parens (gen-side-parens x "(")
                    r-parens (gen-side-parens x ")")]
                (map (fn [step] (gen-parentheses step l-parens r-parens func1 func2 acc))
                     (range 1 (inc x)))))

           (left-combine [x] (map-over nest-child-parens lazy-subvec-fn x [])) ;; OK
           (right-combine [x] (map-over lazy-subvec-fn nest-child-parens x [])) ;; OK
           (all-dup-parens [x] (lazy-cat (left-combine x) (right-combine x)))
           (nest-until
              [x coll]
              (if (= x (count coll)) coll
                (recur x (lazy-cat (repeat 1 "(") coll (repeat 1 ")")))))
           (nest-colls
              [x coll-of-colls]
              (map #(nest-until (* 2 x) %) coll-of-colls))
           (stringify [coll] (reduce str coll))]

       (let [all-parens (all-dup-parens x)
             all-from-first (reduce
                               (fn [init x] (lazy-cat init (all-dup-parens x)))
                               all-parens (range 1 x))
             nested-cols (nest-colls x all-from-first)
             stringified-parens (map stringify nested-cols)]

         (apply hash-set stringified-parens))
      ))
  ;; overloaded func
  ([step l-parens r-parens l-func r-func acc]
      (if (nil? (first l-parens))
            acc
            (if (= 1 (count l-parens))
              (conj acc (first l-parens) (first r-parens))
              (lazy-cat (l-func step l-parens r-parens acc)
                        (r-func step l-parens r-parens acc))))))


(count (gen-parentheses 12))
(sort (gen-parentheses 1))
(sort (gen-parentheses 2))
(sort (gen-parentheses 3))
(sort (gen-parentheses 4))
(nth (sort (gen-parentheses 12)) 20)

;; ("()")
;; ("(())" "()()")
;; ("((()))" "(()())" "(())()" "()(())" "()()()")
;; ("(((())))" "((()()))" "((())())" "((()))()" "(()(()))" "(()()())" "(())(())" "()((()))" "()()()()")
;; 1. wrap + append to l + r() each previous
;; 2. generate incrementaly(reduce) each iterated-until no

(butlast (rest "((()))"))
(butlast (rest "(())"))
(butlast (rest "()"))
;; missing from cip's version of binary:
;; "(()) (())"

;; what he has over my vs:
;; "(() ()) ()" "() (() ())" "() () (())" "(() (()))" "((() ()))" "(()) () ()" "(() () ())"

;; (range 1 (inc x)) -> is saying take (-1 and include the total
(range 1 (inc 3)) ; (1 2 3)
;;

(sequential? (butlast (rest "(())"))) ; true

(defn generate-parens
  ([pairs] (generate-parens pairs pairs "" []))
  ([l-parens r-parens builder acc]
     (if (and (zero? l-parens) (zero? r-parens))
       (conj acc builder)
       (if (> l-parens 0)
         (generate-parens (dec l-parens) r-parens (.concat builder "(") acc)
         (if (> r-parens 0)
           (generate-parens l-parens (dec r-parens) (.concat builder ")") acc))))))

(generate-parens 1)
(map generate-parens (range 1 3))

(.concat "this" "that")
(.concat "" "(")
(shuffle ["(" "(" "(" ")" ")" ")"])

(let [{:keys [foo bar]} {:foo "claudiu"}] foo) ; "claudiu"
(let [{:keys [foo bar]} {:name "claudiu"}] foo) ; nil -> because there's NO :key named :foo in the bound map

(first (repeat 1 "("))
(subvec ["(" "(" "("] 0)

;; new idea/solution:
;; ("()")
;; ("(())" "()()")
;; ("((()))" "(()())" "(())()" "()(())" "()()()")
;; ("(((())))" "((()()))" "((())())" "((()))()" "(()(()))" "(()()())" "(())(())" "()((()))" "()()()()")
;; 1. start with 0 -> ""
;; 2. next to 1 -> ()
;; 3. wrap-fn + append-fn to l-side + r-side using () on each previous
;;         ()
;;   wrap: (())          | append: ()()
;;   wrap: ((())) (()()) | append: (())() ()(()) ()()()
