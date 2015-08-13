;; new idea/solution:
;; ("()")
;; ("(())" "()()")
;; ("((()))" "(()())" "(())()" "()(())" "()()()")
;; ("(((())))" "((()()))" "((())())" "((()))()" "(()(()))" "(()()())" "(())(())" "()((()))" "()()()()")
;; 1. start with 0 -> [""]
;; 2. next to 1 -> ()
;; 3. wrap-fn + append-fn to l-side + r-side using single-parens () on each previous reduced-res
;;         ()
;;   wrap: (())          | append: ()()
;;   wrap: ((())) (()()) | append: (())() ()(()) ()()()
(defn gen-parens
  ([x] (apply hash-set (last (gen-parens x []))))
  ([x acc]
   (letfn [(nest-node [node-str] (apply str (conj (vec (conj (list node-str) "(")) ")")))
           (l-append [node-str] (apply str (conj (list node-str) ")" "(")))
           (r-append [node-str] (apply str (conj (vec node-str) "(" ")")))]
     (if (zero? x)
       (conj acc (vector ""))
       (reduce
          (fn[acc _]
            (if-let [tail (last acc)]
              (conj acc
                      (vec  ;; force realization
                        (lazy-cat
                          (map nest-node tail)
                          (map l-append tail)
                          (map r-append tail))))))
          (vector (vector "()")) (range 1 x))))))

(gen-parens 0) ;     #{""}
(gen-parens 1) ;     #{"()"}
(gen-parens 2) ;     #{"(())" "()()"}
(sort (gen-parens 3)); ("((()))" "(()())" "(())()" "()(())" "()()()")
(sort (gen-parens 4))
(count (gen-parens 10)) ; 4181 -> should be 16796?? from where???

;; 1st test:
(map (fn [n] (gen-parens n)) [0 1 2]) ; (#{""} #{"()"} #{"(())" "()()"}) -> OK

(= [#{""} #{"()"} #{"()()" "(())"}] (map (fn [n] (gen-parens n)) [0 1 2])) ; true on my evaluator
;; 4clojure evaluator sucks some times...
;; as it fails with: java.lang.IllegalArgumentException: Duplicate key: ()()

;; 3rd test:
(= (nth (sort (gen-parens 12)) 5000) "(((((()()()()()))))(()))") ; false -> fails the test
(nth (sort (gen-parens 12)) 5000) ;  "(((()((()))())())()()())" -> my response


;; testing the funcs:
(defn nest-node [node-str] (apply str (conj (vec (conj (list node-str) "(")) ")")))
(map nest-node ["()"])

(defn l-append [node-str] (apply str (conj (list node-str) ")" "(")))
(map l-append ["(())"])

(defn r-append [node-str] (reduce str (conj (vec node-str) "(" ")")))
(map r-append ["((()))"])

(conj []
  (vec
  (lazy-cat
    (map nest-node (last [["(())"]]))
    (map l-append (last [["(())"]]))
    (map r-append (last [["(())"]])))))