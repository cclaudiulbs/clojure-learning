(use 'clojure.repl)

(defn gen-parens
  ([x]
   (letfn [(lazy-take-fn [step l-parens r-parens acc]
              (lazy-cat (take step l-parens) (take step r-parens)))
           (lazy-subvec-fn [step l-parens r-parens acc]
              (lazy-cat (gen-parens step (subvec l-parens step) (subvec r-parens step) lazy-take-fn lazy-subvec-fn acc)))
           (gen-side-parens [x side-paren] (vec (repeat x side-paren)))
           (map-over
              [l-parens r-parens lazy-take-fn lazy-subvec-fn parens-num acc]
              (map (fn [each-step]
                       (gen-parens each-step l-parens r-parens lazy-take-fn lazy-subvec-fn acc))
                   (range 1 (inc parens-num))))]

    (let [l-parens (gen-side-parens x "(")
          r-parens (gen-side-parens x ")")
          l-combinations (map-over l-parens r-parens lazy-take-fn lazy-subvec-fn x [])
          r-combinations (map-over l-parens r-parens lazy-subvec-fn lazy-take-fn x [])]

      (apply hash-set (concat l-combinations r-combinations))
      l-combinations)))

  ([step l-parens r-parens l-func r-func acc]
      (if (nil? (first l-parens))
            acc
            (if (zero? (dec (count l-parens)))
              (conj acc (first l-parens) (first r-parens))
              (lazy-cat (l-func step l-parens r-parens acc)
                        (r-func step l-parens r-parens acc)
              )))))


(gen-parens 4)

;; missing from cip's version of binary:
;; "(())(())"

;; what he has over my vs:
;; "(() ()) ()" "() (() ())" "() () (())" "(() (()))" "((() ()))" "(()) () ()" "(() () ())"

;; (range 1 (inc x)) -> is saying take (-1 and include the total
(range 1 (inc 3)) ; (1 2 3)
;;
