(ns romans
  (use clojure.repl))

;; first draft:: implementing the logic for roman-numerals
(defn from-romans [romans]
  (let [roman-map {\I 1 \V 5 \X 10 \L 50 \C 100 \D 500 \M 1000}
        roman-first (roman-map (first romans))]
    (reduce (fn [acc k]
              (if-let [curr-higher (and (< (last acc) (roman-map k)) (roman-map k))]
                (conj (vec (butlast acc)) (- curr-higher (last acc)))
                (conj acc (roman-map k))))
            [roman-first] (rest romans))))

(from-romans "XX") ;; [10 10]
(from-romans "XVII") ;; [10 5 1 1]
(from-romans "XIX")  ;; [10 9]
(from-romans "CXL")  ;; [100 40]

;; 2nd draft reducing the vector of numbers... :)
(defn from-romans [romans]
  (let [roman-map {\I 1 \V 5 \X 10 \L 50 \C 100 \D 500 \M 1000}
        roman-first (roman-map (first romans))]
    (apply +
      (reduce (fn [acc k]
                (if-let [curr-higher (and (< (last acc) (roman-map k)) (roman-map k))]
                  (conj (vec (butlast acc)) (- curr-higher (last acc)))
                  (conj acc (roman-map k))))
              [roman-first] (rest romans)))))

;; in action:
(from-romans "I")
(from-romans "XXX")
(from-romans "IV")
(from-romans "CXL")
(from-romans "DCCCXXVII")
(from-romans "MMMCMXCIX")
(from-romans "XLVIII")

;; another version maybe small refactorings...
(defn from-romans [romans]
  (let [roman-map {\I 1 \V 5 \X 10 \L 50 \C 100 \D 500 \M 1000}]
    (apply +
      (reduce (fn [acc k]
                (if (and (not (nil? (last acc))) (< (last acc) (roman-map k)))
                  (conj (vec (butlast acc)) (- (roman-map k) (last acc)))
                  (conj acc (roman-map k))))
              [] romans))))

