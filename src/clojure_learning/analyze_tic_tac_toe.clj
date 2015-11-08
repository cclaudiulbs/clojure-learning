(ns clojure_learning.analyze-tic-tac-toe
  (use clojure.repl)
  (use [clojure.test :refer :all]))

;; how it should work?
;;(= :x (__ [[:x :e :o]
;;           [:x :e :e]
;;           [:x :e :o]]))
;;
;;(= :o (__ [[:e :x :e]
;;           [:o :o :o]
;;           [:x :e :x]]))
;;
;;(= :x (__ [[:x :e :e]
;;           [:o :x :e]
;;           [:o :e :x]]))

(map #(apply vector [% %2 %3]) [:x :e :e] [:o :x :e] [:o :e :x])
;; ([:x :o :o] [:e :x :e] [:e :e :x])

(defn analyze [table] 
  (letfn [(winner? [ [head & tail] ]
            (when (and (not= head :e) (every? #{head} tail)) head))
          (map-cols-from [[xs ys zs]]
            (map #(apply vector [% %2 %3]) xs ys zs))
          (map-diags-from [[[fhead _ ftail]
                           [_ mid _]
                           [lhead _ ltail]]]
            (apply vector [[fhead mid ltail] [lhead mid ftail]]))
          (combine [table]
            (reduce conj table (lazy-cat (map-cols-from table)
                                         (map-diags-from table))))]
    (when-let [line (seq (filter winner? (combine table)))] 
      ((comp first last) line))))

(deftest test-analyze-tic-tac-toe
  (testing "tic-tac-toe func should analyze & return true the winner of the game"
    (is (= :x (analyze [[:e :o :x] 
                        [:o :e :o] 
                        [:x :x :x]]))) ;; true

    (is (= :o (analyze [[:e :o :x] 
                        [:o :o :o] 
                        [:x :e :x]]))) ;; true

    (is (= nil (analyze [[:e :o :x] 
                         [:o :o :e] 
                         [:x :e :x]]))) ;; true

    (is (= nil (analyze [[:e :e :e] 
                         [:e :e :e] 
                         [:e :e :e]]))) ;; true

  ))

(analyze [[:e :e :e] 
          [:e :e :e] 
          [:e :e :e]]) ;; --> [:e :e :e]

(analyze  [[:e :o :x] 
           [:o :e :o] 
           [:x :x :x]]) ;; true

;; some neat refactoring of the current solution:: using core-[juxt] to combine the results of diags + cols
(defn analyze [table] 
  (letfn [(winner? [ [head & tail] ]
            (when (and (not= head :e) (every? #{head} tail)) head))
          (map-cols [[xs ys zs]]
            (map #(apply vector [% %2 %3]) xs ys zs))
          (map-diags [[[fhead _ ftail]  ;; pure destructuring
                       [_ mid _]
                       [lhead _ ltail]]]
            (apply vector [[fhead mid ltail] [lhead mid ftail]]))
          (combine [table]
            (reduce conj table ((juxt map-cols map-diags) table)))]
    (when-let [line (seq (filter winner? (combine table)))] 
      ((comp first last) line))))

;; demo juxt
((juxt (partial conj [0]) (partial cons 3)) [1 2]) ;; [ [0 [1 2]]  (3 1 2) ]

;; other user solution:
(fn [board]
    (->> (concat board
                 (apply map vector board)
                 (let [[[a _ _] [_ b _] [_ _ c]] board] [[a b c]])
                 (let [[[_ _ a] [_ b _] [c _ _]] board] [[a b c]]))
     (some #{[:x :x :x] [:o :o :o]})
     first))
