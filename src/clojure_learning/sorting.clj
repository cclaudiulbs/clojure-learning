(ns clojure-learning.sorting
  (use clojure.repl)
  (require clojure.string))

(doc clojure.string)
(dir clojure.string)

[4 3 1 2 9]
(def ul [3 4 1 2 9])

(defn- bubble-sort [[head sec & tail :as init] sorted-list decreaser]
  (if (zero? decreaser) init
    (if (nil? sec)
      (recur (conj sorted-list head) [] (dec decreaser))
      (if (< head sec)
        (recur (cons sec tail) (conj sorted-list head) decreaser)
        (recur (cons sec (cons head tail)) sorted-list decreaser)))))

(def ul2 [4 3 1 7 2 9 8 5])

(bubble-sort ul [] (dec (count ul2)))
(buble-sort ul [] (dec (count ul)))

(defn bubble-sort [ul]
  (letfn [(bubble-sort-priv
            [[head sec & tail :as init] sorted-list decreaser]
            (if (zero? decreaser) init
              (if (nil? sec)
                (recur (conj sorted-list head) [] (dec decreaser))
                (if (< head sec)
                  (recur (cons sec tail) (conj sorted-list head) decreaser)
                  (recur (cons sec (cons head tail)) sorted-list decreaser)))))]
    
    (bubble-sort-priv ul [] (dec (count ul)))))

(bubble-sort ul2)
(bubble-sort (reverse (take 1000 (range))))
(sort (reverse (take 1000 (range))))


