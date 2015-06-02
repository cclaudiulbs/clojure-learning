(ns cclaudiu)

*clojure-version*

(use 'clojure.repl)
(use 'clojure.java.javadoc)

(source defn)

(def a-vector [1 2 3])
(conj a-vector (peek [4 5 6]))

(doc conj)

(source defn)

(def sum-of-nums (fn [x] (fn [y] (+ x y))))
(let [result ((sum-of-nums 1) 5)] result)

(defn traverse-collection
  [collection]
  (if (not (nil? (first collection)))
      (do
        (println (str "Iterating each num: " (first collection)))
        (recur (rest collection)))))
;; invoking::
(traverse-collection [1 2 3 4 5 6])

(defn traverse-container
  [[head-arg & tail-args]]
  (if (not (nil? head-arg))
      (do
        (println (str "Iterating through the container: " head-arg))
        (recur tail-args )
      )
  )
)
;; test the destructuring of args
(traverse-container ["cclaudiu" "gigi" "coca"])

(rest [1 2 3 4])
(first [1 2 3 4])
(peek [1 2 3 4])
(take 3 [1 2 3 4])

(def a-test {:some-test "some-test-val"})
(defn check-nil
  [a-map]
  (if (not (nil? (:some-test a-map)))
    (println "NOT NIL"))
)
(check-nil a-test)