(ns clojure-learning.find-dups
  (use clojure.repl)
  (require [clojure.test :refer :all]
           [clojure.core :refer :all]))

(defn find-dups [xs]
  (letfn [(vec-contains [xs x]
            (some #(= x %) xs))]
    (last
      (reduce
         (fn [[acc dups] x]
            (if (vec-contains acc x)
              [acc (conj dups x)]
              [(conj acc x) dups]))
         [[] []] xs))))

(deftest test-find-dups
  (testing "if find-dups func returns the duplicated items from a coll"
    (is (= [1 2] (find-dups [1 2 3 1 2]))) ;; true
  ))


(some #(= % "foo") ["foo" "bar"]) ;; true
(get ["foo" "bar"] "foo")  ;; nil
(get ["foo" "bar"] 0)      ;; foo

(contains? {:foo ":foo"} :foo);; true
(contains? [:foo :bar] :foo)  ;; false
(contains? [:foo :bar] 0)  ;; true

(zipmap [:foo :bar] [1 2]) ;; {:bar 2, :foo 1}

