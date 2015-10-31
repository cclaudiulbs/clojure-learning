(ns clojure-learning.core-test
  (:require [clojure.test :refer :all]
            [clojure-learning.core :refer :all]))


(deftest a-test
  (testing "true"
    (is (= 0 0)))
  (testing "demo"
    (is (= '(1 2 3) (map-rec inc '(0 1 2))))))

(a-test)
