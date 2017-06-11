(ns clojure-learning.next-missing-from-consecs
  (use (clojure [repl] [test])))

;; abcdxysabc -> d is the immediate next missing element from the char-seq
;; create a function which finds it
(defn find-next-missing-from-consecs [char-seq]
  (letfn [(recur-find-next-missing [[head & tail] is-conseq]
            (if (nil? head) nil                    ;; that means we did not find an early result -> nil
              (if (find-in-seq tail head)
                (recur tail true)
                (if is-conseq 
                  head
                  (recur tail false)))))
          (find-in-seq [a-seq searched] 
            (some #{searched} a-seq))]
  (recur-find-next-missing char-seq false)))

(find-next-missing-from-consecs "abcdxyzabc")       ;; \d
(find-next-missing-from-consecs "fabcdxyzabcl")     ;; \d
(find-next-missing-from-consecs "fghabcdfgabc")     ;; \h
(find-next-missing-from-consecs "abc")              ;; nil

(deftest test-find-next-missing-from-consecs
  (testing "func that finds the next missing element from consecutive sequence"
    (is (= \d (find-next-missing-from-consecs "abcdxyzabc")))
    (is (= \d (find-next-missing-from-consecs "fabcdxyzabcl")))
    (is (= \h (find-next-missing-from-consecs "fghabcdfgabc")))
    (is (= nil (find-next-missing-from-consecs "abc")))  ;; true
))
