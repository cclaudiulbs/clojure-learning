(ns pairwise
  (:use [clojure.repl]))

(defn pairwise-checker?
  [stringified-expr]
    (let [open-paran? #(re-seq #"\[|\(|\{" %)
          close-paran? #(re-seq #"\)|\]|\}" %)
          pair? #(re-seq #"\(\)|\[\]|\{\}" (str % %2))]
      (loop [[head & tail] (seq stringified-expr)
             opening-brackets []
             closing-brackets []]
        (if (nil? head)
          (= (count opening-brackets) (count closing-brackets))
          (if-let [closing (close-paran? (str head))]
            (if (pair? (last opening-brackets) closing)
               (recur tail (vec (butlast opening-brackets)) closing-brackets)
               (recur tail opening-brackets (conj closing-brackets (str head))))
            (if-let [opening (open-paran? (str head))]
               (recur tail (conj opening-brackets (str head)) closing-brackets)))))))

(pairwise-checker? "class Test {
      public static void main(String[] args) {
        System.out.println(\"Hello world.\");
      }
    }")

(doc re-seq)
(re-seq #"\[|\(" "[")

      (int \{)
      (int \})


(defn pair? [a b]
  (re-seq #"\(\)|\[\]|\{\}" (str a b)))
(pair? "(" ")") ;; ("()")
(pair? "(" "}") ;; nil