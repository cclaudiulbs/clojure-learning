(ns pairwise
  (:use [clojure.repl]))

;; first draft:
(defn pairwise-checker?
  [stringified-expr]
    (letfn [(open-paran? [paran-char] (re-seq #"\[|\(|\{" (str paran-char)))
            (close-paran? [paran-char] (re-seq #"\)|\]|\}" (str paran-char)))
            (pair? [open-paran close-paran] (re-seq #"\(\)|\[\]|\{\}" (str open-paran close-paran)))
            (balanced-brackets? [[open-parans closing-parans :as all]]
               (if (= (count open-parans) (count closing-parans))
                 all
                 :not))
            (parse-expression [stringified-expr]
              (loop [[head & tail] (seq stringified-expr)
                      opening-brackets []
                      closing-brackets []]
                (if (nil? head)
                  [opening-brackets closing-brackets]
                  (if-let [closing (close-paran? head)]
                    (if (pair? (last opening-brackets) closing)
                       (recur tail (vec (butlast opening-brackets)) closing-brackets)
                       (recur tail opening-brackets (conj closing-brackets (str head))))
                    (if-let [opening (open-paran? head)]
                       (recur tail (conj opening-brackets (str head)) closing-brackets)
                       (recur tail opening-brackets closing-brackets))))))]
    (balanced-brackets? (parse-expression stringified-expr))))


;; in action:
(pairwise-checker? "class Test {
      public static void main(String[] args) {
        System.out.println(\"Hello world.\");
      }
    }") ;; true [5 5]

(pairwise-checker? "(start, end]") ;; [1 1]


;; work:
(defn pair? [a b]
  (re-seq #"\(\)|\[\]|\{\}" (str a b)))
(pair? "(" ")") ;; ("()")
(pair? "(" "}") ;; nil

;; second draft:
(defn pairwise-checker?
  [stringified-expr]
    (letfn [(open-paran? [paran-char] (re-seq #"\[|\(|\{" (str paran-char)))
            (close-paran? [paran-char] (re-seq #"\)|\]|\}" (str paran-char)))
            (pair? [open-paran close-paran] (re-seq #"\(\)|\[\]|\{\}" (str open-paran close-paran)))
            (balanced-brackets? [[open-parans closing-parans :as all]]
               (if (= (count open-parans) (count closing-parans))
                 all
                 false))
            (parse-expression [stringified-expr]
              (loop [[head & tail] (seq stringified-expr)
                      opening-brackets []
                      closing-brackets []]
                (if (nil? head)
                  [opening-brackets closing-brackets]
                  (if-let [closing (close-paran? head)]  ;; closing -> (")") -> take the first
                    (if (pair? (last opening-brackets) (first closing))
                       (recur tail (vec (butlast opening-brackets)) closing-brackets)
                       (recur tail opening-brackets (conj closing-brackets head)))
                    (if-let [opening (open-paran? head)]
                       (recur tail (conj opening-brackets head) closing-brackets)
                       (recur tail opening-brackets closing-brackets))))))]

    (balanced-brackets? (parse-expression stringified-expr))))

(pairwise-checker? "class Test {
      public static void main(String[] args) {
        System.out.println(\"Hello world.\");
      }
    }") ;; [[] []]

(pairwise-checker? "(start, end]") ;; [ [\(], [\]] ]
(pairwise-checker? "[ { ] }")      ;; [ [\[], [\]] ]
(pairwise-checker? "())")          ;; false -> OK
(pairwise-checker? "[")            ;; false -> OK
(pairwise-checker? "([]([(()){()}(()(()))(([[]]({}([)))())]((((()()))))))") ;; false -> OK
(pairwise-checker? "([]([(()){()}(()(()))(([[]]({}()))())]((((()()))))))")  ;; [[] []]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Third draft and the final version: in the 2nd version we had [[] []] which means are balanced,
;; we also have false for not couting the same number of parans: [[ ], but we also had
;; [ [\(], [\]] ] -> which means they are not balanced at all. this version handles all the cases,
(defn pairwise-checker?
  [stringified-expr]
    (letfn [(open-paran? [paran-char] (re-seq #"\[|\(|\{" (str paran-char)))
            (close-paran? [paran-char] (re-seq #"\)|\]|\}" (str paran-char)))
            (pair? [open-paran close-paran] (re-seq #"\(\)|\[\]|\{\}" (str open-paran close-paran)))
            (balanced-brackets? [[open-parans closing-parans :as all]]
               (if (= (count open-parans) (count closing-parans))       ;; equal? number of parans
                  (every? empty? all)                  ;; check if all open/close colls are empty? else NOT balanced!
                  false))
            (parse-expression [stringified-expr]
              (loop [[head & tail] (seq stringified-expr)
                      opening-brackets []
                      closing-brackets []]
                (if (nil? head)
                  [opening-brackets closing-brackets]
                  (if-let [closing-seq (close-paran? head)]  ;; closing -> (")") -> take the first
                    (if (pair? (last opening-brackets) (first closing-seq))
                       (recur tail (vec (butlast opening-brackets)) closing-brackets)
                       (recur tail opening-brackets (conj closing-brackets head)))
                    (if-let [opening (open-paran? head)]
                       (recur tail (conj opening-brackets head) closing-brackets)
                       (recur tail opening-brackets closing-brackets))))))]
    (balanced-brackets? (parse-expression stringified-expr))))

;; in action 3rd version and the final:
(pairwise-checker? "(start, end]") ;; false
(pairwise-checker? "[ { ] }")      ;; false
(pairwise-checker? "())")          ;; false -> OK
(pairwise-checker? "[")            ;; false -> OK
(pairwise-checker? "([]([(()){()}(()(()))(([[]]({}([)))())]((((()()))))))") ;; false -> OK
(pairwise-checker? "([]([(()){()}(()(()))(([[]]({}()))())]((((()()))))))")  ;; false
