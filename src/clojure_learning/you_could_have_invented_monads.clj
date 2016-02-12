(ns clojure-learning.you-could-have-invented-monads
  (use (clojure [repl] [test])))

;; say we have 2 functions: double-inc & double-dec, which are having the following signature:
;; double-inc:: long -> long
;; double-dec:: long -> long
(defn double-inc [x] ((comp inc inc) x))
(defn double-dec [x] ((comp dec dec) x))

;; say we want to augment the return type to contain also a string, as a debuggable information
;; "double-inc was called";
(double-inc 2)
