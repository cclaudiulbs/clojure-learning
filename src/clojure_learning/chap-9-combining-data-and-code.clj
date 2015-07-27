(use 'clojure.repl)
(in-ns 'chap9)
;; clojure namespacing always uses a 2 layer schema resolution for resolving the
;; bound symbols. The 1st schema resolution is the "prefix" schema to lookup.
;; the 2nd schema is the actual location of the var. what comes after prefix:
(use 'clojure.set)

;; namespaces always refer to the most updated value, and not the one where the
;; vars where reffered
(doc refer) ; adds a reference to the current namespace, so that we can refer
;; to any var contained in the referred namespace.

;; switching to a new namespace + creating if not already:
(in-ns 'chap9)
(def books [:clojure])

books ; [:clojure]

;; switching to a new namespace
(in-ns 'other-ns)

;; refer to chap9 ns
(refer 'chap9)

;; hey, i can use freely anything contained in other ns although i'm in other-ns right now
books ; [:clojure]

;; the [ns] macro is built to help: it gathers all the stuff from: java.lang + clojure.core
;; it should not be used inside the REPL! the [in-ns] should be used with the REPL instead.
;; the [ns] is intended to be used in the source code files, and not the REPL.

;; while the [in-ns] function handles the 'java.lang stuff, without creating any mappings
;; for functions and macros from 'clojure.core

(all-ns)