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

(all-ns) ;; -> list all ns

;; the use of [defn- func] denotes the fact that the function is private to the namespace
;; in which it was defined.
;; hiphens must be replaced by underscores when naming a source file.

;; the [ns] macro provides fined resource-management, through the use of:
;; (ns some-ns (:use :require :load :import)) directives.
;; the :use directive says use everything from that ns, without the need for prefixing the things.
;; the :as defines an alias for the given things.


(defrecord TreeNode [val l-branch r-branch])
(def customer (->TreeNode 4 nil nil))

(str (:val customer)) ; "cclaudiu"
(keys customer)       ; (:val :l-branch :r-branch)

(defn xconj
  [node val]
  (if (nil? node)
    (->TreeNode val nil nil)
    (if (< val (:val node))
      (->TreeNode (:val node) (xconj (:l-branch node) val) (:r-branch node))
      (->TreeNode (:val node) (:l-branch node) (xconj (:r-branch node) val)))))

(def job (xconj customer 2))
job                      ;; TreeNode{:val 4,
;;    :l-branch                                               :r-branch nil}
;; #user.TreeNode{:val 2, :l-branch nil, :r-branch nil}


;; listing a binary tree:
(defn xseq [tree]
  (when tree
    (lazy-cat (xseq (:l-branch tree)) [(:val tree)] (xseq (:r-branch tree)))))
(xseq job) ; (2 4)

