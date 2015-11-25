;;;;;;;;;;;;;;
;; Clojure-Programming:: All about ns 
;;;;;;;;;;;;;;
;; ns are mappings of symbols to vars, or java classes.

;; the [in-ns] construct will SWITCH to the new ns and if not existing will create it as necessary
;; the [def] constructs will create vars within the current ns, which is ALWAYS bound to *ns* special value
;; the clojure.core is bound to the user's default ns, so switching to other ns using [in-ns] will not have loaded the
;; clojure.core library
(in-ns 'clojure-learning.clj-programming-all-about-ns)
(some #{:foo} [:foo]) ;; ->unable to resolve symbol "some"
(clojure.core/some #{:foo} [:foo :bar]) ;; :foo -> works

;; the [refer] construct will add the mappings from the refered ns into our current ns
;; assumming the other(from which we refer) ns is already loaded!
(ns some-other-ns)
(clojure.core/refer 'clojure-learning.clj-programming-all-about-ns)
(clojure.core/refer 'clojure.core)
(filter (partial < 0) [-1 1 2 3]) ;; (1 2 3)

;; we can now use the core ns and the one created just as if they were defined locally in our some-other-ns ns.

;; the other two actors are more smart: [require] + [use]
;; they actually handle the LOADING of those "required" modules INTO THE CURRENT ns, and establish aliases optionally,
;; and "use" is doing one more thing (that require doesnt), it automatically triggers "refer" enabling direct
;; reference from the current ns to other ns, whitout abs path refernce. hence [use] is built on top of [require] +
;; [refer]

(clojure.set/union #{1 2} #{3 4})
(require 'clojure.set)

;; require provides a way to establish an alias for a specific ns
(require '[clojure.string :as str])
(str/join ",") ;; ","

;; as seen there's no need in specifying the full path name of the clojure.string ns
;; the alias is enabled by placing the ns into a vector, else 
(require '(clojure 
              string 
              [set :as s]))

(s/intersection #{1 2} #{2 4}) ;; #{2}

;; what happens if we want to use clojure.string lib and clojure.set lib. they both have a [join] operation
;; but we don't have any join func created into our ns, and we want to use more string.joins than set.joins
;; hence we want to refer to it directly, however it collides with the set.join
(use '(clojure [set :exclude (join) :as set]
               [string :only (join) :as str]))
(eval join) ;; ->str
(eval clojure.set/join)

;; (use '(clojure string set))

;; While clojure ns are simply mappings of clojure-symbols to vars, those vars can be also java classes
;; or interfaces. The [import] comes to the rescue. it takes the fully qualified class-name.
;; after importing the entities can be refferd directly via short-class-name.
(Date.) ;; -> compiler unable to resolve Date

(import 'java.util.Date 'java.text.SimpleDateFormat)
(Date.) ;; -> instance of curr date

(defn format-date [date date-pattern]
  (.format (SimpleDateFormat. date-pattern) date))

(parse-date (Date.) "dd/MM/yyyy") ;; 25/11/2015

;; remember: java.lang entities are automatically imported into each and every ns

(import '(java.util Collections Arrays))
;; same construct as in [refer] case

;; while [in-ns] is a pure function, [ns] is a macro, hence cleans up some quotes and adds more readability
;; when defining ns. Always use in prod-code [ns] over [in-ns] which should be more used in REPL env.

(ns some-foo-ns
  (:use (clojure [test] [core]))
  (:require (clojure [set :as set]
                      [string :as str])))

;; --> more readable than [in-ns] construct

;; There are several rules when defining ns:
;; 1. Use one file per namespace. -> dir-tree should correspond to ns-name
;; 2. Use underscores in filenames when namespaces contain dashes.
;; 3. Use declare to enable forward references
;;    Clojure does NOT support forward-references, hence the use of [declare] might be an option
;;    to enable fw-references! (they act like proxies)
;;    in this manner clojure acts as C declaring whatever you might fw-reference at the top of the file
;;    and then -use defn, or def to define those declared vars

;;;;;;;;;
;; classpath
;;;;;;;;;
;; once we defined the dependencies in project.clj file -> opening a REPL -> they will be automatically loaded
;; in the REPL classpath.

(defn find-max-consecs [head & tail]
  (reduce 
    (fn [v x] 
      (if (= x (inc ((comp last last) v)))
        (conj v x)
        (conj v [x])))
    [head] tail))

;; 1 2 4 6 7 8 10 12 -> 6 7 8
