(ns clojure-learning.java-interoperability
  (use (clojure test repl)))

;; Clojure being a hosted language simply reuses all of the work that is available from JVM 
;; Clojure is never interpreted! rather it is ALWAYS compiled down to efficient JVM bytecode, prior being run,
;; even in interactive settings such as the REPL! -> hence clojure is as java in terms of compilation and
;; runtime, in that 
;; 1. all of the clojure-functions compile down to Java-classes 
;; 2. Clojure defrecord + deftypes compile-down to java-classes having defined instance-members
;; 3. while defprotocols define bytecode-java-interfaces 

;; While Clojure is hosted on the JVM, it does not inherit Java’s checked exceptions
;; Checked exceptions are a fabrication of the Java compiler, and simply don’t exist in the JVM at runtime

;;So, we can (for example) create temporary files using a method that specifies that it can throw java.io.IOException, a checked exception:
 ;; (File/createTempFile "clojureTempFile" ".txt")
;; outside the scope of any try/catch expression, and without any corollary to Java’s throws declaration to indicate that that code may throw an IOException. This obviously
;; allows Clojure to be more concise when calling methods with checked exception declarations
;; compared to the equivalent Java code.

;; Clojure provides an equivalent with-open form that ensures that resources are closed prior to control exiting its scope.
;; (try-with-resources)

;; Type hints on function arguments or returns are not signature declarations: they do not affect the types that a function can accept or return
(defn foo [^String x] (.toUpperCase x))
(foo "cla") ;; CLA

;; Using Type hints is only needed for interoperabilities, and is NOT considered a Type Signature! it will ONLY help
;; compiler not going through java-reflection to find the class-type.

;; You’ll generally find that array-handling is one of the rare domains where Clojure is more verbose than Java out of the box

;; Array working:
(into-array ["a" "b" "c"])  ;; -> create an array from a collection
(make-array String 3)       ;; -> create an array of type String with size 3
(aget (into-array [:foo :bar]) 1) ;; -> :bar, access the item from the array by index referring

(def names (make-array String 10))
(aset names 0 "cclaudiu")
(println (aget names 0))  ;; cclaudiu

;; Arrays are supported by Clojure’s sequence abstraction,21 so you can use them just like any other seqable collection:
