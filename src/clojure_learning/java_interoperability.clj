(ns clojure-learning.java-interoperability
  (use (clojure test repl)))

;; Clojure being a hosted language simply reuses all of the work that is available from JVM 
;; Clojure is never interpreted! rather it is ALWAYS compiled down to efficient JVM bytecode, prior being run,
;; even in interactive settings such as the REPL! -> hence clojure is as java in terms of compilation and
;; runtime, in that 
;; 1. all of the clojure-functions compile down to Java-classes 
;; 2. Clojure defrecord + deftypes compile-down to java-classes having defined instance-members
;; 3. while defprotocols define bytecode-java-interfaces 


