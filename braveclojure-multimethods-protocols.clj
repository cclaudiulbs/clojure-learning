(in-ns 'braveclojure)
(use '[clojure.core])
(use 'clojure.repl)

;;;;;;;;;;;;;;;;;;;;
;; The more a programming language lets you think and write in terms of abstractions,
;; the more productive you will be.
;; Usually an abstraction is a name for a collection of things/operations. The SEQUENCE ABSTRACTION
;; provides some funcs: [first] + [rest] that clojure vectors are implementing -> hence
;; vectors implement this Sequence Abstraction.
;;
;; The main way that we achieve abstraction in Clojure is through polymorphism,
;; or the association of an operation name with more than one algorithm.
;; Take for instance the [conj] function, which for vectors behaves by appending/removing
;; from top of the stack(last), while for lists the [conj] pushes/removes from the beginning of
;; the container. But hey, they both share the same function name, the only thing is that the
;; behavior is different. both vectors and lists implement the same abstraction:
;; "add an element to the business end of this data structure."

;; each multi-method should have defined a "dispatching function" that takes dispatches based on
;; the type/value of its arg, and based on the result of its computation -> which method to use
;; based on the produced value of the dispatching-function.

(defmulti writer-strategy
  (fn dispaching-fn [dispaching-context] (:dispatch dispaching-context)))

(defmethod writer-strategy :console
  [context-map]
  (println (str "going to print something on console..." (:what context-map))))
(defmethod writer-strategy :memory
  [context-map]
  (format "going to log some' in memory...%1s" (:what context-map)))

(writer-strategy {:dispatch :console, :what "some console logging"})
(writer-strategy {:dispatch :memory, :what "some memory filling"})

;; rules here: the dispaching function normally takes the same argument-context that every multi-method
;; will take. this is just a decision-taker; using our map-context we lookup using the :dispatch key Func
;; the corresponding value. now when called the multimethod is mapped with the value from this context
;; and hence the lookup by :dispatch key identifies the map-context value. founds that there's a multimethod
;; declared with that dispatch-context-value and calls it.
;; it's good to send the context as a map structure.

;; whenever calls the multimethod -> run the dispatching function -> which identifies the dispachee method
;; to delegate to.

;; the multi-method takes the name of the method + immediatelly the dispach token.

;; we can define multimethod for nil for instance:
(defmethod writer-strategy nil
  [context-map]
  (do
    (println "going to print to the console the default value")
    (writer-strategy {:dispatch :console :what (:what context-map)})))

;; as seen ALL methods share the same NAME. what varies is the dispatched value, that the dispatching function
;; resolves at runtime. The nil multi-method dispatches further on the :console method

(writer-strategy {:dispatch nil :what "nill method was dispatched..."})

;; we can also dispatch by other things, such as Types:
(defmulti formatter
  (fn formatter-dispatcher
    [context]
    (:type context)))

;; i like to pass a context-map since this is a small principle of achieving decoupling
(defmethod formatter java.lang.String
  [context]
  (str "String is dispatched..."))

(formatter {:type java.lang.String}) ; "Stirng is dispatched..."

;; one important thing as convention, is that the dispached-value should NOT contain any special dash
;; or other special chars

