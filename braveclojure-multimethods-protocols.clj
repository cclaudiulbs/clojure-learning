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

;;;;;;;;;;;;
;; Protocols
;; are specially built for Type dispatching. The same thing can be achieved using multimethods as well.
;; being built into the language -> language support
;; whereas multimethods dispatch based on the returned func-dispatcher value, a protocol always
;; dispatches based on the Type of the first argument.
;; whereas a multimethod is one polymorphic operation, a protocol defines a collection of abstract
;; polymorphic operations.
;; Being a protocol it makes NO sense to provide implementation, in the exposed protocol.
;; It makes sense, on the other side, to Types to implement this protocol -> therefore
;; Types provide the implementation for the protocol polymorphic operations.
;; So a protocol has a [name, optional-doc-string, methods-signatures-only]
(defprotocol split-by-protocol
  "optional doc-string for salute protocol"
  (split [x] "optional method doc-string:: split by whitespace")
  (split-by [x] [x splitter] "split by a given splitter"))

;; a protocol cannot make use of the [rest] varargs. an exception is triggered if the case.
;; therefore a protocol defines a fixed set of arguments. In the split-by signature
;; this method is declared so that it can take 1 or 2 arguments respectively.

;; as said it makes no sense for the protocol to implement itself. it is the responsibility
;; of the Types to implement a protocol.
;; the magnific side is that in clojure we can augment an existing compiled Type at runtime
;; with additional polymorphic behavior. This cannot be done in Java not in 50 years from now...

;; Protocols are NOT attached to a Type; we're coming from an OOP world, where each method must
;; coexist in the context of a Type. Well this is Clojure world, and in Clojure the protocol's
;; polymorphic behavior is attached on the namespaces. If there are 2 messages with the same
;; name, for them to be used to augment a Type at runtime, they must exist in SEPARATED namespaces.
;; this is the only restrictions. Since any declared method is bounded to the namespace
;; where it was created:

(in-ns 'other-ns)
(class split-by-protocol) ; -> !!!RuntimeException: unable to resolve symbol
(doc refer) ; -> [in-ns] does NOT load the 'clojure.core funcs!!! nor 'clojure.repl
(require 'clojure.core)
(require 'clojure.repl) ; -> now's better
;; back to our protocols cake ;)
(clojure.core/refer 'braveclojure)

(for [each (all-ns)] each)

(class split-by-protocol) ; -> persistent-array-map

;; hence namespaces being first-class in clojure, the protocol's methods are bounded to the
;; namespaces and NOT Types.
;; "when we define protocls we define abstractions" i like this :)

;; to make an existing Type extend a protocol there's the simple syntax sugar:
(extend-type java.lang.String
  split-by-protocol
  (split [x] (clojure.string/split x #""))
  (split-by
     ([x] (split x))
     ([x splitter] (clojure.string/split x splitter))))

;; if a default implementation is desired, we can just extend the java.lang.Object Type...

(split-by "cclaudiu")      ; ["c" "c" "l" "a" "u" "d" "i" "u"]
(split-by "cclaudiu" #"a") ; ["ccl" "udiu"]

;; NICeeeeeeeeeeeeeeeeeeeeee

;; instead of using the [extend-type] macro all the time, we can just use the
;; [extend-protocol] and list the Types we want our protocol to augment together
;; with the implementations for the protocol, as in the [extend-type] scenario.

(extend-protocol split-by-protocol
  java.lang.String
    (split
       [x] (clojure.string/split x #""))
    (split-by
        ([x] (split x))       ;; delegate
        ([x split-pattern] (clojure.string/split x split-pattern)))

  java.lang.Object
    (split [x] (clojure.string/split (.toString x) #""))
    (split-by
       ([x] (split x))
       ([x split-pattern] (clojure.string/split (.toString x) split-pattern))))

;; all in one: now we have a default Object protocol extended, and the specific String like.

;; testing the default-object
(split (Integer. 323)) ; ["3" "2" "3"]
(split "some") ; ["s" "o" "m" "e"]
(split-by "clojure-is-awesome" #"-") ; ["clojure" "is" "awesome"]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Records
;; in clojure are like custom-maps data-types. they(anaphora) associate keys with values
;; and hence we can lookup for values by record-labels. The difference to maps comes
;; in the form of "statically" defining fixed labels for values. While in maps the
;; keys can vary, in records the labels are locked down upon record-creation.
;; (think of class-fields)
;; The other difference is that records can implement protocols.
;; Vector-syntax is again used(as in the let-bindings or function-args, or loop-bindings
;; or for-iterations) for enclosing the labels.
;; Firsts things first we DEFINE the record using: [defrecord] macro:
(defrecord Programmer [name company favourite-language])

;; once we defined the Record, we MUST instantiate it somehow and "pair" the labels with values.
;; one downside is that records are using formal label-positioning when binding values.
;; this is because under-the-hood, records are java classes with no behavior:

;; one way to instantiate a Record:
(Programmer. "cclaudiu" "sap" "clojure")
; #braveclojure.Programmer{:name "cclaudiu", :company "sap", :favourite-language "clojure"}

;; --> Records can be instantiated as plain java Types using the "dot" special form

;; another way is to use the ARROW notation:
(->Programmer "cclaudiu" "sap" "clojure")
; #braveclojure.Programmer{:name "cclaudiu", :company "sap", :favourite-language "clojure"}

;; and the savior, providing named-labels support, when it comes to not enumerate the
;; records-values and rely on the positional arguments is the:
(map->Programmer {:company "sap" :name "cclaudiu" :favourite-language "clojure"})
;; nice :)

;; in the last 2 examples, when declaring the record definition automatically 2 factory funcs
;; are created in the current-ns. the factory funcs are: ->Programmer and map->Programmer
;; we can then reuse them to create instances of the declared record.

;; records can be looked-up using 2 options:
;; -> use map like lookups: record-labels as functions to lookup the record.
;;       (the label-key is a function that looksup for itself in the provided record as argument)
;; -> use java interop syntax
(def cc (->Programmer "cclaudiu" "sap" "clojure"))
(:name cc) ; "cclaudiu"

;; (cc :name) ; -> ClassCastException:: Programmer record cannot be cast to IFn

(.name cc) ; "cclaudiu"

;; when testing for equality of records, clojure tests for values offcourse, and should have the
;; same types.

;; another powerfull thing in the records-abstraction is that any function that works on the
;; core [map] works on the Records.
(map-indexed vector cc)
;; ([0 [:name "cclaudiu"]] [1 [:company "sap"]] [2 [:favourite-language "clojure"]])

(sort-by first > (map-indexed vector cc))
;; ([2 [:favourite-language "clojure"]] [1 [:company "sap"]] [0 [:name "cclaudiu"]])

(sort-by first (comp - (comparator >)) (map-indexed vector cc))
;; ([0 [:name "cclaudiu"]] [1 [:company "sap"]] [2 [:favourite-language "clojure"]])

;; note: every predicate that takes 2 arguments is an function instance of the Comparator.
;; normaly every predicate it translated under-the-hood into a comparator.
;; that's why the last function inverses the sign using: comp - (comparator >)
;; (comparator >) is returning a comparator func that returns
;;   -> 1(for first > second)
;;   -> -1(for first < second)
;;   -> 0 (for equality)
;; we are composing the comparator, that says: give me the record-data, mapped as
;; a vector having the first item the index, sorted descendantly, but by
;; composing the comparaotr func with a [minus] func we're reversing the order.

;; [assoc] works also for records
(assoc cc :address "titan")
;; #braveclojure.Programmer{:name "cclaudiu", :company "sap", :favourite-language "clojure", :address "titan"}

;; keep in mind that records are IMMUTABLE, as maps and other structures! so we should say like:
(def cc-with-address (assoc cc :address "titan"))
(class cc-with-address) ; -> braveclojure.Programmer

;; the downside comes when dissoc iating stuff from records -> a new map-structure is returned instead
;; of the record type:
(class (dissoc cc :company)) ; -> PersistentArrayMap... NO MORE a Programmer Type!!


;; records can be augmented with protocols, that is can implement protocols.
;; and in the same context, while implementing the protocols you have access to the record's fields.
(defprotocol rec-demo-protocol
  "just to illustrate the power of records and protocols for abstracting things"
  (print-everything [x] "will defintelly print everything"))

(defrecord car-rec [brand car-type]
  rec-demo-protocol
    (print-everything
       [that]
       (println (format "car brand: %1s, car-type: %2s" (:brand that) (:car-type that)))))

;; defrecord record-name [labels]
;;    implemented-protocol (protocol-funcs-impl)

(def bmw (->car-rec "bmw" "sport"))
(print-everything bmw) ; -> will print:  car brand: bmw, car-type: sport
(.print-everything bmw) ; -> same-thing, but as java interop syntactic sugar. print returns nil ;)

;; we can augment any record at a later point using: [extend-type] or [extend-protocol] for more records

;; one more thing to mention, is that records provide a faster lookup and performance than maps
;; so when to use maps and when records? whenever we want to express the domain-logic into meaningfull
;; terms that will have business value use records over maps.
;; "your code will communicate its purpose better if you give a name to the concept you're trying to model"

;; and this closes enough of clojure: multi-methods + protocols + records you want to know in the wild
;; for further advanced reading: proxy, deftype will suite. but not my case for the moment ;)
