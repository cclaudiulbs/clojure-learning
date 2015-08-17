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

;; NOTE: after reading the braveclojure-multimethods tut, we can dig further into the fun-clojure.
;; When the time comes and we [derive] many types, from a type that is augmented with the multi-method
;; protocol, we may want to [prefer-method] a certain method over other. if we didnt specify a
;; [prefer-method] than we would have a runtime exception: no prefered-method is defined, dont know
;; which one to choose.
;; Clojure maintains a global schema resolution for type hierarchies, that consults to yield a
;; certain multi-method implementation by the value returned by dispatching-function.
;; Clojure provides the option to not polute the global data-structure with all the type derivations
;; in the form of a custom-hierarchy. this is possible so that we can define more specific the
;; type-hierarchy schema resolution for dispatching multi-methods.

;; using [juxt] we can model some neat dynamic output of the dispatching function.
(doc juxt)
(defn juxt-funcs [vectorize mk-hash-set] (juxt vectorize mk-hash-set))
(def juxt-args (juxt-funcs #(vec %) #(apply hash-set %)))
(juxt-args '(1 2 3)) ; [[1 2 3] #{1 3 2}]

;; juxt takes a set of functions, and returns a curried function back, the returned closure
;; takes an argument and applies each "fixed" function(from the partial funcs) on the argument.
;; (juxt func1 func2 -> (fn [arg] [(func1 arg) (func2 arg)])

((juxt vec hash-set) [1 2 3]) ; [[1 2 3] #{[1 2 3]}]

;; notice here that without using "apply", "hash-set" invoked on vector -> yields #{[]}.

;; THERE'S SOME INTERESTING THINGS WITH MULTI-METHODS AND JUXT :) READ ON FROM JOY-CLOJURE
;; we can define a multi-method which uses the [juxt] power to build a dynamic dispatching value:
(defmulti compile-cmd (juxt :os :compiler))
(defmethod compile-cmd ['unix 'gcc]
  [context] (str "/usr/bin/" (get context :c-compiler)))
(defmethod compile-cmd ['ios 'gacc]
  [context] (str "some path for ios devices: " (get context :c-compiler)))

;; demo:
(compile-cmd {:os 'unix :compiler 'gcc}) ;; /usr/bin/
(compile-cmd {:os 'ios :compiler 'gacc :c-compiler "ios-compiler-here"})

;; the dispatch values for the compile-cmd is a vector, that is the result of invoking the dispatching
;; function: using [juxt] take the 2 functions which when invoked on the bound values for each defined method
;; yield the right "annotated" method for dispatching to.


;; multimethods allow for polymorphic dispatch based on arbitrary functions.

(defn present [name] (str "name is " name))
(defn ask-question [question] (str "asking a stupid question: " question))
((juxt present ask-question) "maxi max")

;; The use of defrecord buys you several important benefits. For instance, it provides a
;; simple and specific idiom for documenting the expected fields of the object. But it
;; also delivers several important performance improvements. A record is created more
;; quickly, consumes less memory, and looks up keys in itself more quickly than the
;; equivalent array map or hash map. Record types can also store primitive values (byte,
;; int, long, and so on), which take up considerably less memory than the equivalent

;; playground: implement the binary-tree from chap5 using Records, instead of Maps, this should be much faster
;; Requirements:
;; the binary-tree should have 2 funcs: xconj + xseq; class-based object versions.

(defrecord TreeNode [val l-branch r-branch])
(TreeNode. 5 nil nil)  ; user.TreeNode{:val 5, :l-branch nil, :r-branch nil}
(->TreeNode 3 nil nil)
(map->TreeNode {:l-branch nil :r-branch nil :val "cclaudiu"})
;; 3 ways of instantiating the defrecord Type.

;; use-case for TreeNode:
;; (xconj num-types (range 1 4))
;; (xconj num-types (range 5 8))

;;           TreeNode
;;       val: nums-category
;;   l-branch     r-branch
;;   ----------|---------
;;   |                  |
;; even                odd
;; val: 2 4 6        val: 1 3 5
;; l:nil r:nil      l:nil   r:nil

(ns records
  (:import java.util.Date))

(defrecord TreeNode [value l-branch r-branch])

(defn xconj
  [tree nums]
  (if (nil? tree)
    (->TreeNode nums nil nil)
    (if-let [even-nums (seq (filter even? nums))]
      (if-let [odd-nums (seq (remove even? nums))]
        (->TreeNode (:value tree) (xconj (:l-branch tree) even-nums) (xconj (:r-branch tree) odd-nums))
        (->TreeNode (:value tree) (xconj (:l-branch tree) even-nums) (:r-branch tree)))
      (->TreeNode (:value tree) (:l-branch tree) (xconj (:r-branch tree) (remove even? nums)))
     )))

;; define one start binary-tree
(def num-container (->TreeNode "numeric-aggregations" nil nil))

;; conj into the binary-tree num container
(def nums-categorization (xconj num-container (range 1 6)))
;; output-readable-format:

;;         TreeNode{:value "numeric-aggregations",
;;    :l-branch                           :r-branch
;;    TreeNode{:value (2 4),              TreeNode{:value (1 3 5),
;; :l-branch nil, :r-branch nil},         :l-branch nil, :r-branch nil}}

;; denormalizing into a seqable collection the binary-tree numeric values:
(defn xseq [tree]
  (when tree
    (lazy-cat (xseq (:l-branch tree)) (seq (:value tree)) (xseq (:r-branch tree)))))
(filter number? (xseq nums-categorization))  ;; (2 4, 1 3 5)

;;;;;;;;;;;;;;;
;; Protocols in Clojure
;;;;;;;;;;;;;;;
;; A procotol in clojure is a collection of abstract functions(signatures) each with at least one parameter
;; that have a collective name. They are like interfaces in Java.
;; The type of the first parameter is important as the dispatching is done polymorphically based on it.
;; But in order for a protocol to do any good, something must implement it. Protocols
;; are implemented using one of the extend forms: extend, extend-type,5 or
;; extend-protocol.
;; Each of these does essentially the same thing, but extend-type and
;; extend-protocol are convenience macros for when you want to provide multiple
;; functions for a given type
(defprotocol FIXO
  (fixo-push [fixo value])
  (fixo-pop [fixo])
  (fixo-peek [fixo]))

(extend-type TreeNode
  FIXO
  (fixo-push
     [node value]
     (xconj node value)))

;; this is better than defining a single-self-contained function, because protocols allows for polymorphism.
;; -> the same function can have a different implementation for a extended-type.

;; we can extend core-vector implementation by augmenting it with the FIXO protocol:
(extend-type clojure.lang.IPersistentVector
  FIXO
  (fixo-push [vc value]
    (conj vc value)))

(fixo-push [] "some") ; ["some"]

;; here we're augmenting the Interface not a concrete class, that means any impl that extends from this interface
;; will have out of the box the protocol implementation at its use.

;; one thing to mention is that we cannot define partially that a type extends a protocol, and bind implementation
;; partially (not for all the functions defined by the protocol). In clojure augmenting a Type with a protocol
;; is Protocol based and does not augment on function granularity. There's a small trick here. Using clojure's
;; mixins, we can workaround this issue.(say we dont have yet all the functions in place for that protocol).
(defprotocol StringOps
  (rev [that])
  (upper [that]))

(extend-type String
  StringOps
  (rev [that] (clojure.string/reverse that)))

;; ok partially -> although not all the protocol's implementation is bound to String
(extend-type String StringOps
  (upper [that] (.toUpperCase that)))

(-> "cclaudiu" rev upper) ;; --> Exception: no implementation for method :rev of protocol found
;; partially implementing the protocol -> last attempt of protocol implementation will override the first one.

;; mixins to the rescue! Define maps that when [merge] 'd together will provide a fully-mixin that can
;; be used as the protocol implementation
(def rev-mixin {:rev #(clojure.string/reverse %)})
(def upper-mixin {:upper #(.toUpperCase %)})

;; then we can merge the two maps into one:
(def fully-mixin (merge rev-mixin upper-mixin))

;; not the [extend] use here!!! and not the [extend-type] ***
(extend String StringOps fully-mixin)

;; demo:
(-> "claudiu" rev upper) ;; UIDUALC

;; Mixins in Clojure refer to the creation of discrete MAPS CONTAINING PROTOCOL FUNCTION
;; IMPLEMENTATIONS that are combined in such a way as to CREATE A COMPLETE IMPLEMENTATION
;; OF A PROTOCOL.

;; *** only after we have all the procotol's implementation in place we can EXTEND-TYPE!

;; this is IMPOSSIBLE in Java or C++, as any concrete classes must at the creation time specify
;; all the interface-functions it implements:
;; With either of those languages, the concrete type (such as TreeNode
;; or vector) must name at the time it’s defined all the interfaces or classes it’s going to
;; implement.
;; + the interfaces in java|c++ must exist when the concrete classes are created. Here we ve taken
;; the other way around. we had a TreeNode concrete type(defrecord) and after we defined the protocol
;; FIXO(First-In-X-Out).

;; Clojure's polymorphism lives in protocol's functions and not in the classes.
;; threating nil? requires that nil object implement the correspondent protocol.

;; as a lesson, always use [extend-type] to provide full protocol's implementation of those
;; polymorphic functions, and use [extend] when you provide mixins-implementation through the
;; [map] interface.

;; How about sharing some implementations? Other languages provide the notation to build an
;; abstract-base class in which one provides all the common behavior shared among the descendants,
;; while every descendant defines it's own specific behavior, inheriting from this base-class.
;; Clojure prefers the notaion of sharing. Building a factory-polymorphic-dispatcher function
;; outside which polymorphically will delegate using the protocol's polymorphism to the
;; right Type implementation.
;; same but in other words:
;; The simplest solution is to write a regular function that builds on the protocol’s
;; methods. For example, Clojure’s own into takes a collection and uses the conj implementation
;; provided by the collection

(def tree-node-fixo
  {:fixo-push (fn [node value] (xconj node value))
   :fix-pop (fn [node] ())}) ;; and so on
(extend TreeNode FIXO tree-node-fixo)
;; extend the type TreeNode WITH protocol FIXO BY PROvIDING map-fixo-mixin implementation of those methods.

;; one note about protocol's methods namespacing:
;; methods of the protocol are namespaced in a way that Java and C++ interfaces aren’t.
;; In those languages, all methods of a class
;; share the same effective namespace, regardless of interfaces they’re implementing. In
;; Clojure, the methods always use the same namespace as the protocol,
;; that is in clojure there might be protocol-methods sharing the same names with other
;; protocol-methods, and don't collide each with each. because their namespaces are different.
;; this provides a great deal of flexibility when combining third party APIs into one codebase.
;; therefore every protocol-method will be namespace-bounded to the namespace where the protocol
;; is defined.
;; one important thing here: because the protocol-methods share the NAMESPACE, we CANNOT have
;; 2 methods defined in 2 protocols and 1 namespace having the same name!
;; since protocol-methods are namespace dependendant.

;;;;;;;;;;;;;;;;
;; method implementation in defrecord:
;;;;;;;;;;;;;;;;
;; this is when you know the protocol you want to implement by heart at the moment of defrecord creation Type.
;; is like implementing FIXO's protocol methods INLINE.
(defrecord TreeNode [value l-branch r-branch]
  FIXO
  (fixo-push [node value] (xconj node value)))

;; apart from defining the record Type, and the labels, we define also the protocol implementation for this
;; particular TreeNode record INLINE.

;; the normal defrecord signature was:
(defrecord TreeNode [value l-branch r-branch])

;; this idiom is also powerful for performance, because declaring a protocol's implementation methods
;; inline is several times faster than later-declaring the protocol and using [extend] to extend the
;; type with that protocol.
;; another neat part is that once declared the protocol inline, the fields of the extended-type are
;; also available locally by the protocol-methods implementation.

;; We've been using the custom xseq function, but because clojure provides the ISeqable interface, we can
;; implement that interface for our new Type.
(defrecord SomeCustomType [cust-val]
  clojure.lang.ISeq
  (seq [custom-rec] (lazy-seq (cons cust-val (seq custom-rec)))))
;; --> Exception: Duplicate method name&signature in class file user/SomeCustomType
;; we get this exception, because clojure-records are maps under the hood, that ALREADY implement the
;; map funcs: seq, assoc, dissoc, get and so forth. Because these funcs are already provided we CANNOT implement
;; them again <- the resulting exception.
;; the neat part here is also how we can implement a lazy-sequence using recursion:
;; cons cust-val is trying to push something to the next-recursive-call to seq on that cust-rec passed to [seq]
;; function. the next recursive call is in fact a lazy-seq, hence a lazy-list we can say.

;; for the rare cases where we want to implement our own data-structure, other than what clojure provides
;; (for instance defrecord) we can use the [deftype] construct, that is similar with [defrecord] but does not
;; provide anything at all!
(deftype SomeCustomType [cust-val]
  clojure.lang.ISeq
  (seq [some-rec]
       (lazy-seq (cons cust-val (seq some-rec)))))
(take 3 (seq (->SomeCustomType 3))) ;; (3 3 3)

;; we can clearly see that for recursive invocation we don't need any exit branch out of the recursive calls!
;; because it's a lazy-seq by definition <- the need to explicitly state to [take] only the first 3 items.

(:cust-val (->SomeCustomType 3)) ;; nil
;; well this is because our custom type, defined with [deftype] does NOT implement anything out-of-the-box
;; like [defrecord] does, hence no: get, assoc in-place!
;; for these reasons, we can still lookup the value bound to the new custom-type using java interoperability:
(.cust-val (->SomeCustomType 3)) ;; 3
