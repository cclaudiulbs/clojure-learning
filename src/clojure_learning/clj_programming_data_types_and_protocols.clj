;;;;;;;;;;;;;;;;;;;;;
;; Clojure Programming:: DataTypes and Protocols
;;;;;;;;;;;;;;;;;;;;;
(ns clojure-learning.clj-programming-data-types-and-protocols
  (use [clojure.repl])
  (require [clojure.test]))

;;The dynamic Expression Problem is a new name for an old problem. The goal is to have
;;interfaces and type where one can create new types implementing existing interfaces and
;;provide implementation of a new interface for an existing type, without recompiling
;;existing code.

;; The first half of this goal—having new types to implement old interfaces—is commonly
;; met: every object-oriented language offers this capacity. The second half—providing
;; implementations of new interfaces to old types—is the interesting part of the problem,


;; A protocol consists of one or more methods, where each method can have multiple
;; arities. All methods have at least one argument, corresponding to the privileged this
;; in Java
(defprotocol EmailSender
  (send-email [recipient message]))

;; each method should have at least one argument. when protocol implementation is provided
;; the dispatch will be based on the method first argument TYPE.
;; the guideline is to write protocol names in camelCase, as they shall compile down to interfaces
;; and classes.
;; FROM A USER PERSPECTIVE protocol methods are functions <- meaning there's no special syntax
;; in calling them
;; A good protocol should be designed with few methods that do not overlap their concerns.
;; A good protocol is one that is easy to implement. There's nothing wrong with a protocol having
;; ONLY ONE SINGLE METHOD.

;; there's a litlle downside when using protocols is that we cannot use destructuring in the
;; moment that the protocol is declared, hence NO first/rest naming destructuring. This is because
;; protocols as said compile down to java interfaces which do not support all the argument-structure
;; variations clojure provide.

;; Convenience functions should not be part of the protocol, but rather built on top of protocols,
;; Protocols should define type-specific behaviors.

;; Because protocols dispatch on the first argument to the method, there's no need for explicit "this"

(defprotocol Matrix
  (lookup [vov col-pos row-pos])
  (update [vov col-pos row-pos value])
  (rows [vov])
  (cols [vov])
  (dims [vov]))

;; because clojure and neither jvm does not have the Matrix built-in type we are going to declare 
;; new protocol, and augment the IPersistentVector

;; IMPORTANT: using [extend-protocol] there should be ONLY ONE usage of extend-protocol methods
;; otherwise an exception is thrown saying: there are more than one impl for the methods of that protocol

(ns matrix)
(extend-protocol Matrix
  clojure.lang.IPersistentVector
  (lookup [vov col-pos row-pos]
    (get-in vov [col-pos row-pos]))
  (update [vov col-pos row-pos value]
    (update-in vov [col-pos row-pos] nil value))
  (rows [vov] (count vov))
  ;; (cols [vov] (apply (partial map vector) vov))
  (cols [vov] (apply (partial map vector) vov))
  (dims [vov] [(count vov) (count (first vov))]))

(cols [[1 2] [3 4]]) ;;  ([1 3] [2 4])

;; in this scenario the first argument on which the dispatch is issued by its type is the vov.

;; there are 4 options to extend a protocol:
;; 1/ using the extend-protocol: providing multiple implementation to several types of the same protocol
;; 2/ using extend: to extend ONE single Type with multiple protocols, BUT PROVIDING the impl into a hash-map
;;   having func-keys keywordized and values the corresponding functions
;; 3/ using extend-type: is the complement to extend-protocol->Providing multiple protocols to ONE Type
;;    (internally is a macro over [extend] but without the keywordized-funcs needed)
;; 4/ inline implementation

(extend clojure.lang.IPersistentVector
  Matrix
  {:keywordized-func-cols (fn [vov] (apply (comp map vector) vov))}
)

(cols [[1 2] [3 4]])

(extend-type AType
  AProtocol
  (method-1-from-AProtocol [dispatched-type more] ())
  (method-2-from-AProtocol [dispatched-type more] ())

  AnotherProtocol
  (method-1-from-AnotherProtocol [dispatched-type more] ())
  (method-2-from-AnotherProtocol [dispatched-type more]))


(extend-protocol AProtocol
  AType
    (method-1-from-AProtocol [dispatched-type more] ())
    (method-2-from-AProtocol [dispatched-type more] ())

  BType
    (method-1-from-AProtocol [dispatched-type more] ())
    (method-2-from-AProtocol [dispatched-type more]))

;; a protocol can be extended to "nil" providing some default behavior, hence saying goodbye to NPE.
(comment 
  (extend-protocol Matrix
    nil
    (lookup [vov col-pos row-pos])
    (update [vov col-pos])
    (cols [vov])
    (rows [vov]))
)

(lookup nil 0 0) ;; nil

;; it would be helpfull if we would have a factory function to create def VoVs.
(defn vov [rows cols]
  "creates a empty-nil VoV of cols and rows"
  (apply vector 
      (repeat cols 
          (apply vector 
                (repeat rows nil)))))

(def matrix (vov 3 2))
(update matrix 1 1 nil "claudiu")

(def marr (make-array Integer/TYPE 3))
(aset marr 0 2)
(map print marr)


;; With protocols we can bring any preexisting type in to satisfy the contract of our
;; protocols without modifying those types or having any other special access to them

;;;;;;;;;;;;
;; Clojure Types
;;;;;;;;;;;;
;; Any Clojure Type is compiled down to a java-class -> by convention the Type Name should be in camelCase
;; A Type in clojure is as simple as:
(defrecord Point [x y])
;; or
(deftype Point [x y])
;; Point will compile into a java class, having the x + y public final members.

;; creating an instance of this Type as simple as:
(Point. 3 4) ;; {:x 3, :y 4}
;; the order counts as the arguments are declared and initialized in a positional manner.

;; accessing a member:
(.x (Point. 2 3)) ;; 2

;; hinting and type declarations
(defrecord Circle [^long x])
(.x (Circle. "radius")) ;; --> ClassCastException!
(.x (Circle. 3))        ;; 3

;; usually Records are defined to represent application-data logic!
;; whereas deftype's are defined to represent low-level infrastructural types (implementing a NEW data-structure)

;; one of the main differences between deftype and defrecord, is that type definition is NOT imported
;; automatically using "use" or "require" in another namespace. deftype should be explicitly imported
;; via: import 'some-ns.Point, however defrecords ARE imported using: use or require

;; defrecords are a specialization of deftypes.
(deftype Rectangle [x y])
(=  (Rectangle. 2 2) (Rectangle. 2 2)) ;; false
;; --> deftypes are not values!!!

;; defrecords add some additional facilities over deftypes
;; 1. are behaving as VALUES hence are IMMUTABLE!
;; 2. participate in the associative collection abstraction => all the map-like apis can be applied on defrecords

;; defrecords can use their members are function-keywordized to:
;; look themselves up in the associative collection that they are called with.

(:x (Circle. 4)) ;; 4
(map :x [(Circle. 2) (Circle. 3)])  ;; 2 3

;; defrecords implement java.util.Map -> hence all the map-collection-funcs are disposable
;; the thing is that calling any Map-like funcitons on defrecords will NOT return a defrecord back!
;; but a map 
;; also, a Record can be augmented with other members, however it will NOT be anymore a Record, since the
;; type differs now
(type (assoc (Circle. 3) :radius 2)) ;; Circle
(assoc (Circle. 4) :radius 2)        ;; Circle {:x 4 :radius 2}

(.radius (assoc (Circle. 4) :radius 2)) ;; IllegalArgumentException -> no matching field found :radius in Circle

;; dissocing a new member added via assoc to a defrecord, will NOT yield a Map structure back, but the initial defrecord
;; however dissocing the defrecord declaration member will yield a Map back.

;; Rule: any member added after the defrecord definition, will NOT be part of the clojure-record class (java-class)
;; but they can be used with the map-coll-abstraction
(:radius (assoc (Circle. 4) :radius 3)) ;; 3
(.radius (assoc (Circle. 4) :radius 2)) ;; IllegalArgumentException -> no matching field found :radius in Circle

;; Clojure deftype and defrecords provide a factory function: ->MyType, that takes the positional-ordered args
;; this factory function, can be passed to a HOF
(map ->Circle [1 2 3])  ;; building 3 instances of Circle in one shot

;; in addition, defrecords provide a secondary factory-function that takes a map, hence the order of args
;; is NOT important anymore:
(map->Circle {:x 3}) ;; Circle {:x 3}}

(map (partial apply ->Circle) [[3] [4] [5]]) ;; -> 3 instances in one shot, for VoVs

;; when to use maps or records?
;; prototyping a simple solution will often be best if using maps, however getting the benefit of polymorphism
;; and performance requires the change to defrecords. there are 2 important differences between maps and
;; defrecords:
;; 1. defrecords are NOT FUNCTIONS. so cannot be used as functions in a HOF or to lookup some fields
;; 2. defrecords are NOT equal to maps!

;; so if we plan to change from maps to records, there should be some guideliness in when is appropriate
;; to change from maps to records.

;;;;;;;;
;; more on deftype's
;;;;;;;;
;; while defrecords and maps should be used to model application-data-specifics, deftype should be used
;; whenever a new low-level-infrastructure structure comes in action.
;; one important aspect is that deftype exposes mutable fields, while defrecord is immutable from start.
;; deftype is NOT associative! so it does NOT inherit from clojure.lang.Associative interface.
;; therefore they cannot be used as accessor-functions to lookup some entries

;; while defrecords expose public final immutable fields, deftypes do not. they can use the two annotations
;; to expose mutable fields: ^:volatile-mutable and ^:unsynchronized-mutable.

;; ^:volatile has the same semantics as in java; reads and writes are atomic; hence thread-safe, however they
;; are NOT COORDINATED, hence uncoordinated, hence the JIT can re-order them, in such a way, that race-conditions
;; appear.

;; immutable fields are public, while mutable fields are always private; they can be mutated only through
;; accessor methods: set! or use inside the body by simply referencing them.

;; to expose some methods on the defrecord animal -> we can create a protocol to mutate-access the Animal defrecord

;; here's a version of augmenting an existing protocol to a new TYpe via inline implementation:
(defprotocol AnimalMutation
  "A simple protocol to allow access to Animal mutable members"
  (get-specie [this])
  (set-specie! [this new-specie]))

(deftype Animal [^:volatile-mutable specie]
  AnimalMutation
  (get-specie [animal] (. animal specie))
  (set-specie! [animal new-specie] (set! specie new-specie)))

(def wolf (->Animal "wolf"))
(get-specie wolf)     ;; "wolf"
(set-specie! wolf "cat")
(get-specie wolf)     ;; "cat"

;; there are ONLY TWO ways to augment a given type with a protocol:
;;;;;;;;;;
;; NOTE: these 2 forms are ONLY for public-final fields!!! and not for private mutable fields
;;;;;;;;;;
;; 1. augmenting a type when the type is defined, via inline implementation for the protocol
;; 2. augmenting an already existing type, with a new created protocol via extend-protocol

;; let's start from scratch by defining a new type (this should be the existig type in historical time)
(deftype HumanBeing [^:unsynchronized-mutable who ^:unsynchronized-mutable age])

;; then define a new protocol that we're gonna use to augment the existing mutable type
(defprotocol HumanMutation
  (getName [this] (.who this))
  (setName! [this new-name] (set! (. this who) new-name)))

;; then extend the new-protocol to the existing type
(extend-protocol HumanMutation
  HumanBeing
  (getName [this] (. this who))
  (setName! [this new-name] (set! (. this who) new-name)))

(def cclaudiu (->HumanBeing "cclaudiu" 34))
(.getName cclaudiu)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(deftype Languages [lang-code paradigm])

(defprotocol LanguageServiceProtocol
  (get-lang-code [this])
  (get-lang-paradigm [this]))

(extend-protocol LanguageServiceProtocol
  Languages
  (get-lang-code [this] (.lang-code this))
  (get-lang-paradigm [this] (.paradigm this)))

(def clojure-lang (->Languages "CLJ" "FP"))
(get-lang-code clojure-lang)   ;; CLJ
;; ---> WORKS NICEEEEEEE because the fields for deftype are public and can be accessed AFTER THE TYPE is DECLARED.

;; ONE IMPORTANT THING TO NOTE HERE:
;; defrecord participating in the MAP Collection Abstraction -> it's keys/fields can be used
;; as accessor-functions to lookup values bound to the fields, 
;; on the other side deftype is NOT participating in the Map-Collection-Abstraction and therefore
;; deftype will work ONLY using java interoperabilty, to access values bound to fields.
(.member DEF-TYPE)
(:member DEF-RECORD)
;; this is valid only for public static final members, to be accessible AFTER THE TYPE is declared!

;;;;;;;;;;;;
;; Types Inline Implementation
;;;;;;;;;;;;
;; generally inline implementation provides better performance, since protocol polymorphic methods
;; can have DIRECT access to fields defined in declared Types + will be as fast as calling
;; java interfaces methods.
;; JVM optimizes method calls to extremme!
;; inline implementation raises the potential for duplicate-method-signature exception to be thrown,
;; if ONE Type defines implementation for 2 protocols which have methods with the same signatures.
;; this is true also for any attempt to implement a protocol, for a type, which type already implements
;; some of the java.util.Map interface.

;; guideliness in clojure:: always use inline implementation for performance optimizations! not as a
;; first idea!
;; inline implementation are the only to provide implementation for java interfaces!!! Just as with
;; protocols there's NOT mandatory to implement all the methods that the interface defines ->
;; an exception is triggered on the first attempt to use an-unimplemented method!

;; there's YET another way to declare a new type. this is more likely behaving as the anonymous inner classes
;; that java provides. this is realizable using "REIFY". which has the declaration rules as the deftype
;; or defrecord, but without the Type NAME. using [reify] one can opt to provide implementation ONLY
;; for SOME of the methods that protocol defines (and not all).

(def clojure-reified
      (reify LanguageServiceProtocol
        (get-lang-code [this] 
          (.lang-code (->Languages "clojure" nil)))))

(get-lang-code clojure-lang-code)  ;; clojure

;; reify is a powerfull inliner tool, that behaves more like lambda-expressions. We can reify on the fly
;; a protocol for an anonymous type, providing some "hardcoded/concrete" types in the body of the
;; protocol/method we implement. This whole thing returns a reified object back. from this on
;; since our object-reified implemented the protocol, we can apply the functions which were implemented on it.

;; another example/usage of reify::
(def reified 
  (reify 
    LanguageServiceProtocol
    (get-lang-paradigm [this] (.paradigm (->Languages "clojure" "functional-programming")))))

(get-lang-paradigm reified) ;; functional-programming

;; Clojure drops the support for Type based inheritance in the favour of "programming to interfaces/protocols".
;; Types can ONLY satisfy protocols or implement interfaces!
;; theefore clojure enforces the idea to prefer composition over type-based-inheritance reusable entities.

;; [extend] is a function, it takes the context of protocol to implement as func-keys inside a map;
;; [extend] is a function, while [extend-type] [extend-protocol] are both macros;
;; [extend] taking the context-implementation in a map, this map can be augmented with other implementations
;; simulating the mechanism of mixins or traits.

;; to simulate inheritance, this gate is opened by the [extend] function:
;; 1. declare a abstract-parent-object
(def abstract-vov
  {:cols (fn [this] (apply map vector this))
   :rows (fn [this] (count this))})
(extend clojure.lang.IPersistentVector
  Matrix
  (assoc abstract-vov 
     :lookup (fn [this i j] (get-in this [i j]))
     :dims (fn [this] [(count this) (count (first this))])
     :update (fn [this i j new-val] (update-in this [i j] (fn [old-val] new-val)))))

(dims [[1 2 3] [3 4 5]])
(update [[1 2] [3 4]] 0 0 24) ;; [[24 2] [3 4]]
(cols [[1 2] [3 4]]) ;; [[1 3] [2 4]] --> cols in "mixed-in" with the abstraction

