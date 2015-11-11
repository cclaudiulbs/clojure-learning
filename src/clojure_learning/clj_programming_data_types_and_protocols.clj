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


;; the only way to access the mutation capabilities on clojure deftypes mutable fields is TO EXPOSE 
;; behavior for such operations, ONLY when the TYPE is declared/defined!!!


