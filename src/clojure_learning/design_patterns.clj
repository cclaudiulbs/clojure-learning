(ns clojure-learning.design-patterns
  (use (clojure repl test)))

;; design-patterns are signs that the language itself is not flexible and power-enough to hide the structural tools
;; that help achieving/solving a functional requirement. Paul Graham once said that if he sees design-patterns in code,
;; it means the code is busy implementing the structural-non-functional-requirements and not the problem at hand.
;; in functional languages, 16 of the 23 design patterns are evaporated in the air, by the use of callback-functions,
;; HOFs, and composition of functions that help build another higher-abstraction.

;; in OO languages, DI is a way to decouple a class's concrete/direct-dependencies, by the use of
;; abstractions(interfaces). Instead of leaving the class itself managing it's own dependencies, and create
;; concrete-instances of its dependencies, it externalize those dependencies through arguments, deferring this
;; responsibility on runtime, when a magic-container is responsible of injecting those dependencies through a
;; wiring-configuration. In java this is accomplished by programming to interfaces not to concrete classes.

;; in the followings we shall demonstrate the Dependency injection in clojure simplified through the use of
;; clojure-protocols and records.
(defprotocol Bark
  (bark [this]))

(defrecord Pitbull [weight price]
  Bark
  (bark [this] "pitbull"))

(defrecord Ciobanesc [price]
  Bark
  (bark [this] "ciobanesc"))

(defrecord DogStore [dog])

(defn bootstrap [some-dog]
  (let [dog-store (DogStore. some-dog)]
    (println (bark (:dog dog-store)))))

(bootstrap (Ciobanesc. 33))  ;; ciobanesc
(bootstrap (Pitbull. 30 25)) ;; pitbull

;; the nice thing about this is that through protocols clojure can extend functionality of existing types without
;; touching the types or changing them.

(def p (promise))
(deliver p :fred)
