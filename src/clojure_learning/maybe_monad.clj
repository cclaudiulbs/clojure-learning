(ns clojure-learning.maybe-monad
    (use (clojure [test] [repl])))

(defn maybe-monad []
  "Monad describing computations with possible failures. Failure is
   represented by nil, any other value is considered valid. As soon as
   a step returns nil, the whole computation will yield nil as well.")

((-> (return 10)
     (lift-add 2)
     (lift-divide 0)
     lift-inc))

;; assuming we have core functions which should be transformed to return monadic-values Mv
;; these lifted functions by monad-contracts should TAKE actions instead of concrete-types
;; to allow chaining the functions that are to work in conjuction with each-other
(defn lift-inc [action]
  (bind action (fn [to-inc-x] (return (inc to-inc-x)))))

((lift-inc (return 0)))

;; the return function does NOT do anything at all -> it just returns an Mv back in the form of a function
;; that wrapps the current value; remember functions are data! and can be considered as data-types as well
(defn return [v] 
  (fn [] v))

;; bind should return another chain-able action
(defn bind [action func]
  (let [result (action)]     ;; reify to concrete type by invoking the action and get the type: v
    (if (nil? result)
      (return nil)
      (func result))         ;; (func: result -> action)
))

(defn lift-add [action r-ops]
  (bind action (fn [l-ops] (return (vector ( apply + (into l-ops r-ops)))))))

((lift-add (return [12 44 33]) [1])) ;; 89

; there are several rules in implementing the monads:
;; function which by contract should return action(monadic-values) are:
;;    1. [return] by definition converts a concrete type into a monadic-type: lifts that type
;;    2. [bind] by defition should return an action -> to allow chain-able computations
;;    3. [func] as the 2nd arg that is passed to bind
;;   return: v -> Mv
;;   bind:  Mv >>= (func: v -> Mv) -> Mv
;;
;; the last Mv -> is the return type of bind

;; to abstract away and avoid code repetition, this is a HOF which lifts-up core math-functions
(defn math-lifter [math-fn]
  (fn [action r-ops] 
    (bind action (fn [l-ops] (return (vector (apply math-fn (into l-ops r-ops))))))))

(def lift-subtract (math-lifter -))

;; the power of prefix notation
((lift-subtract (return [10 2 3]) [1])) ;; 4

;; the power of prefix notation
((-> (return [1 2])
     (lift-add [3 4])
     (lift-add [4 5])
)) ;; [19]

(def lift-divide (math-lifter /))

((-> (return [2 2])
     (lift-add [3 4])
     (lift-add [4 5])
     (lift-divide [0])
)) ;; ArithmeticException -> hence divide has a special case that should treat

(defn lift-divide [action r-ops]
  (bind 
    action 
    (fn [l-ops] 
      (if (some #{0} r-ops) 
     4::    (return nil)
        (return (vector (apply / (into l-ops r-ops)) 
 ))))))
;; 0 being a special case into a division -> it is handled inside the divide func -> which returns nil for any POSSIBLE
;; failings -> the bind function handles nils by simply returning a monadic-value back without evaluating the form

((-> (return [2 2])
     (lift-add [3 4])
     (lift-add [4 5])
     (lift-divide [2])
     (lift-add [2])
)) ;; nil! -> the whole chain of computations will return nil!

(identity [1 2 3]) ;; -> functor
(map identity [1 2 3]) ;; -> functor

(= (map identity [1 2 3]) (identity [1 2 3])) ;; true
