(ns clojure-learning.monads-explained
  (use (clojure repl test)))

;; monads where introduced in haskel, which is pure lazy FP language
;; monads introduced the concept of actions, which are executed in sequence.
;; monads are the patterns which provides a way to glue together functions composition,
;; in such way that the output of a function is not compatible with the output of the 
;; piped function.
;; f -> type-a -> type-b
;; g -> type-b -> type-c
;; this is the normal flow of chaining functions, given the output of func f, serves
;; is compatible with the domain-args of func g.

;; one can write something like: g (f x)
;; but the trick comes when:
;; f -> type-a -> type-b
;; g -> type c -> type-d
;; the previous afirmation is not valid anymore, because the domain-types of funcs f and g are
;; not compatible anymore.

;; there should be an option to allow function composition, in such way that we can still glue them
;; into functions composition; my 2 cents are that we can create a middle Type which is a function
;; "lift" both of the functions to work with the middle-func type.

;; a pure function is a function which does not have any side-effects, are those functions
;; which are said to be referential transparent.
;; side-effect functions change the state of the world and hence are not idempotent;

(defn square [x] 
  (fn [] (* x x))) ;; -> returns a function which takes no args, but closes over the enclosing func arg x

(square 2) ;; func
((square 2)) ;; 4

;; in monad world we're dealing with actions. Actions are functions which are executed in sequence.

;; 1. create a function which converts any data-type(map, string...) into an action/function.
;; in monad world, by convention any function which converts a concrete type into an action takes the name:
;; [return] or [unit].

(defn return [v]
  (fn [] v))      ;; the result is another function -> creating an action

(return 5)   ;; -> the return is another function = action
((return 5)) ;; 5
;; the action is a wrapper around the data.

;; the next step is to create a function which operates on the concrete-data
;; [bind] by convention
(defn bind [action func]
  (func (action))) 
;; 1st execute the action -> to take the result
;; 2nd using the function passed to bind -> apply that func on the result returned by invoking the action
;; MANDATORY:: the func given to bind MUST return another action!

;; building a func which returns an action, actually lifting the [inc] func
(defn f-inc [v]
  (return (inc v))) ;; returns an action(another func, which wrappes the value)

;; applying [bind] with the action, and the lifted-func
(bind (return 5) f-inc) ;; the result is another action

;; being another action -> we can throw it in another bind application:
(bind (bind (return 5) f-inc) f-inc)   ;; -> returns another action

((bind (bind (return 5) f-inc) f-inc))  ;; -> result should be 7, applying twice the inc func on 5

;; encapsulating the bind invocation into a common-sense function:
(defn lift-inc [action]
  (bind action (fn [v] (return (inc v)))))

;; we said that the bind takes an action, and another func(which in this case takes a concrete-value)
;; and the last-function passed to bind -> MUST return another action, after it doeses the operation
;; once the operation is incremented -> it is used as arg to the [return] function -> returns another action.
(lift-inc (return 5))   ;; -> an action back
((lift-inc (return 5))) ;; 6

(lift-inc (lift-inc (return 5))) ;; -> an action back
((lift-inc (lift-inc (return 5)))) ;; -> 7

;; from here we can thread-it using the thread-first operator:
((-> (return 5) 
     lift-inc 
     lift-inc)) ;; 7

;; the next thing is to lift a more interesting operation: [add]: (a a) -> a

(defn lift-add [action val-1]
  (bind action (fn [val-2] (return (+ val-1 val-2)))))

((-> (return 5) 
     (lift-add 10) 
     lift-inc 
     lift-inc)) ;; 17

;; what do we gain from this ...you might say: incidental complexity :) ???
;; we could have write something like:
(-> 5
    (+ 10)
    inc
    inc) ;; 17

;; here's the advantage: what about introducing a new function which by mistake attempts to
;; divide by 0
(-> 5
    (+ 10)
    (/ 0)
    inc
    inc) ;; ArithmeticException : divide by 0!!!

;; now comes the fun: branching is an option ... an ugly option... introducing conditionals 
;; is like coding with compile-time exceptions -> "there might be a case" where this might happen
;; so please wrapp the functionality with if-else just to be sure that we're not failing :)

(let [x 0]
  (-> 5
      (+ 10)
      (/ (if (zero? x) 1 0))
      inc
      inc)) ;; 17 --> cool isn't it???

;; the nice thing about the monads is that the checking for error is done wrapped up inside the
;; the operation itself, and not expose for every invocation.
;; therefore implement a lift-divide function which handles the divide-by-zero scenario:
(defn lift-divide [action a-val]
  (bind action
        (fn [action-result] (return (/ a-val action-result))) ;; func:: int -> action
  ))

;; bind:: action is invoked -> the result is served to the next-func which takes the val and returns another action
((-> (return 5) 
     (lift-divide 2))) ;; 2/5

((lift-divide (return 5) 2)) ;; 2/5

;; enhancing the lift-divide with some error handling -> error monad:
(defn lift-divide [action a-val]
  (if (zero? a-val)
    (return nil) ;; -> returning another action which val is nil
    (bind action
          (fn [action-result] (return (/ action-result a-val))) ;; func:: int -> action
      )))

;; remember: bind 1st invokes the action -> with the result -> it invokes the func, which takes in this case
;; one arg -> and returns an action

((-> (return 5)
     (lift-add 10)
     (lift-divide 0)
     lift-inc
     lift-inc))
;; -> results in a NullPointerException. 

;; That means the [bind] function should be adapted to nil type of vals.
(defn bind [action func]
  (let [result (action)]   ;; invoke the action to get the result
    (if (nil? result)      ;; check for nil?
      (return nil)         ;; bind by-contract returns another action, which wrapps the nil-value
      (func result))))     ;; else -> it will use the func-to-bind to apply on result -> returning by-contract a new action

((-> (return 5)
     (lift-add 10)
     (lift-divide 0)
     lift-inc
     lift-inc)) ;; -> result is:: nil

;; bind now returns a nil-action for any nil-action-passed 
;;    -> every func which is lifted: lift-add, lift-divide, lift-inc
;; uses this bind func:: the "real" operation is not invoked anymore and each one will return a nil back,
;; which when checked inside the bind -> returns the same nil-action, and again and again...

;; the good side is that the high-order-actions can be chained, and the error-checking is encapsulated
;; in the lifted-function.

;; the downside of this approach, is that if one action returns nil, in a chain of actions,
;; we cannot know for sure which one stopped the chain computations.

;;;;;;;;;;;;;;;
;; how about state-managing monads?
;;;;;;;;;;;;;;;
;; we might want to start some chained-actions having an initial state, 
;; persist that state, and returning a structure containing the initial-state and the new-state.

;; the return function should adapt:
(defn return [init-state]
  (fn [new-state] [init-state new-state]))

;; every action, now is a function which takes an additional state 
;;  -> adapt the actions to take an additional state
(defn bind [action func]
  (fn [state]
    (let [[init-state new-state] (action state)]
      ((func init-state) new-state))))

;; because every action now takes a value, bind itself returns an action which takes a value
;; then, action is applied on the state(because every action now takes a state), and normally
;; the action will return a new action(provided by return) which is destructured to the vector;
;; the last form:: the func(which itself returns an action) is applied on the init-state -> returning
;; a new action, that itself takes an additional domain-arg, which is now the NEW-state.

;; remember signature of return:
;; fn-return: S -> (Action: S -> NEW-S)

;; the good part, is that by adding state, the lifted-functions implemented so far will NOT change!
;; the only change is done inside the return + bind functions.

((-> (return 2)
     (lift-add 8)
     (lift-divide 2)
     (lift-inc)
     (lift-inc)
     ) 10)
;; [7 10]

;; the initial state is passed as an additional argument; the new-state is 7

;; monads are in general just a combination between [bind] + [return] actions
;; the firs monad, was the identity monad, while the second, was the maybe-monad, implementing the
;; error logic. while the state monad is pretty ovious for what stands :)
;; Monad actions signatures:
;; return:: v -> A'v
;; bind:: A'v -> (func:: v -> A'm) -> A'm

;; think of the func involved are curried by default.
;; the return func: takes a value:v -> and returns an action. the action returned -> returns another
;; type(high-order-type)
;; the bind func: takes an action, and a func, which func takes the result of the first-arg action invoked
;; and returns another action of a diff type; that action of a diff type -> will be the final value.

;; the monad is how we do structure the functions.
;; the concept of lifting is because our functions work with concrete-diff-types, and we need
;; a way to change the functions to all return a monadic-data-type, that is the action(provided by return)

;;;;;;;;;;;
;; THE MONAD LAWS
;;;;;;;;;;;
;; 1. Left Identity
;;  (= (bind (return v) func) 
;;     (func v))
;; 2. Right Identity
;; 3. Associativity

;; provide:: separation of concerns!
