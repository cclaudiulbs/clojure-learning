;;;;;;;;;;;;;;;;;;;;;;;;
;; Mutation and Concurrency
;; refs + agents + atoms
;;;;;;;;;;;;;;;;;;;;;;;;
;; Clojure’s main tenet isn’t the facilitation of concurrency. Instead, Clojure at its core
;; is concerned with the sane management of state, and facilitating concurrent programming
;; naturally falls out of that

;; "Concurrency refers to designing systems using independently executing, logic processes" Pike

;; concurrent designs are all about independent processes and not necessarily about operating on related
;; tasks.
(doc clojure.core/some)
(clojure.core/some #{1 2 3} [2 4 5]) ;; 2
;; clojure.core/some predicate-fn xs -> 1st item matching the predicate

;; Concurrency refers to the execution of different tasks at roughly the same time, each
;; sharing a common resource, but not necessarily performing related tasks.
;; Concurrency promotes a the notion of non-determinism.
;; On the other side:
;; Parallelism refers to partitioning a task into multiple parts, each run at the same time.
;; Typically, parallel tasks work toward an aggregate goal, and the result of one doesn’t affect the behavior of any
;; other parallel task, thus maintaining determinacy. The parallel tasks dont need to be related
;; althought the results are "reduced" in the end somehow.

;; Clojure's STM reffers to the definition that concurrent updates of mutable states can be coordinated easily.

;; "A faster program that doesn’t work right is useless"

;; in concurrent programming there are 3 notions that should be detailed:
;; 1. Time - The relative moments when events occur
;; 2. State - A SNAPSHOT of an entity's properties at a moment in time
;; 3. Identity - the logical entity identified by some states

;; in clojure accomodating to the state-management-model means, one should think of an identity as receiving
;; a snapshot of its properties at a certain moment in time. This model is alike people as well, since
;; we should accomodate and take a decision based on the event that occur at a certain point in time
;; -> a natural model.

;; A transaction in Clojure is demarked by the dosync form and is used to build a
;; set of the changeable data cells embedded within that should all change together.
;; Similar to a db transaction where all the data is behaving atomically, either succeeds or fails as a whole.

;; Clojure currently provides the 4 data-types used in a shared manner:
;; 1. Refs -> Coordinated + Retriable
;; 2. Agent -> Async
;; 3. Atom -> Retriable
;; 4. Vars -> thread local scope
;; All but vars are considered shared mutable references, that allow changes to be visible to diff threads.

;; Coordinated(Refs): means reads + writes are guaranteed that execute in a non-race-condition manner.
;; Async(Agents): means the request to update is queued in a thread that will be executed later in time(fire-and-forget)
;; Retriable(Atom+Refs): the request to make the update is retried
;; Thread-Local(vars): means the changes can be isolated to a single thread of execution.

(for [x [1 2 3]] (str x)) ;; ("1" "2" "3")
(for [x [1 2 3] y [4 5 6] :while (< x y)] [x y]) ;; -> cartesian product of tuples x, y

(doc future)
(import 'java.util.concurrent.Executors)

;; In Clojure, you can use futures to place a task on another thread. You can create a future with the future macro.
(do
  (future (Thread/sleep 3000)
    (println "Awaked going to print some'"))
  (println "this is printing right now"))
;; executing a Thread/sleep on the current REPL would block the repl-user from doing anything until the time expires.
;; however future THROWS your body of the future into another thread, allowing for non-blocking mechanisms.

;; Futures differ from the plain clojure objects, in that we should DEREFERENCE them in order to reach the values.
;; futures can be dereference either by using the [deref] function, either using the [@] macro.

(def f (future (Thread/sleep 5000) (println "Going to print in that future-thread after 5 secs") "some"))
@f ;; --> dereferencing a future BLOCKS, that means the dereference opeeation blocks the whole word when invoked!
;; "some" here is returned from the future right after the bodies have completed, hence @ or deref are blocking mechanism.

;; an important part is that the future values WILL BE CACHED!!!
(let [f (future (println "This is cached!") (+ 1 1))]
  (deref f)   ;; prints only once
  @f)         ;; this would not print anything
;; This is cached + 2

;; Dereferencing a future will block if the future hasn't finished running!!!

;; but there's the [realized?] function which lets us check the status of a running future.
(realized? (future (Thread/sleep 1000))) ;; false
(if (realized? (future (Thread/sleep 1000)))
      (str "realized")) ;; nil

;; Calling future defines the task and indicates that it should start being evaluated immediately.
;; But, it also indicates that you don't need the result immediately.
;; otherwise -> think chronological couplings

;; When you dereference a future, you indicate that the result is required right now, and when dereferencing
;; we wait until the future-task is completely finished.
;; Alternatively we can ignore the result, as when writing to a log file <- fire and forget...
;; This can help creating the Actor's model -> sending a message to other actor without bothering
;; of the result of execution.

;; Offcourse futures dereference mechanism can be somehow controlled by the time-outs.

;;;;;;
;; Delay macro
;;;;;;
;; Delays allow us to define a "task-definition" without having to execute it, right aways, or
;; require the result immediately.
;; Delays are behaving as complement to futures, in that we controll WHEN the task should be executed.
;; we do this by using [force] function. <-- causing a task to start.
(def d (delay
          (let [msg "some nice message would only be executed when [force] is invoked on this delay"]
                msg)))

d         ;; Delay -> pending -> not executed YET
(force d) ;; "some nice message..." gets printed

;; As in [future] case [delay] requests are executed ONLY ONCE! and the results are CACHED!

;;;;;;;;;
;; Promises
;;;;;;;;;
;; Promises allow to decouple the results of executions, from the task that produce those results,
;; and WHEN that task is scheduled to run.
;; for creating a promise: [promise], and serve a value to a promise using: [deliver]
(doc deliver) ;; deliver to-promise value -> nil
;; One can only deliver a value to a promise ONLY ONCE, as in the case with futures/delays.
;; trying to dereference the value earlier would yield a blocking mechanism.

;; Promises open the world to clean asynchronous programming style. For instance in javascript,
;; jQuery provides promises, that provide the solution to what is called - callback-hell scenario.
;; the cool thing in clojure, is that one can [deliver] values to the promise in cause.
;; in fact this is the only mechanism to pass something to a promise, since a promise
;; returns a promise-object
(let [my-promise (promise)] (class my-promise)) ;; clojure.core$promise$reify
(let [my-promise (promise)]
  (deliver my-promise 4)
  (deref my-promise))  ;; 4

;; the [deliver] func can be used oNLY ONCE to pass a value to a promise.
(let [my-promise (promise)]
    (println "Future will start immediately in another thread but waiting the deref-promise-value")
    (future  (println (format "future gets promise value and executes: %1s" (deref my-promise))))
    (Thread/sleep 2000)
    (deliver my-promise "cclaudiu"))

;; This is in fact what i'm finding interesting, while using promises. To control the execution
;; from outside and not from inside the callbacks.
;; Here, defined [future] starts execution immediately in another thread, but it has a dependency
;; on the dereferenced value for the promise. Other computation might replace Thread/sleep,
;; and once we have some ajax-callback-respone, we can [deliver] it to our RUNNING FUTURE,
;; that will resume execution.
;; This concept requires mixin the promise with the future use-cases.

;; Instead, "state" means "the value of an identity at a point in time."
;; Atoms correspond to the idea of values in Clojure.
;; It's clear that numbers are values. How would it make sense for the number 15 to "mutate"
;; into another number? It wouldn't! All of Clojure's built-in data structures are likewise values.
;; They're just as unchanging and stable as numbers.

;; These atoms are produced by some metaphysical process.
;; You'll see how Clojure implements this idea in a minute.
;; For example, the "Cuddle Zombie" process gets applied to the atom F1 to produce atom F2.
;; The process then gets applied to the atom F2 to produce atom F3, and so on.

;; This makes even more sense when you consider that, in your programs, you are dealing with
;; information about the world. It doesn't make sense to say that information has changed;
;; it only makes sense to say that you've received new information.
;; At 12:00pm on Friday, Fred was in a state of 50% decayed. At 1:00pm, he was 60% decayed.
;; These are both facts that you can process, and
;; THE INTRODUCTION OF A NEW FACT DOES NOT INVALIDATE A PREVIOUS FACT.

;; Now i consider the functional thinking is more into normal processes and concepts than OOP.
;; Why because, it makes sense to think, that if you change, you don't invalidate old states.
;; those states remain unchanged, but it is the thinking that NEW events/states are added
;; and old/states cannot be changed anymore.


