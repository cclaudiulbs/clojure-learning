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

;;;;;;;;;;;;;
;; Atom Reference Types
;;;;;;;;;;;;;
;; Atoms are bound to reference only, as they point to some immutable data-structures. however
;; how i see it, is that an atom operations are defined atomically, that is: either they all succeed
;; as a unit, either they fail as a whole unit. Now an atom implements the "compare-and-set" semantics
;; in that the value to be set on the atom is compared first with the previous value, and then set to the
;; new state. The mechanism is more complex...though. unlike futures/delays/promises any attempts to
;; dereference them would NOT result in a blocking mechanism. Oposed with futures/delays/promises which
;; may be interpreted as: "give the result of the computation NOW, i will wait until is done",
;; the atoms behave slightly in other direction: "give the result of the atom-reference-type regardless
;; the state in which the computation/value is" -> non-blocking!

(def my-atom (atom {:name "claudiu" :age 34}))

;; 2 types of dereferencing are available:
@my-atom
(deref my-atom)

;; once, the atom may be updated, however is not the atomic referenced data which changes, but is only the
;; reference type. remember clojure has (oposed to java) only mutable references(not mutable references
;; that point to mutable data, as java). but the operations on the atoms are entirely thread-safe and easy to
;; share accross threads.

;; the operation to update an atom-reference-type is: [swap!]:
;; func signature:   swap! [atom-ref func[atom-curr-state]] -> atom-ref-updated

(swap! my-atom (fn [curr-state] (merge-with + curr-state {:age 50})))
@my-atom

;; it is impossible for my-atom to remain in an unconsistent state.
;; it is also possible to not lock/hardcode the behavior of updating the atom in the callback func itself,
;; but rather pass an external value to "update" the atom-reference-type.
;; the thing is that the callback-func to be applied on the atom-reference-type, takes normally one argument
;; that is the atom-current-state. but it also can behave and take n-ary arguments. the condition is that
;; the current-atom-reference-state should be the 1st argument:

;; 1. define a func agnostic:
(defn update-my-atom
  [curr-state new-age]
  (merge-with + curr-state {:age new-age}))

(update-my-atom {:name "cclaudiu" :age 24} 60) ;; {:name "cclaudiu" :age 84}

;; use the defined function to handle atom's state
@my-atom
(swap! my-atom update-my-atom 20)   ;; {:age 170 :name "cclaudiu"}

;; Compare and set:how it works?
;; It reads the current state of the atom
;; It then applies the update function to that state
;; Next, it checks whether the value it read in step 1 is identical to the atom's current value
;; If it is, then swap! updates the atom to refer to the result of step 2
;; If it isn't, then swap! retries, going through the process again with step 1.

;; one thing to note, is that [swap!] operates synchronous, that is if the callback function happen to
;; call Thread/sleep -> the update blocks for certain period.

;; The power of prefix notation:
(< 100 200 300) ;; true
(let [x 200]
  (< 100 x 300)) ;; true

;; While atom-reference-types allow us to manage the state of independent identities, there are several times
;; when we want to cascade a set of changes over a suite of other references. This operation is not approach-able
;; using atoms, as they relate to mutate reference on each atom indepdendently. We need some sort of mechanism
;; to simulate transactional db semantics. In Clojure ACI (without D-urables) are supported, since Durable
;; reffers to persist in some data-store the changes, so that are used later in time.
;; A-tomic: reffers that either all operations are cascaded as a single unit, either not.
;; C-onsistent: the state always appear in a consistent state to other-transactions, no partial state here.
;; I-solated: the changes that are done in one transactions are not seen until there's a commit mechanims
;;           by other transactions. each transaction has its piece of cake, which is private from others

;; Clojure provides all the A-C-I properties of a database transaction, but in-memory.
;; [Refs] provide the same concurrency safetyness as a database-transaction.(we're not detail STM for now
;; but only how to it is applied in clojure)

;; modifying a ref is done by using the mutable-function: [alter] which MUST be done in a transaction.
;; each transaction is started by invoking the: [dosync] macro.
(doc dosync) ;; runs the expression in an implicit [do] in a transaction. starts a transaction if NONE
;; is already running on this thread. -> any uncaught exception aborts the transaction. the effects on Refs
;; are atomic.
;; any [alter] ing is done in isolation, and cannot be see from outside of the current transaction -> can
;; call [alter] as many times as it is needed.
;; a toy example would be sufficient to understand the power of STM that clojure provides:
(doc alter) ;; -> must be called in a transaction. alter [ref func & args]

(def counter (ref 0))
;; spawn 2 threads asynchronous: 1 starts a transaction
(do
  (future
    (dosync
       (alter counter inc)
       (println @counter)
       (Thread/sleep 1000)
       (alter counter inc)
       (println @counter)))
  (Thread/sleep 300)
  (println @counter))

;; 1   --> printed from inside the future-spawned thread. that thread now sleeps and Scheduler passes to the main thread
;; 0   --> which does NOT sees the update yet, and prints 0, the initial state of counter, time passes
;; 2   --> and the future-thread has the chance again to execute, alters again the state and prints 2
@counter ;; 2

;; automatically the transaction commits the changes when it ends.no need for explicitly closing the transaction.
;; the commit works similarly to the atom-reference-types compare-and-set semantics. including retries for transaction
;; commits.

;; "Safe, easy, concurrent coordination of state changes."

;; Here's how [alter] behaves:
;; 1. Reach outside the transaction and read the ref's current state.
;; 2. Compare the current state TO the state the ref STARTED within the transaction.
;; 3. If the two differ, make the entire transaction RETRY
;; 4. Otherwise COMMIT the altered ref state

;; Refs have one more trick in the bag: [commute]; here's how commute behave as opossed to [alter]
;; 1. Reach outside the transaction, and take the current state of ref(same as alter until here)
;; 2. Run the [commute] function AGAIN using the current taken state.
;; 3. Commit the result

;; [commute] does NOT use a transaction-retry mechanism!!! instead it is used mainly as a performance improver
;; but this function can leave the data in an inconsistent state. because it does not uses a retrial mechanism
;; it takes the outside-curr-state only once, and then commits it.

;;;;;;
;; vars
;;;;;;
;; vars are associations/mappings from symbols to objects. New vars can be created with [def]

;; this example is just to debate with what the author from the braveclojure says about constant vars binding:
;; "Unlike regular vars, however, you can temporarily change the value of dynamic vars by using binding"
(def my-job "clojure-programmer")
(do
  (println my-job)
  (let [my-job "js-programmer"]
    (println my-job)))
;; prints both: clojure-programmer from outside scope, and also js-programmer as a rebinding of the inner local scope.

;; clojure also has dynamic-vars, that simulate the use of static-bindings defined by [let] and add several
;; things on top.
;; a dynamic var is in the form of:
(def ^:dynamic *some-job* "clojure-programmer")
(binding [*some-job* "js-programmer"]
  *some-job*)
;; js-programmer

;; the lisp notion of: earmuffers when it comes to declare the name of a dynamic binding: *name*
;; we can also nest bindings in other bindings, and the scope is kept consistent.

(def ^:dynamic *to-mail* "claudiu.cosar@gmail.com")
(defn send-mail [message]
  (str "TO: " *to-mail* ", message: " message))

(send-mail "aloha") ;; "TO: claudiu.cosar@gmail.com, message: aloha"
;; in our tests we may want to mock and don't spam claudiu with all emails, so we need a re-binding
(binding [*to-mail* "test@gmail.com"]
  (send-mail "test case sending mail to test recipients..."))
;; "TO: test@gmail.com, message: test case sending mail to test recipients..."


;; let's try it with a static-binding-let:
(def somebody "cclaudiu@gmail.com")
(defn send-mail [message]
  (format "TO: %1s, message: %2s" somebody message))

(send-mail "clojure real name") ;; wysiwyg

;; attempt to re-bind using local-let
(let [somebody "test@gmail.com"]
  (send-mail "mocking through core [let] a test-recipient"))
;; "TO: cclaudiu@gmail.com, message: mocking through core [let] a test-recipient"

;; hence --> therefore same behavior for [let] and [binding] of "static-binding" vs "dynamic-binding"

;; most common dynamic vars are used to name a shared-common resource, like a built in dynamic resource:
;; *out*, or *print-length* and so on...
;; because they are dynamic we can control their visibility, and can "override" some of them to make
;; them approapriate in our context:
(binding [*print-length* 1]
  (print ["cclaudiu" "cosar"])) ;; prints: [cclaudiu ...]

;; well this is a behavior we cannot approach using well-known let-bindings. This sort of use is defined
;; in terms of configurations, hence dynamic-bindings are often used in configurations...

;; because dynamic-binding allows to mutate the state of some vars, using the [set!] function
;; and because i don't quite use mutation in clojure, i m gonna skip this part of side-effecting world.

;;;;;;;;
;; binding dynamically per thread of execution
;;;;;;;;
;; as we know by now there are dynamic vars bound to some values depdnedending on their execution context.
;; REPL binds the dynamic *out* to its REPL console, and so on.
;; when we create manually a new thread, every dynamic-binding var rollsback to its initial value.
(.write *out* "printed on the REPL as REPL bound this dynamic out-var to its REPL console")
(.start (Thread. #(.write *out* "printed on the STDOUT")))
(println "repl console")

;; to overcome this, we can use an intermediary var that captures the current *out* value(as "that" in js)
;; and then rebinding the *out* from within the local thread to the saved value
(let [repl-out *out*]
  (.start
     (Thread. #(binding [*out* repl-out]
                 (.write *out* "repl console in action")))))


;; Also note that any dynamic binding needs to be declared FIRST, prior using it and [binding]
(binding [*c* "that"])  ;; unable to resolve symbol var: c

;; declared:
(def ^:dynamic *c* "initial root value")
;; used:
(binding [*c* "that"]
  *c*)   ;; that
;; so using dynamic binding requires a 2-step approach

;; Clojure provides the option to alter a var's root value. using the function: [alter-var-root] we can
;; mutate the value of a var. I'm not interested in mutation when it comes to Clojure but we have to know
;; that we CAN if we want :)

;;;;;;;;;;;;;;;
;; Clojure parallelism with: pmap (divide and conquer)
;;;;;;;;;;;;;;;


(ns for-clojure)
(declare struct-type?)
(map struct-type? [#{} {} [] '()])  ;; (:set :map :vector :list)
(defn struct-type?
  [ds]
  (let [seqable? (fn [xs ys]
                   (and (= (inc (count xs)) (count ys)) (nil? (get ys xs))))
        new-ds (conj ds ds)]
    (if (seqable? ds new-ds)
      (if (identical? (last new-ds) ds)
        :vector
        (if (identical? (first new-ds) ds)
          :list))
      (if (= new-ds ds)
        :map
        :set
       ))))

(struct-type? [1 2])
(struct-type? [])
(struct-type? {})
(struct-type? #{})
(conj [] [])
(conj [1 2] [1 2])
(conj [:a :b] [:a :b])
(def ab (conj [:a :b] [:a :b]))
(get ab [:a :b])

(def ab (conj {:a :b} {:a :b}))
(get ab {:a :b})
(count [[]])
(def lol (conj '() '()))
(first lol)
