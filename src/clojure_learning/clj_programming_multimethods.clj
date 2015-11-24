;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Clojure Programming: Multimethods(Chap 7)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(ns clojure-learning.clj-programming-multimethods
	(use [clojure.repl])
	(require [clojure.test :refer :all]
             [clojure.string :as str]))

;; define the defmulti
(defmulti date-parser 
  "simple polymorphic dispatcher for selecting the corresponding date-format for date-conversions"
  (fn dispatcher [context] (:formatter context)))

;; if one knows or wants to lock the implementation of some types at defmulti creation time
;; a solution might be to hardcode the possible values that the dispatch-func is delegating as:
(fn [{:keys [formatter]}]
  (if-let [result (some #{:eu :us :none} [formatter])] result :none))

;; define the implementation for each defmulti -> the continuation for multi...->defmethod
(defmethod date-parser ::eu
  [context]
  (println "implementing the eur date-formatter logic" (:formatter context)))

(defmethod date-parser ::none
  [context]
  (println "no method found!" (:formatter context)))

;; whenever the dispatcher implementation is NOT FOUND -> provide the :default implementation by convention!
(defmethod date-parser :default
  [context] (println "default is invoked"))

;; client-exported api
(date-parser {:formatter ::eu :date "29/01/1987"})
(date-parser {:formatter ::none :date "29/01/1987"})

;; the dispatcher function takes exactly the context-arguments (domain args) that the client will pass.
;; however, based on some logic, it will return a value. That returned value will have TO MATCH
;; with any defined defmethods tag: defmethod a-method :tag
;; the :tag will be the returned value from the dispatcher function.
;; if the dispatcher function uses a general implementation -> new defmethods can be introduced at a later
;; point in time (after the defmulti-dispatcher-func is implemented):
(defmulti a-multi-method (fn dispatcher [caller-context] (:delegate caller-context)))

;; this way the dispatcher function is clever enough to adapt to a new defmethod, so that the client
;; can pass a key for it to be dispatched to.
(defmethod a-multi-method ::console 
  [context] (println "printing on " (:delegate context)))
(defmethod a-multi-method ::memory
  [context] (println "printing on " (:delegate context)))

(a-multi-method {:delegate ::console})
(a-multi-method {:delegate ::memory})

;; arg destructuring works for maps if the :keys are known when doing the destructuring
;; if "foo" is replaced by bar, when the destructuring is done -> exception thrown
(let [{:keys [foo]} {:foo "some"}]
  foo) ;; some

(let [a-val (last (vals {:foo "context"}))]
  a-val) ;; -> context is out

(let [a-val (last (vals {:foo "dd/mm/yyyy"}))]
  (= 3 (count (str/split a-val #"\/")))) ;; true

(let [a-val (last (vals {:foo "dd/mm/yyyy"}))]
  (str/split a-val #"\/")) ;; ["dd" "mm" "yyyy"]

(let [{:keys [foo] :as m} {:foo "dd/mm/yyyy"}]
  (= 3 (count (str/split foo #"\/")))) ;; true

(let [{:keys [formatter _]} {:formatter :eu :date "some-date"}]
  (some #{:eu :us :none} [formatter])) ;; :eu

;; Note: defmulti has defonce semantics, in that if we try to redefine an existing defmulti, our changes are silently
;; ignored, because we should unmap it from the current ns: "removes the mappings for the symbol from the current ns"
(ns-unmap *ns* 'a-multi-method)
(a-multi-method {:delegate ::console}) ;; -> unable to resolve symbol...! cool

;; defmulti openes the gate to multiple-inheritance, using "derive" function.
;; derive ::from ::descendants
;; for instance we can define a multimethod:
(defmulti run "Should invoke the corresponding routine based on the class Type" class)
(defmethod run Runnable
  ([func] 
   (do (func)
       (println "Runnable invoked!"))))
(defmethod run Callable
  ([func] 
   (do 
      (func) 
      (println "Callable invoked!"))))

;; note that we defined the Multimethod "run" here for both Runnable + Callable. Since any clojure-functions inherit
;; from both Runnable and Callable, when the first attempt to call the runnable will yield an exception!
(run #(println "somethign")) 
;; -> Exception: multiple matches, and neither is preffered!

;; prefer-method first-favourite sec-favourite
(prefer-method run Runnable Callable)
(run #(println "from a function"))  ;; -> runnable invoked!!!

;; What happened?
;; the function to dispatch in this case is too wide: "class" -> taking the domain arg in this case the 
;; anonymous literal function, the multimethod dispatcher function yields a Func back.
;; the thing is, that we created defmethods for broader Types than function, and every function
;; inherits from both of them. -> in this case multiple-behavioral-inheritance.

;; Using multimethods one can change the behavior of the multimethod on the fly dynamically dependning
;; on the Type that is used by the dispatching function. in other words, changing the Type of dispathing
;; function (the domain args) will change the output of the multimethod.

;; on the other hand there's a downside to this:
;; A multimethod whose behavior is not strictly dependent upon the values provided as arguments is, 
;; by definition, not idempotent.
;; but let's implement this dependent example

;; the keys are identifying the actions uniquely; atom is used to maintain a mutable state
(def priorities (atom {:911-call :high
                       :evacuation :high
                       :go-lunch :low
                       :finish-work :low}))

(defmulti route-message 
  "will dispatch based on the priority assigned to each task"
  (fn dispatch-func [domain-args]
    (@priorities (:type domain-args)))) ;; deref the priorities atom using "@priorities"

;; using the :type which is the :key of the domain-arg-map -> fetch the :key used in the atom-map
;; using that value of the atom-priorities -> return the dispatched value for the defmethod

(defmethod route-message :high
  ([{:keys [type]}]
    (println "alert the authorities, there's a: " (name type))))

;; the domain-arg-map is destructured and we're taking the name for the value bound to :type-key
(defmethod route-message :low
  ([{:keys [type]}]
   (println "oh, there's another...put in the log..." (name type))))

(route-message {:type :911-call})    ;; ...alert...there's a 911-call
(route-message {:type :evacuation})  ;; ...alert...there's a evacuation
(route-message {:type :go-lunch})    ;; ...oh...put...log go-lunch

;; then swapping the priorities (mutating them) will yield in a change in the behavior, that the
;; other dispatched function defmethod is invoked.
;; what if the message priorities themselves can change dynamically
(swap! priorities assoc :911-call :low) ;; -> changing the priorities
(pr  @priorities) ;; -> mutated

(route-message {:type :911-call})    ;; ...of...put...log 911-call

;; Final Thoughts:
;; whenever you find yourself, having some nested conditional ifs or "cond"s -> think about refactoring
;; the logic for a defmulti approach


