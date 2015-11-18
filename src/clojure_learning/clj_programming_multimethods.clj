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



