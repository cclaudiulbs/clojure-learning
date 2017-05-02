(ns clojure-learning.state-monad
  (use (clojure repl test)))

(defrecord State [language])
(def clojure (->State "clojure"))
(println (:language clojure))

;; next, we may want to replace something making the State mutable

(defprotocol StateMutation
  (set-state! [on new-val] "replaces the value of a given defrecord"))

(extend-type State
  StateMutation
  (set-state! [on new-val] (assoc on :language new-val)))

(do
  (println (:language clojure))
  (set-state! clojure "javascript")
  (println (:language clojure)))
;; clojure
;; clojure

;; as seen there's no way we can mutate a hash -> a new hash is returned back, the mechanism for encapsulating this
;; behavior is kept inside the [bind] function


;; (set-state! (return "old-state") "new-state")

(defn update-state! [mv]
  (bind mv (fn [state-map new-value] (assoc state-map :state new-value))))

(defn return [new-val] 
  (fn [] {:state new-val})) ;; -> mv

(defn bind [mv func]
  (fn [new-val] (func (mv) new-val)))

;; in action:
(def old-state (return "old-state"))

(def change-old-state-to (update-state! old-state))
(change-old-state-to "new-state")     ;; {:state "new-state"}
(change-old-state-to "future-state")  ;; {:state "future-state"}

;; in action using thread-first macro:
((-> (return "old-state")
     update-state!) 
 "future-state")

;; output:  {:state "future-state"}

(def old-state (return "old-state"))
(def to-new-state (set-state! old-state "new-state"))

(old-state)
(to-new-state)

