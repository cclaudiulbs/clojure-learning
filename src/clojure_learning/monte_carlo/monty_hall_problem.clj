(ns clojure-learning.monte-carlo.monty-hall-problem)

;; Monty Hall in Monte Carlo
;; https://data-sorcery.org/2009/06/23/monty-hall-in-monte-carlo/

;; each room has a weight of 1/3 chances of wining. Once a room has been selected,
;; before opening it, Monty will then OPEN the door of another room. That room
;; is DEFINETELLY NOT the winner, leaving you the choosing option to select the LAST
;; maybe winner door(switching the already selected room). Monty will NOT provide
;; the door-openining for the winner room by any means. So the thing concludes whether
;; the chances of switching between the already selected(but not opened door)
;; and the last-room.

;; for a range of generated 1000 attempts -> check what are the chances of winning
;; if choosing the already-selected door
;; for a range of generated 1000 attempts -> check what are the chances of wining
;; if choosing the last door(not the one selected, not the one opened by Monty)

;; Set a simulation sample size, generate samples of initial-guesses, prize-doors, 
;; and switch decisions with the sample function. The probability that the prize is 
;; behind any of the three doors is 1/3, as is the probability that you select a particular 
;; door as an initial guess, while the probability that you switch doors is 1/2.

(defrecord Door [door-idx prize])

(defn doors-generator []
  "will generate 3 rooms guarded by doors and will place a prize inside a
   randomly chose room, leaving the other 2 rooms with some goats inside"
  (let [doors-idxs (take 3 (iterate inc 1))
        rand-prize-room (rand-nth doors-idxs)]
    (map (fn [door-idx] 
           (if (= rand-prize-room door-idx)
               (Door. door-idx :win)
               (Door. door-idx :goat)))
          doors-idxs)
))

(doors-generator)
#_({:door 1, :prize :goat}
  {:door 2, :prize :win}
  {:door 3, :prize :goat})
#_({:door 1, :prize :win}
  {:door 2, :prize :goat}
  {:door 3, :prize :goat})

(defn select-door-nmber [] (rand-nth (take 3 (iterate inc 1))))
(select-door-nmber) ;; randomly chose rooom-number between 1..3

(defn get-selected-door [doors selected-door-nmber]
  (first (filter #(= selected-door-nmber (.door-idx %)) doors)))

(get-selected-door [(Door. 1 :goat)] 1) ;; {:door-idx 1, :prize :goat}
(get-selected-door [(Door. 1 :goat)] 2) ;; nil

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; final function that computes the probabilities a user has to WIN, if staying to
;; the initial decision or switching the door for monte-carlo simulation::monty-hall problem
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn monte-carlo-monty-problem [doors-number attempts]
  "decision should be [:stay] or [:reconsider]"
  (letfn[(select-door-number [] (rand-nth (take doors-number (iterate inc 1))))

         (doors-generator [doors-num]
           "will generate n rooms guarded by doors and will place a prize inside a
            randomly chose room, leaving the other 2 rooms with some goats inside"
           (let [doors-idxs (take doors-num (iterate inc 1))
                 rand-prize-door (select-door-number)]
             (map (fn [door-idx] 
                    (if (= rand-prize-door door-idx)
                        (Door. door-idx :win)
                        (Door. door-idx :goat)))
                   doors-idxs)))
         
         (door-excluding [doors-to-exclude doors]
           (first ;; realize from seq
             (remove doors-to-exclude doors)))
         
         (get-selected-door [doors selected-door-nmber]
           (first ;; realize from seq
             (filter #(= selected-door-nmber (.door-idx %)) doors)))
         
         (find-winner-door [doors] 
           (first (filter #(= :win (.prize %)) doors)))
         
         (probabilities [doors-number attempts-bound]
           (repeatedly attempts-bound
             (fn[] 
               (let [doors (doors-generator doors-number)
                     selected-door (get-selected-door doors (select-door-number))
                     monty-non-winner-door (door-excluding 
                                             (set [selected-door (find-winner-door doors)])
                                             doors)
                     switch-door (door-excluding (set [selected-door monty-non-winner-door]) doors)]
                 [selected-door switch-door]
              ))
         ))
         
         (winner-door-occurences [doors]
           (count (filter (fn [door] (= :win (.prize door))) doors)))
         
         (to->percentage [win-result attempts]
           (str (/ (* (double win-result) 100) 
                   attempts) "%"
         ))]
    (let [generated-probabilities (probabilities doors-number attempts)
          win-by-select-door (winner-door-occurences 
                               (map first generated-probabilities))
          win-by-switch-door (winner-door-occurences 
                               (map second generated-probabilities))]
      
      {:win-by-stay (to->percentage win-by-select-door attempts)
       :win-by-switch (to->percentage win-by-switch-door attempts)}
)))

;; exercising monte-carlo monty-hall problem::
(monte-carlo-monty-problem 3 10000)
