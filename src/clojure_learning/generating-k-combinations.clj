;; func 3 #{0 1 2 3 4} -> possible k-combinations
;; my algorithm :)
;; = 3 k
;; = 5 n
;; -> 10 unique combinations
;; -> cond k <= n
;; #{0 1 2 3 4} ->
;; #{#{0 1 2} #{0 1 3} #{0 1 4} #{0 2 3} #{0 2 4}
;;   #{0 3 4} #{1 2 3} #{1 2 4} #{1 3 4} #{2 3 4}}
;;;;;;;;;;;;;;;;;
II
;; 0 1 2 3 4
;; start from 0: first and go cycle
;; 0 1 2
;; 1 2 3
;; 2 3 4
;; 3 4 0
;; 4 0 1
;; 
;; -> pop second -> and push to tail -> 0 2 3 4 1 -> there can be a separate rem-coll which decreases when all cycle is completed
;; 0 2 3
;; 2 3 4
;; 3 4 1
;; 4 1 0
;; 1 0 2

;; -> pop second -> and push to tail -> 0 3 4 1 2
;; 0 3 4
;; 3 4 1
;; 4 1 2
;; 1 2 0
;; 2 0 3

;; -> pop second -> and push to tail -> 0 4 1 2 3
;; 0 4 1
;; 4 1 2
;; 1 2 3
;; 2 3 0
;; 3 0 4

;; -> pop second -> and push to tail -> 0 1 2 3 4 -> DONE recurring! with this branch

III
;; -> pop third -> and push it to tail -> 0 1 3 4 2
;; 0 1 3
;; 1 3 4
;; 3 4 2
;; 4 2 0
;; 2 0 1

;; -> pop third and push it to tail -> 0 1 4 3 2
;; 0 1 4
;; 1 4 3
;; 4 3 2
;; 3 2 0
;; 2 0 1

;; -> pop third and push it to tail -> 0 1 3 2 4
;; 0 1 3
;; 1 3 2
;; 3 2 4
;; 2 4 0
;; 4 0 1

;; -> pop third and push it to tail -> 0 1 2 4 3
;; 0 1 2
;; 1 2 4
;; 2 4 3
;; 4 3 0
;; 3 0 1

;; pop thid and push it to tail -> 0 1 4 3 2 --> DONE -> the case is met above

