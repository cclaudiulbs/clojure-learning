(ns clojure-learning.game-of-life)
(require '[clojure.string :as str])

;Each cell interacts with its eight neighbours (horizontal, vertical, diagonal)
;1) Any live cell with fewer than two live neighbours dies, as if caused by under-population.
;2) Any live cell with two or three live neighbours lives on to the next generation.
;3) Any live cell with more than three live neighbours dies, as if by overcrowding.
;4) Any dead cell with exactly three live neighbours becomes a live cell, as if by reproduction.

(def life ["     "
           "     "
           " ### "
           "     "
           "     "])

(defn replace-ws-with-char [strings replacer] 
  (map #(str/replace % #"\s" replacer) strings))

(def r (replace-ws-with-char life "0"))

(defn table->matrix [table]
  (map #(str/split % #"") table))

(def matrix (table->matrix r))

[row col]
[0 0]:: [0 1] [1 0] [1 1]

[1 1]:: [0 0] [0 1] [0 2]
        [1 0]       [1 2]
        [2 0] [2 1] [2 2]


(defn gen-neighbours-by-row [[row col] row-func]
  (letfn [(clean-alien-cels [cels-coordinates]
            (filter (fn [cell-coordinate]
                      (= 2 (count (remove neg? cell-coordinate))))
                    cels-coordinates))
          (gen-raw-neighbours-by-row [[row coll] which-row-func]
            (partition 2 
                       (interleave (repeat 3 (row-func row)) 
                                   (range (dec col) (inc (inc col))))))]
    (clean-alien-cels
      (gen-raw-neighbours-by-row [row col] row-func))))
  
(gen-neighbours-by-row [0 1] dec) ;; ()
(gen-neighbours-by-row [1 1] dec) ;; ((0 0) (0 1) (0 2))

(defn gen-neighbours-for [[row col :as cell-coordinates]]
  (letfn [(clean-alien-cels [cels-coordinates]
                      (filter (fn [cell-coordinate]
                                (= 2 (count (remove neg? cell-coordinate))))
                              cels-coordinates))

          (gen-raw-neighbours-by-row [[row coll] which-row-func]
            (partition 2 
                (interleave (repeat 3 (which-row-func row)) 
                            (range (dec col) (inc (inc col))))))

          (gen-neighbours-by-row [[row col] row-func]
            (clean-alien-cels
              (gen-raw-neighbours-by-row [row col] row-func)))

          (previous-row-func [coordinate] (dec coordinate))
          (next-row-func [coordinate] (inc coordinate))
          (current-row-func [coordinate] (identity coordinate))]
    (concat
      (gen-neighbours-by-row [row col] previous-row-func)
      (remove #(= cell-coordinates %) (gen-neighbours-by-row [row col] current-row-func))
      (gen-neighbours-by-row [row col] next-row-func)
    )))
    
(gen-neighbours-for [0 1]) 
;; ((0 0)   ..   (0 2)
;;  (1 0) (1 1)  (1 2))

matrix
