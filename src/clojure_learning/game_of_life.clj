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
matrix

;[row col]
;[0 0]:: [0 1] [1 0] [1 1]
;
;[1 1]:: [0 0] [0 1] [0 2]
;        [1 0]       [1 2]
;        [2 0] [2 1] [2 2]


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
(gen-neighbours-by-row [0 1] inc) ;; ((1 0) (1 1) (1 2))
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
    ((comp vec concat)
      (map vec (gen-neighbours-by-row [row col] previous-row-func))
      (map vec (remove #(= cell-coordinates %) (gen-neighbours-by-row [row col] current-row-func)))
      (map vec (gen-neighbours-by-row [row col] next-row-func))
    )))
    
(gen-neighbours-for [0 1]) 
;; ((0 0)   ..   (0 2)
;;  (1 0) (1 1)  (1 2))

matrix
(defn transpose-table->positions 
  [[head-row-with-cols & tail-rows] row-counter transposed-table-acc]
    (letfn [(generate-row-with-cols-tuple [row-counter cols-counter]
              (map vec 
                   (partition 2
                        (interleave
                          (repeat cols-counter row-counter)
                          (take cols-counter (iterate inc 0))))))

            (structure-cells [head-row-with-cols row-counter]
              (vec (map vec
                        (partition 2
                            (interleave 
                                head-row-with-cols 
                                (generate-row-with-cols-tuple row-counter (count head-row-with-cols))))))) ]
      (if (nil? head-row-with-cols) 
        transposed-table-acc
        (recur tail-rows 
               (inc row-counter) 
               (conj transposed-table-acc 
                     (structure-cells head-row-with-cols row-counter)
)))))
        
(take 3 (iterate inc 0))
(transpose-table->positions [["0" "0" "0" "#" "0"] ["0" "0" "0" "0" "0"]] 0 [])

(def transposed (transpose-table->positions [["0" "0" "0" "#" "0"] ["0" "0" "0" "0" "0"]] 1 []))
transposed

(get-in [[0 1 2 3] [4 5 6 7]] [0 2])
(get-in [
         [{:alive false :adj [[1 0] [1 1] [0 1]]}, {:alive false :adj [[0 0] [0 2] [1 0] [1 1] [1 2]]}]
         [{:alive false :adj [[3 4] [5 6]]}, {:alive true :adj [[1 2] [1 4] [0 1]]}] ]
        [1 0]) ;; this is the data-structure i expect!!!
(if (:alive {:alive true}) (str "OK") (str "NOK")) ;; OK!

(defn structure-cells-with-neighours 
  [[head-row-cols-with-positions & tail-rows] mapped-cells-with-neighbours]
  (if (nil? head-row-cols-with-positions) 
    mapped-cells-with-neighbours
    (recur tail-rows
       (conj mapped-cells-with-neighbours
             (vec (map (fn [cell-with-position]
                                     {:alive (= "#" (first cell-with-position))
                                      :neighbours-pos (gen-neighbours-for (second cell-with-position))})
                             head-row-cols-with-positions))))
))

(defn map-cells-status-with-neighbours [matrix]
  (let [starting-row 0
        transposed-cells-with-positions (transpose-table->positions matrix starting-row [])]
    (structure-cells-with-neighours transposed-cells-with-positions [])
    ;; iterate
))

matrix
(map-cells-status-with-neighbours matrix)

;; once we mapped one cell "0" as::
; { :alive false,
;   :neighbours-pos ((3 3) (3 4) (3 5) (4 3) (4 5) (5 3) (5 4) (5 5))}

;; next thing is to iterate each cell's neighbour and find in the 
;; transposed cells for each neighbour its liveness state

(defn map-cells-with-alive-neighbours 
  [[head-mapped-cols-with-neighbours & tail-rows]
   mapped-cells
   cells-table-with-alive-neighbours]
  (if (nil? head-mapped-cols-with-neighbours) 
    cells-table-with-alive-neighbours
    (recur 
      tail-rows
      mapped-cells
      (conj cells-table-with-alive-neighbours
            (map
              (fn [cell-with-neighbours-positions]
                (reduce 
                  (fn [alive-neighbours-acc neighbour-position]
                    (let [neighbour (get-in mapped-cells neighbour-position)]
                      (if (:alive neighbour)
                        (conj alive-neighbours-acc neighbour-position)
                        alive-neighbours-acc
                        )))
                  [] (:neighbours-pos cell-with-neighbours-positions))
                )
              head-mapped-cols-with-neighbours)
      ))
  ))

(def neighs (map-cells-status-with-neighbours matrix))
neighs
(get-in neighs [4 4])
(map-cells-with-alive-neighbours neighs neighs [])