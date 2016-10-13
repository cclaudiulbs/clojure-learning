(ns clojure-learning.game-of-life)
(require '[clojure.string :as str])

;http://www.4clojure.com/problem/94
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

;; function outer
(defn replace-ws-with-char [strings replacer] 
  (map #(str/replace % #"\s" replacer) strings))

(def r (replace-ws-with-char life "0"))

;; function outer
(defn table->matrix [table]
  (map #(str/split % #"") table))

(def matrix (table->matrix r))
matrix

;[1 1]:: [0 0] [0 1] [0 2]
;        [1 0]       [1 2]
;        [2 0] [2 1] [2 2]

;; function inner
(defn gen-neighbours-by-row [[row col] row-func]
  "function that generates neighbours for given coordinate by applying the row-func
   which can be any of: inc(neighbours-for-next-row), dec(neighbours-for-previous-row)"
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
  
;; test
(gen-neighbours-by-row [0 1] dec) ;; ()
(gen-neighbours-by-row [0 1] inc) ;; ((1 0) (1 1) (1 2))
(gen-neighbours-by-row [1 1] dec) ;; ((0 0) (0 1) (0 2))

;; function inner
(defn gen-neighbours-for [[row col :as cell-coordinates]]
  "given a pair of coordinates on the table, generate all possible adjacent coordinates
   that serve as neighbours for the given cell"
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
    
;; testing
(gen-neighbours-for [0 1]) 
;; ((0 0)   ..   (0 2)
;;  (1 0) (1 1)  (1 2))

;; function outer
(defn transpose-table->positions 
  "given a matrix of cells(0 1) this function will map each dump-cell with it's corresponding
   coordinates on the table"
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
        
;; test
(def transposed->positions
  (-> matrix
    (transpose-table->positions 0 [])))
transposed->positions
;[... [["0" [4 0]] ["0" [4 1]] ["0" [4 2]] ["0" [4 3]] ["0" [4 4]]]]

;; function inner
(defn transpose-table-w-positions->map [table-cells-w-positions]
  "function that will flatten and normalize the cells-with-positions to a map for 
   faster/easier-to-understand/constant-time lookups"
  (let [flatten-cell->pos-pairs #(partition 3 (flatten %))]
    (reduce 
      (fn [mapped-cells [cell-primitive-state & cell-position]]
        (assoc mapped-cells (vec cell-position) cell-primitive-state))
      {}
      (flatten-cell->pos-pairs table-cells-w-positions))))

;; test
(def transposed->mapped-positions (transpose-table-w-positions->map transposed->positions))
transposed->mapped-positions
;{...[2 1] "#", [4 4] "0", [1 2] "0", ... }

;; function inner
(defn get-alive-neighbours [neighbours table-of-cells-w-positions]
  "function that takes some neighbours and returns the ones for which cells are alive"
  (let [map-of-cells-w-positions (transpose-table-w-positions->map table-of-cells-w-positions)
        alive? #(= "#" %1)]
  (reduce (fn [alive-neighbours-acc neighbour]
            (if (alive? (get map-of-cells-w-positions neighbour))
              (conj alive-neighbours-acc neighbour)
              alive-neighbours-acc))
    [] neighbours)))


;; function outer-highest
(defn one-generation->map-cells
  "function that takes a table-of-cells and maps each cell to the INITIAL SEED given.
   Each cell will have it's own view of the initial-seed hence might be updated simultaneously"
  [table-of-cells-w-positions-view]
  (letfn [(convert-cell [cell] (if (= "#" cell) {:alive true} {:alive false}))
          (identify-cell-state [cell] (if (true? (:alive cell)) :alive :dead))
          
          (cell-should-born? [cell identified-alive-neighbours] 
            (and (false? (:alive cell)) (= 3 (count identified-alive-neighbours))))

          (cell-should-die? [cell identified-alive-neighbours] 
            (and (:alive cell) (or (> (count identified-alive-neighbours) 3) ;; overpopulation
                                   (< (count identified-alive-neighbours) 2)))) ;; underpopulation

          (cell-should-live? [cell identified-alive-neighbours] 
            (and (:alive cell) (or (<= (count identified-alive-neighbours) 3)
                                   (>= (count identified-alive-neighbours) 2))))
          
          (bless-cell [cell alive-neighbours]
            (cond (cell-should-born? cell alive-neighbours)
                  :alive
                  (cell-should-die? cell alive-neighbours)
                  :dead
                  (cell-should-live? cell alive-neighbours)
                  :alive
                  :else (identify-cell-state cell)))]

    (map (fn [row-cells-with-position]
        (map (fn [cell-with-position]
               (let [[cell-primitive-state cell-position] cell-with-position
                     converted-cell (convert-cell cell-primitive-state)
                     alive-neighbours (get-alive-neighbours 
                                                  (gen-neighbours-for cell-position) table-of-cells-w-positions-view)]
                 (bless-cell converted-cell alive-neighbours)
             ))
             row-cells-with-position
        ))
        table-of-cells-w-positions-view
)))

;;test
(def transposed
  (-> matrix
    (transpose-table->positions 0 [])))

;; DONE! game of life!!!!
(one-generation->map-cells transposed)
;((:dead :dead :dead :dead :dead)
; (:dead :dead :alive :dead :dead)
; (:dead :dead :alive :dead :dead)
; (:dead :dead :alive :dead :dead)
; (:dead :dead :dead :dead :dead))

;;;;;;;;;;;;;;;;;;;;;;;
;; another test-case:: => try beating clojure at structuring the code into pipelined-chaining-computations
(->
  ["      "  
   " ##   "
   " ##   "
   "   ## "
   "   ## "
   "      "]
  (replace-ws-with-char "0")
  table->matrix
  (transpose-table->positions 0 [])
  one-generation->map-cells)

;; comparing outputs::
;["      "  
; " ##   "
; " #    "
; "    # "
; "   ## "
; "      "]
;((:dead :dead :dead :dead :dead :dead)
; (:dead :alive :alive :dead :dead :dead)
; (:dead :alive :dead :dead :dead :dead)
; (:dead :dead :dead :dead :alive :dead)
; (:dead :dead :dead :alive :alive :dead)
; (:dead :dead :dead :dead :dead :dead))
;; WHOOOOOOHOOOOOO :)