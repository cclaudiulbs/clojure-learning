(ns clojure-learning.monte-carlo.estimating-pi)

(defrecord Point [x y])

;; juxt:: juxtaposition func will take a coll of functions -> returns a NEW function
;; such that when applied on some args -> will apply each function on the arg/s and
;; will return a coll of those resulted values
(defn gen-point [end]
  "generating a single point between [0,0] -> [1,1]"
  (let [[x y] ((juxt rand rand) end)]
    (->Point x y)))

;; exercising
(gen-point 1)
(type (gen-point 1)) ;; clojure_learning.monte_carlo.estimating_pi.Point
;; {:x 0.961021656080538, :y 0.576415979741155}

(defn gen-points [until]
  (repeatedly until (partial gen-point 1)))

;; exercising
(gen-points 100)

;; center of the circle is identified by the Point{0.5, 0.5}
;; [X,Y]  that is the center of the Circle
;; R that is the radius of the Circle
;; [X1,Y1] that may or may not be in the circle.
;; -> (X - X1)^2 + (Y - Y1)^2 <= R^2
(defn point-in-circle? [radius center-point searched-point]
  "pure math Pythagorean theorem that identifies based on the radius, center-point and
   given-point of the given-point is inside the circle or not"
  (<= (Math/sqrt (+ (Math/pow (- (.x center-point) 
                                 (.x searched-point)) 2)
                    (Math/pow (- (.y center-point) 
                                 (.y searched-point)) 2)))
    radius))
        
(defn locate-points [points]
  "should return a map consisting of {:circle-points x :total-points y}"
  (let [center-point (->Point 0.5 0.5)
        radius 0.5]
    (count 
      (filter 
        (partial point-in-circle? radius center-point) points))))

;; exercise::
(locate-points (gen-points 100000))
;; 21805 or another invocation 21990

(defn calculate-pi [points-in-circle total-points]
  "applying a simple math-associative equation to get the value of PI
   Taken that the circle is inside the square and the division of circle-area
   with the square area: (pi x R^2) / (2 x R)^2 => pi / 4
   Now taken the total Npoints-in-circle / Ntotal-points = pi / 4
   => pi = 4 x (Npoints-in-circle/Ntotal-points)
   that's why an very big number of points will establish the accuracy of [pi]"
  (* 4 (/ points-in-circle total-points)))

(defn pi [points-num]
  "more number of points will ensure a more accurate value for PI"
  (let [points-in-circle (locate-points (gen-points points-num))]
    (double (calculate-pi points-in-circle points-num))))

(pi 100000000)