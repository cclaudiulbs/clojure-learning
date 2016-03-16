(ns clojure-learning.sausages
  (use clojure.repl))

(defn gcd [x y]
  (if (zero? y) x
    (recur y (rem x y))))

(gcd 8 2)

(gcd 6 2)
(gcd 4 3)

(gcd 12 4)

(defn sausages-cuts 
  ([sausage taster] (sausages-cuts sausage taster 0))
  ([sausage taster cuts]
   (if (= 1 taster) cuts
      (if (> sausage taster)
      (recur (quot sausage taster) taster (quot sausage taster))
        (if (zero? (rem taster sausage))
          (* sausage (dec (quot taster sausage)))
          (recur (rem taster sausage) taster (* sausage (quot taster sausage))))))))

(sausages-cuts 4 12) ;; 8
(sausages-cuts 2 6)  ;; 4
(sausages-cuts 3 4)  ;; 3
(sausages-cuts 4 8)  ;; 4
(sausages-cuts 2 1)  ;; 0
(sausages-cuts 2 4)  ;; 2
(sausages-cuts 4 5)  ;; 4
