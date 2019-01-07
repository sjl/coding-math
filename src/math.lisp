(in-package #:coding-math.math)

(declaim (inline outsidep insidep wrap-zero wrap-range
                 norm lerp clamp distance))



;; Geometry
(defun distance (x0 y0 x1 y1)
  (sqrt (+ (losh:square (- x0 x1))
           (losh:square (- y0 y1)))))


;;;; Random
(defun random-range (min max)
  (+ min (random (- max min))))

(defun random-around (val range)
  (random-range (- val range)
                (+ val range)))

(defun random-dist (min max iterations)
  (loop :repeat iterations
        :summing (random-range min max) :into total
        :finally (return (/ total iterations))))


;;;; Wrapping
(defun wrap-zero (max val)
  "Wrap `val` around the range [0, max)."
  (mod val max))

(defun wrap-range (min max val)
  "Wrap `val` around the range [min, max)."
  (+ min
     (mod (- val min)
          (- max min))))

(defmacro wrapf (place min max)
  `(losh:zapf ,place (wrap-range ,min ,max losh:%)))


(defun insidep (from to val)
  (< (min from to) val (max from to)))

(defun outsidep (from to val)
  (not (insidep from to val)))

(defun ranges-overlap-p (from0 to0 from1 to1)
  (not (or (< (max from0 to0)
              (min from1 to1))
           (> (min from0 to0)
              (max from1 to1)))))


;;;; Rounding
(defun round-to-places (f places)
  ;; This is a bit janky because it's working with floats, but it's in the
  ;; videos so I'll write it, what the heck.
  (let ((d (expt 10 (- places))))
    (* d (fround f d))))

(defun round-to-nearest (n divisor)
  (* divisor (round n divisor)))

