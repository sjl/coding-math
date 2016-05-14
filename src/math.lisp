(in-package #:coding-math.math)

(declaim (inline square outsidep insidep wrap-zero wrap-range
                 norm lerp clamp distance))


;;;; Constants
(defparameter tau (coerce (* pi 2) 'single-float))


;; Basics
(defun square (x)
  (* x x))

(defun dividesp (n divisor)
  "Return whether `n` is evenly divisible by `divisor`."
  (zerop (mod n divisor)))

(defun square (n)
  "Return the square of `n`."
  (* n n))

(defmacro mulf (place n)
  "Multiply `place` by `n` in-place."
  `(zap% ,place #'* % ,n))


;; Geometry
(defun distance (x0 y0 x1 y1)
  (sqrt (+ (square (- x0 x1))
           (square (- y0 y1)))))


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


;;;; Number range mapping
(defun norm (min max val)
  (/ (- val min)
     (- max min)))

(defun lerp (from to n)
  "Lerp together `from` and `to` by factor `n`.

  Note that you might want `precise-lerp` instead.

  "
  (+ from
     (* n (- to from))))

(defun precise-lerp (from to n)
  "Lerp together `from` and `to` by factor `n`, precisely.

  Vanilla lerp does not guarantee `(lerp from to 0.0)` will return exactly
  `from` due to floating-point errors.  This version will return exactly `from`
  when given a `n` of `0.0`, at the cost of an extra multiplication.

  "
  (+ (* (- 1 n) from)
     (* n to)))

(defun map-range (source-from source-to dest-from dest-to source-val)
  "Map `source-val` from the source range to the destination range."
  (lerp dest-from dest-to
        (norm source-from source-to source-val)))

(defun clamp (from to n)
  (let ((max (max from to))
        (min (min from to)))
    (cond
      ((> n max) max)
      ((< n min) min)
      (t n))))


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
  `(zap% ,place #'wrap-range ,min ,max %))


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

