(in-package #:coding-math.math)

;; Constants
(defconstant tau (* pi 2))


;; Random
(defun random-range (min max)
  (+ min (random (- max min))))

(defun random-around (val range)
  (random-range (- val range)
                (+ val range)))


;; Number range mapping
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

(defun clamp (min max n)
  (cond
    ((> n max) max)
    ((< n min) min)
    (t n)))


;; Wrapping
(defun wrap-zero (max val)
  "Wrap `val` around the range [0, max)."
  (mod val max))

(defun wrap-range (min max val)
  "Wrap `val` around the range [min, max)."
  (+ min
     (mod (- val min)
          (- max min))))

(defun outside-p (min max val)
  (or (< val min)
      (> val max)))
