(in-package #:coding-math.math)

(declaim (inline outsidep insidep wrap-zero wrap-range))
(declaim (inline norm lerp clamp distance))

;;;; Constants
(defparameter tau (* pi 2))


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


(defun insidep (from to val)
  (< (min from to) val (max from to)))

(defun outsidep (from to val)
  (not (insidep from to val)))

(defun ranges-overlap-p (from0 to0 from1 to1)
  (not (or (< (max from0 to0)
              (min from1 to1))
           (> (min from0 to0)
              (max from1 to1)))))


;;;; Collisions
(defgeneric hitbox-x (object))

(defgeneric hitbox-y (object))

(defgeneric hitbox-radius (object))


(defmethod hitbox-x ((object list))
  (getf object :x))

(defmethod hitbox-y ((object list))
  (getf object :y))

(defmethod hitbox-radius ((object list))
  (getf object :radius))

(defmethod hitbox-width ((object list))
  (getf object :width))

(defmethod hitbox-height ((object list))
  (getf object :height))


(defun circles-collide-p (c0 c1)
  (let ((d (distance (hitbox-x c0) (hitbox-y c0)
                     (hitbox-x c1) (hitbox-y c1))))
    (< d (+ (hitbox-radius c0)
            (hitbox-radius c1)))))

(defun circle-point-collide-p (c p)
  (let ((d (distance (hitbox-x c) (hitbox-y c)
                     (hitbox-x p) (hitbox-y p))))
    (< d (hitbox-radius c))))

(defun rect-point-collide-p (r p)
  (with-vals ((rx hitbox-x)
              (ry hitbox-y)
              (rw hitbox-width)
              (rh hitbox-height))
      r
    (and (insidep rx (+ rx rw) (hitbox-x p))
         (insidep ry (+ ry rh) (hitbox-y p)))))

(defun rects-collide-p (r0 r1)
  (with-vals ((r0x hitbox-x) ; lol
              (r0y hitbox-y)
              (r0w hitbox-width)
              (r0h hitbox-height)) r0
    (with-vals ((r1x hitbox-x)
                (r1y hitbox-y)
                (r1w hitbox-width)
                (r1h hitbox-height)) r1
      (and (ranges-overlap-p r0x (+ r0x r0w)
                             r1x (+ r1x r1w))
           (ranges-overlap-p r0y (+ r0y r0h)
                             r1y (+ r1y r1h))))))
