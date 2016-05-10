(in-package #:coding-math.2d.hitboxes)

(defgeneric hitbox-x (object))

(defgeneric hitbox-y (object))

(defgeneric hitbox-radius (object))

(defgeneric hitbox-width (object))

(defgeneric hitbox-height (object))


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


(defmethod hitbox-x ((object vec))
  (vec-x object))

(defmethod hitbox-y ((object vec))
  (vec-y object))


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


(defgeneric drag-location-vec (object))
(defgeneric (setf drag-location-vec) (new-value object))
(defgeneric drag-requested-p (object mouse))
