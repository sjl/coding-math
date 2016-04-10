(in-package #:coding-math)

(defclass particle ()
  ((pos :type 'vec :initarg :pos :accessor particle-pos)
   (vel :type 'vec :initarg :vel :accessor particle-vel)
   (mass :type 'real :initarg :mass :initform 1.0 :accessor particle-mass)))


(defun make-particle (x y &key (speed 0) (direction 0) (mass 1.0))
  (make-instance 'particle
    :pos (make-vec x y)
    :vel (make-vec-md speed direction)
    :mass mass))


(defun particle-x (particle)
  (vec-x (particle-pos particle)))

(defun particle-y (particle)
  (vec-y (particle-pos particle)))


(defun (setf particle-x) (new-value particle)
  (setf (vec-x (particle-pos particle)) new-value))

(defun (setf particle-y) (new-value particle)
  (setf (vec-y (particle-pos particle)) new-value))


(defun particle-update! (particle)
  (vec-add! (particle-pos particle)
            (particle-vel particle)))


(defun particle-accelerate! (particle acceleration)
  (vec-add! (particle-vel particle)
            acceleration))


(defun particle-angle-to (particle other-particle)
  (let ((distance (vec-sub (particle-pos other-particle)
                           (particle-pos particle))))
    (atan (vec-y distance)
          (vec-x distance))))

(defun particle-distance-to (particle other-particle)
  (vec-magnitude (vec-sub (particle-pos particle)
                          (particle-pos other-particle))))


(defun particle-gravitate-to! (particle attractor-particle)
  (let ((gravity (make-vec))
        (distance (particle-distance-to particle attractor-particle)))
    (setf (vec-magnitude gravity)
          (/ (particle-mass attractor-particle)
             (* distance distance))
          (vec-angle gravity)
          (particle-angle-to particle attractor-particle))
    (particle-accelerate! particle gravity)))
