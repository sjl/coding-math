(in-package #:coding-math)

(defclass particle ()
  ((pos :type 'vec :initarg :pos :accessor particle-pos)
   (vel :type 'vec :initarg :vel :accessor particle-vel)
   (grv :type 'vec :initarg :grv :accessor particle-grv)))


(defun make-particle (x y speed direction &optional (gravity 0))
  (make-instance 'particle
    :pos (make-vec x y)
    :vel (make-vec-md speed direction)
    :grv (make-vec-md gravity (/ tau 4))))


(defun particle-x (particle)
  (vec-x (particle-pos particle)))

(defun particle-y (particle)
  (vec-y (particle-pos particle)))


(defun particle-update! (particle)
  (vec-add! (particle-pos particle)
            (particle-vel particle))
  (vec-add! (particle-vel particle)
            (particle-grv particle)))


(defun particle-accelerate! (particle acceleration)
  (vec-add! (particle-vel particle)
            acceleration))

