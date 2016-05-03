(in-package #:coding-math.particles)

(defstruct (particle
             (:constructor make-particle%))
  (pos (make-vec) :type vec)
  (vel (make-vec) :type vec)
  (grv (make-vec) :type vec)
  (radius 1 :type fixnum)
  (friction 0.0 :type single-float)
  (mass 1.0 :type single-float)
  (springs nil :type list)
  (gravitations nil :type list))

(defstruct spring
  (target (make-vec) :type vec)
  (constant 0.0 :type single-float)
  (offset 0.0 :type single-float))


(defun make-particle
    (x y
     &key
     (speed 0)
     (direction 0)
     (mass 1.0)
     (radius 1)
     (gravity 0.0)
     (friction 0.0))
  (make-particle%
    :pos (make-vec x y)
    :vel (make-vec-md speed direction)
    :grv (make-vec-md gravity (/ tau 4))
    :friction friction
    :mass mass
    :radius radius))


(defun particle-x (particle)
  (vec-x (particle-pos particle)))

(defun particle-y (particle)
  (vec-y (particle-pos particle)))

(defun particle-speed (particle)
  (vec-magnitude (particle-vel particle)))

(defun particle-direction (particle)
  (vec-direction (particle-vel particle)))

(defun particle-wrap! (particle width height)
  (let ((radius (particle-radius particle)))
    (wrapf (particle-x particle)
           (- radius)
           (+ radius width))
    (wrapf (particle-y particle)
           (- radius)
           (+ radius height))))


(defun (setf particle-x) (new-value particle)
  (setf (vec-x (particle-pos particle)) new-value))

(defun (setf particle-y) (new-value particle)
  (setf (vec-y (particle-pos particle)) new-value))

(defun (setf particle-speed) (new-value particle)
  (setf (vec-magnitude (particle-vel particle)) new-value))

(defun (setf particle-direction) (new-value particle)
  (setf (vec-direction (particle-vel particle)) new-value))


(defun particle-angle-to (particle other-particle)
  (let ((distance (vec-sub (particle-pos other-particle)
                           (particle-pos particle))))
    (atan (vec-y distance)
          (vec-x distance))))

(defun particle-distance-to (particle other-particle)
  (vec-magnitude (vec-sub (particle-pos particle)
                          (particle-pos other-particle))))


(defun particle-accelerate! (particle acceleration)
  (vec-add! (particle-vel particle)
            acceleration))


(defun particle-gravitate-add! (particle target)
  (push target (particle-gravitations particle)))

(defun particle-gravitate-remove! (particle target)
  (zap% (particle-gravitations particle)
        #'remove target %))

(defun particle-gravitate-to! (particle attractor-particle)
  (let ((distance (particle-distance-to particle attractor-particle)))
    (particle-accelerate!
      particle
      (make-vec-md (/ (particle-mass attractor-particle)
                      (* distance distance))
                   (particle-angle-to particle attractor-particle)))))


(defun particle-spring-to! (particle target spring-constant &optional (offset 0))
  (let ((distance (vec-sub target (particle-pos particle))))
    (decf (vec-magnitude distance) offset)
    (vec-add! (particle-vel particle)
              (vec-mul distance spring-constant))))

(defun particle-spring-add! (particle target spring-constant &optional (offset 0))
  (push (make-spring :target target
                     :constant (float spring-constant)
                     :offset (float offset))
        (particle-springs particle)))

(defun particle-spring-remove! (particle target)
  (zap% (particle-springs particle)
        #'remove target % :key #'spring-target))


(defun particle-update! (particle)
  (with-accessors
      ((pos particle-pos)
       (vel particle-vel)
       (grv particle-grv)
       (friction particle-friction))
      particle
    (vec-add! pos vel)
    (vec-add! vel grv)
    (vec-mul! vel (- 1 friction))
    (loop :for g :in (particle-gravitations particle)
          :do (particle-gravitate-to! particle g))
    (loop :for s :in (particle-springs particle)
          :do (particle-spring-to! particle
                                   (spring-target s)
                                   (spring-constant s)
                                   (spring-offset s)))))


(defmethod hitbox-x ((p particle))
  (particle-x p))

(defmethod hitbox-y ((p particle))
  (particle-y p))

(defmethod hitbox-radius ((p particle))
  (particle-radius p))
