(in-package #:coding-math.vectors)


(declaim (inline vec-x vec-y make-vec))

(defstruct (vec
             (:constructor make-vec
              (&optional (x 0) (y 0)))
             (:type vector))
  (x 0 :type real)
  (y 0 :type real))


(defun make-vec-md (magnitude angle)
  (let ((v (make-vec 0 0)))
    (setf (vec-magnitude v) magnitude
          (vec-angle v) angle)
    v))

(defun make-vec-ma (magnitude angle)
  (make-vec-md magnitude angle))


(defmacro with-vec (bindings vec &body body)
  (once-only (vec)
    `(let ((,(first bindings) (vec-x ,vec))
           (,(second bindings) (vec-y ,vec)))
       ,@body)))


(defun vec-magnitude (vec)
  (with-vec (x y) vec
    (sqrt (+ (* x x)
             (* y y)))))

(defun vec-angle (vec)
  (with-vec (x y) vec
    (atan y x)))

(defun vec-direction (vec)
  (vec-angle vec))


(defun (setf vec-angle) (angle vec)
  (let ((magnitude (vec-magnitude vec)))
    (setf (vec-x vec) (* magnitude (cos angle)))
    (setf (vec-y vec) (* magnitude (sin angle))))
  angle)

(defun (setf vec-direction) (angle vec)
  (setf (vec-angle vec) angle))

(defun (setf vec-magnitude) (magnitude vec)
  (let ((angle (vec-angle vec)))
    (setf (vec-x vec) (* magnitude (cos angle)))
    (setf (vec-y vec) (* magnitude (sin angle))))
  magnitude)


(defun vec-add (v1 v2)
  (make-vec (+ (vec-x v1) (vec-x v2))
            (+ (vec-y v1) (vec-y v2))))

(defun vec-sub (v1 v2)
  (make-vec (- (vec-x v1) (vec-x v2))
            (- (vec-y v1) (vec-y v2))))

(defun vec-mul (v s)
  (make-vec (* (vec-x v) s)
            (* (vec-y v) s)))

(defun vec-div (v s)
  (make-vec (/ (vec-x v) s)
            (/ (vec-y v) s)))


(defun vec-add! (v1 v2)
  (incf (vec-x v1) (vec-x v2))
  (incf (vec-y v1) (vec-y v2)))

(defun vec-sub! (v1 v2)
  (decf (vec-x v1) (vec-x v2))
  (decf (vec-y v1) (vec-y v2)))

(defun vec-mul! (v s)
  (setf (vec-x v) (* (vec-x v) s)
        (vec-y v) (* (vec-y v) s)))

(defun vec-div! (v s)
  (setf (vec-x v) (/ (vec-x v) s)
        (vec-y v) (/ (vec-y v) s)))


(defun vec-to-string (v)
  (format nil "[~A ~A]" (vec-x v) (vec-y v)))


(defun vec-distance-between (v0 v1)
  (distance (vec-x v0) (vec-y v0)
            (vec-x v1) (vec-y v1)))
