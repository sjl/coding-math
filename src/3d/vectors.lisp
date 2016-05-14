(in-package #:coding-math.3d.vectors)


(declaim (inline vec3-x vec3-y vec3-z make-vec3
                 vec3-magnitude
                 vec3-add vec3-sub vec3-mul vec3-div
                 vec3-add! vec3-sub! vec3-mul! vec3-div!
                 vec3-lerp))

(defstruct (vec3
             (:constructor make-vec3
              (&optional (x 0) (y 0) (z 0)))
             (:type vector))
  (x 0 :type real)
  (y 0 :type real)
  (z 0 :type real))

(defun vec3-radius (v)
  (vec3-x v))

(defun vec3-angle (v)
  (vec3-y v))

(defun vec3-height (v)
  (vec3-z v))


(defun make-random-vec3 (max-x max-y max-z)
  (make-vec3 (random max-x) (random max-y) (random max-z)))


(defmacro with-vec3 (bindings vec &body body)
  (once-only (vec)
    `(let ((,(first bindings) (vec3-x ,vec))
           (,(second bindings) (vec3-y ,vec))
           (,(third bindings) (vec3-z ,vec)))
      ,@body)))

(defmacro with-vec3s (bindings &body body)
  (if (null bindings)
    `(progn ,@body)
    (destructuring-bind (vars vec-form . remaining) bindings
      `(with-vec3 ,vars ,vec-form (with-vec3s ,remaining ,@body)))))

(defmacro with-vec3-slots (bindings vec &body body)
  `(with-accessors ((,(first bindings) vec3-x)
                    (,(second bindings) vec3-y)
                    (,(third bindings) vec3-z))
    ,vec
    ,@body))


(defun vec3-magnitude (vec)
  (with-vec3 (x y z) vec
    (sqrt (+ (square x)
             (square y)
             (square z)))))


(defun vec3-add (v1 v2)
  (make-vec3 (+ (vec3-x v1) (vec3-x v2))
             (+ (vec3-y v1) (vec3-y v2))
             (+ (vec3-z v1) (vec3-z v2))))

(defun vec3-sub (v1 v2)
  (make-vec3 (- (vec3-x v1) (vec3-x v2))
             (- (vec3-y v1) (vec3-y v2))
             (- (vec3-z v1) (vec3-z v2))))

(defun vec3-mul (v s)
  (make-vec3 (* (vec3-x v) s)
             (* (vec3-y v) s)
             (* (vec3-z v) s)))

(defun vec3-div (v s)
  (make-vec3 (/ (vec3-x v) s)
             (/ (vec3-y v) s)
             (/ (vec3-z v) s)))


(defun vec3-add! (v1 v2)
  (incf (vec3-x v1) (vec3-x v2))
  (incf (vec3-y v1) (vec3-y v2))
  (incf (vec3-z v1) (vec3-z v2))
  v1)

(defun vec3-sub! (v1 v2)
  (decf (vec3-x v1) (vec3-x v2))
  (decf (vec3-y v1) (vec3-y v2))
  (decf (vec3-z v1) (vec3-z v2))
  v1)

(defun vec3-mul! (v s)
  (setf (vec3-x v) (* (vec3-x v) s)
        (vec3-y v) (* (vec3-y v) s)
        (vec3-z v) (* (vec3-z v) s))
  v)

(defun vec3-div! (v s)
  (setf (vec3-x v) (/ (vec3-x v) s)
        (vec3-y v) (/ (vec3-y v) s)
        (vec3-z v) (/ (vec3-z v) s))
  v)


(defun vec3-lerp (v1 v2 n)
  (with-vec3s ((x1 y1 z1) v1
               (x2 y2 z2) v2)
    (make-vec3 (lerp x1 x2 n)
               (lerp y1 y2 n)
               (lerp z1 z2 n))))


(defun vec3-normalized (vec)
  (vec3-div vec (vec3-magnitude vec)))


(defun vec3-dot (v1 v2)
  (+ (* (vec3-x v1) (vec3-x v2))
     (* (vec3-y v1) (vec3-y v2))
     (* (vec3-z v1) (vec3-z v2))))


(defun vec3-angle-between (v1 v2)
  (acos (/ (vec3-dot v1 v2)
           (* (vec3-magnitude v1)
              (vec3-magnitude v2)))))


(defun vec3-cross (v1 v2)
  (with-vec3s ((ax ay az) v1
               (bx by bz) v2)
    (make-vec3 (- (* ay bz) (* az by))
               (- (* az bx) (* ax bz))
               (- (* ax by) (* ay bx)))))


(defmacro with-vec (bindings vec &body)
  (once-only (vec)
    `(symbol-macrolet ((,(first bindings) (aref ,vec 0))
                       (,(second bindings) (aref ,vec 1))
                       (,(third bindings) (aref ,vec 2)))
      ,@body)))
