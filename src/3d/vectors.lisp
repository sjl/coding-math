(in-package #:coding-math.3d.vectors)

;; Wrappers around sb-cga

(declaim (inline vec3-x vec3-y vec3-z
                 vec3-r vec3-a vec3-h))

(defun vec-x (v) (aref v 0))
(defun vec-y (v) (aref v 1))
(defun vec-z (v) (aref v 2))
(defun vec-r (v) (aref v 0))
(defun vec-a (v) (aref v 1))
(defun vec-h (v) (aref v 2))

(defun random-vec (max-x max-y max-z)
  (vec (random max-x) (random max-y) (random max-z)))


(defun angle-between (v1 v2)
  (acos (/ (dot-product v1 v2)
           (* (vec-length v1)
              (vec-length v2)))))


(defmacro with-vec (bindings vec &body body)
  (once-only (vec)
    `(symbol-macrolet ((,(first bindings) (aref ,vec 0))
                       (,(second bindings) (aref ,vec 1))
                       (,(third bindings) (aref ,vec 2)))
      ,@body)))

(defmacro with-vecs (bindings &body body)
  (if (null bindings)
    `(progn ,@body)
    (destructuring-bind (vars vec-form . remaining) bindings
      `(with-vec ,vars ,vec-form
        (with-vec3s ,remaining ,@body)))))
