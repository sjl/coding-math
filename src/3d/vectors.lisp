(in-package #:coding-math.3d.vectors)

;; Wrappers around sb-cga

(declaim (inline vec3-x vec3-y vec3-z
                 vec3-r vec3-a vec3-h
                 (setf vec3-x) (setf vec3-y) (setf vec3-z)
                 (setf vec3-r) (setf vec3-a) (setf vec3-h)))


(defun vec-x (v) (aref v 0))
(defun vec-y (v) (aref v 1))
(defun vec-z (v) (aref v 2))
(defun vec-r (v) (aref v 0))
(defun vec-a (v) (aref v 1))
(defun vec-h (v) (aref v 2))

(defun (setf vec-x) (n v) (setf (aref v 0) n))
(defun (setf vec-y) (n v) (setf (aref v 1) n))
(defun (setf vec-z) (n v) (setf (aref v 2) n))
(defun (setf vec-r) (n v) (setf (aref v 0) n))
(defun (setf vec-a) (n v) (setf (aref v 1) n))
(defun (setf vec-h) (n v) (setf (aref v 2) n))

(defun zero-vec ()
  (vec 0.0 0.0 0.0))

(defun random-vec (max-x max-y max-z)
  (vec (random max-x) (random max-y) (random max-z)))


(defun angle-between (v1 v2)
  (acos (/ (dot-product v1 v2)
           (* (vec-length v1)
              (vec-length v2)))))


; (defmacro with-vec (bindings vec &body body)
;   (once-only (vec)
;     `(symbol-macrolet ((,(first bindings) (aref ,vec 0))
;                        (,(second bindings) (aref ,vec 1))
;                        (,(third bindings) (aref ,vec 2)))
;       ,@body)))

; (defmacro with-vecs (bindings &body body)
;   (if (null bindings)
;     `(progn ,@body)
;     (destructuring-bind (vars vec-form . remaining) bindings
;       `(with-vec ,vars ,vec-form
;         (with-vec3s ,remaining ,@body)))))


;; thanks squirl
(defmacro with-vec (form &body body)
  "FORM is either a symbol bound to a `vec', or a list of the form:
  (name form)
where NAME is a symbol, and FORM evaluates to a `vec'.
WITH-VEC binds NAME.x and NAME.y in the same manner as `with-accessors'."
  (let* ((name (ensure-car form))
         (place (ensure-cadr form))
         (*package* (symbol-package name)))
    `(with-place (,(symbolicate name ".") vec-)
         (x y z r a h) ,place
       ,@body)))

(defmacro with-vecs ((form &rest forms) &body body)
  "Convenience macro for nesting WITH-VEC forms"
  `(with-vec ,form ,@(if forms `((with-vecs ,forms ,@body)) body)))
