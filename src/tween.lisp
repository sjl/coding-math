(in-package #:coding-math.tween)

(defun tween-linear (start amount duration time)
  (let ((time (/ time duration)))
    (+ start (* amount time))))

(defun tween-quadratic-in (start amount duration time)
  (let ((time (/ time duration)))
    (+ start (* amount (* time time)))))

(defun tween-quadratic-out (start amount duration time)
  (let ((time (/ time duration)))
    (+ start (* (- amount) (* time (- time 2))))))
