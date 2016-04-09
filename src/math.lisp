(in-package #:coding-math)

(defconstant tau (* pi 2))


(defun normalize (n min max)
  (/ (- n min)
     (- max min)))


(defun lerp (from to n)
  (+ from
     (* n (- to from))))

