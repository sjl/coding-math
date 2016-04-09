(in-package #:coding-math)

;;;; Constants
(defconstant tau (* pi 2))


;;;; Maths
(defun normalize (n min max)
  (/ (- n min)
     (- max min)))

