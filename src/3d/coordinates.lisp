(in-package #:coding-math.3d.coordinates)

;;; For this project, we'll define X and Y to be the typical 2D screen axes, and
;;; Z to be going into the screen (so it's a left-handed coordinate system).
;;;
;;; Cartesian coordinate vectors are:
;;;
;;;     (x y z)
;;;
;;; Cylindrical coordinate vectors are:
;;;
;;;     (radius azimuth height) where height is Y
;;;
;;; Polar coordinate vectors are:
;;;
;;;     TODO

(defun cylindrical-to-cartesian (coords)
  (with-vec (radius azimuth height) coords
    (vec (* radius (cos azimuth)) ; x
         height ; y
         (* radius (sin azimuth))))) ; z

(defun cartesian-to-cylindrical (coords)
  (with-vec (x y z) coords
    (vec (sqrt (+ (square x) (square z))) ; r
         (atan z x) ; a
         y))) ; h


(defun cylindrical-to-cartesian-cga (coords)
  (with-vec (radius azimuth height) coords
    (vec (* radius (cos azimuth)) ; x
         height ; y
         (* radius (sin azimuth))))) ; z
