(in-package #:coding-math.2d.lines)

;;;; Conversions
;;; We have three ways to represent a line:
;;;
;;; * Two points (x1, y1) and (x2, y2)
;;; * Slope/intercept form (y = mx + b)
;;; * "Standard" form (Ax + By = C)

(defun xys-to-mxb (x1 y1 x2 y2)
  (if (= x1 x2)
    (values)
    (let ((slope (/ (- y2 y1)
                    (- x2 x1))))
      (values slope
              (- y1 (* slope x1))))))

(defun xys-to-std (x1 y1 x2 y2)
  (let* ((a (- y2 y1))
         (b (- x1 x2))
         (c (+ (* a x1) (* b y1))))
    (values a b c)))


(defun mxb-to-std (slope intercept)
  ;; y = mx + b
  ;; -mx + y = b
  ;; Ax + By = C
  (let* ((a (- slope))
         (b 1)
         (c intercept))
    (values a b c)))

(defun mxb-to-xys (slope intercept)
  ;; y = mx + b
  ;;
  ;; y = 0x + b
  ;; y = 1x + b
  (let ((x1 0)
        (y1 intercept)
        (x2 1)
        (y2 (+ intercept slope)))
    (values x1 y1 x2 y2)))


(defun std-to-mxb (a b c)
  ;; Ax + By = C
  ;; By = -Ax + C
  ;; y = -(A/B)x + (C/B)
  (if (zerop b)
    (values)
    (values (- (/ a b))
            (/ c b))))

(defun std-to-xys (a b c)
  ;; Ax + By = C
  ;;
  ;; A0 + By = C  ->  By = C      ->  y = C / B
  ;; A1 + By = C  ->  By = C - A  ->  y = (C - A) / B
  (let ((x1 0)
        (y1 (/ c b))
        (x2 1)
        (y2 (/ (- c a) b)))
    (values x1 y1 x2 y2)))


(defun std-intersection-point (a1 b1 c1 a2 b2 c2)
  (let ((denominator (- (* a1 b2) (* a2 b1))))
    (if (zerop denominator)
      (values)
      (values (/ (- (* b2 c1) (* b1 c2)) denominator) ; x
              (/ (- (* a1 c2) (* a2 c1)) denominator))))) ; y

(defun xys-intersection-point (x11 y11 x12 y12 x21 y21 x22 y22)
  (multiple-value-call #'std-intersection-point
                       (xys-to-std x11 y11 x12 y12)
                       (xys-to-std x21 y21 x22 y22)))

(defun mxb-intersection-point (slope1 intercept1 slope2 intercept2)
  (multiple-value-call #'std-intersection-point
                       (mxb-to-std slope1 intercept1)
                       (mxb-to-std slope2 intercept2)))

(defun xys-segments-intersection-point (x11 y11 x12 y12 x21 y21 x22 y22)
  (multiple-value-bind (x y)
      (xys-intersection-point x11 y11 x12 y12 x21 y21 x22 y22)
    (cond
      ((null x)
       (values)) ; parallel/colinear
      ((or (outsidep x11 x12 x)
           (outsidep x21 x22 x)
           (outsidep y11 y12 y)
           (outsidep y21 y22 y))
       (values)) ; intersection outside segment(s)
      (t (values x y)))))
