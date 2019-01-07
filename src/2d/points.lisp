(in-package #:coding-math.2d.points)

(defun quadratic-bezier (from to control n)
  (vec-lerp (vec-lerp from control n)
            (vec-lerp control to n)
            n))

(defun fast-quadratic-bezier (from to control n
                              &optional (destination (make-vec)))
  (with-vecs ((fx fy) from
              (tx ty) to
              (cx cy) control)
    (setf (vec-x destination)
          (+ (* (losh:square (- 1 n)) fx)
             (* 2 (- 1 n) n cx)
             (* n n tx))
          (vec-y destination)
          (+ (* (losh:square (- 1 n)) fy)
             (* 2 (- 1 n) n cy)
             (* n n ty))))
  destination)


(defun cubic-bezier (from to control-1 control-2 n)
  (vec-lerp (vec-lerp (vec-lerp from control-1 n)
                      (vec-lerp control-1 control-2 n)
                      n)
            (vec-lerp (vec-lerp control-1 control-2 n)
                      (vec-lerp control-2 to n)
                      n)
            n))


(declaim (inline draw-function))
(defun draw-function (fn &key (start 0.0) (end 1.0))
  (let ((steps (sketch::pen-curve-steps (sketch::env-pen sketch::*env*))))
    (apply #'polyline
           (mapcan (compose (rcurry #'coerce 'list) fn)
                   (iota (1+ steps)
                         :start 0.0
                         :step (/ (- end start) steps))))))

(defun quadratic-bezier-curve (from to control)
  (draw-function (curry #'fast-quadratic-bezier from to control)))


(defun multicurve (from controls to)
  (labels ((midpoint (pair)
             (vec-lerp (car pair) (cadr pair) 0.5))
           (midpoints (points)
             (mapcar #'midpoint (n-grams 2 points))))
    (let ((mids (midpoints controls)))
      (loop :for start :in (cons from mids)
            :for end :in (append mids (list to))
            :for control :in controls
            :do (quadratic-bezier-curve start end control)))))
