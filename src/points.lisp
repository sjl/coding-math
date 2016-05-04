(in-package #:coding-math.points)

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
          (+ (* (square (- 1 n)) fx)
             (* 2 (- 1 n) n cx)
             (* n n tx))
          (vec-y destination)
          (+ (* (square (- 1 n)) fy)
             (* 2 (- 1 n) n cy)
             (* n n ty))))
  (values))


(defun cubic-bezier (from to control-1 control-2 n)
  (vec-lerp (vec-lerp (vec-lerp from control-1 n)
                      (vec-lerp control-1 control-2 n)
                      n)
            (vec-lerp (vec-lerp control-1 control-2 n)
                      (vec-lerp control-2 to n)
                      n)
            n))
