(in-package #:coding-math.ballistics)

;;;; Config
(defparameter *width* 600)
(defparameter *height* 400)

(defparameter *center-x* (/ *width* 2))
(defparameter *center-y* (/ *height* 2))

(defun draw-gun (gun)
  (in-context
    (translate (a gun 'x) (a gun 'y))
    (with-pen (make-pen :stroke (gray 0.0) :fill (gray 0.0))
      (circle 0 0 15)
      (rotate (degrees (a gun 'angle)))
      (rect 0 -4 25 8)
      )))

(defun aim (gun x y)
  (setf (cdr (assoc 'angle gun))
        (clamp (- (/ tau 4))
               -0.3
               (atan (- y (a gun 'y))
                     (- x (a gun 'x))))))

(defsketch game (:width *width*
                 :height *height*
                 :debug :scancode-d)
    ((frame 1)
     (aiming nil)
     (gun `((x . 40)
            (y . ,*height*)
            (angle . ,(- (/ tau 8))))))
  (background (gray 1))
  (incf frame)
  ;;
  (draw-gun gun)

  ;;
  (when (zerop (mod frame 20))
    (calc-fps 20))
  (draw-fps))


(defmethod kit.sdl2:mousebutton-event
    ((game game) state timestamp button x y)
  (declare (ignore timestamp x y))
  (when (= 1 button)
    (case state
      (:mousebuttondown (setf (slot-value game 'aiming) t))
      (:mousebuttonup (setf (slot-value game 'aiming) nil)))))

(defmethod kit.sdl2:mousemotion-event
    ((game game) timestamp button-mask x y xrel yrel)
  (declare (ignore timestamp button-mask xrel yrel))
  (when (slot-value game 'aiming)
    (aim (slot-value game 'gun) x y)))

;;;; Run
; (defparameter *demo* (make-instance 'game))
