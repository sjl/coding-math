(in-package #:coding-math)

(declaim (optimize (speed 3)
                   (safety 1)
                   (debug 0)))

;;;; Config
(defparameter *width* 600)
(defparameter *height* 400)

(defparameter *center-x* (/ *width* 2))
(defparameter *center-y* (/ *height* 2))


;;;; Sketch
(defun draw-grid ()
  (with-pen (make-pen :stroke (gray 0.8))
    (loop :for x :from 0 :below *width* :by 40
          :do (line x 0 x *height*))
    (loop :for y :from 0 :below *height* :by 40
          :do (line 0 y *width* y))))

(defsketch cm (:width *width*
               :height *height*
               :debug :scancode-d)
    ((mouse)
     (k)
     (separation)
     (particles)
     (connections)
     )
  (with-fps
    (background (gray 1))
    ;;

    (draw-grid)
    (with-pen (make-pen :stroke (gray 0.5) :fill (gray 0.9))
      (circle (round-to-nearest (getf mouse :x) 40)
              (round-to-nearest (getf mouse :y) 40)
              10))

    ;;
    ))

(defun make-cm ()
  (make-sketch 'cm
    (mouse (list :x 0 :y 0))
    (k 0.01)
    (separation 100)
    ))


;;;; Mouse
(defmethod kit.sdl2:mousemotion-event ((window cm) ts b x y xrel yrel)
  (declare (ignore ts b xrel yrel))
  (with-slots (mouse) window
    (setf (getf mouse :x) x)
    (setf (getf mouse :y) y)
    ;;
    ;;
    ))


;;;; Keyboard
(defun keydown (instance scancode)
  (declare (ignorable instance))
  (scancode-case scancode
    (:scancode-space
     nil)))

(defun keyup (instance scancode)
  (declare (ignorable instance))
  (scancode-case scancode
    (:scancode-space
     nil)))


(defmethod kit.sdl2:keyboard-event ((instance cm) state timestamp repeatp keysym)
  (declare (ignore timestamp repeatp))
  (cond
    ((eql state :keyup) (keyup instance (sdl2:scancode-value keysym)))
    ((eql state :keydown) (keydown instance (sdl2:scancode-value keysym)))
    (t nil)))


;;;; Run
; (defparameter *demo* (make-cm))
