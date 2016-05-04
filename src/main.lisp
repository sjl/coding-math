(in-package #:coding-math)

;;;; Config
(defparameter *width* 600)
(defparameter *height* 400)

(defparameter *center-x* (/ *width* 2))
(defparameter *center-y* (/ *height* 2))


;;;; Sketch
(defun draw-particle (p pen)
  (with-pen pen
    (circle (particle-x p) (particle-y p) (particle-radius p))))


(defun add-result (results)
  (incf (aref results (floor (random-dist 0 100 4)))))

(defsketch cm (:width *width*
               :height *height*
               :debug :scancode-d)
    ((ready)
     (mouse)
     (graph-pen (make-pen :fill (gray 0.8)))
     (dot-pen (make-pen :fill (gray 0.8)))
     (results)
     (dots)
     )
  (with-fps
    (background (gray 1))
    ;;
    (when ready

      (with-pen dot-pen
        (loop :for (x . y) :in dots
              :do (circle x y 2)))
      (add-result results)
      (with-pen graph-pen
        (loop :for r :across results
              :for i :from 0
              :do (rect (map-range 0 100
                                   0 *width*
                                   i)
                        0
                        (- (/ *width* 100) 1)
                        (map-range 0 200
                                   0 *height*
                                   r))))

      )

    ;;
    ))

(defun make-cm ()
  (make-sketch 'cm
    (mouse (make-vec))))


(defun reset (game)
  (setf (slot-value game 'ready) nil)
  (setf (slot-value game 'results)
        (make-array 100 :initial-element 0)
        (slot-value game 'dots)
        (loop :repeat 500
              :collect (cons (random-dist 0 *width* 4)
                             (random-dist 0 *height* 4))))
  (setf (slot-value game 'ready) t))


;;;; Mouse
(defmethod kit.sdl2:mousemotion-event ((window cm) ts b x y xrel yrel)
  (declare (ignore ts b xrel yrel))
  (with-slots (mouse) window
    (setf (vec-x mouse) x)
    (setf (vec-y mouse) y)
    ;;
    ;;
    ))


;;;; Keyboard
(defun keydown (instance scancode)
  (declare (ignorable instance))
  (scancode-case scancode
    (:scancode-space (reset instance))))

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
