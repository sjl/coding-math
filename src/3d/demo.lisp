(in-package #:coding-math.3d.demo)


;;;; Config
(defparameter *width* 600)
(defparameter *height* 400)

(defparameter *center-x* (/ *width* 2))
(defparameter *center-y* (/ *height* 2))

;;;; Utils
(defmacro with-centered-coords (&body body)
  `(in-context
     (translate *center-x* *center-y*)
     ,@body))

(defmacro with-setup (ready-form &body body)
  `(with-fps
    (background (gray 1))
    (when ,ready-form
      (with-centered-coords
        ,@body))))

;;;; Draw
(defun draw-shape (shape focal-length size)
  (destructuring-bind (x y z) shape
    (let ((perspective (/ focal-length (+ focal-length z))))
      (in-context
        (translate (* x perspective)
                   (* y perspective))
        (scale perspective)
        (circle 0 0 size)))))

;;;; Sketch
(defsketch demo (:width *width*
                 :height *height*
                 :debug :scancode-d)
    ((ready)
     (mouse)
     (fl 300)
     (shapes)
     (simple-pen (make-pen :fill (gray 0.2)))
     )
  (with-setup ready
    ;;
    (with-pen simple-pen
      (loop :for shape :in shapes :do
            (incf (caddr shape) -50)
            (wrapf (caddr shape) 0 10000))
      ; (setf shapes (sort shapes #'> :key #'caddr))
      (mapc (rcurry #'draw-shape fl 30)
            shapes)
      )
    ;;
    ))

(defun make-demo ()
  (make-sketch 'demo
    (mouse nil)))


(defun reset (game)
  (setf (slot-value game 'ready) nil)
  (setf
    (slot-value game 'shapes)
    (loop :repeat 200
          :collect (list (random-range -500 500)
                         (random-range -400 400)
                         (random-range 0 10000))))
  (setf (slot-value game 'ready) t))



;;;; Mouse
(defmethod kit.sdl2:mousemotion-event ((window demo) ts b x y xrel yrel)
  (declare (ignore ts b xrel yrel))
  (with-slots (mouse) window
    (setf (slot-value window 'mouse) ; todo fix
          (list x y))
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


(defmethod kit.sdl2:keyboard-event ((instance demo) state timestamp repeatp keysym)
  (declare (ignore timestamp repeatp))
  (cond
    ((eql state :keyup) (keyup instance (sdl2:scancode-value keysym)))
    ((eql state :keydown) (keydown instance (sdl2:scancode-value keysym)))
    (t nil)))


;;;; Run
; (defparameter *demo* (make-demo))
