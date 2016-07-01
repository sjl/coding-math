(in-package #:coding-math.2d.demo)

;;;; Config
(defparameter *width* 600)
(defparameter *height* 400)

(defparameter *center-x* (/ *width* 2))
(defparameter *center-y* (/ *height* 2))

(defvar *shift* nil)
(defvar *control* nil)
(defvar *command* nil)
(defvar *option* nil)


;;;; Utils
(defmacro with-setup (&body body)
  `(with-fps
    (background (gray 1))
    ,@body))


;;;; Sketch
(defun draw-particle (p)
  (circle (particle-x p) (particle-y p) (particle-radius p)))

(defun draw-line (p1 p2)
  (with-vecs ((x1 y1) p1 (x2 y2) p2)
    (line x1 y1 x2 y2)))

(defun draw-circle (p &optional (radius 5))
  (if (listp p)
    (circle (getf p :x) (getf p :y) (or (getf p :radius) radius))
    (circle (vec-x p) (vec-y p) radius)))

(defun draw-square (p radius)
  (rect (- (vec-x p) radius)
        (- (vec-y p) radius)
        (* 2 radius)
        (* 2 radius)))

(defun draw-point (p)
  (point (vec-x p) (vec-y p)))


(defun oob-p (p &optional (r 0.0))
  (or (outsidep (- 0 r) (+ *width* r) (vec-x p))
      (outsidep (- 0 r) (+ *height* r) (vec-y p))))


(defmacro map-static (function-symbol &rest arguments)
  `(progn
     ,@(loop :for arg :in arguments :collect `(,function-symbol ,arg))))

(defun graph-tween (tweening-function)
  (graph-function (curry tweening-function 0.0 1.0 1.0)
                  :fn-start 0.0 :fn-end 1.0
                  :fn-min 0.0 :fn-max 1.0
                  :graph-start 0 :graph-end *width*
                  :graph-min *height* :graph-max 0))

(defsketch cm
    ((width *width*) (height *height*) (y-axis :down) (title "Coding Math 2D")
     (mouse (make-vec 0 0))
     ;; Data
     (current (make-vec 100 100))
     ;; Pens
     (particle-pen (make-pen :fill (gray 0.9) :stroke (gray 0.4)))
     (line-pen (make-pen :curve-steps 40 :stroke (gray 0.7)))
     (black-function-pen (make-pen :curve-steps 20 :stroke (rgb 0 0 0) :weight 1))
     (red-function-pen (make-pen :curve-steps 40 :stroke (rgb 0.8 0 0) :weight 1))
     (green-function-pen (make-pen :curve-steps 40 :stroke (rgb 0 0.8 0) :weight 1))
     (blue-function-pen (make-pen :curve-steps 40 :stroke (rgb 0 0 0.8) :weight 1))
     )
  (with-setup
    ;;
    (in-context
      (draw-axes *width* *height*)
      (with-pen black-function-pen
        (graph-tween #'tween-linear))
      (with-pen red-function-pen
        (map-static graph-tween
                    #'tween-quadratic-in
                    #'tween-cubic-in
                    #'tween-quartic-in
                    #'tween-quintic-in))
      (with-pen green-function-pen
        (map-static graph-tween
                    #'tween-quadratic-out
                    #'tween-cubic-out
                    #'tween-quartic-out
                    #'tween-quintic-out))
      (with-pen blue-function-pen
        (map-static graph-tween
                    #'tween-quadratic-inout
                    #'tween-cubic-inout
                    #'tween-quartic-inout
                    #'tween-quintic-inout))
      (update-tweens!)
      (with-pen particle-pen
        (draw-circle current)))
    ;;
    )
  )


;;;; Mouse
(defun mousemove (instance x y)
  (with-slots (mouse) instance
    (setf mouse (make-vec x (- *height* y)))
    ;;
    ;;
    )
  )

(defun draw-time ()
  (text (format nil "~d" (get-internal-real-time))
        300 300))

(defun mousedown-left (instance x y)
  (declare (ignorable instance x y))
  (with-slots (current) instance
    (tween-places!
        (#'tween-quadratic-inout 10.0
         :callback-progress #'draw-time)
      (vec-x current) x
      (vec-y current) y)))

(defun mousedown-right (instance x y)
  (declare (ignorable instance x y))
  )

(defun mouseup-left (instance x y)
  (declare (ignorable instance x y))
  )

(defun mouseup-right (instance x y)
  (declare (ignorable instance x y))
  )


(defmethod kit.sdl2:mousemotion-event ((window cm) ts b x y xrel yrel)
  (declare (ignore ts b xrel yrel))
  (mousemove window x y))

(defmethod kit.sdl2:mousebutton-event ((window cm) state ts button x y)
  (declare (ignore ts))
  (funcall (case state
             (:mousebuttondown
              (case button
                (1 #'mousedown-left)
                (3 #'mousedown-right)))
             (:mousebuttonup
              (case button
                (1 #'mouseup-left)
                (3 #'mouseup-right))))
           window x y))


;;;; Keyboard
(defun keydown (instance scancode)
  (declare (ignorable instance))
  (scancode-case scancode
    (:scancode-space (sketch::prepare instance))
    (:scancode-lshift (setf *shift* t))
    (:scancode-lctrl (setf *control* t))
    (:scancode-lgui (setf *command* t))
    (:scancode-lalt (setf *option* t))
    ;;
    ;;
    ))

(defun keyup (instance scancode)
  (declare (ignorable instance))
  (scancode-case scancode
    (:scancode-lshift (setf *shift* nil))
    (:scancode-lctrl (setf *control* nil))
    (:scancode-lgui (setf *command* nil))
    (:scancode-lalt (setf *option* nil))
    (:scancode-space nil)))


(defmethod kit.sdl2:keyboard-event ((instance cm) state timestamp repeatp keysym)
  (declare (ignore timestamp repeatp))
  (cond
    ((eql state :keyup) (keyup instance (sdl2:scancode-value keysym)))
    ((eql state :keydown) (keydown instance (sdl2:scancode-value keysym)))
    (t nil)))


;;;; Run
; (defparameter *demo* (make-instance 'cm))
