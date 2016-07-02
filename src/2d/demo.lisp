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


(defsketch cm
    ((width *width*) (height *height*) (y-axis :down) (title "Coding Math 2D")
     (mouse (make-vec 0 0))
     ;; Data
     (m1 (random-range -5 5))
     (m2 (random-range -5 5))
     (b1 (random-range -5 5))
     (b2 (random-range -5 5))
     (x11 (random-range -5 5))
     (y11 (random-range -5 5))
     (x12 (random-range -5 5))
     (y12 (random-range -5 5))
     (x21 (random-range -5 5))
     (y21 (random-range -5 5))
     (x22 (random-range -5 5))
     (y22 (random-range -5 5))
     ;; Pens
     (particle-pen (make-pen :fill (gray 0.9) :stroke (gray 0.4)))
     (black-pen (make-pen :stroke (rgb 0 0 0) :fill (rgb 0.4 0.4 0.4) :weight 1 :curve-steps 50))
     (red-pen (make-pen :stroke (rgb 0.6 0 0) :fill (rgb 0.9 0 0) :weight 1 :curve-steps 50))
     (green-pen (make-pen :stroke (rgb 0 0.6 0) :fill (rgb 0 0.9 0) :weight 1 :curve-steps 50))
     (blue-pen (make-pen :stroke (rgb 0 0 0.6) :fill (rgb 0 0 0.9) :weight 1 :curve-steps 50))
     )
  (with-setup
    ;;
    (in-context
      (translate *center-x* *center-y*)
      (draw-axes *width* *height*)
      (flet ((map-to-screen (x y)
               (values (map-range -10.0 10.0 (- *center-x*) *center-x* x)
                       (map-range -10.0 10.0 (- *center-y*) *center-y* y)))
             (valid-line-p (x1 y1 x2 y2)
               (not (and (= x1 x2)
                         (= y1 y2))))
             (g (fn)
               (graph-function
                 fn
                 :fn-start -10 :fn-end 10
                 :fn-min -10 :fn-max 10
                 :graph-start (- *center-x*) :graph-end *center-x*
                 :graph-min (- *center-y*) :graph-max *center-y*)))
        (with-pen green-pen
          (g (lambda (x) (+ (* m1 x) b1)))
          (g (lambda (x) (+ (* m2 x) b2)))
          (multiple-value-bind (x y)
              (mxb-intersection-point m1 b1 m2 b2)
            (when x
              (draw-circle (multiple-value-call #'make-vec
                             (map-to-screen x y))
                           6))))
        (when (and (valid-line-p x11 y11 x12 y12)
                   (valid-line-p x21 y21 x22 y22))
          (with-pen red-pen
            (multiple-value-call #'line
              (map-to-screen x11 y11)
              (map-to-screen x12 y12))
            (multiple-value-call #'line
              (map-to-screen x21 y21)
              (map-to-screen x22 y22))
            (draw-circle (multiple-value-call #'make-vec (map-to-screen x11 y11)) 3)
            (draw-circle (multiple-value-call #'make-vec (map-to-screen x12 y12)) 3)
            (draw-circle (multiple-value-call #'make-vec (map-to-screen x21 y21)) 3)
            (draw-circle (multiple-value-call #'make-vec (map-to-screen x22 y22)) 3)
            (multiple-value-bind (x y)
                (xys-intersection-point x11 y11 x12 y12 x21 y21 x22 y22)
              (when x
                (draw-circle (multiple-value-call #'make-vec
                               (map-to-screen x y))
                             6)))
            ))
        )
      )
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
  (declare (ignorable instance x y)))

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
