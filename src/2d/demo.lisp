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

(defparameter *wheel-rim-pen* (make-pen :weight 10 :stroke (rgb 0.5 0 0)))
(defparameter *wheel-point-pen* (make-pen :fill (rgb 0.8 0 0)))

(defun draw-wheel (angle)
  (in-context
    (translate *center-x* *center-y*)
    (rotate angle)
    (with-pen *wheel-rim-pen*
      (circle 0 0 100))
    (with-pen *wheel-point-pen*
      (ngon 3 0 80 30 30 (degrees (/ tau 4)))
      (rect -5 0 10 80)
      (rotate (degrees (/ tau 8)))
      (rect -5 -90 10 80)
      (rotate (degrees (- (/ tau 4))))
      (rect -5 -90 10 80))))


(defun ease (rate current goal)
  (+ current (* rate (- goal current))))

(defmacro easef (rate place goal)
  `(zap% ,place #'ease ,rate % ,goal))

(defsketch cm
    ((width *width*) (height *height*) (y-axis :up) (title "Coding Math 2D")
     (mouse (make-vec 0 0))
     ;; Data
     (p (make-particle 0.0 (random height) :radius 10))
     (points (loop :repeat 50
                   :collect (make-particle 0.0 0.0 :radius 5)))
     (target (make-vec width (random height)))
     (easing nil)
     (wheel-angle 0.0)
     ;; Pens
     (particle-pen (make-pen :fill (gray 0.9) :stroke (gray 0.4)))
     (line-pen (make-pen :curve-steps 100
                         :stroke (gray 0.7)))
     )
  (with-setup
    ;;
    (in-context
      (draw-axes *width* *height*)
      (easef 0.05 wheel-angle
             (degrees (map-range 0 *width*
                                 (/ tau 2) (- (/ tau 2))
                                 (vec-x mouse))))
      (draw-wheel wheel-angle)
      (when easing
        ; (text "easing" 0 100)
        ; (text (format nil "points: ~D" (length points)) 0 100)
        (setf easing (particle-ease-to! p target 0.2))
        (particle-update! p))
      (with-pen particle-pen
        (draw-particle p)
        (do ((previous p current)
             (current (car points) (car remaining))
             (remaining (cdr points) (cdr remaining)))
            ((null current))
          (particle-ease-to! current (particle-pos previous) 0.2 t)
          (particle-update! current)
          (draw-particle current)
          ))

      )
    ;;
    )
  )


;;;; Mouse
(defun mousemove (instance x y)
  (with-slots (target mouse easing) instance
    (setf mouse (make-vec x (- *height* y)))
    ;;
    (setf target mouse
          easing t)
    ;;
    )
  )


(defun mousedown-left (instance x y)
  (declare (ignorable instance x y))
  )

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

