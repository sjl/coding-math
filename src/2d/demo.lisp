(in-package #:coding-math.2d.demo)

;;;; Config
(setf *bypass-cache* t)
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

(defun real-time ()
  (/ (get-internal-real-time) internal-time-units-per-second))

(defun random-location ()
  (vec2 (random-range 0 *width*)
        (random-range 0 *height*)))

(defun random-location-centered ()
  (vec2 (random-range (- *center-x*) *center-x*)
        (random-range (- *center-y*) *center-y*)))


;;;; Episode
(defstruct (arm (:constructor %make-arm))
  pos length angle parent center-angle rotation-range phase-offset)

(defun make-arm (length center-angle rotation-range parent phase-offset)
  (%make-arm :pos (vec2 0 0)
             :length length
             :angle center-angle
             :parent parent
             :center-angle center-angle
             :rotation-range rotation-range
             :phase-offset phase-offset))

(define-with-macro arm pos length angle parent center-angle rotation-range phase-offset)


(defun arm-set-phase (arm phase)
  (with-arm arm
    (setf angle (+ center-angle (* rotation-range (sin (+ phase-offset phase)))))))

(defun arm-total-angle (arm)
  (loop :for a = arm :then (arm-parent a)
        :while a
        :sum (arm-angle a)))

(defun arm-end (arm)
  (with-arm arm
    (vec2-add pos (vec2-magdir length (arm-total-angle arm)))))

(defparameter *arm-pen* (make-pen :stroke (gray 0.1) :weight 5))

(defun draw-arm (arm &optional (pen *arm-pen*))
  (with-arm arm
    (with-pen pen (draw-line pos (arm-end arm)))))


(defclass forward-kinematics-system ()
  ((origin :type vec2 :initarg :origin :reader fks-origin)
   (arms   :type vector :initarg :arms :accessor fks-arms)
   (last-arm :initarg :last-arm :accessor fks-last-arm)
   (phase :initarg :phase :accessor fks-phase)
   (speed :initform 0.05 :accessor fks-speed)))

(defun make-fks (x y &optional (starting-phase 0.0))
  (make-instance 'forward-kinematics-system
                 :origin (vec2 x y)
                 :arms (make-array 10 :fill-pointer 0)
                 :last-arm nil
                 :phase starting-phase))


(defun fks-update (fks)
  (iterate (with phase = (fks-phase fks))
           (for arm :in-vector (fks-arms fks))
           (for parent = (arm-parent arm))
           (arm-set-phase arm phase)
           (setf (arm-pos arm)
                 (if parent
                   (arm-end parent)
                   (fks-origin fks))))
  (incf (fks-phase fks) (fks-speed fks))
  (wrapf (fks-phase fks) 0 tau)
  )


(defun fks-add-arm (fks length center-angle rotation-range &optional (phase-offset 0.0))
  (let* ((arm (make-arm length center-angle rotation-range
                        (fks-last-arm fks) phase-offset)))
    (vector-push-extend arm (fks-arms fks) 10)
    (setf (fks-last-arm fks) arm))
  (fks-update fks))

(defun draw-fks (fks)
  (map nil #'draw-arm (fks-arms fks)))

(defun fks-rotate-arm (fks arm-index angle)
  (setf (arm-angle (aref (fks-arms fks) arm-index))
        angle))


;;;; Sketch
(defun draw-particle (p)
  (circle (particle-x p) (particle-y p) (particle-radius p)))

(defun draw-line (p1 p2)
  (line (vec2-x p1) (vec2-y p1)
        (vec2-x p2) (vec2-y p2)))

(defun draw-circle (p &optional (radius 5))
  (circle (vec2-x p) (vec2-y p) radius))

(defun draw-square (p radius)
  (rect (- (vec2-x p) radius)
        (- (vec2-y p) radius)
        (* 2 radius)
        (* 2 radius)))

(defun draw-polygon (points)
  (when points
    ;; why is this fucked?
    (apply #'polygon
           (iterate (for p :in points)
                    (collect (vec2-x p))
                    (collect (vec2-y p))))))

(defun vec-to-list (v)
  (list (vec2-x v) (vec2-y v)))

(defun draw-triangle (p1 p2 p3)
  #+sketch-polygon-fn-is-fucked (polygon (vec2-x p1) (vec2-y p1)
                                         (vec2-x p2) (vec2-y p2)
                                         (vec2-x p3) (vec2-y p3))
  (let ((vertices (list (vec-to-list p1)
                        (vec-to-list p2)
                        (vec-to-list p3))))
    (sketch::draw-shape :triangles vertices vertices)))


(defsketch demo
    ((width *width*) (height *height*) (y-axis :up) (title "Coding Math 2D")
     (copy-pixels nil)
     (mouse (vec2 0 0))
     (frame 0)
     (start-time (real-time))
     (current-time 0)
     (previous-time 0)
     (total-time 0)
     ;; Data
     (a 0.0)
     (leg0 (make-fks 0 0))
     (leg1 (make-fks 0 0 pi))
     (lol (progn
            (fks-add-arm leg0 90 (* tau  -1/4) (* tau 1/6))
            (fks-add-arm leg0 60 (* tau -1/12) (* tau 1/12) -1.5)
            (fks-add-arm leg0 20 (* tau   1/5) (* tau (- 1/4 1/5)) -1.5)
            (fks-add-arm leg1 90 (* tau  -1/4) (* tau 1/6))
            (fks-add-arm leg1 60 (* tau -1/12) (* tau 1/12) -1.5)
            (fks-add-arm leg1 20 (* tau   1/5) (* tau (- 1/4 1/5)) -1.5)
            ))
     ;; Pens
     (particle-pen (make-pen :fill (gray 0.9) :stroke (gray 0.4)))
     (black-pen (make-pen :stroke (rgb 0 0 0) :fill (rgb 0.4 0.4 0.4) :weight 1 :curve-steps 50))
     (red-pen (make-pen :stroke (rgb 0.6 0 0) :fill (rgb 0.9 0 0) :weight 1 :curve-steps 50))
     (green-pen (make-pen :stroke (rgb 0 0.6 0) :fill (rgb 0 0.9 0) :weight 1 :curve-steps 50))
     (blue-pen (make-pen :stroke (rgb 0 0 0.6) :fill (rgb 0 0 0.9) :weight 1 :curve-steps 50))
     )
  (setf previous-time current-time
        current-time (real-time))
  (incf total-time (- current-time previous-time))
  (incf frame)
  ;;
  (with-setup
    (in-context
      (translate *center-x* *center-y*)

      (fks-update leg0)
      (fks-update leg1)
      (draw-fks leg0)
      (draw-fks leg1)
      ))
  ;;

  )


;;;; Mouse
(defun mousemove (instance x y)
  (with-slots (mouse) instance
    (setf (vec2-x mouse) x
          (vec2-y mouse) (- *height* y))
    ;;
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


(defmethod kit.sdl2:mousemotion-event ((window demo) ts b x y xrel yrel)
  (declare (ignore ts b xrel yrel))
  (mousemove window x y))

(defmethod kit.sdl2:mousebutton-event ((window demo) state ts button x y)
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


(defmethod kit.sdl2:keyboard-event ((instance demo) state timestamp repeatp keysym)
  (declare (ignore timestamp repeatp))
  (cond
    ((eql state :keyup) (keyup instance (sdl2:scancode-value keysym)))
    ((eql state :keydown) (keydown instance (sdl2:scancode-value keysym)))
    (t nil)))


;;;; Run
; (defparameter *demo* (make-instance 'demo))
