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
(defstruct (arm (:constructor make-arm (pos length angle &optional parent)))
  pos length angle parent)

(define-with-macro arm pos length angle parent)


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
     (arms (iterate (repeat 8)
                    (for arm = (make-arm (if prev
                                           (arm-end prev)
                                           (vec2 0 0))
                                         (random-range 30 60)
                                         (random-around 0.0 (* tau 0.3))
                                         prev))
                    (for prev :previous arm)
                    (collect arm)))
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

      (incf a 0.03)

      (iterate
        (for arm :in arms)
        (for prev :previous arm)
        (for incr :first 1.0 :then (* incr 0.8))
        (if-first-time
          (setf (arm-angle arm) (* incr pi (sin a)))
          ;; uncomment to twirl vvvvv
          ; (setf (arm-angle arm) (* incr pi (sin a)))
          )
        (when prev
          (setf (arm-pos arm) (arm-end prev)))
        (draw-arm arm))
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
