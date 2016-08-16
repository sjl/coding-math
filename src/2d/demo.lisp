(in-package #:coding-math.2d.demo)

;;;; Config
(setf *bypass-cache* t)
(defparameter *width* 600)
(defparameter *height* 400)

(defparameter *center-x* (/ *width* 2))
(defparameter *center-y* (/ *height* 2))
(defparameter *center* (vec2 *center-x* *center-y*))


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
(defparameter *arm-pen* (make-pen :weight 10 :stroke (gray 0.1)))

(defstruct (arm (:constructor make-arm (pos length angle &optional parent)))
  pos length angle parent)

(define-with-macro arm
  pos length angle parent)


(defun arm-end (arm)
  (with-arm (arm)
    (vec2-add pos (vec2-magdir length angle))))

(defun draw-arm (arm &optional (pen *arm-pen*))
  (with-pen pen (draw-line (arm-pos arm) (arm-end arm))))

(defun arm-point-at (arm target)
  (with-arm (arm)
    (setf angle (-<> target
                  (vec2-sub <> pos)
                  vec2-angle))))

(defun arm-drag (arm target)
  (arm-point-at arm target)
  (with-arm (arm)
    (setf pos (vec2-sub target
                        (vec2-magdir length angle)))
    (when parent
      (arm-drag parent pos))))


(defun vector-last (vector)
  (let ((l (length vector)))
    (if (zerop l)
      nil
      (aref vector (1- l)))))


(defclass inverse-kinematic-system ()
  ((origin :initarg :origin
           :accessor iks-origin)
   (arms :initform (make-array 10 :fill-pointer 0)
         :accessor iks-arms)))

(define-with-macro iks origin arms)


(defun make-iks (origin)
  (make-instance 'inverse-kinematic-system :origin origin))

(defun iks-last-arm (iks)
  (vector-last (iks-arms iks)))

(defun iks-add-arm (iks length)
  (with-iks (iks)
    (let ((parent (iks-last-arm iks)))
      (vector-push-extend (make-arm (if parent
                                      (arm-pos parent)
                                      origin)
                                    length 0 parent)
                        arms))))

(defun draw-iks (iks)
  (map nil #'draw-arm (iks-arms iks)))

(defun iks-drag (iks target)
  (let ((arm (iks-last-arm iks)))
    (when arm
      (arm-drag arm target))))

(defun iks-correct (iks)
  (iterate
    (for arm :in-vector (iks-arms iks))
    (for parent = (arm-parent arm))
    (setf (arm-pos arm)
          (if parent
            (arm-end parent)
            (iks-origin iks)))))

(defun iks-reach (iks target)
  (iks-drag iks target)
  (iks-correct iks))


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
     (iks1 (make-iks (vec2 (- 150) (- *center-y*))))
     (iks2 (make-iks (vec2 (+ 150) (- *center-y*))))
     (ball (make-particle 0 0
                          :speed (random-around 20 2.0)
                          :direction (random tau)
                          :mass 10.0
                          :radius 10
                          :gravity 0.0))
     (lol (progn
            (iterate (repeat 6)
                     (iks-add-arm iks1 (random-around 50 30)))
            (iterate (repeat 6)
                     (iks-add-arm iks2 (random-around 50 30)))
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
      (draw-axes *width* *height*)

      (particle-update! ball)

      ;; Garbage wrapping code because I don't feel like rewriting the particle
      ;; system to use the new vectors and centered coordinates.
      (let ((x (particle-x ball))
            (y (particle-y ball)))
        (when (not (< (- *center-x*) x *center-x*))
          (negatef (coding-math.2d.vectors::vec-x (particle-vel ball)))
          (setf
            (particle-x ball) (max (- *center-x*) x)
            (particle-x ball) (min (+ *center-x*) x)

            (coding-math.2d.vectors::vec-magnitude (particle-vel ball))
            (* 0.95 (coding-math.2d.vectors::vec-magnitude (particle-vel ball)))))
        (when (not (< (- *center-y*) y *center-y*))
          (negatef (coding-math.2d.vectors::vec-y (particle-vel ball)))
          (setf
            (particle-x ball) (max (- *center-x*) x)
            (particle-x ball) (min (+ *center-x*) x)

            (coding-math.2d.vectors::vec-magnitude (particle-vel ball))
                (* 0.95 (coding-math.2d.vectors::vec-magnitude (particle-vel ball))))))

      (draw-particle ball)

      (let ((target (vec2 (particle-x ball)
                          (particle-y ball))))
        (iks-reach iks1 target)
        (draw-iks iks1)
        (iks-reach iks2 target)
        (draw-iks iks2))

      ))
  ;;

  )


;;;; Mouse
(defun mousemove (instance x y)
  (with-slots (mouse) instance
    (setf mouse (vec2-sub (vec2 x (- *height* y)) *center*))
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
