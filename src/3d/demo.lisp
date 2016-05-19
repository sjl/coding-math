(in-package #:coding-math.3d.demo)


;;;; Config
(setf *bypass-cache* t)
(defparameter *width* 600)
(defparameter *height* 400)

(defparameter *center-x* (/ *width* 2))
(defparameter *center-y* (/ *height* 2))


;;;; Utils
(defmacro with-centered-coords (&body body)
  `(in-context
     (translate *center-x* *center-y*)
     ,@body))

(defmacro with-setup (&body body)
  `(with-fps
    (background (gray 1))
    (with-centered-coords
      ,@body)))


;;;; Draw
(defun draw-point (screen size)
  (circle (vec-x screen) (vec-y screen) size))


;;;; Structs
(defstruct (point (:constructor make-point (world &optional screen)))
  world screen)


;;;; Functions
(defun rotate-x (angle points)
  (let ((s (sin angle))
        (c (cos angle)))
    (map nil (lambda (p)
               (with-vec (p (point-world p))
                 (psetf p.y (- (* p.y c) (* p.z s))
                        p.z (+ (* p.z c) (* p.y s)))))
         points)))

(defun rotate-y (angle points)
  (let ((s (sin angle))
        (c (cos angle)))
    (map nil (lambda (p)
               (with-vec (p (point-world p))
                 (psetf p.x (- (* p.x c) (* p.z s))
                        p.z (+ (* p.z c) (* p.x s)))))
         points)))

(defun rotate-z (angle points)
  (let ((s (sin angle))
        (c (cos angle)))
    (map nil (lambda (p)
               (with-vec (p (point-world p))
                 (psetf p.x (- (* p.x c) (* p.y s))
                        p.y (+ (* p.y c) (* p.x s)))))
         points)))


;;;; Sketch
(defun project (points focal-length center-z)
  (map nil
       (lambda (p)
         (with-vecs ((screen (point-screen p))
                     (world (point-world p)))
           (let ((scale (/ focal-length (+ focal-length world.z center-z))))
             (setf screen.x (* scale world.x)
                   screen.y (* scale world.y)))))
       points))

(defun translate-model (points x y z)
  (map nil (lambda (p)
             (with-vec (p (point-world p))
               (incf p.x x)
               (incf p.y y)
               (incf p.z z)))
       points))


(defsketch demo
    ((width *width*) (height *height*) (y-axis :up) (title "Coding Math")
     (mouse (cons 0 0))
     ;; variables
     (fl 300.0)
     (r 200.0)
     (center-z 1500.0)
     (points
       (make-array 8
         :initial-contents
         (list
           (make-point (vec (- r) (- r)     r) (zero-vec))
           (make-point (vec     r (- r)     r) (zero-vec))
           (make-point (vec     r (- r) (- r)) (zero-vec))
           (make-point (vec (- r) (- r) (- r)) (zero-vec))
           (make-point (vec (- r)     r     r) (zero-vec))
           (make-point (vec     r     r     r) (zero-vec))
           (make-point (vec     r     r (- r)) (zero-vec))
           (make-point (vec (- r)     r (- r)) (zero-vec)))))
     ;; pens
     (simple-pen (make-pen :fill (gray 0.1)))
     (line-pen (make-pen :stroke (gray 0.1) :weight 1))
     )
  (with-setup
    (flet
        ((draw-line (&rest vertices)
           (loop :for (a b) ; lame way to close the loop...
                 :in (n-grams 2 (append vertices (list (car vertices))))
                 :do (with-vecs ((a (point-screen (aref points a)))
                                 (b (point-screen (aref points b))))
                       (line a.x a.y b.x b.y)))))
      (project points fl center-z)
      (when *shift* (text "shift!" 100 100))
      (when *control* (text "control!" 100 120))
      (with-pen simple-pen
        ; (loop :for p :across points
        ;       :do (draw-point (point-screen p) 5))
        nil)
      (with-pen line-pen
        (draw-line 0 1 2 3)
        (draw-line 0 3 7 4)
        (draw-line 1 2 6 5)
        (draw-line 6 5 4 7)
        nil
        ))
    ;;
    ))


;;;; Mouse
(defun mousemove (instance x y)
  (with-slots (mouse) instance
    (setf (car mouse) x)
    (setf (cdr mouse) y)
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
(defvar *shift* nil)
(defvar *control* nil)
(defvar *command* nil)
(defvar *option* nil)


(defun keydown (instance scancode)
  (declare (ignorable instance))
  (scancode-case scancode
    (:scancode-space (sketch::prepare instance))
    (:scancode-lshift (setf *shift* t))
    (:scancode-lctrl (setf *control* t))
    (:scancode-lgui (setf *command* t))
    (:scancode-lalt (setf *option* t))
    ;;
    (:scancode-left  (if *shift*
                       (rotate-y -0.05 (demo-points instance))
                       (translate-model (slot-value instance 'points) -15 0 0)))
    (:scancode-right (if *shift*
                       (rotate-y 0.05 (demo-points instance))
                       (translate-model (slot-value instance 'points) 15 0 0)))
    (:scancode-up    (if *shift*
                       (rotate-x -0.05 (demo-points instance))
                       (translate-model (slot-value instance 'points) 0 15 0)))
    (:scancode-down  (if *shift*
                       (rotate-x 0.05 (demo-points instance))
                       (translate-model (slot-value instance 'points) 0 -15 0)))
    (:scancode-s     (if *shift*
                       (rotate-z -0.05 (demo-points instance))
                       (translate-model (slot-value instance 'points) 0 0 -15)))
    (:scancode-w     (if *shift*
                       (rotate-z 0.05 (demo-points instance))
                       (translate-model (slot-value instance 'points) 0 0 15)))
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
