(in-package #:coding-math.3d.demo)


;;;; Config
(setf *bypass-cache* nil)
(defparameter *width* 600)
(defparameter *height* 400)

(defparameter *center-x* (/ *width* 2))
(defparameter *center-y* (/ *height* 2))


(defvar *shift* nil)
(defvar *control* nil)
(defvar *command* nil)
(defvar *option* nil)


(defvar *tile-width* 100)
(defvar *tile-height* 50)


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

(defun real-time ()
  (/ (get-internal-real-time) internal-time-units-per-second))


;;;; Draw
(defun draw-point (screen size)
  (circle (vec-x screen) (vec-y screen) size))

(defun draw-tile (x y pen)
  ;;         0 0
  ;;          h
  ;;  a        wwwwww b
  ;;
  ;;          c
  (in-context
    (let* ((w (/ *tile-width* 2))
           (h (/ *tile-height* 2))
           (ax (- w))
           (ay (- h))
           (bx (+ w))
           (by (- h))
           (cx 0)
           (cy (- *tile-height*)))
      (translate (* (- x y) w)
                 (* -1 (+ x y) h))
      (with-pen pen
        (polygon 0 0 ax ay cx cy bx by)))))

(defun draw-block (x y z)
  ;;          a
  ;;          h
  ;;  b        wwwwww c
  ;;
  ;;          d
  ;;
  ;;
  ;;
  ;;          O
  ;;
  ;;  e               g
  ;;
  ;;          f
  (in-context
    (let* ((ww *tile-width*)
           (hh *tile-height*)
           (w (/ ww 2.0))
           (h (/ hh 2.0))
           (ax 0.0)
           (ay (* z hh))
           (bx (- w))
           (by (- ay h))
           (cx (+ w))
           (cy (- ay h))
           (dx 0.0)
           (dy (- ay hh))
           (ex (- w))
           (ey (- h))
           (gx (+ w))
           (gy (- h))
           (fx 0.0)
           (fy (- hh))
           )
      (translate (* (- x y) w)
                 (* -1 (+ x y) h))
      (with-pen (make-pen :fill (rgb 0 0 1.0))
        (polygon ax ay bx by dx dy cx cy))
      (with-pen (make-pen :fill (rgb 0 0 0.6))
        (polygon bx by dx dy fx fy ex ey))
      (with-pen (make-pen :fill (rgb 0 0 0.3))
        (polygon dx dy cx cy gx gy fx fy)))))


;;;; Sketch
(defsketch demo
    ((width *width*) (height *height*) (y-axis :up) (title "Coding Math 3D")
     (copy-pixels nil)
     (mouse (cons 0 0))
     (frame 0)
     (start-time (real-time))
     (current-time 0)
     (previous-time 0)
     (total-time 0)
     ;; data
     (h 0.1)
     (tiles (iterate
              tiles
              (for x :from 0 :below 10)
              (iterate (for y :from 0 :below 10)
                       (in tiles
                           (collect (list x y (make-pen :fill (gray (random-range 0.2 0.8)))))))))
     (blocks (iterate
               blocks
               (for x :from 0 :below 4)
               (iterate (for y :from 0 :below 4)
                        (in blocks
                            (collect (list (+ 3 x)
                                           (+ 3 y)
                                           (random-range 1 4)))))))
     ;; pens
     (black-pen (make-pen :stroke (rgb 0 0 0) :fill (rgb 0.4 0.4 0.4) :weight 1 :curve-steps 50))
     (red-pen (make-pen :stroke (rgb 0.6 0 0) :fill (rgb 0.9 0 0) :weight 1 :curve-steps 50))
     (green-pen (make-pen :stroke (rgb 0 0.6 0) :fill (rgb 0 0.9 0) :weight 1 :curve-steps 50))
     (blue-pen (make-pen :stroke (rgb 0 0 0.6) :fill (rgb 0 0 0.9) :weight 1 :curve-steps 50)))
  (setf previous-time current-time
        current-time (real-time))
  (incf total-time (- current-time previous-time))
  (incf frame)
  (with-setup
    ;;
    (incf h 0.02)
    (wrapf h 0.1 2.5)
    (in-context
      (translate 0 (- *center-y* 20))
      (iterate (for (x y pen) :in tiles)
               (draw-tile x y pen))
      (iterate (for (x y z) :in blocks)
               (draw-block x y z))
      (draw-block 3 7 h)
      )))


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
