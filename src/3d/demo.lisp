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
(defun perspective (focal-length z)
  (/ focal-length (+ focal-length z)))

(defun draw-shape (shape focal-length size)
  (in-context
    (scale (perspective focal-length (getf shape :z)))
    (translate (getf shape :x)
               (getf shape :y))
    ; (rect (- size) (- size) (* 2 size) (* 2 size))
    (circle 0 0 size)
    ))


;;;; Sketch
(defsketch demo (:width *width*
                 :height *height*
                 :debug :scancode-d)
    ((ready)
     (mouse)
     (fl)
     (shapes)
     (cz)
     (radius)
     (base-angle)
     (angle-speed)
     (y-speed)
     ; (simple-pen (make-pen :weight 4 :stroke (gray 0.0) :fill (gray 0.6)))
     (simple-pen (make-pen :fill (gray 0.1)))
     )
  (with-setup ready
    ;;
    (setf angle-speed (map-range 0 *height* -0.08 0.08 (cdr mouse)))
    (incf base-angle angle-speed)
    (setf shapes (sort shapes #'> :key (rcurry #'getf :z)))
    (with-pen simple-pen
      (loop :for shape :in shapes
            :for angle = (getf shape :angle)
            :do
            (setf (getf shape :x)
                  (* (car mouse) (cos (+ base-angle angle)))
                  (getf shape :z)
                  (+ cz (* radius (sin (+ base-angle angle)))))
            (incf (getf shape :y) y-speed)
            (wrapf (getf shape :y) (* *height* -1/2) (* *height* 1/2))
            (draw-shape shape fl 5)))
    ;;
    ))


(defun reset (game)
  (setf-slots game
    ready nil
    ;;
    fl 300
    cz 200
    radius 50
    base-angle 0.0
    angle-speed 0.01
    y-speed 0.5
    shapes (loop
             :with nshapes = 200
             :for i :from 0 :to nshapes
             :collect
             (list :x nil
                   :y (- (* i (/ *height* nshapes))
                         (/ *height* 2))
                   :z 0
                   :angle (* i (/ (* 6 tau) nshapes))))
    ;;
    ready t))


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


;;;; Make
(defun make-demo ()
  (make-sketch 'demo
    (mouse (cons nil nil))))


;;;; Run
; (defparameter *demo* (make-demo))
