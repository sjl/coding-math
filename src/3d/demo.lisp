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

(defmacro with-setup (&body body)
  `(with-fps
    (background (gray 1))
    (with-centered-coords
      ,@body)))


;;;; Draw
(declaim (inline perspective apply-perspective))
(defun perspective (focal-length z)
  (/ focal-length (+ focal-length z)))

(defun apply-perspective (vec focal-length)
  (let ((p (perspective focal-length (aref vec 2))))
    (sb-cga:transform-point vec (sb-cga:scale* p p p))))

(defun draw-point (point focal-length size)
  (let ((p (apply-perspective point focal-length)))
    (in-context
      (translate (aref p 0) (aref p 1))
      (circle 0 0 size))))

(defun draw-line (p1 p2 focal-length)
  (let ((p1 (apply-perspective p1 focal-length))
        (p2 (apply-perspective p2 focal-length)))
    (line (aref p1 0) (aref p1 1)
          (aref p2 0) (aref p2 1))))


;;;; Sketch
(defsketch demo
    ((width *width*)
     (height *height*)
     (mouse (cons 0 0))
     (fl 300.0)
     (cz 700.0)
     (radius 400.0)
     (cyl-height 380.0)
     (wraps 6)
     (base-angle 0.0)
     (angle-speed -0.02)
     (circle-size 3)
     (y-speed -0.5)
     (shapes (loop
               :with nshapes = 400
               :for i :from 0 :to nshapes
               :collect
               (sb-cga:vec radius
                           (* i (/ (* wraps tau) (1+ nshapes)))
                           (+ #+no (random-around 0.0 50.0)
                              (map-range 0.0 nshapes cyl-height (- cyl-height) i)))))
     (model-to-world (sb-cga:translate* 0.0 0.0 cz))
     ;;
     (simple-pen (make-pen :fill (gray 0.1)))
     (line-pen (make-pen :stroke (gray 0.1) :weight 2))
     )
  (with-setup
    ; (setf angle-speed (map-range 0 *height* -0.08 0.08 (cdr mouse)))
    ; (setf shapes (sort shapes #'> :key (rcurry #'getf :z)))
    (with-pen simple-pen
      (loop :for shape :in shapes
            :do
            (setf (aref shape 0) (map-range 0.0 *width* 10 600 (car mouse)))
            (incf (aref shape 1) angle-speed)
            (incf (aref shape 2) (random-around 0.0 0.2))
            ; (incf (aref shape 2) y-speed)
            ; (wrapf (aref shape 2) (- cyl-height) cyl-height)
            #+debug (draw-point
                   (sb-cga:transform-point
                     (cylindrical-to-cartesian-cga shape)
                     model-to-world)
                   fl
                   circle-size))
      )
    (with-pen line-pen
      (loop :for (a b) :in (n-grams 2 shapes) :do
            (draw-line (sb-cga:transform-point
                         (cylindrical-to-cartesian-cga a)
                         model-to-world)
                       (sb-cga:transform-point
                         (cylindrical-to-cartesian-cga b)
                         model-to-world)
                       fl)))
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
(defun keydown (instance scancode)
  (declare (ignorable instance))
  (scancode-case scancode
    (:scancode-space (sketch::prepare instance))))

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
; (defparameter *demo* (make-instance 'demo))
