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

(defun draw-polygon (points)
  (when points
    ;; why is this fucked?
    (apply #'polygon
           (iterate (for p :in points)
                    (collect (vec-x p))
                    (collect (vec-y p))))))


(defun move-star (star x y)
  (let ((center (getf star :center)))
    (setf (particle-x center) x
          (particle-y center) y

          (getf star :points)
          (iterate
            (repeat 10)
            (for a :from (getf star :angle) :by (/ tau 10))
            (for iteration :from 0)
            (for r = (* (getf star :radius)
                        (if (evenp iteration) 1.0 0.5)))
            (collect (vec-add (particle-pos center)
                              (make-vec-md r a)))))))

(defun make-star (angle radius)
  (let ((star (list :center (make-particle 0 0 :radius 5)
                    :angle angle
                    :radius radius
                    :points nil)))
    (move-star star (random *width*) (random *height*))
    star))


(defun draw-star (star)
  (draw-particle (getf star :center))
  ; (draw-polygon (getf star :points))
  (iterate
    (with points = (getf star :points))
    (for (p1 . p2) :pairs-of-list points)
    (draw-circle p1 3)
    (draw-line p1 p2)))

(defun oob-p (p &optional (r 0.0))
  (or (outsidep (- 0 r) (+ *width* r) (vec-x p))
      (outsidep (- 0 r) (+ *height* r) (vec-y p))))


(defun stars-collide-p (star-1 star-2)
  (iterate
    main
    (for (p1 . p2) :pairs-of-list (getf star-1 :points))
    (iterate
      (for (p3 . p4) :pairs-of-list (getf star-2 :points))
      (with-vecs ((x11 y11) p1
                  (x12 y12) p2
                  (x21 y21) p3
                  (x22 y22) p4)
        (in main
            (thereis (xys-segments-intersection-point
                       x11 y11 x12 y12 x21 y21 x22 y22)))))))


(defsketch cm
    ((width *width*) (height *height*) (y-axis :down) (title "Coding Math 2D")
     (mouse (make-vec 0 0))
     ;; Data
     (star-1 (make-star (random tau) (random-range 30.0 70.0)))
     (star-2 (make-star (random tau) (random-range 30.0 70.0)))
     (dragging nil)
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
      (draw-axes *width* *height*)
      (with-pen red-pen
        (draw-star star-1))
      (with-pen blue-pen
        (draw-star star-2))
      (when (stars-collide-p star-1 star-2)
        (text "BOOM!" *center-x* *center-y*))

      )
    ;;
    )
  )


;;;; Mouse
(defun mousemove (instance x y)
  (with-slots (mouse) instance
    (setf mouse (make-vec x (- *height* y)))
    ;;
    (with-slots (dragging) instance
      (when dragging
        (move-star dragging x y)
        )
      )
    ;;
    )
  )

(defun draw-time ()
  (text (format nil "~d" (get-internal-real-time))
        300 300))

(defun mousedown-left (instance x y)
  (declare (ignorable instance x y))
  (with-slots (dragging star-1 star-2) instance
    (let ((p1 (getf star-1 :center))
          (p2 (getf star-2 :center))
          (target (make-vec x y)))
      (cond
        ((circle-point-collide-p p1 target) (setf dragging star-1))
        ((circle-point-collide-p p2 target) (setf dragging star-2))
        (t nil)
        )))
  )

(defun mousedown-right (instance x y)
  (declare (ignorable instance x y))
  )

(defun mouseup-left (instance x y)
  (declare (ignorable instance x y))
  (with-slots (dragging) instance
    (setf dragging nil))
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
