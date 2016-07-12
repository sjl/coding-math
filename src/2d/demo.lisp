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

(defun oob-p (p &optional (r 0.0))
  (or (outsidep (- 0 r) (+ *width* r) (vec-x p))
      (outsidep (- 0 r) (+ *height* r) (vec-y p))))

(defun real-time ()
  (/ (get-internal-real-time) internal-time-units-per-second))

(defun random-location ()
  (make-random-vec *width* *height*))

(defun random-location-centered ()
  (make-vec (random-range (- *center-x*) *center-x*)
            (random-range (- *center-y*) *center-y*)))


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

(defun draw-polygon (points)
  (when points
    ;; why is this fucked?
    (apply #'polygon
           (iterate (for p :in points)
                    (collect (vec-x p))
                    (collect (vec-y p))))))

(defun draw-triangle (p1 p2 p3)
  #+sketch-polygon-fn-is-fucked (polygon (vec-x p1) (vec-y p1)
                                         (vec-x p2) (vec-y p2)
                                         (vec-x p3) (vec-y p3))
  (let ((vertices (list (vec-to-list p1)
                        (vec-to-list p2)
                        (vec-to-list p3))))
    (sketch::draw-shape :triangles vertices vertices)))


(defstruct (point
             (:constructor make-point (pos old-pos)))
  pos old-pos)

(defstruct (stick
             (:constructor make-stick (a b length &key
                                         (hidden nil)
                                         (color (gray 0.0))
                                         (width 1))))
  a b length hidden color width)


(defun make-random-point ()
  (let ((v (make-random-vec *width* *height*)))
    (make-point v v)))

(defun update-point (point)
  (with-slots (pos old-pos) point
    (let* ((friction 0.999)
           (gravity (make-vec 0 -0.2))
           (vel (vec-mul (vec-sub pos old-pos) friction)))
      (setf old-pos pos
            pos (vec-add gravity (vec-add pos vel))))))

(defun constrain-point (point)
  (with-slots (pos old-pos) point
    (let* ((bounce 0.9)
           (vel (vec-sub pos old-pos)))
      (macrolet ((wrap ((cur old vel) comp bound)
                   `(when (,comp ,cur ,bound)
                     (setf ,cur ,bound
                           ,old (+ ,cur (* bounce ,vel))))))
        (wrap ((vec-x pos) (vec-x old-pos) (vec-x vel)) > *width*)
        (wrap ((vec-x pos) (vec-x old-pos) (vec-x vel)) < 0)
        (wrap ((vec-y pos) (vec-y old-pos) (vec-y vel)) > *height*)
        (wrap ((vec-y pos) (vec-y old-pos) (vec-y vel)) < 0)))))

(defun update-stick (stick)
  (with-slots (a b length) stick
    (let* ((pos-a (point-pos a))
           (pos-b (point-pos b))
           (between (vec-sub pos-a pos-b))
           (distance (vec-magnitude between))
           (change (/ (- length distance) 2))
           (correction (vec-set-magnitude between change)))
      (setf (point-pos a) (vec-add pos-a correction))
      (setf (point-pos b) (vec-sub pos-b correction)))))

(defun render-point (point)
  (draw-circle (point-pos point) 5))

(defun render-stick (stick)
  (with-slots (a b hidden color width) stick
    (unless hidden
      (with-pen (make-pen :stroke color :weight width)
        (draw-line (point-pos a) (point-pos b))))))


(defsketch cm
    ((width *width*) (height *height*) (y-axis :up) (title "Coding Math 2D")
     (copy-pixels nil)
     (mouse (make-vec 0 0))
     (frame 0)
     (start-time (real-time))
     (current-time 0)
     (previous-time 0)
     (total-time 0)
     ;; Data
     (points (iterate (repeat 4) (collect (make-random-point))))
     (sticks (append
               (iterate (for (a . b) :pairs-of-list points)
                        (collect (make-stick a b (random-range 50 200) :width 5)))
               (list (make-stick (nth 0 points) (nth 2 points) 100 :hidden t))))
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
      (mapc #'update-point points)
      (iterate (repeat 3)
               (mapc #'update-stick sticks)
               (mapc #'constrain-point points))
      (mapc #'render-stick sticks)
      (with-pen red-pen
        (mapc #'render-point points))
      ))
  ;;

  )


;;;; Mouse
(defun mousemove (instance x y)
  (with-slots (mouse) instance
    (setf mouse (make-vec x (- *height* y)))
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
