(in-package #:coding-math)

;;;; Config
(defparameter *width* 600)
(defparameter *height* 400)

(defparameter center-x (/ *width* 2))
(defparameter center-y (/ *height* 2))


;;;; FPS
(defvar *last-draw*
  (get-internal-real-time))

(defvar *fps* 0.0)


(defun calc-fps (frames)
  (let* ((current-draw (get-internal-real-time))
         (elapsed (float (/ (- current-draw *last-draw*)
                            internal-time-units-per-second))))
    (setf *last-draw* current-draw)
    (setf *fps* (* frames (/ 1 elapsed)))))

(defun draw-fps ()
  (text (format nil "FPS: ~,1F" *fps*) 0 0))


;;;; Sketch
(defmacro in-context (&rest body)
  `(prog1
     (push-matrix)
     (progn ,@body)
     (pop-matrix)))

(defmacro wrap (place min max)
  ;; todo: how do i places
  (with-gensyms (min-val max-val)
    `(let ((,min-val ,min) (,max-val ,max))
      (when (< ,place ,min-val) (setf ,place ,max-val))
      (when (> ,place ,max-val) (setf ,place ,min-val)))))


(defun draw-ship (ship angle thrustingp)
  (in-context
    (translate (particle-x ship) (particle-y ship))
    (rotate (degrees angle))
    (when thrustingp
      (with-pen (make-pen :fill (rgb 1.0 0.0 0.0))
        (ngon 3 -15 0 10 6))) ; fire 
    (with-pen (make-pen :stroke (gray 0) :fill (gray 0.5))
      (rect -10 -3 10 6) ; engine
      (ngon 3 0 0 10 10) ; hull
      (ngon 3 6 0 6 3)))) ; cockpit


(defsketch cm (:width *width*
               :height *height*
               :debug :scancode-d)
    ((mx 0)
     (my 0)
     (ship (make-particle center-x center-y 0 0))
     (angle 0)
     (frame 1)
     (turning-left nil)
     (turning-right nil)
     (thrusting nil))
  (background (gray 1))
  (incf frame)
  (when (zerop (mod frame 20))
    (calc-fps 20))
  (particle-update! ship)
  (wrap (particle-x ship) 0 *width*)
  (wrap (particle-y ship) 0 *height*)
  (when turning-left (decf angle 0.05))
  (when turning-right (incf angle 0.05))
  (when thrusting
    (particle-accelerate! ship (make-vec-md 0.1 angle)))
  (draw-ship ship angle thrusting)
  (draw-fps))


;;;; Mouse
(defmethod mousemotion-event ((window cm) ts b x y xrel yrel)
  (declare (ignore ts b xrel yrel))
  (with-slots (mx my) window
    (setf mx x)
    (setf my y)))


;;;; Keyboard
(defmacro scancode-case (scancode-form &rest pairs)
  (let ((scancode (gensym "scancode")))
    `(let ((,scancode ,scancode-form))
      (cond
        ,@(mapcar (lambda (pair)
                    (destructuring-bind (key-scancode &rest body) pair
                      `((sdl2:scancode= ,scancode ,key-scancode)
                        ,@body)))
                  pairs)))))


(defun keydown (instance scancode)
  (scancode-case scancode
    (:scancode-left (setf (slot-value instance 'turning-left) t))
    (:scancode-right (setf (slot-value instance 'turning-right) t))
    (:scancode-up (setf (slot-value instance 'thrusting) t))))

(defun keyup (instance scancode)
  (scancode-case scancode
    (:scancode-left (setf (slot-value instance 'turning-left) nil))
    (:scancode-right (setf (slot-value instance 'turning-right) nil))
    (:scancode-up (setf (slot-value instance 'thrusting) nil))))


(defmethod kit.sdl2:keyboard-event ((instance cm) state timestamp repeatp keysym)
  (declare (ignore timestamp repeatp))
  (cond
    ((eql state :keyup) (keyup instance (sdl2:scancode-value keysym)))
    ((eql state :keydown) (keydown instance (sdl2:scancode-value keysym)))
    (t nil)))


;;;; Run
(defparameter *demo* (make-instance 'cm))

