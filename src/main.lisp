(in-package #:coding-math)

(declaim (optimize (speed 3)
                   (safety 2)
                   (debug 0)))

;;;; Config
(defparameter *width* 600)
(defparameter *height* 400)

(defparameter center-x (/ *width* 2))
(defparameter center-y (/ *height* 2))


;;;; FPS
(defvar *last-draw*
  (get-internal-real-time))

(defvar *fps* 0.0)
(defvar *mspf* 0.0)


(defun calc-fps (frames)
  (let* ((current-draw (get-internal-real-time))
         (elapsed (float (/ (- current-draw *last-draw*)
                            internal-time-units-per-second))))
    (setf *last-draw* current-draw)
    (setf *mspf* (* 1000 (/ elapsed frames)))
    (setf *fps* (* frames (/ 1 elapsed)))))

(defun draw-fps ()
  (text (format nil "MSPF: ~,1F" *mspf*) 0 0)
  (text (format nil "FPS: ~,1F" *fps*) 0 20))


;;;; Sketch
(defmacro in-context (&rest body)
  `(prog1
     (push-matrix)
     (progn ,@body)
     (pop-matrix)))


(defun particle-oob-p (particle)
  (let ((r (particle-radius particle)))
    (or (outside-p (- 0 r)
                   (+ *width* r)
                   (particle-x particle))
        (outside-p (- 0 r)
                   (+ *height* r)
                   (particle-y particle)))))


(declaim (inline draw-particle))
(defun draw-particle (particle)
  (circle (particle-x particle)
          (particle-y particle)
          (particle-radius particle)))


(defsketch cm (:width *width*
               :height *height*
               :debug :scancode-d)
    ((mx 0)
     (my 0)
     (frame 1)
     (particle (make-particle center-x *height*
                              :gravity 0.025
                              :speed (random-range 4.0 6.0)
                              :direction (random tau)
                              :radius 5))
     (bounce -0.7))
  (background (gray 1))
  (incf frame)
  ;;
  (with-pen (make-pen :stroke (gray 0) :fill (gray 0.5))
    (draw-particle particle)
    (mulf (vec-magnitude (particle-vel particle)) 0.998)
    (particle-update! particle)
    (let ((r (particle-radius particle)))
      (when (> (+ (particle-x particle) r) *width*)
        (setf (particle-x particle) (- *width* r))
        (mulf (vec-x (particle-vel particle)) bounce))
      (when (< (- (particle-x particle) r) 0)
        (setf (particle-x particle) r)
        (mulf (vec-x (particle-vel particle)) bounce))
      (when (> (+ (particle-y particle) r) *height*)
        (setf (particle-y particle) (- *height* r))
        (mulf (vec-y (particle-vel particle)) bounce))
      (when (< (- (particle-y particle) r) 0)
        (setf (particle-y particle) r)
        (mulf (vec-y (particle-vel particle)) bounce))))
  ;;
  (when (zerop (mod frame 20))
    (calc-fps 20))
  (draw-fps)
  )


;;;; Mouse
(defmethod kit.sdl2:mousemotion-event ((window cm) ts b x y xrel yrel)
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
                 ))

(defun keyup (instance scancode)
  (scancode-case scancode
    (:scancode-space
     (setf (vec-magnitude (particle-vel (slot-value instance 'particle)))
           (random-range 4.0 6.0)
           (vec-angle (particle-vel (slot-value instance 'particle)))
           (random tau)
           )
     )
    ))


(defmethod kit.sdl2:keyboard-event ((instance cm) state timestamp repeatp keysym)
  (declare (ignore timestamp repeatp))
  (cond
    ((eql state :keyup) (keyup instance (sdl2:scancode-value keysym)))
    ((eql state :keydown) (keydown instance (sdl2:scancode-value keysym)))
    (t nil)))


;;;; Run
(defparameter *demo* (make-instance 'cm))
