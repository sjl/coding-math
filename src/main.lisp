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
     (particles (loop :repeat 30
                      :collect (make-particle center-x *height*
                                              :gravity 0.05
                                              :speed (random-range 1.0 6.0)
                                              :direction (random-around (* tau 3/4) (/ tau 30))
                                              :radius (random-around 5 3.0))))
     )
  (background (gray 1))
  (incf frame)
  ;;
  (with-pen (make-pen :stroke (gray 0) :fill (gray 0.5))
    (loop :for particle :in particles :do
          (draw-particle particle)
          (particle-update! particle)
          (when (> (particle-y particle)
                   (+ (particle-radius particle)
                      *height*))
            (setf (particle-x particle) center-x
                  (particle-y particle) *height*
                  (vec-magnitude (particle-vel particle)) (random-range 1.0 6.0)
                  (vec-angle (particle-vel particle)) (random-around (* tau 3/4) (/ tau 30))))))
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


; (defun keydown (instance scancode)
;   (scancode-case scancode
;     (:scancode-left (setf (slot-value instance 'turning-left) t))
;     (:scancode-right (setf (slot-value instance 'turning-right) t))
;     (:scancode-up (setf (slot-value instance 'thrusting) t))))

; (defun keyup (instance scancode)
;   (scancode-case scancode
;     (:scancode-left (setf (slot-value instance 'turning-left) nil))
;     (:scancode-right (setf (slot-value instance 'turning-right) nil))
;     (:scancode-up (setf (slot-value instance 'thrusting) nil))))


; (defmethod kit.sdl2:keyboard-event ((instance cm) state timestamp repeatp keysym)
;   (declare (ignore timestamp repeatp))
;   (cond
;     ((eql state :keyup) (keyup instance (sdl2:scancode-value keysym)))
;     ((eql state :keydown) (keydown instance (sdl2:scancode-value keysym)))
;     (t nil)))


;;;; Run
(defparameter *demo* (make-instance 'cm))
