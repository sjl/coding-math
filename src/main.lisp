(in-package #:coding-math)

(declaim (optimize (speed 0)
                   (safety 3)
                   (debug 3)))

;;;; Config
(defparameter *width* 600)
(defparameter *height* 400)

(defparameter *center-x* (/ *width* 2))
(defparameter *center-y* (/ *height* 2))


;;;; Sketch
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
     (p nil))
  (background (gray 1))
  (incf frame)
  ;;
  (with-pen (make-pen :stroke (gray 0.3)
                      :fill (if (> (distance mx my *center-x* *center-y*) 100)
                              (gray 1)
                              (gray 0.5)))
    (circle *center-x* *center-y* 100))
  ;;
  (when (zerop (mod frame 20))
    (calc-fps 20))
  (draw-fps))


;;;; Mouse
(defmethod kit.sdl2:mousemotion-event ((window cm) ts b x y xrel yrel)
  (declare (ignore ts b xrel yrel))
  (with-slots (mx my rect-x rect-y rect-w rect-h cx cy cr) window
    (setf mx x)
    (setf my y)
    ;;
    ;;
    ))


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
    (:scancode-space
     (setf (slot-value instance 'p)
           (make-particle *center-x* *center-y*
                          :speed 4
                          :radius 10
                          :friction 0.008
                          :direction (random tau))))))

(defun keyup (instance scancode)
  (scancode-case scancode
    (:scancode-space
     nil
     )
    ))


(defmethod kit.sdl2:keyboard-event ((instance cm) state timestamp repeatp keysym)
  (declare (ignore timestamp repeatp))
  (cond
    ((eql state :keyup) (keyup instance (sdl2:scancode-value keysym)))
    ((eql state :keydown) (keydown instance (sdl2:scancode-value keysym)))
    (t nil)))


;;;; Run
; (defparameter *demo* (make-instance 'cm))
