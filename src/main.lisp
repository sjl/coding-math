(in-package #:coding-math)

(declaim (optimize (speed 1)
                   (safety 1)
                   (debug 1)))

;;;; Config
(defparameter *width* 600)
(defparameter *height* 400)

(defparameter *center-x* (/ *width* 2))
(defparameter *center-y* (/ *height* 2))


;;;; Sketch
(defun particle-oob-p (particle)
  (let ((r (particle-radius particle)))
    (or (outsidep (- 0 r)
                  (+ *width* r)
                  (particle-x particle))
        (outsidep (- 0 r)
                  (+ *height* r)
                  (particle-y particle)))))


(defun draw-rect (r)
  (rect (getf r :x)
        (getf r :y)
        (getf r :width)
        (getf r :height)))

(defun draw-circle (c)
  (circle (getf c :x)
          (getf c :y)
          (getf c :radius)))

(defun draw-particle (particle)
  (circle (particle-x particle)
          (particle-y particle)
          (particle-radius particle)))


(defsketch cm (:width *width*
               :height *height*
               :debug :scancode-d)
    ((mouse)
     (frame)
     (r)
     (mr)
     )
  (background (gray 1))
  (incf frame)
  ;;

  (setf (getf mr :x) (getf mouse :x))
  (setf (getf mr :y) (getf mouse :y))

  (with-pen (make-pen :stroke (gray 0.5)
                      :fill (cond
                              ((rects-collide-p r mr)
                               (rgb 0 0 0.7 0.5))
                              (t (gray 0.9))))
    (draw-rect r)
    (draw-rect mr))

  ;;
  (when (zerop (mod frame 20))
    (calc-fps 20))
  (draw-fps))

(defun make-cm ()
  (make-sketch 'cm
    (mouse (list :x 0 :y 0))
    (frame 1)
    (r (list :x 20 :y 50 :width (- *width* 40) :height 30))
    (mr (list :x 300 :y 300 :width 90 :height 90))
    ))


;;;; Mouse
(defmethod kit.sdl2:mousemotion-event ((window cm) ts b x y xrel yrel)
  (declare (ignore ts b xrel yrel))
  (with-slots (mouse) window
    (setf (getf mouse :x) x)
    (setf (getf mouse :y) y)
    ;;
    ;;
    ))


;;;; Keyboard
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
  (declare (ignore instance))
  (scancode-case scancode
    (:scancode-space
     nil)))


(defmethod kit.sdl2:keyboard-event ((instance cm) state timestamp repeatp keysym)
  (declare (ignore timestamp repeatp))
  (cond
    ((eql state :keyup) (keyup instance (sdl2:scancode-value keysym)))
    ((eql state :keydown) (keydown instance (sdl2:scancode-value keysym)))
    (t nil)))


;;;; Run
; (defparameter *demo* (make-cm))
