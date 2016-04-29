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
     (spring-point)
     (weight)
     (k)
     )
  (with-fps
    (background (gray 1))
    ;;

    (setf spring-point (make-vec (getf mouse :x) (getf mouse :y)))

    (when weight
      (let* ((distance (vec-sub spring-point
                                (particle-pos weight)))
             (force (vec-mul distance k)))
        (vec-add! (particle-vel weight) force))

      (particle-update! weight)
      (with-pen (make-pen :fill (gray 0 0.8))
        (draw-particle weight))

      (let ((sx (vec-x spring-point))
            (sy (vec-y spring-point))
            (wx (particle-x weight))
            (wy (particle-y weight)))
        (with-pen (make-pen :fill (gray 0))
          (circle sx sy 5))
        (unless (and (= sx wx)
                     (= sy wy))
          (with-pen (make-pen :stroke (gray 0))
            (line sx sy wx wy)))))

    ;;
    ))

(defun make-cm ()
  (make-sketch 'cm
    (mouse (list :x 0 :y 0))
    (spring-point (make-vec *center-x* *center-y*))
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
     (setf (slot-value instance 'k)
           (random 0.5)
           (slot-value instance 'weight)
           (make-particle (random *width*) (random *height*)
                          :speed (random-range 20.0 50.0)
                          :radius 15
                          :direction (random tau)
                          :friction (random 0.9))))))

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
