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


(defun bounce-particle (particle)
  (with-accessors ((x particle-x)
                   (y particle-y)
                   (r particle-radius))
      particle
    (when (outsidep r (- *width* r) x)
      (setf x (clamp r (- *width* r) x))
      (mulf (vec-x (particle-vel particle)) -0.9))
    (when (outsidep r (- *height* r) y)
      (setf y (clamp r (- *height* r) y))
      (mulf (vec-y (particle-vel particle)) -0.9))))

(defun spring (pa pb separation k)
  (let ((distance (vec-sub (particle-pos pa)
                           (particle-pos pb))))
    (decf (vec-magnitude distance) separation)
    (let ((force (vec-mul distance k)))
      (vec-add! (particle-vel pb) force)
      (vec-sub! (particle-vel pa) force))))


(defsketch cm (:width *width*
               :height *height*
               :debug :scancode-d)
    ((mouse)
     (k)
     (separation)
     (particle-a)
     (particle-b)
     (particle-c)
     )
  (with-fps
    (background (gray 1))
    ;;

    (when particle-a
      (spring particle-a particle-b separation k)
      (spring particle-b particle-c separation k)
      (spring particle-c particle-a separation k)

      (bounce-particle particle-a)
      (bounce-particle particle-b)
      (bounce-particle particle-c)

      (particle-update! particle-a)
      (particle-update! particle-b)
      (particle-update! particle-c)

      (with-pen (make-pen :fill (gray 0 0.8))
        (draw-particle particle-a)
        (draw-particle particle-b)
        (draw-particle particle-c))
      (with-pen (make-pen :stroke (gray 0))
        (line (particle-x particle-a)
              (particle-y particle-a)
              (particle-x particle-b)
              (particle-y particle-b))
        (line (particle-x particle-b)
              (particle-y particle-b)
              (particle-x particle-c)
              (particle-y particle-c))
        (line (particle-x particle-c)
              (particle-y particle-c)
              (particle-x particle-a)
              (particle-y particle-a)))
      )

    ;;
    ))

(defun make-cm ()
  (make-sketch 'cm
    (mouse (list :x 0 :y 0))
    (spring-length 100)
    (k 0.01)
    (separation 100)
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
     (setf (slot-value instance 'particle-a)
           (make-particle (random *width*) (random *height*)
                          :speed (random-range 10.0 60.0)
                          :radius 20
                          :friction 0.1
                          :gravity 0.3
                          :direction (random tau))
           (slot-value instance 'particle-b)
           (make-particle (random *width*) (random *height*)
                          :speed (random-range 10.0 60.0)
                          :radius 20
                          :friction 0.1
                          :gravity 0.3
                          :direction (random tau))
           (slot-value instance 'particle-c)
           (make-particle (random *width*) (random *height*)
                          :speed (random-range 10.0 60.0)
                          :radius 20
                          :friction 0.1
                          :gravity 0.3
                          :direction (random tau))
           ))))

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
