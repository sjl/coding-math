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

(defun draw-rope (p0 p1)
  (line (particle-x p0)
        (particle-y p0)
        (particle-x p1)
        (particle-y p1)))


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


(defun random-particle ()
  (make-particle (random *width*) (random *height*)
                 :speed (random-range 10.0 60.0)
                 :radius 20
                 :friction (random-range 0.01 0.1)
                 :gravity (random 0.5)
                 :direction (random tau)))

(defun generate-particle-graph ()
  (let ((n (random-range 5 15)))
    (values (coerce (loop :repeat n :collect (random-particle))
                    'vector)
            (remove-duplicates
              (append
                (loop :for i :from 0 :below (1- n)
                      :collect (cons i (1+ i)))
                (loop :repeat (/ n 2)
                      :for (a b) = (sort (list (random n) (random n)) #'<)
                      :unless (= a b)
                      :collect (cons a b)))
              :test #'equal))))


(defsketch cm (:width *width*
               :height *height*
               :debug :scancode-d)
    ((mouse)
     (k)
     (separation)
     (particles)
     (connections)
     )
  (with-fps
    (background (gray 1))
    ;;

    (flet
        ((map-particles (fn)
           (map 'list fn particles))
         (map-connections (fn)
           (mapc (lambda (edge)
                   (funcall fn
                            (aref particles (car edge))
                            (aref particles (cdr edge))))
                 connections)))

      (when particles
        (map-connections (lambda (a b) (spring a b separation k)))

        (map-particles #'bounce-particle)
        (map-particles #'particle-update!)

        (with-pen (make-pen :fill (gray 0 0.8))
          (map-particles #'draw-particle))

        (with-pen (make-pen :stroke (gray 0))
          (map-connections #'draw-rope))))

    ;;
    ))

(defun make-cm ()
  (make-sketch 'cm
    (mouse (list :x 0 :y 0))
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
     (multiple-value-bind (nodes edges)
         (generate-particle-graph)
       (setf (slot-value instance 'particles) nodes
             (slot-value instance 'connections) edges)))))

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
