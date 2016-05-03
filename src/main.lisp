(in-package #:coding-math)

;;;; Config
(defparameter *width* 600)
(defparameter *height* 400)

(defparameter *center-x* (/ *width* 2))
(defparameter *center-y* (/ *height* 2))


;;;; Sketch
(defun draw-particle (p pen)
  (with-pen pen
    (circle (particle-x p) (particle-y p) (particle-radius p))))


(defsketch cm (:width *width*
               :height *height*
               :debug :scancode-d)
    ((ready)
     (mouse)
     (sp)
     (p)
     (earth)
     (sun)
     (emitter)
     (ps)
     (particle-pen (make-pen :stroke (gray 0.5) :fill (gray 0.8)))
     (sp-pen (make-pen :stroke (gray 0.0) :fill (gray 0.0)))
     (rope-pen (make-pen :stroke (gray 0.0)))
     (sun-pen (make-pen :fill (rgb 1.0 1.0 0.0) :stroke (gray 0)))
     (earth-pen (make-pen :fill (rgb 0.0 1.0 0.0) :stroke (gray 0)))
     (p-pen (make-pen :fill (rgb 1.0 0.0 0.0)))
     )
  (with-fps
    (background (gray 1))
    ;;
    (when ready

      (particle-update! p)
      (mapcar #'particle-update! ps)
      (draw-particle sun sun-pen)
      (draw-particle earth earth-pen)
      (draw-particle emitter sp-pen)
      (loop :for p :in ps :do (draw-particle p p-pen))

      (draw-particle p particle-pen)
      (with-pen rope-pen
        (line (particle-x p) (particle-y p) (vec-x sp) (vec-y sp))
        (line (particle-x p) (particle-y p) (vec-x mouse) (vec-y mouse)))
      (with-pen sp-pen
        (circle (vec-x sp) (vec-y sp) 3)
        (circle (vec-x mouse) (vec-y mouse) 3))

      )

    ;;
    ))

(defun make-cm ()
  (make-sketch 'cm
    (mouse (make-vec))))


(defun reset (game)
  (setf (slot-value game 'ready) nil)
  (setf (slot-value game 'sp)
        (make-vec (random *width*)
                  (random *height*))
        (slot-value game 'p)
        (make-particle (random *width*)
                       (random *height*)
                       :radius 10
                       :friction (random-range 0.1 0.2)
                       :gravity (random-range 0.1 0.5))
        (slot-value game 'sun) (make-particle *center-x* *center-y*
                                              :mass 500.0
                                              :radius 20)
        (slot-value game 'earth) (make-particle 320 80
                                                :mass 100.0
                                                :radius 8)
        (slot-value game 'emitter) (make-particle 100 100 :radius 5)
        (slot-value game 'ps) nil
        )
  (particle-spring-add! (slot-value game 'p)
                        (slot-value game 'mouse)
                        0.05
                        80)
  (particle-spring-add! (slot-value game 'p)
                        (slot-value game 'sp)
                        0.05
                        80)
  (loop :repeat 300
        :for p = (make-particle 100 100
                                :speed (random 3.0)
                                :direction (random tau)
                                :radius 3)
        :do
        (push p (slot-value game 'ps))
        (particle-gravitate-add! p (slot-value game 'sun))
        (particle-gravitate-add! p (slot-value game 'earth)))
  (setf (slot-value game 'ready) t))


;;;; Mouse
(defmethod kit.sdl2:mousemotion-event ((window cm) ts b x y xrel yrel)
  (declare (ignore ts b xrel yrel))
  (with-slots (mouse) window
    (setf (vec-x mouse) x)
    (setf (vec-y mouse) y)
    ;;
    ;;
    ))


;;;; Keyboard
(defun keydown (instance scancode)
  (declare (ignorable instance))
  (scancode-case scancode
    (:scancode-space (reset instance))))

(defun keyup (instance scancode)
  (declare (ignorable instance))
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
