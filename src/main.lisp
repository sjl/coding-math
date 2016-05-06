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

(defun draw-line (p1 p2)
  (with-vecs ((x1 y1) p1 (x2 y2) p2)
    (line x1 y1 x2 y2)))

(defun draw-circle (p radius)
  (circle (vec-x p) (vec-y p) radius))

(defun draw-square (p radius)
  (rect (- (vec-x p) radius)
        (- (vec-y p) radius)
        (* 2 radius)
        (* 2 radius)))

(defun draw-point (p)
  (point (vec-x p) (vec-y p)))

(defun oob-p (p &optional (r 0.0))
  (or (outsidep (- 0 r) (+ *width* r) (vec-x p))
      (outsidep (- 0 r) (+ *height* r) (vec-y p))))


(defsketch cm (:width *width*
               :height *height*
               :debug :scancode-d)
    ((ready)
     (mouse)
     (particles)
     (pen (make-pen :fill (gray 0.2)))
     )
  (with-fps
    (background (gray 1))
    ;;
    (when ready

      (with-pen pen
        (loop :for p :in particles :do
              (particle-update! p)
              (if (oob-p (particle-pos p))
                (setf particles (remove p particles))
                (draw-circle (particle-pos p) 3)))
        )

      )

    ;;
    ))

(defun make-cm ()
  (make-sketch 'cm
    (mouse (make-vec))))


(defun reset (game)
  (setf (slot-value game 'ready) nil)
  (setf (slot-value game 'particles)
        (loop :repeat 200
              :collect (make-particle *center-x*
                                      *center-y*
                                      :speed (random 2.0)
                                      :direction (random tau)
                                      )


              )

        )
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
