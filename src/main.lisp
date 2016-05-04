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


(defsketch cm (:width *width*
               :height *height*
               :debug :scancode-d)
    ((ready)
     (mouse)
     (p-from)
     (p-to)
     (p-c1)
     (p-c2)
     (ts)
     (l0-pen (make-pen :stroke (gray 0) :fill (rgb 0.0 0.0 0.0)))
     (l1-pen (make-pen :stroke (rgb 0 0 0.5) :fill (rgb 0.0 0.0 1.0)))
     (l2-pen (make-pen :stroke (rgb 0 0.5 0.0) :fill (rgb 0.0 1.0 0.0)))
     (lines-pen (make-pen :stroke (gray 0)))
     (final-pen (make-pen :stroke (rgb 0.5 0 0) :fill (rgb 1.0 0.0 0.0)))
     (fast-pen (make-pen :fill (rgb 0.0 0.0 1.0)))
     (results)
     (dots)
     )
  (with-fps
    (background (gray 1))
    ;;
    (when ready

      (incf ts 0.01)

      (let* ((n (abs (sin ts)))
             (i1 (vec-lerp p-from p-c1 n))
             (i2 (vec-lerp p-c1 p-c2 n))
             (i3 (vec-lerp p-c2 p-to n))
             (ii1 (vec-lerp i1 i2 n))
             (ii2 (vec-lerp i2 i3 n))
             (f (vec-lerp ii1 ii2 n))
             )
        (with-pen lines-pen
          (draw-line p-from p-c1)
          (draw-line p-c1 p-c2)
          (draw-line p-c2 p-to))
        (with-pen l0-pen
          (draw-circle p-from 10)
          (draw-circle p-to 10)
          (draw-square p-c1 6)
          (draw-square p-c2 6))
        (with-pen l1-pen
          (draw-line i1 i2)
          (draw-line i2 i3)
          (draw-circle i1 5)
          (draw-circle i2 5)
          (draw-circle i3 5))
        (with-pen l2-pen
          (draw-line ii1 ii2)
          (draw-circle ii1 3)
          (draw-circle ii2 3))
        (with-pen final-pen
          (bezier (vec-x p-from) (vec-y p-from)
                  (vec-x p-c1) (vec-y p-c1)
                  (vec-x p-c2) (vec-y p-c2)
                  (vec-x p-to) (vec-y p-to))
          (loop :for i :from 0.0 :to 1.0 :by 0.01
                :do (draw-point (cubic-bezier p-from p-to p-c1 p-c2 i)))
          (draw-circle f 5))

        )
      )


    ;;
    ))

(defun make-cm ()
  (make-sketch 'cm
    (mouse (make-vec))))


(defun reset (game)
  (setf (slot-value game 'ready) nil)
  (setf (slot-value game 'p-from)
        (make-vec 20 (random-around *center-y* 50))

        (slot-value game 'p-c1)
        (make-vec (* *width* 1/3) (random *height*))

        (slot-value game 'p-c2)
        (make-vec (* *width* 2/3) (random *height*))

        (slot-value game 'p-to)
        (make-vec (- *width* 20) (random-around *center-y* 50))

        (slot-value game 'ts) 0
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
