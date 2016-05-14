(in-package #:coding-math.2d.demo)

;;;; Config
(defparameter *width* 600)
(defparameter *height* 400)

(defparameter *center-x* (/ *width* 2))
(defparameter *center-y* (/ *height* 2))


;;;; Sketch
(defun draw-particle (p)
  (circle (particle-x p) (particle-y p) (particle-radius p)))

(defun draw-line (p1 p2)
  (with-vecs ((x1 y1) p1 (x2 y2) p2)
    (line x1 y1 x2 y2)))

(defun draw-circle (p &optional (radius 5))
  (if (listp p)
    (circle (getf p :x) (getf p :y) (or (getf p :radius) radius))
    (circle (vec-x p) (vec-y p) radius)))

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


(defsketch cm
    ((mouse (make-vec 0 0))
     (width *width*)
     (height *height*)
     (dragging)
     (p1 (make-particle (random *width*) (random *height*) :radius 10))
     (c1 (make-particle (random *width*) (random *height*) :radius 8))
     (c2 (make-particle (random *width*) (random *height*) :radius 8))
     (p2 (make-particle (random *width*) (random *height*) :radius 10))
     (handles (list p1 c1 c2 p2))
     (control-pen (make-pen :stroke (gray 0.1)
                            :weight 1
                            :fill (rgb 0.5 0.5 0.9)))
     (end-pen (make-pen :stroke (gray 0.1)
                        :weight 1
                        :fill (gray 0.5)))
     (line-pen (make-pen :stroke (gray 0.7)))
     (curve-pen (make-pen :stroke (rgb 0.7 0.2 0.2)))
     )
  (with-fps
    (background (gray 1))
    ;;
    (with-pen line-pen
      (draw-line (particle-pos p1)
                 (particle-pos c1))
      (draw-line (particle-pos c1)
                 (particle-pos c2))
      (draw-line (particle-pos c2)
                 (particle-pos p2)))
    (with-pen end-pen
      (draw-particle p1)
      (draw-particle p2))
    (with-pen control-pen
      (draw-particle c1)
      (draw-particle c2))
    (with-pen curve-pen
      (with-vecs ((p1x p1y) (particle-pos p1)
                  (c1x c1y) (particle-pos c1)
                  (c2x c2y) (particle-pos c2)
                  (p2x p2y) (particle-pos p2))
        (bezier p1x p1y c1x c1y c2x c2y p2x p2y))
      )
    ;;
    ))


;;;; Mouse
(defun mousemove (instance x y)
  (with-slots (dragging mouse) instance
    (setf (vec-x mouse) x)
    (setf (vec-y mouse) y)
    ;;
    (when dragging
      (destructuring-bind (thing . offset) dragging
        (setf (drag-location-vec thing)
              (vec-add mouse offset)))
      )
    ;;
    )
  )

(defun mousedown-left (instance x y)
  (declare (ignorable instance x y))
  (with-slots (dragging mouse handles) instance
    (loop :for handle :in handles
          :when (drag-requested-p handle (make-vec x y))
          :do (setf dragging
                    (cons handle
                          (vec-sub (drag-location-vec handle)
                                   mouse)))))
  )

(defun mousedown-right (instance x y)
  (declare (ignorable instance x y))
  )

(defun mouseup-left (instance x y)
  (declare (ignorable instance x y))
  (setf (slot-value instance 'dragging) nil)
  )

(defun mouseup-right (instance x y)
  (declare (ignorable instance x y))
  )


(defmethod kit.sdl2:mousemotion-event ((window cm) ts b x y xrel yrel)
  (declare (ignore ts b xrel yrel))
  (mousemove window x y))

(defmethod kit.sdl2:mousebutton-event ((window cm) state ts button x y)
  (declare (ignore ts))
  (funcall (case state
             (:mousebuttondown
              (case button
                (1 #'mousedown-left)
                (3 #'mousedown-right)))
             (:mousebuttonup
              (case button
                (1 #'mouseup-left)
                (3 #'mouseup-right))))
           window x y))


;;;; Keyboard
(defun keydown (instance scancode)
  (declare (ignorable instance))
  (scancode-case scancode
    (:scancode-space (sketch::prepare instance))))

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
; (defparameter *demo* (make-instance 'cm))

