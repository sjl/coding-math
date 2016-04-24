(in-package #:coding-math.ballistics)

;;;; Config
(defparameter *width* 600)
(defparameter *height* 400)

(defparameter *center-x* (/ *width* 2))
(defparameter *center-y* (/ *height* 2))


;;;; Drawing
(defun draw-gun (gun)
  (in-context
    (translate (getf gun 'x) (getf gun 'y))
    (with-pen (make-pen :stroke (gray 0.0))
      (circle 0 0 25)
      (rotate (degrees (getf gun 'angle)))
      (rect 0 -8 40 16)
      )))

(defun draw-ball (ball)
  (with-pen (make-pen :stroke (gray 0.1) :fill (gray 0.6))
     (circle (particle-x ball) (particle-y ball) (particle-radius ball))))


;;;; Game
(defun aim (gun x y)
  (setf (getf gun 'angle)
        (clamp (- (/ tau 4))
               -0.3
               (atan (- y (getf gun 'y))
                     (- x (getf gun 'x))))))

(defun shoot (game)
  (force-output)
  (with-slots (gun cannonball can-shoot-p firedp) game
    (let ((angle (getf gun 'angle)))
      (setf
        can-shoot-p nil
        firedp t
        (particle-x cannonball) (+ (getf gun 'x) (* 40 (cos angle)))
        (particle-y cannonball) (+ (getf gun 'y) (* 40 (sin angle)))
        (particle-speed cannonball) 10
        (particle-direction cannonball) angle))))

(defun update-ball (game)
  (with-slots (cannonball firedp can-shoot-p) game
    (particle-update! cannonball)
    (when (> (- (particle-y cannonball)
                (particle-radius cannonball))
             *height*)
      (setf can-shoot-p t
            firedp nil))))

(defsketch game (:width *width*
                 :height *height*
                 :debug :scancode-d)
    ((frame)
     (aiming)
     (gun)
     (cannonball)
     (can-shoot-p)
     (firedp)
     )
  (background (gray 1))
  (incf frame)
  ;;
  (draw-gun gun)
  (draw-ball cannonball)
  (when firedp
    (update-ball sketch::sketch-window))
  ;;
  (when (zerop (mod frame 20))
    (calc-fps 20))
  (draw-fps))


(defun make-game ()
  (make-sketch 'game
    (frame 1)
    (aiming nil)
    (can-shoot-p t)
    (firedp nil)
    (gun `(x 40
           y ,*height*
           angle ,(- (/ tau 8))))
    (cannonball (make-particle (getf gun 'x)
                               (getf gun 'y)
                               :speed 15
                               :direction (getf gun 'angle)
                               :radius 7
                               :gravity 0.2))))


;;;; Mouse
(defmethod kit.sdl2:mousebutton-event
    ((game game) state timestamp button x y)
  (declare (ignore timestamp x y))
  (when (= 1 button)
    (case state
      (:mousebuttondown (setf (slot-value game 'aiming) t))
      (:mousebuttonup (setf (slot-value game 'aiming) nil)))))

(defmethod kit.sdl2:mousemotion-event
    ((game game) timestamp button-mask x y xrel yrel)
  (declare (ignore timestamp button-mask xrel yrel))
  (when (slot-value game 'aiming)
    (aim (slot-value game 'gun) x y)))


;;;; Keyboard
(defun keydown (game scancode)
  (declare (ignore game))
  (scancode-case scancode
    (:scancode-space
     nil)))

(defun keyup (game scancode)
  (scancode-case scancode
    (:scancode-space
     (when (can-shoot-p game)
       (shoot game)))))


(defmethod kit.sdl2:keyboard-event ((instance game) state timestamp repeatp keysym)
  (declare (ignore timestamp repeatp))
  (cond
    ((eql state :keyup) (keyup instance (sdl2:scancode-value keysym)))
    ((eql state :keydown) (keydown instance (sdl2:scancode-value keysym)))
    (t nil)))



;;;; Run
; (defparameter *demo* (make-game))
