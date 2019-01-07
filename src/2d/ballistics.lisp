(in-package #:coding-math.2d.ballistics)

;;;; Config
(defparameter *demo* nil)
(defparameter *width* 600)
(defparameter *height* 400)

(defparameter *center-x* (/ *width* 2))
(defparameter *center-y* (/ *height* 2))


;;;; Drawing
(defparameter *gun-pen* (make-pen :stroke (gray 0.0) :fill (gray 0.0)))
(defparameter *ball-pen* (make-pen :stroke (gray 0.1) :fill (gray 0.6)))
(defparameter *force-bg-pen* (make-pen :fill (gray 0.6)))
(defparameter *target-pen* (make-pen :stroke (rgb 0.6 0 0) :weight 2 :fill (rgb 1.0 0 0)))
(defparameter *force-fg-pen* (make-pen :fill (rgb 1.000 0.478 0.749)))


(defun draw-gun (gun)
  (in-context
    (translate (getf gun 'x) (getf gun 'y))
    (with-pen *gun-pen*
      (circle 0 0 25)
      (rotate (degrees (getf gun 'angle)))
      (rect 0 -8 40 16))))

(defun draw-ball (ball)
  (with-pen *ball-pen*
     (circle (particle-x ball) (particle-y ball) (particle-radius ball))))

(defun draw-force (force)
  (with-pen *force-bg-pen*
    (circle 20 (- *height* 50) 15))
  (with-pen *force-fg-pen*
    (circle 20
            (- *height* 50)
            (losh:map-range -1.0 1.0 0 15 force))))

(defun draw-target (target)
  (when target
    (with-pen *target-pen*
      (circle (getf target :x)
              (getf target :y)
              (getf target :radius)))))


;;;; Game
(defun aim (gun x y)
  (setf (getf gun 'angle)
        (losh:clamp (- (/ losh:tau 4))
                    -0.3
                    (atan (- y (getf gun 'y))
                          (- x (getf gun 'x))))))

(defun shoot (game)
  (force-output)
  (with-slots (gun cannonball firedp raw-force) game
    (let ((angle (getf gun 'angle)))
      (setf
        firedp t
        (particle-x cannonball) (+ (getf gun 'x) (* 40 (cos angle)))
        (particle-y cannonball) (+ (getf gun 'y) (* 40 (sin angle)))
        (particle-speed cannonball) (losh:map-range -1.0 1.0 2 20.0 raw-force)
        (particle-direction cannonball) angle))))

(defun update-ball (game)
  (with-slots (cannonball firedp) game
    (particle-update! cannonball)
    (when (> (- (particle-y cannonball)
                (particle-radius cannonball))
             *height*)
      (setf firedp nil))))

(defun check-target (game)
  (when (and (game-target game)
             (circles-collide-p (game-cannonball game)
                                (game-target game)))
    (setf (game-win game) t)))

(defun random-target ()
  (list :x (random-range 200 *width*)
        :y *height*
        :radius (random-range 10 40)))


(defsketch game
    ((width *width*)
     (height *height*)
     (aiming)
     (gun)
     (cannonball)
     (can-shoot-p)
     (firedp)
     (force-speed 0.05)
     (force-angle 0.0)
     (raw-force)
     (target)
     (win)
     )
  (with-fps
    (background (gray 1))
    ;;
    (when (not firedp)
      (incf force-angle force-speed)
      (setf raw-force (sin force-angle)))

    (when (not target)
      (setf target (random-target)))

    (draw-ball cannonball)
    (draw-gun gun)
    (draw-force raw-force)
    (draw-target target)

    (when (and *demo* firedp)
      (update-ball *demo*)
      (check-target *demo*))
    (when win
      (text "You win!" *center-x* *center-y*))

    ;;
    ))


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
     (when (not (game-firedp game))
       (shoot game)))))


(defmethod kit.sdl2:keyboard-event ((instance game) state timestamp repeatp keysym)
  (declare (ignore timestamp repeatp))
  (cond
    ((eql state :keyup) (keyup instance (sdl2:scancode-value keysym)))
    ((eql state :keydown) (keydown instance (sdl2:scancode-value keysym)))
    (t nil)))



;;;; Run
;; (defparameter *demo* (make-instance 'game))
