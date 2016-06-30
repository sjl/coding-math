(in-package #:coding-math.tween)

;;;; Utils
(declaim (inline get-seconds-real-time))
(defun get-seconds-real-time ()
  (/ (get-internal-real-time) internal-time-units-per-second))


;;;; Framework
(defvar *active-tweens* nil)
(defvar *callbacks* nil)

(defmacro tween-place!  (place target duration tweening-function
                         &key callback-progress callback-finished
                         &environment env)
  "Tween `place` to `target` over `duration` seconds with `tweening-function`"
  (multiple-value-bind (temp-vars temp-vals stores store-expr access-expr)
      (get-setf-expansion place env)
    (once-only (duration callback-progress callback-finished)
      (with-gensyms (start-time start-value change time finished)
        `(let* (,@(mapcar #'list temp-vars temp-vals)
                (,start-value ,access-expr)
                (,start-time (get-seconds-real-time))
                (,change (- ,target ,start-value)))
          (push
            (lambda ()
              (let* ((,time (- (get-seconds-real-time) ,start-time))
                     (,finished (> ,time ,duration))
                     (,(car stores)
                      (funcall ,tweening-function
                               ,start-value ,change ,duration ,time)))
                ,store-expr
                (when ,callback-progress
                  (push ,callback-progress *callbacks*))
                (when (and ,finished ,callback-finished)
                  (push ,callback-finished *callbacks*))
                ,finished))
            *active-tweens*))))))

(defmacro tween-places!
    ((tweening-function duration &key callback-progress callback-finished)
     &rest places-and-targets)
  "Tween `places-and-targets` over `duration` seconds with `tweening-function`

      (tween-places! (#'tween-quadratic-in 1.5)
        (vec-x location) (find-target :x)
        (vec-y location) (find-target :y)
        (color-alpha (slot-value shape 'color)) 0.0
        ;...
        )

  "
  (once-only (duration tweening-function)
    `(progn
      ,@(loop :for (place target . remaining) :on places-and-targets :by #'cddr
              :collect
              `(tween-place! ,place ,target ,duration ,tweening-function
                ,@(when (null remaining)
                    `(:callback-progress ,callback-progress
                      :callback-finished ,callback-finished)))))))

(defun update-tweens! ()
  (setf *active-tweens* (remove-if #'funcall *active-tweens*)
        *callbacks* (map nil #'funcall *callbacks*)))


;;;; Tweening Functions
(defun tween-linear (start amount duration time)
  (let ((time (/ time duration)))
    (+ start (* amount time))))

(defun tween-quadratic-in (start amount duration time)
  (let ((time (/ time duration)))
    (+ start (* amount (* time time)))))

(defun tween-quadratic-out (start amount duration time)
  (let ((time (/ time duration)))
    (+ start (* (- amount) (* time (- time 2))))))
