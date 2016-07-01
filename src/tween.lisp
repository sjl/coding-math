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
(defmacro with-normalized-time ((time-symbol duration-form) &body body)
  `(let ((,time-symbol (/ ,time-symbol ,duration-form)))
    ,@body))

(defmacro tween-inout% (start amount duration time in out)
  (once-only (start amount duration)
    (with-gensyms (half)
      `(with-normalized-time (,time (/ ,duration 2.0))
        (let ((,half (/ ,amount 2.0)))
          (if (< ,time 1.0)
            (,in ,start ,half 1.0 ,time)
            (,out (+ ,start ,half) ,half 1.0 (1- ,time))))))))

(defun tween-linear (start amount duration time)
  (with-normalized-time (time duration)
    (+ start (* amount time))))


(defun tween-quadratic-in (start amount duration time)
  (with-normalized-time (time duration)
    (+ start (* amount (* time time)))))

(defun tween-quadratic-out (start amount duration time)
  (with-normalized-time (time duration)
    (+ start (* (- amount) (* time (- time 2))))))

(defun tween-quadratic-inout (start amount duration time)
  (tween-inout% start amount duration time
                tween-quadratic-in tween-quadratic-out))


(defun tween-cubic-in (start amount duration time)
  (with-normalized-time (time duration)
    (+ start (* amount (expt time 3)))))

(defun tween-cubic-out (start amount duration time)
  (with-normalized-time (time duration)
    (+ start (* amount (1+ (expt (1- time) 3))))))

(defun tween-cubic-inout (start amount duration time)
  (tween-inout% start amount duration time
                tween-cubic-in tween-cubic-out))


(defun tween-quartic-in (start amount duration time)
  (with-normalized-time (time duration)
    (+ start (* amount (expt time 4)))))

(defun tween-quartic-out (start amount duration time)
  (with-normalized-time (time duration)
    (+ start (* (- amount) (1- (expt (1- time) 4))))))

(defun tween-quartic-inout (start amount duration time)
  (tween-inout% start amount duration time
                tween-quartic-in tween-quartic-out))


(defun tween-quintic-in (start amount duration time)
  (with-normalized-time (time duration)
    (+ start (* amount (expt time 5)))))

(defun tween-quintic-out (start amount duration time)
  (with-normalized-time (time duration)
    (+ start (* amount (1+ (expt (1- time) 5))))))

(defun tween-quintic-inout (start amount duration time)
  (tween-inout% start amount duration time
                tween-quintic-in tween-quintic-out))

