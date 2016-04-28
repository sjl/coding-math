(in-package #:coding-math.fps)

;;;; FPS
(defvar *last-draw* 0)
(defvar *fps* 0.0)
(defvar *mspf* 0.0)
(defvar *frame* 0)


(defparameter *rolling-average* 0.0)
(defparameter *rolling-average-count* 10)


(defun update-average (frame-time)
  (setf *rolling-average*
        (/ (+ frame-time
              (* *rolling-average* *rolling-average-count*))
           (1+ *rolling-average-count*))))

(defun update-fps ()
  (setf *mspf* (* 1000.0
                  (/ *rolling-average*
                     internal-time-units-per-second))
        *fps* (/ 1000.0 *mspf*)))

(defun draw-fps ()
  (text (format nil "MSPF: ~,2F" *mspf*) 0 0)
  (text (format nil "PFPS: ~,2F" *fps*) 0 20))


(defmacro with-fps (&body body)
  (let ((start (gensym "start")))
    `(let ((,start (get-internal-real-time)))
      ,@body
      (update-average (- (get-internal-real-time) ,start))
      (draw-fps)
      (incf *frame*)
      (when (dividesp *frame* 15)
        (update-fps)))))
