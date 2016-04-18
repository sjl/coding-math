(in-package #:coding-math.fps)

;;;; FPS
(defvar *last-draw* 0)
(defvar *fps* 0.0)
(defvar *mspf* 0.0)


(defun calc-fps (frames)
  (let* ((current-draw (get-internal-real-time))
         (elapsed (float (/ (- current-draw *last-draw*)
                            internal-time-units-per-second))))
    (setf *last-draw* current-draw)
    (setf *mspf* (* 1000 (/ elapsed frames)))
    (setf *fps* (* frames (/ 1 elapsed)))))

(defun draw-fps ()
  (text (format nil "MSPF: ~,1F" *mspf*) 0 0)
  (text (format nil "FPS: ~,1F" *fps*) 0 20))


