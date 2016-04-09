(defpackage #:coding-math.utils
  (:use #:cl #:coding-math.quickutils)
  (:export
    #:dividesp))

(defpackage #:coding-math
  (:use #:cl
        #:sketch
        #:coding-math.quickutils
        #:coding-math.utils))
