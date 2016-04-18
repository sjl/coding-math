(defpackage #:coding-math.utils
  (:use #:cl #:coding-math.quickutils)
  (:export
    #:mulf
    #:dividesp))

(defpackage #:coding-math.math
  (:use #:cl
        #:coding-math.quickutils
        #:coding-math.utils)
  (:export
    #:tau
    #:random-range
    #:random-around
    #:norm
    #:lerp
    #:precise-lerp
    #:map-range
    #:clamp
    #:wrap-zero
    #:wrap-range
    #:outside-p))

(defpackage #:coding-math.vectors
  (:use #:cl
        #:coding-math.quickutils
        #:coding-math.utils)
  (:export
    #:vec
    #:vec-x
    #:vec-y
    #:make-vec
    #:make-vec-md
    #:make-vec-ma
    #:vec-magnitude
    #:vec-direction
    #:vec-angle
    #:vec-add
    #:vec-sub
    #:vec-mul
    #:vec-div
    #:vec-add!
    #:vec-sub!
    #:vec-mul!
    #:vec-div!
    #:vec-to-string))

(defpackage #:coding-math.particles
  (:use #:cl
        #:coding-math.vectors
        #:coding-math.quickutils
        #:coding-math.utils)
  (:export
    #:particle
    #:particle-vel
    #:particle-pos
    #:particle-grv
    #:particle-radius
    #:particle-mass
    #:make-particle
    #:particle-x
    #:particle-y
    #:particle-wrap!
    #:particle-update!
    #:particle-accelerate!
    #:particle-angle-to
    #:particle-distance-to
    #:particle-gravitate-to!))

(defpackage #:coding-math
  (:use #:cl
        #:sketch
        #:coding-math.quickutils
        #:coding-math.utils
        #:coding-math.math
        #:coding-math.vectors
        #:coding-math.particles
        ))

(defpackage #:coding-math.ballistics
  (:use #:cl
        #:sketch
        #:coding-math.quickutils
        #:coding-math.utils))
