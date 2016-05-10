;;;; Generic stuff
(defpackage #:coding-math.utils
  (:use
    #:cl
    #:sketch
    #:coding-math.quickutils)
  (:export
    #:in-context
    #:make-sketch
    #:scancode-case
    #:with-vals
    #:zap%
    #:%
    ))

(defpackage #:coding-math.math
  (:use
    #:cl
    #:coding-math.quickutils
    #:coding-math.utils)
  (:export
    #:tau
    #:mulf
    #:dividesp
    #:square
    #:distance
    #:random-range
    #:random-around
    #:random-dist
    #:norm
    #:lerp
    #:precise-lerp
    #:map-range
    #:clamp
    #:wrap-zero
    #:wrap-range
    #:wrapf
    #:outsidep
    #:insidep
    #:round-to-places
    #:round-to-nearest
    #:ranges-overlap-p
    ))

(defpackage #:coding-math.fps
  (:use
    #:cl
    #:sketch
    #:coding-math.quickutils
    #:coding-math.math
    #:coding-math.utils)
  (:export
    #:with-fps
    #:draw-fps))


;;;; 2D stuff
(defpackage #:coding-math.2d.vectors
  (:use
    #:cl
    #:coding-math.math
    #:coding-math.quickutils
    #:coding-math.utils)
  (:export
    #:vec
    #:vec-x
    #:vec-y
    #:make-vec
    #:make-vec-md
    #:make-vec-ma
    #:make-random-vec
    #:vec-magnitude
    #:vec-direction
    #:vec-angle
    #:vec-add
    #:vec-sub
    #:vec-mul
    #:vec-div
    #:vec-lerp
    #:vec-add!
    #:vec-sub!
    #:vec-mul!
    #:vec-div!
    #:vec-to-string
    #:with-vec
    #:with-vecs
    ))

(defpackage #:coding-math.2d.hitboxes
  (:use
    #:cl
    #:sketch
    #:coding-math.2d.vectors
    #:coding-math.math
    #:coding-math.quickutils
    #:coding-math.utils)
  (:export
    #:hitbox-x
    #:hitbox-y
    #:hitbox-radius
    #:hitbox-width
    #:hitbox-height
    #:drag-requested-p
    #:drag-location-vec
    #:circles-collide-p
    #:circle-point-collide-p
    #:rect-point-collide-p
    #:rects-collide-p))

(defpackage #:coding-math.2d.particles
  (:use
    #:cl
    #:coding-math.math
    #:coding-math.2d.vectors
    #:coding-math.2d.hitboxes
    #:coding-math.quickutils
    #:coding-math.utils)
  (:export
    #:particle
    #:particle-vel
    #:particle-pos
    #:particle-grv
    #:particle-radius
    #:particle-mass
    #:particle-friction
    #:particle-speed
    #:particle-direction
    #:make-particle
    #:particle-x
    #:particle-y
    #:particle-wrap!
    #:particle-update!
    #:particle-accelerate!
    #:particle-angle-to
    #:particle-distance-to
    #:particle-gravitate-to!
    #:particle-gravitate-add!
    #:particle-gravitate-remove!
    #:particle-spring-to!
    #:particle-spring-add!
    #:particle-spring-remove!))

(defpackage #:coding-math.2d.points
  (:use
    #:cl
    #:sketch
    #:coding-math.math
    #:coding-math.2d.vectors
    #:coding-math.quickutils
    #:coding-math.utils)
  (:export
    #:quadratic-bezier
    #:fast-quadratic-bezier
    #:cubic-bezier
    #:quadratic-bezier-curve
    #:draw-function
    #:multicurve
    ))


(defpackage #:coding-math.2d.demo
  (:use
    #:cl
    #:sketch
    #:coding-math.quickutils
    #:coding-math.utils
    #:coding-math.fps
    #:coding-math.math
    #:coding-math.2d.vectors
    #:coding-math.2d.points
    #:coding-math.2d.hitboxes
    #:coding-math.2d.particles))

(defpackage #:coding-math.2d.ballistics
  (:use
    #:cl
    #:sketch
    #:coding-math.quickutils
    #:coding-math.2d.particles
    #:coding-math.2d.hitboxes
    #:coding-math.utils
    #:coding-math.math
    #:coding-math.fps))


;;;; 3D stuff
(defpackage #:coding-math.3d.vectors
  (:use
    #:cl
    #:coding-math.math
    #:coding-math.utils
    #:coding-math.quickutils)
  (:export
    #:vec3
    #:vec3-x
    #:vec3-y
    #:make-vec3
    #:make-random-vec3
    #:vec3-magnitude
    #:vec3-add #:vec3-add!
    #:vec3-sub #:vec3-sub!
    #:vec3-mul #:vec3-mul!
    #:vec3-div #:vec3-div!
    #:vec3-lerp
    #:with-vec3
    #:with-vec3s))

(defpackage #:coding-math.3d.demo
  (:use
    #:cl
    #:sketch
    #:coding-math.quickutils
    #:coding-math.utils
    #:coding-math.fps
    #:coding-math.math
    #:coding-math.3d.vectors
    ))

