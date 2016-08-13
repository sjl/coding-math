;;;; Generic stuff
(defpackage #:coding-math.utils
  (:use
    #:cl
    #:losh
    #:sketch
    #:iterate
    #:coding-math.quickutils)
  (:shadowing-import-from #:iterate
    #:in)
  (:export
    #:in-context
    #:scancode-case
    #:with-vals
    #:setf-slots
    #:symbolicate
    #:ensure-car
    #:ensure-cadr
    #:with-place
    #:draw-axes
    #:graph-function))

(defpackage #:coding-math.math
  (:use
    #:cl
    #:losh
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
    #:losh
    #:sketch
    #:coding-math.quickutils
    #:coding-math.math
    #:coding-math.utils)
  (:export
    #:with-fps
    #:draw-fps))

(defpackage #:coding-math.tween
  (:use
    #:cl
    #:losh
    #:coding-math.quickutils
    #:coding-math.math
    #:coding-math.utils)
  (:export
    #:tween-linear
    #:tween-quadratic-in
    #:tween-quadratic-out
    #:tween-quadratic-inout
    #:tween-cubic-in
    #:tween-cubic-out
    #:tween-cubic-inout
    #:tween-quartic-in
    #:tween-quartic-out
    #:tween-quartic-inout
    #:tween-quintic-in
    #:tween-quintic-out
    #:tween-quintic-inout
    #:tween-place!
    #:tween-places!
    #:update-tweens!
    ))

(defpackage #:coding-math.vectors
  (:use #:cl)
  (:export
    vec2
    vec2f
    vec2d
    vec2i
    vec2-add
    vec2f-add
    vec2d-add
    vec2i-add
    vec2-sub
    vec2f-sub
    vec2d-sub
    vec2i-sub
    vec2-mul
    vec2f-mul
    vec2d-mul
    vec2i-mul
    vec2-div
    vec2f-div
    vec2d-div
    vec2i-div
    vec2-eql
    vec2f-eql
    vec2d-eql
    vec2i-eql
    vec2-magnitude
    vec2f-magnitude
    vec2d-magnitude
    vec2i-magnitude
    vec2-length
    vec2f-length
    vec2d-length
    vec2i-length
    vec2-angle
    vec2f-angle
    vec2d-angle
    vec2i-angle
    vec2-direction
    vec2f-direction
    vec2d-direction
    vec2i-direction
    vec2-x
    vec2f-x
    vec2d-x
    vec2i-x
    vec2-y
    vec2f-y
    vec2d-y
    vec2i-y
    )
  )


;;;; 2D stuff
(defpackage #:coding-math.2d.vectors
  (:use
    #:cl
    #:losh
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
    #:vec-rotate
    #:vec-lerp
    #:vec-add!
    #:vec-sub!
    #:vec-mul!
    #:vec-div!
    #:vec-rotate!
    #:vec-to-string
    #:with-vec
    #:with-vecs
    #:vec-to-list
    #:vec-set-angle
    #:vec-set-direction
    #:vec-set-magnitude
    ))

(defpackage #:coding-math.2d.hitboxes
  (:use
    #:cl
    #:losh
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
    #:losh
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
    #:particle-ease-to!
    #:particle-spring-to!
    #:particle-spring-add!
    #:particle-spring-remove!))

(defpackage #:coding-math.2d.points
  (:use
    #:cl
    #:losh
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

(defpackage #:coding-math.2d.lines
  (:use
    #:cl
    #:losh
    #:sketch
    #:coding-math.math
    #:coding-math.2d.vectors
    #:coding-math.quickutils
    #:coding-math.utils)
  (:export
    #:std-intersection-point
    #:mxb-intersection-point
    #:xys-intersection-point
    #:xys-segments-intersection-point
    #:xys-to-mxb
    #:xys-to-std
    #:std-to-xys
    #:std-to-mxb
    #:mxb-to-xys
    #:mxb-to-std
    ))


(defpackage #:coding-math.2d.demo
  (:use
    #:cl
    #:losh
    #:sketch
    #:iterate
    #:coding-math.quickutils
    #:coding-math.utils
    #:coding-math.fps
    #:coding-math.math
    #:coding-math.tween
    #:coding-math.2d.vectors
    #:coding-math.2d.points
    #:coding-math.2d.lines
    #:coding-math.2d.hitboxes
    #:coding-math.2d.particles)
  (:shadowing-import-from #:iterate
    #:in)
  (:shadow #:point))

(defpackage #:coding-math.2d.ballistics
  (:use
    #:cl
    #:losh
    #:sketch
    #:coding-math.quickutils
    #:coding-math.tween
    #:coding-math.2d.particles
    #:coding-math.2d.hitboxes
    #:coding-math.utils
    #:coding-math.math
    #:coding-math.fps))


;;;; 3D stuff
(defpackage #:coding-math.3d.vectors
  (:use
    #:cl
    #:losh
    #:sb-cga
    #:coding-math.math
    #:coding-math.utils
    #:coding-math.quickutils)
  (:export
    #:vec-x #:vec-y #:vec-z
    #:vec-r #:vec-a #:vec-h
    #:random-vec
    #:zero-vec
    #:with-vec
    #:with-vecs
    #:angle-between))

(defpackage #:coding-math.3d.coordinates
  (:use
    #:cl
    #:losh
    #:sb-cga
    #:coding-math.math
    #:coding-math.3d.vectors
    #:coding-math.utils
    #:coding-math.quickutils)
  (:export
    #:cartesian-to-cylindrical
    #:cylindrical-to-cartesian
    #:cylindrical-to-cartesian-cga
    ))

(defpackage #:coding-math.3d.demo
  (:use
    #:cl
    #:losh
    #:iterate
    #:sketch
    #:coding-math.quickutils
    #:coding-math.utils
    #:coding-math.fps
    #:coding-math.math
    #:coding-math.tween
    #:coding-math.3d.vectors
    #:coding-math.3d.coordinates
    )
  (:import-from :sb-cga
    :vec)
  (:shadowing-import-from #:iterate
    #:in)
  (:shadow #:point))

