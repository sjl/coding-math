(asdf:defsystem #:coding-math
  :name "coding-math"
  :description "Working through the Coding Math videos."

  :author "Steve Losh <steve@stevelosh.com>"

  :license "MIT/X11"
  :version "0.0.1"

  :depends-on (#:defstar
               #:optima
               #:sketch
               #:iterate
               #:sb-cga
               #:trivial-types
               #:cl-arrows
               #:fare-quasiquote-optima
               #:fare-quasiquote-readtable)

  :serial t
  :components
  ((:file "quickutils") ; quickutils package ordering crap
   (:file "package")
   (:module "src"
    :serial t
    :components ((:file "utils")
                 (:file "math")
                 (:file "fps")
                 (:file "tween")
                 (:module "2d"
                  :serial t
                  :components ((:file "vectors")
                               (:file "hitboxes")
                               (:file "particles")
                               (:file "points")
                               (:file "lines")
                               (:file "demo")
                               (:file "ballistics")))
                 (:module "3d"
                  :serial t
                  :components ((:file "vectors")
                               (:file "coordinates")
                               (:file "demo")))))))

