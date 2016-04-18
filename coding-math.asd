(asdf:defsystem #:coding-math
  :name "coding-math"
  :description "Working through the Coding Math videos."

  :author "Steve Losh <steve@stevelosh.com>"

  :license "MIT/X11"
  :version "0.0.1"

  :depends-on (#:defstar
               #:optima
               #:sketch
               #:trivial-types
               #:cl-arrows
               #:fare-quasiquote-optima
               #:fare-quasiquote-readtable)

  :serial t
  :components ((:file "quickutils") ; quickutils package ordering crap
               (:file "package")
               (:module "src"
                :serial t
                :components ((:file "utils")
                             (:file "math")
                             (:file "vectors")
                             (:file "particles")
                             (:file "main")
                             (:file "ballistics")
                             ))))

