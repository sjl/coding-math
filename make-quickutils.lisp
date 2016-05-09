(ql:quickload 'quickutil)

(qtlc:save-utils-as
  "quickutils.lisp"
  :utilities '(:define-constant
               :switch
               :while
               :ensure-boolean
               :with-gensyms
               :once-only
               :iota
               :curry
               :rcurry
               :compose
               )
  :package "CODING-MATH.QUICKUTILS")
