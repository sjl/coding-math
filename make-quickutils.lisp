(ql:quickload 'quickutil)

(qtlc:save-utils-as
  "quickutils.lisp"
  :utilities '(:define-constant
               :switch
               :while
               :ensure-boolean
               :with-gensyms
               )
  :package "CODING-MATH.QUICKUTILS")
