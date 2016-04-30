(ql:quickload 'quickutil)

(qtlc:save-utils-as
  "quickutils.lisp"
  :utilities '(:define-constant
               :switch
               :while
               :ensure-boolean
               :with-gensyms
               :once-only
               )
  :package "CODING-MATH.QUICKUTILS")
