.PHONY: 

quickutils.lisp: make-quickutils.lisp
	sbcl-rlwrap --noinform --load make-quickutils.lisp  --eval '(quit)'
