(in-package #:coding-math.utils)

(defun dividesp (n divisor)
  "Return whether `n` is evenly divisible by `divisor`."
  (zerop (mod n divisor)))


(defmacro mulf (place n &environment env)
  "Multiply `place` by `n` in-place."
  (multiple-value-bind (temps exprs stores store-expr access-expr)
      (get-setf-expansion place env)
    `(let* (,@(mapcar #'list temps exprs)
            (,(car stores) (* ,n ,access-expr)))
       ,store-expr)))
