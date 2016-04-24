(in-package #:coding-math.utils)

(defun dividesp (n divisor)
  "Return whether `n` is evenly divisible by `divisor`."
  (zerop (mod n divisor)))

(defun square (n)
  "Return the square of `n`."
  (* n n))


(defmacro mulf (place n &environment env)
  "Multiply `place` by `n` in-place."
  (multiple-value-bind (temps exprs stores store-expr access-expr)
      (get-setf-expansion place env)
    `(let* (,@(mapcar #'list temps exprs)
            (,(car stores) (* ,n ,access-expr)))
       ,store-expr)))


(defmacro in-context (&body body)
  `(prog1
    (push-matrix)
    (progn ,@body)
    (pop-matrix)))


(defmacro make-sketch (class &rest bindings)
  `(let*
    (,@(loop :for (k v) :in bindings
             :collect (list k v)))
    (make-instance
      ,class
      ,@(loop :for (k) :in bindings
              :append (list (alexandria:make-keyword k) k)))))


(defmacro scancode-case (scancode-form &rest pairs)
  (with-gensyms (scancode)
    `(let ((,scancode ,scancode-form))
      (cond
        ,@(mapcar (lambda (pair)
                    (destructuring-bind (key-scancode &rest body) pair
                      `((sdl2:scancode= ,scancode ,key-scancode)
                        ,@body)))
           pairs)))))

(defmacro with-vals (bindings value-form &body body)
  (with-gensyms (val)
    `(let* ((,val ,value-form)
            ,@(loop :for (s accessor) :in bindings
                    :collect `(,s (,accessor ,val))))
      ,@body)))
