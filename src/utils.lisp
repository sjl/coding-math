(in-package #:coding-math.utils)

(defmacro zap% (place function &rest arguments &environment env)
  "Update `place` by applying `function` to its current value and `arguments`.

  `arguments` should contain the symbol `%`, which is treated as a placeholder
  where the current value of the place will be substituted into the function
  call.

  For example:

  (zap% foo #'- % 10) => (setf foo (- foo 10)
  (zap% foo #'- 10 %) => (setf foo (- 10 foo)

  "
  ;; original idea/name from http://malisper.me/2015/09/29/zap/
  (assert (find '% arguments)
          ()
          "Placeholder % not included in zap macro form.")
  (multiple-value-bind (temps exprs stores store-expr access-expr)
      (get-setf-expansion place env)
    `(let* (,@(mapcar #'list temps exprs)
            (,(car stores)
             (funcall ,function
                      ,@(substitute access-expr '% arguments))))
      ,store-expr)))


(defmacro in-context (&body body)
  `(prog1
    (push-matrix)
    (progn ,@body)
    (pop-matrix)))


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

(defmacro setf-slots (object &rest bindings)
  `(with-slots ,(remove-duplicates
                  (loop :for (slot) :on bindings :by #'cddr
                        :collect slot))
    ,object
    (setf
      ,@(loop :for (slot val) :on bindings :by #'cddr
              :append (list slot val)))))


;; snagged from squirl
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun symbolicate (&rest things)
    "Concatenate together the names of some strings and symbols,
producing a symbol in the current package."
    (let ((name (make-string (reduce #'+ things
                                     :key (compose #'length #'string)))))
      (let ((index 0))
        (dolist (thing things (values (intern name)))
          (let ((x (string thing)))
            (replace name x :start1 index)
            (incf index (length x))))))))

(macrolet
    ((define-ensure-foo (place) ; Lisp macros are nice
       `(defun ,(symbolicate "ENSURE-" place) (place &optional (default place))
         (if (atom place) default (,place place)))))
  (define-ensure-foo car)
  (define-ensure-foo cadr))

(defmacro with-place (conc-name (&rest slots) form &body body)
  (let* ((sm-prefix (ensure-car conc-name))
         (acc-prefix (ensure-cadr conc-name))
         (*package* (symbol-package sm-prefix)))
    `(with-accessors
      ,(mapcar (lambda (v)
                 (list (symbolicate sm-prefix (ensure-car v))
                       (symbolicate acc-prefix (ensure-cadr v))))
               slots)
      ,form
      ,@body)))
