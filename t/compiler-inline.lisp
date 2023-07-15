;;;; <https://github.com/armedbear/abcl/issues/591> 
(unless (ignore-errors (asdf:load-system :fset))
  (ql:quickload :fset))

(in-package :fset-user)
(defun foo (x)
  (labels ((bar (x)
             (cond ((null x) nil)
                   ((eq (car x) '&rest) (tail (cadr x)))
                   (t (cons (car x) (bar (cdr x))))))
           (tail (x)
             (list 'local-tail x)))
    (bar x)))
(compile 'foo)

(prove:plan 1)
(prove:is
    (foo '(a &rest b))
    '(a local-tail b))
(prove:finalize)

