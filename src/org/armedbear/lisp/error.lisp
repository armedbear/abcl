;;; error.lisp

(in-package "COMMON-LISP")

(export '(ignore-errors))

(defmacro ignore-errors (&rest forms)
  `(handler-case (progn ,@forms)
                 (error (condition) (values nil condition))))
