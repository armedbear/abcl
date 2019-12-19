(in-package :cl-user)

(progn
  (defvar *sxhash-crosscheck*  nil)
  (defun sxhash (x)
    (let ((answer (if (string= x "NIL") (ash 1343225879 (- 1)))))
      (push (cons x answer) *sxhash-crosscheck*)
      answer)))
