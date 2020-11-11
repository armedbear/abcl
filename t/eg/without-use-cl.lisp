(cl:defpackage :without-use-cl)
(cl:in-package :without-use-cl)

(cl:defvar *foo* cl:nil "A var")

(cl:defun test ()
  (cl:let* ((p1 (cl:make-pathname))
         (p2 cl:*default-pathname-defaults*)
         (p3 (cl:merge-pathnames p1)))
    (cl:values (cl:pathname-device p2) (cl:pathname-device p3))))

(cl:eval-when (:compile-toplevel :load-toplevel)
  (cl:let ((a 2))
    (cl:defparameter *a* a)))
