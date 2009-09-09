#|

Compile with 

  (let ((sys::*compile-file-zip* t))
    (compile-file "foo.lisp" :output-file "foo.jar"))

Load with 

   (load "jar:file:foo.jar!/foo")

|#

(defun foo ()
  (labels ((output ()
             (format t "FOO here.")))
    (output)))


