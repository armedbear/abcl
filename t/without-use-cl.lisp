(in-package :cl-user)

(prove:plan 1)
(prove:ok
 (let ((*package* (defpackage :without-use-cl)))
   (cl:load (cl:compile-file
             (asdf:system-relative-pathname :abcl
                                            "t/eg/without-use-cl.lisp")))
   "Compiling in a package without a (:USE :CL) clause"))

(prove:plan 1)
(let ((source (make-pathname :type "lisp"
                             :defaults (uiop:with-temporary-file (:pathname o) o))))
  (with-open-file (o source :direction :output :if-exists :supersede)
    (write '(cl:format cl:t "~&I am in package ~s.~%" cl:*package*) :stream o))
  (let ((output (compile-file source)))
    (prove:ok 
     (let ((*package* (defpackage :without-use-cl)))
       (cl:load cl-user::output))
     "Loading a fasl from a package without a (:USE :CL) clause.")))

(prove:finalize)


            
