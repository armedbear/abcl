(in-package :cl-user)

(prove:plan 1)
(let* ((uri #p"http://example.org/directory/name.version")
       (p (make-pathname :host nil :defaults uri)))
  (prove:like (namestring p) "^/directory/name.version$"))

(prove:finalize)

   
