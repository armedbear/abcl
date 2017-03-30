(in-package abcl/test)

(plan 1)
(let* ((uri #p"http://example.org/directory/name.version")
       (p (make-pathname :host nil :defaults uri)))
  (like (namestring p) "^/directory/name.version$"))

(finalize)

   
