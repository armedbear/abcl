(require :abcl-contrib)
(require :quicklisp-abcl)
(require :abcl-asdf)

(time
  (progn
    (ql:quickload :jeannie/test)
    (asdf:load-system :jeannie)))

(prove:plan 1)

(time 
 (let ((p (asdf:system-relative-pathname :abcl "abcl.rdf")))
   (prove:ok
    (jeannie:read-rdf p :format :n3))
   (format nil "Reading RDF from <file:~a>." (namestring p))))

(prove:finalize)

