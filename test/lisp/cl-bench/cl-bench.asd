(defpackage :cl-bench-asdf 
  (:use :cl :asdf))

(in-package :cl-bench-asdf)

(defsystem :cl-bench
  :documentation "http://www.chez.com/emarsden/downloads/cl-bench.tar.gz"
  :version "20081231a"
  :components 
  ((:module cl-bench-source :pathname "" :components
	    ((:file "defpackage")
	     (:file "do-compilation-script")
	     (:file "do-execute-script")
	     (:file "do-interpret-script")
	     (:file "generate")
	     (:file "graph-report")
	     (:file "pdf-report")
	     (:file "report")
	     (:file "support")
	     (:file "tests")))))

(defmethod perform ((o test-op) (c (eql (find-system 'cl-bench))))
  "Invoke tests with:  (asdf:operate 'asdf:test-op :cl-bench)."
  (asdf:oos 'asdf:load-op :cl-bench)
  (load "sysdep/setup-ablisp.lisp")
;  (load "do-compilation-script.lisp")
  (load "do-execute-script"))

