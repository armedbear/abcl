(defpackage :abcl.tests.ansi-tests
  (:use :cl :asdf)
  (:nicknames "ansi-tests" "abcl-ansi-tests")
  (:export :run))

(in-package :abcl.tests.ansi-tests)

(defparameter *ansi-tests-master-source-location*
  "<svn://common-lisp.net/project/ansi-test/svn/trunk/ansi-tests>")  

(defparameter *ansi-tests-directory*
  (merge-pathnames
   #p"../ansi-tests/"
   (asdf:component-pathname (asdf:find-system :abcl))))

(defun run (&key (compile-tests nil)) 
  "Run the ANSI-TESTS suite, found in *ANSI-TESTS-DIRECTORY*.
Possibly running the compiled version of the tests if COMPILE-TESTS is non-NIL."
  (let* ((original-pathname-defaults *default-pathname-defaults*)
	(ansi-tests-directory *ansi-tests-directory*)
	(boot-file (if compile-tests "compileit.lsp" "doit.lsp"))
	(message (format nil "Invocation of '~A' in ~A"
			       boot-file ansi-tests-directory)))
    (handler-case 
	(progn
	  (setf  *default-pathname-defaults*
		 (merge-pathnames ansi-tests-directory 
				  *default-pathname-defaults*))
	  (format t "--->  ~A begins.~%" message)
	  (format t "Invoking ABCL hosted on ~A ~A.~%" 
		  (software-type) (software-version))
	  (if (find :unix *features*)
	      (run-shell-command "cd ~A; make clean" ansi-tests-directory)
	      ;; XXX -- what to invoke on win32?  Please verify
	      (run-shell-command 
	       (format nil ("~A~%~A")
		       (format nil "cd ~A" *ansi-tests-directory*)
		       (format nil "erase *.cls *.abcl"))))
	  (time (load boot-file))
	  (format t "<--- ~A ends.~%" message))
      (file-error (e)
		(error 
		 (format nil
			 "Failed to find the GCL ANSI tests in '~A'.
Because ~A.
To resolve, please locally obtain ~A, 
and set the value of *ANSI-TESTS-DIRECTORY* to that location."
		 ansi-tests-directory e 
		 *ansi-tests-master-source-location*))))
    (setf *default-pathname-defaults* original-pathname-defaults)))
		   
	     

