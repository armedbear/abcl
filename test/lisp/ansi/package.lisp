(defpackage :abcl.test.ansi
  (:use :cl :asdf)
  (:nicknames "ansi-tests" "abcl-ansi-tests" "gcl-ansi")
  (:export :run :report :parse))

(in-package :abcl.test.ansi)

(defparameter *ansi-tests-master-source-location*
  "<svn://common-lisp.net/project/ansi-test/svn/trunk/ansi-tests>")  

(defparameter *ansi-tests-directory*
  (if (find :asdf2 *features*)
      (asdf:system-relative-pathname 
       :ansi-compiled "../ansi-tests/")
      (merge-pathnames
       #p"../ansi-tests/"
       (asdf:component-pathname (asdf:find-system :ansi-compiled)))))

(defun run (&key (compile-tests nil)) 
  "Run the ANSI-TESTS suite, to be found in *ANSI-TESTS-DIRECTORY*.
Possibly running the compiled version of the tests if COMPILE-TESTS is non-NIL."
  (let* ((ansi-tests-directory 
	  *ansi-tests-directory*)
	 (boot-file 
	  (if compile-tests "compileit.lsp" "doit.lsp"))
	 (message 
	  (format nil "Invocation of '~A' in ~A" boot-file ansi-tests-directory)))
    (handler-case 
	(progv 
	    '(*default-pathname-defaults*) 
	    `(,(merge-pathnames *ansi-tests-directory* *default-pathname-defaults*))
	  (format t "--->  ~A begins.~%" message)
	  (format t "Invoking ABCL hosted on ~A ~A.~%" 
		  (software-type) (software-version))
          ;; Do what 'make clean' would do from the GCL ANSI tests,
          ;; so we don't have to hunt for 'make' on win32.
          (mapcar #'delete-file
                  (append (directory (format nil "~A/*.cls" *default-pathname-defaults*))
                          (directory (format nil "~A/*.abcl" *default-pathname-defaults*))
                          (directory (format nil "~A/scratch/*" *default-pathname-defaults*))
                          (mapcar (lambda(x) (format nil "~A/~A" *default-pathname-defaults* x))
                                  '("scratch/"
                                    "scratch.txt" "foo.txt" "foo.lsp"
                                    "foo.dat" 
                                    "tmp.txt" "tmp.dat" "tmp2.dat"
                                    "temp.dat" "out.class" 
                                    "file-that-was-renamed.txt"
                                    "compile-file-test-lp.lsp"
                                    "compile-file-test-lp.out" 
                                    "ldtest.lsp"))))
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
		 *ansi-tests-master-source-location*))))))

		   
	     

