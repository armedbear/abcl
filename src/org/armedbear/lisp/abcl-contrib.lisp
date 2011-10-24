(in-package :system)

(require :asdf)

;;; XXX make less sensitive to ABCL jar being called "abcl.jar"
;;;     allow being called "abcl-x.y.z.jar for semantic versioning 
;;;     allow customization in system.lisp
(defun find-system-jar () 
  (dolist (loader (java:dump-classpath))
    (let ((abcl-jar
	   (find-if (lambda (p) (and 
                                 (or (equal (pathname-name p) "abcl")
                                     (equal (pathname-name p) 
                                            (format nil "abcl-~A" 
                                                    (lisp-implementation-version))))
				     (equal (pathname-type p) "jar")))
		    (rest loader))))
      (when abcl-jar
	(return abcl-jar)))))

(defvar *abcl-jar* nil
  "Pathname of the jar that ABCL was loaded from.
Initialized via SYSTEM::FIND-SYSTEM-JAR.")

(defvar *abcl-contrib* nil
  "Pathname of the ABCL contrib.
Initialized via SYSTEM:FIND-CONTRIB")

(defun find-contrib (&optional (verbose nil))
"Attempt to find the ABCL contrib jar and add its contents to ASDF."
  (unless *abcl-contrib*
    (unless *abcl-jar*
      (setf *abcl-jar* (find-system-jar)))
    (when *abcl-jar*
      (let ((abcl-contrib (make-pathname :defaults *abcl-jar*
					 :name "abcl-contrib")))
	(when (probe-file abcl-contrib)
	  (setf *abcl-contrib* abcl-contrib)
	  (dolist (asdf-file
		    (directory (make-pathname :device (list *abcl-contrib*)
					      :directory '(:absolute :wild)
					      :name :wild
					      :type "asd")))
	    (let ((asdf-directory 
		   (make-pathname :defaults asdf-file :name nil :type nil)))
	      (when verbose
		(format t "Adding ~A to ASDF.~%" asdf-directory))
	      (push asdf-directory asdf:*central-registry*)))
	  *abcl-contrib*)))))

(when (find-contrib)
  (provide :abcl-contrib))



		   
  
