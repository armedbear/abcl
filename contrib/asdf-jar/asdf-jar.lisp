(defpackage :asdf-jar
  (:use :cl)
  (:export #:package))

(in-package :asdf-jar)

(defvar *systems*)
(defmethod asdf:perform :before ((op asdf:compile-op) (c asdf:system))
       (push c *systems*))

(defun package (system-name &key (recursive t) (verbose t))
  (declare (ignore recursive))
  (asdf:disable-output-translations)
  (let* ((system (asdf:find-system system-name))
	 (name (slot-value system 'asdf::name)))
    (when verbose 
      (format verbose "Packaging ASDF definition of~A~%" system))
    (setf *systems* nil)
    (asdf:compile-system system :force t)
    (let* ((dir (asdf:component-pathname system))
	   (wild-contents (merge-pathnames "**/*" dir))
	   (contents (directory wild-contents))
	   (output (format nil "/var/tmp/~A.jar" name))
	   (topdir (truename (merge-pathnames "../" dir))))
      (when verbose
	(format verbose "Packaging contents in ~A.~%" output))
      (system:zip output contents topdir)))
  (asdf:initialize-output-translations))


    
	

  
  
