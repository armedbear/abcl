(defpackage :asdf-jar
  (:use :cl)
  (:export #:package))

(in-package :asdf-jar)


(defvar *systems*)
(defmethod asdf:perform :before ((op asdf:compile-op) (c asdf:system))
       (push c *systems*))

;; (defvar *sources*)
;; (defmethod asdf:perform :before ((op asdf:compile-op) (s asdf:source-file))
;;        (push c *sources*))

(eval-when (:compile-toplevel :execute)
  (ql:quickload "cl-fad"))

(defun package (system-name 
                &key (out #p"/var/tmp/") 
                     (recursive t) 
                     (verbose t))
  (asdf:disable-output-translations)
  (let* ((system 
          (asdf:find-system system-name))
	 (name 
          (slot-value system 'asdf::name))
         (version 
          (slot-value system 'asdf:version))
         (package-jar-name 
          (format nil "~A~A-~A.jar" name (when recursive "-all") version))
         (package-jar
          (make-pathname :directory out :defaults package-jar-name))
         (tmpdir (tmpdir (pathname-name (pathname package-jar-name)))))
    (when verbose 
      (format verbose "~&Packaging ASDF definition of ~A~&as ~A." system package-jar))
    (setf *systems* nil)
    (asdf:compile-system system :force t)
    (let* ((dir (asdf:component-pathname system))
	   (wild-contents (merge-pathnames "**/*" dir))
	   (contents (directory wild-contents))
	   (topdir (truename (merge-pathnames "../" dir))))
      (when verbose
	(format verbose "~&Packaging contents in ~A." package-jar))
      (dolist (system (append (list system) *systems*))
        (copy-recursively system tmpdir))
      (system:zip package-jar contents topdir)))
  (asdf:initialize-output-translations))

(defun copy-recursively (source destination)
  (let* ((source (truename source))
         (source-directories (1- (length (pathname-directory source))))
         (destination (truename destination)))
    (cl-fad:walk-directory 
     source
   (lambda (p) 
     (let* ((relative-depth (- (length (pathname-directory p))
                               (length (pathname-directory source))))
            (subdir '(nthcdr (+ source-directories relative-depth)
                      (pathname-directory source)))
            (orig (merge-pathnames p
                                   (make-pathname :directory (append (pathname-directory
                                                                      source)
                                                                     subdir))))
            (dest (merge-pathnames p
                                  (make-pathname :directory (append (pathname-directory
                                                                     destination)
                                                                    subdir)))))
       (format t "~&Would copy ~A~&to ~A." orig dest))))))
                          

(defun tmpdir (name)
  "Return a the named temporary directory."
  (let* ((temp-file (java:jcall "getAbsolutePath" 
                               (java:jstatic "createTempFile" "java.io.File" "foo" "tmp")))
         (temp-path (pathname temp-file)))
    (make-pathname 
     :directory (nconc (pathname-directory temp-path)
                       (list name)))))















    
	

  
  
