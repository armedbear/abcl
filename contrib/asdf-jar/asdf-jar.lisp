(defpackage #:asdf-jar
  (:use :cl)
  (:export #:package))

(in-package :asdf-jar)

(defvar *systems*)
(defmethod asdf:perform :before ((op asdf:compile-op) (c asdf:system))
  (push c *systems*))

(defun package (system-name 
                &key (out #p"/var/tmp/") 
                     (recursive t) 
                     (verbose t))
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
         (mapping (make-hash-table :test 'equal)))
    (when verbose 
      (format verbose "~&Packaging ASDF definition of ~A~&as ~A." system package-jar))
    (setf *systems* nil)
    (when verbose
      (format verbose "~&Forcing recursive compilation of ~A." package-jar))
    (asdf:compile-system system :force t)
    (when verbose
      (format verbose "~&Packaging contents in ~A." package-jar))
    (dolist (system (append (list system) *systems*))
      (let ((base (slot-value system 'asdf::absolute-pathname))
            (name (slot-value system 'asdf::name))
            (asdf (slot-value system 'asdf::source-file)))
        (setf (gethash asdf mapping) (relative-path base name asdf))
        
          ;;; XXX iterate through the rest of the contents of the
          ;;; system, adding appropiate entries
        (let ((sources
               (mapwalk (lambda (c) (typep c 'asdf::source-file))
                        (lambda (c) (input-files c )))))
          (loop :for source :in sources
             :do (setf (gethash (pathname-namestring source) mapping)
                       (make-pathname :defaults source
                                      :type "abcl"))))))
  (system:zip package-jar mapping)))

;;; This more Map than Walk at this point ...
(defun mapwalk (system test-if callable)
  (declare (type system asdf:system))
  (let ((components 
         (loop 
            :for component :being :each :hash-value
              :of (slot-value system 'asdf::components-by-name)
            :when (funcall test-if component)
            :collect component)))
    (loop :for component :in components
       :collecting (apply callable component))))

(defun relative-path (base dir file) 
  (let* ((relative 
          (nthcdr (length (pathname-directory base)) (pathname-directory file)))
         (entry-dir `(:relative ,dir ,@(when relative relative))))
    (make-pathname :directory entry-dir
                   :defaults file)))

(defun tmpdir (name)
  "Return temporary directory."
  (let* ((temp-file (java:jcall "getAbsolutePath" 
                               (java:jstatic "createTempFile" "java.io.File" "foo" "tmp")))
         (temp-path (pathname temp-file)))
    (make-pathname 
     :directory (nconc (pathname-directory temp-path)
                       (list name)))))















    
	

  
  
