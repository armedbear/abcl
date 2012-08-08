(in-package :system)

(require :asdf)

;;; TODO possibly allow customization in system.lisp?
(defun find-system-jar () 
  (flet ((match-system-jar (p)
           "Match `abcl.jar` or `abcl-1.0.1.jar` or `abcl-1.0.1-something.jar`"
           (and (pathnamep p)
                (equal (pathname-type p) "jar")
                (java:jstatic "matches"
                              "java.util.regex.Pattern" 
                              "abcl(-[0-9]\\.[0-9]\\.[0-9](-.+)?)?" 
                              (pathname-name p))
                p)))
    (dolist (loader (java:dump-classpath))
      (let ((abcl-jar (some #'match-system-jar loader)))
        (when abcl-jar
          (return abcl-jar))))))

(defvar *abcl-jar* nil
  "Pathname of the jar that ABCL was loaded from.
Initialized via SYSTEM::FIND-SYSTEM-JAR.")

(defvar *abcl-contrib* nil
  "Pathname of the ABCL contrib.
Initialized via SYSTEM:FIND-CONTRIB")

(defun find-contrib (&key (verbose nil))
"Attempt to find the ABCL contrib jar and add its contents to ASDF."
  (unless *abcl-contrib*
    (unless *abcl-jar*
      (setf *abcl-jar* (find-system-jar)))
    (when *abcl-jar*
      (let* ((abcl-contrib-name
              (concatenate 'string "abcl-contrib"
                           (subseq (pathname-name *abcl-jar*) 4)))
             (abcl-contrib (make-pathname :defaults *abcl-jar*
                                          :name abcl-contrib-name)))
	(if (probe-file abcl-contrib)
            (progn
              (setf *abcl-contrib* abcl-contrib)
              (dolist (asdf-file
                        (directory (make-pathname :device (list *abcl-contrib*)
                                                  :directory '(:absolute :wild)
                                                  :name :wild
                                                  :type "asd")))
                (let ((asdf-directory (make-pathname :defaults asdf-file :name nil :type nil)))
                  (unless (find asdf-directory asdf:*central-registry* :test #'equal)
                    (push asdf-directory asdf:*central-registry*)
                    (format verbose "~&Added ~A to ASDF.~&" asdf-directory))))
              *abcl-contrib*)
        (format verbose "Failed to find abcl-contrib at '~A'." abcl-contrib))))))

(when (find-contrib :verbose t)
  (provide :abcl-contrib))







		   
  
