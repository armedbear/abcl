(in-package :abcl/build)

;; TODO function to deal with looking up a locally preferred mirrors
(defun ant-zip-uri ()
  #p"http://archive.apache.org/dist/ant/binaries/apache-ant-1.9.4-bin.zip"
  
  #+nil ;; https on OPEN fails; probably attempting to upgrade
  #p"https://archive.apache.org/dist/ant/binaries/apache-ant-1.9.4-bin.zip"

  #+nil ;; need apache-ant-1.9 for JVM version 49.0 
  #p"http://www-eu.apache.org/dist/ant/binaries/apache-ant-1.10.1-bin.zip")

(defun xdg/ant-executable ()
  (probe-file
   (localize-executable-name
    (merge-pathnames #p"bin/ant"
                     (xdg/abcl-install-root (ant-zip-uri))))))

(defun ant/install ()
  (unless (xdg/ant-executable)
    (xdg/install (ant-zip-uri) :type :unzip))
  (values
   (xdg/ant-executable)
   (directory (merge-pathnames "**/*"
                               (xdg/abcl-install-root (ant-zip-uri))))))

(defparameter *ant-home* nil)

(define-condition no-installed-ant (error)
  ((searched))
  (:report (lambda (condition stream)
             (format stream "Unable to introspect Apache Ant installation."))))

;; TODO after this routines executes *ANT-EXECUTABLE-DIRECTORY* and XDG/ANT-EXECUTABLE will work
(defun ensure-ant (&key (ant-home nil ant-home-p))
  (cond
    ((and (null ant-home) ant-home-p)
     (warn "Unimplemented explicit auto-configuration run."))
    ((and ant-home ant-home-p)
     (warn "Unimplemented explicit configuration with specified directory directory."))
    (t 
     (if *ant-home*
         *ant-home*
         (restart-case
             (let ((ant-home (some-directory-containing "ant")))
               (unless ant-home
                 (signal 'no-installed-ant))
               (setf *ant-home ant-home))
           (install-ant ()
             (ant/install)))))))

(defmacro with-ensured-ant ((ant) &body body)
  `(progn
     (unless ,ant
       (setf ,ant (ensure-ant)))
     ,@body))

(defun ant/call (ant-file target-or-targets)
  (let (ant)
    (with-ensured-ant (ant)
      (uiop:run-program
       `(,ant "-buildfile"
              ,(stringify ant-file)
              ,@(listify target-or-targets))
       :output :string))))




    
