(in-package :abcl/build)

(defun maven-zip-uri ()
  #p"http://www-eu.apache.org/dist/maven/maven-3/3.5.0/binaries/apache-maven-3.5.0-bin.zip")

(defun xdg/mvn-executable ()
  (xdg/executable (maven-zip-uri) "bin/mvn"))

(defparameter *maven-install-root* nil)
  
(defun mvn/install ()
  (unless (xdg/mvn-executable)
    (xdg/install (maven-zip-uri) :type :unzip))
  (values
   (xdg/mvn-executable)
   (directory (merge-pathnames
               "**/*" (xdg/abcl-install-root (maven-zip-uri))))))

(defparameter *mvn-home* nil)

(define-condition no-installed-maven (error)
  ((searched :initarg :searched))
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (format stream "Unable to introspect local Apache Maven installation."))))

(defun ensure-maven (&key (mvn-home *mvn-home* mvn-home-p))
    (cond
      ((and (null mvn-home) mvn-home-p)
       (warn "Unimplemented explicit auto-configuration run."))
      ((and mvn-home mvn-home-p)
       (warn "Unimplemented explicit configuration with specified directory directory."))
      (t 
       (if *mvn-home*
           *mvn-home*
           (restart-case
               (let ((mvn-home (some-directory-containing "mvn")))
                 (unless mvn-home
                   (signal 'no-installed-maven))
                 (setf *mvn-home* mvn-home))
             (install-maven ()
               (mvn/install)))))))

(defmacro with-ensured-mvn ((maven) &body body)
  `(progn
     (unless ,maven
       (setf ,maven (ensure-maven))
     ,@body)))

(defun mvn/call (pom-file target-or-targets)
  (let (mvn)
    (with-ensured-mvn (mvn)
      (uiop:run-program
       `(,mvn "--file" ,(stringify pom-file)
              ,@(listify target-or-targets))
       :output :string))))

    
