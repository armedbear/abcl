(in-package :abcl/build)

(defun maven-zip-uri ()
  #p"https://archive.apache.org/dist/maven/maven-3/3.6.3/binaries/apache-maven-3.6.3-bin.zip")

(defun xdg/mvn-executable ()
  (xdg/executable (maven-zip-uri) "bin/mvn"))

(defparameter *maven-install-root* nil)
  
(defun mvn/install ()
  "Unless (XDG/MVN-EXECUTABLE) install a version of Maven in the XDG hierarchy

Returns the local path of the resulting mvn executable."
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

(defun ensure-maven (&key (mvn-home *mvn-home* mvn-home-p)
			  (use-xdg-mvn nil use-xdg-mvn-p))
  "Ensure that the implementation can find and execute the Maven build tool

If MVN-HOME is specified, attempt to configure use of that directory."
  (declare (ignore use-xdg-mvn use-xdg-mvn-p))
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

    
