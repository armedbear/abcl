(in-package :abcl/build)

;; TODO function to deal with looking up a locally preferred mirrors
(defun ant-zip-uri ()
  #p"http://archive.apache.org/dist/ant/binaries/apache-ant-1.9.4-bin.zip"
  
  #+(or) ;; https on OPEN fails; probably attempting to upgrade
  #p"https://archive.apache.org/dist/ant/binaries/apache-ant-1.9.4-bin.zip"

  #+(or) ;; need apache-ant-1.9 for JVM version 49.0 
  #p"http://www-eu.apache.org/dist/ant/binaries/apache-ant-1.10.1-bin.zip")

(defun xdg/ant-executable ()
  (xdg/executable (ant-zip-uri) "bin/ant"))

#+(or)
(defun xdg/ant-executable ()
  (let* ((uri (ant-zip-uri))
         (directory (xdg/abcl-install-root uri))
         (ant-root-name (let ((name (pathname-name uri)))
                          (subseq name 0 (- (length name) (length "-bin")))))
         (ant-home (merge-pathnames (make-pathname :directory `(:relative ,ant-root-name))
                                    directory))
         (ant (merge-pathnames #p"bin/ant" ant-home))
         result)
    (dolist (p (possible-executable-names ant))
      (when (probe-file p)
        (return-from xdg/ant-executable
          (values
           (probe-file p)
           ant))))
    ;; failure
    (values
     nil
     ant)))

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




    
