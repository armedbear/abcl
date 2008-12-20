;;; build-abcl.lisp

#+abcl
(require 'format)

(defpackage build-abcl
  (:use "COMMON-LISP")
  (:export #:build-abcl #:make-dist)
  #+abcl (:import-from #:extensions #:run-shell-command #:probe-directory)
  #+allegro (:import-from #:excl #:probe-directory)
  #+clisp (:import-from #:ext #:probe-directory)
  )

(in-package #:build-abcl)

;; Platform detection.

(defun platform ()
  #-clisp
  (let ((software-type (software-type)))
    (cond ((search "Linux" software-type)
           :linux)
          ((or (search "Mac OS X" software-type) ; abcl
               (search "Darwin" software-type))  ; sbcl
           :darwin)
          ((search "Windows" software-type)
           :windows)
          (t
           :unknown)))
  #+clisp
  (cond ((member :win32 *features*)
         :windows)
        ((zerop (ext:run-shell-command "uname | grep -i darwin" :output nil))
         :darwin)
        ((zerop (ext:run-shell-command "uname | grep -i linux" :output nil))
         :linux)
        (t
         :unknown)))

(defparameter *platform* (platform))

(defparameter *file-separator-char*
  (if (eq *platform* :windows) #\\ #\/))

(defparameter *path-separator-char*
  (if (eq *platform* :windows) #\; #\:))

(defmacro with-current-directory ((directory) &body body)
  `(let ((*default-pathname-defaults* ,directory)
         #+clisp
         (old-directory (ext:cd)))
     #+clisp
     (ext:cd ,directory)
     (unwind-protect
         (progn ,@body)
       #+clisp
       (ext:cd old-directory)
       )))

#+sbcl
(defun run-shell-command (command &key directory (output *standard-output*))
  (when directory
    (setf command (concatenate 'string
                               "\\cd \""
                               (namestring (pathname directory))
                               "\" && "
                               command)))
  (sb-ext:process-exit-code
   (sb-ext:run-program
    "/bin/sh"
    (list  "-c" command)
    :input nil :output output)))

#+cmu
(defun run-shell-command (command &key directory (output *standard-output*))
  (when directory
    (setf command (concatenate 'string
                               "\\cd \""
                               (namestring (pathname directory))
                               "\" && "
                               command)))
  (ext::process-exit-code
   (ext:run-program
    "/bin/sh"
    (list  "-c" command)
    :input nil :output output)))

#+clisp
(defun run-shell-command (command &key directory (output *standard-output*))
  (declare (ignore output)) ;; FIXME
  (let (status old-directory)
    (when directory
      (setf old-directory (ext:cd))
      (ext:cd directory))
    (unwind-protect
        (setf status (ext:shell command))
      (when old-directory
        (ext:cd old-directory)))
    (cond ((numberp status)
           status)
          ((eq status t)
           0)
          (t
           -1))))

#+lispworks
(defun run-shell-command (command &key directory (output *standard-output*))
  (when directory
    (unless (eq *platform* :windows)
      (setf command (concatenate 'string
                                 "\\cd \""
                                 (namestring (pathname directory))
                                 "\" && "
                                 command))))
  (system:call-system-showing-output command
                                     :shell-type "/bin/sh"
                                     :output-stream output))

#+allegro
(defun run-shell-command (command &key directory (output *standard-output*))
  (excl:run-shell-command command
                          :directory directory
                          :input nil
                          :output #+ide nil #-ide output))

#+openmcl
(defun run-shell-command (command &key directory (output *standard-output*))
  (when directory
    (setf command (concatenate 'string
                               "\\cd \""
                               (namestring (pathname directory))
                               "\" && "
                               command)))
  (multiple-value-bind (status exitcode)
      (ccl:external-process-status
       (ccl:run-program
        "/bin/sh"
        (list  "-c" command)
        :wait t :input nil :output output))
    (declare (ignore status))
    exitcode))

#+(or sbcl cmu lispworks openmcl)
(defun probe-directory (pathspec)
  (let* ((truename (probe-file pathspec)) ; TRUENAME is a pathname.
         (namestring (and truename (namestring truename)))) ; NAMESTRING is a string.
    (and namestring
         (> (length namestring) 0)
         (eql (char namestring (1- (length namestring))) *file-separator-char*)
         truename)))

(defparameter *build-root*
  (make-pathname :device (pathname-device *load-truename*)
                 :directory (pathname-directory *load-truename*)))

(defparameter *customizations-file*
  (merge-pathnames "customizations.lisp" *build-root*))

(defparameter *abcl-dir*
  (merge-pathnames "src/org/armedbear/lisp/" *build-root*))

(defparameter *jdk*           nil)
(defparameter *java-compiler* nil)
(defparameter *javac-options* nil)
(defparameter *jikes-options* nil)
(defparameter *jar*           nil)

(defvar *classpath*)
(defvar *java*)
(defvar *java-compiler-options*)
(defvar *java-compiler-command-line-prefix*)

(defun initialize-build ()
  (setf *jdk*           nil
        *java-compiler* nil
        *javac-options* nil
        *jikes-options* nil
        *jar*           nil)
  (load *customizations-file*)
  (setf *java* (probe-file (merge-pathnames (if (eq *platform* :windows)
                                                "bin\\java.exe"
                                                "bin/java")
                                            *jdk*)))
  (unless *java*
    (error "Can't find Java executable."))
  (unless *java-compiler*
    (setf *java-compiler* (merge-pathnames (if (eq *platform* :windows)
                                               "bin/javac.exe"
                                               "bin/javac")
                                           *jdk*)))
  (unless *jar*
    (setf *jar* (merge-pathnames (if (eq *platform* :windows)
                                     "bin/jar.exe"
                                     "bin/jar")
                                 *jdk*)))
  (let ((classpath-components (list (merge-pathnames "src" *build-root*)
                                    (if (eq *platform* :darwin)
                                        #p"/System/Library/Frameworks/JavaVM.framework/Classes/classes.jar"
                                        (merge-pathnames "jre/lib/rt.jar" *jdk*)))))
    (setf *classpath*
          (with-output-to-string (s)
            (do* ((components classpath-components (cdr components))
                  (component (car components) (car components)))
                 ((null components))
              (princ (safe-namestring component) s)
              (unless (null (cdr components))
                (write-char *path-separator-char* s))))))
  (let ((prefix (concatenate 'string
                             (safe-namestring *java-compiler*)
                             " -classpath " *classpath*)))
    (setf *java-compiler-options*
          (if (string-equal (pathname-name (pathname *java-compiler*)) "jikes")
              *jikes-options*
              *javac-options*))
    (setf prefix
          (if *java-compiler-options*
              (concatenate 'string prefix " " *java-compiler-options* " ")
              (concatenate 'string prefix " ")))
    (setf *java-compiler-command-line-prefix* prefix)))

(defun substitute-in-string (string substitutions-alist)
  (dolist (entry substitutions-alist)
    (let ((index (search (car entry) string :test #'string=)))
      (when index
        (setf string (concatenate 'string
                                  (subseq string 0 index)
                                  (cdr entry)
                                  (subseq string (+ index (length (car entry)))))))))
  string)

(defun copy-with-substitutions (source-file target-file substitutions-alist)
  (with-open-file (in source-file :direction :input)
    (with-open-file (out target-file :direction :output :if-exists :supersede)
      (loop
        (let ((string (read-line in nil)))
          (when (null string)
            (return))
          (write-line (substitute-in-string string substitutions-alist) out))))))

(defun build-javac-command-line (source-file)
  (concatenate 'string
               *java-compiler-command-line-prefix*
               (namestring source-file)))

(defun java-compile-file (source-file)
  (let ((cmdline (build-javac-command-line source-file)))
    (zerop (run-shell-command cmdline :directory *abcl-dir*))))

(defun make-classes (force batch)
  (let* ((source-files
          (append (with-current-directory (*abcl-dir*)
                    (directory "*.java"))
                  (with-current-directory ((merge-pathnames "java/awt/" *abcl-dir*))
                    (directory "*.java"))))
         (to-do ()))
    (if force
        (setf to-do source-files)
        (dolist (source-file source-files)
          (let ((class-file (merge-pathnames (make-pathname :type "class"
                                                            :defaults source-file))))
            (when (or (null (probe-file class-file))
                      (>= (file-write-date source-file)
                          (file-write-date class-file)))
              (push source-file to-do)))))
    (format t "~&JDK: ~A~%" *jdk*)
    (format t "Java compiler: ~A~%" *java-compiler*)
    (format t "Compiler options: ~A~%~%" (if *java-compiler-options* *java-compiler-options* ""))
    (finish-output)
    (cond ((null to-do)
           (format t "Classes are up to date.~%")
           (finish-output)
           t)
          (t
           (cond (batch
                  (let* ((dir (pathname-directory *abcl-dir*))
                         (cmdline (with-output-to-string (s)
                                    (princ *java-compiler-command-line-prefix* s)
                                    (dolist (source-file to-do)
                                      (princ
                                       (if (equal (pathname-directory source-file) dir)
                                           (file-namestring source-file)
                                           (namestring source-file))
                                       s)
                                      (princ #\space s))))
                         (status (run-shell-command cmdline :directory *abcl-dir*)))
                    (zerop status)))
                 (t
                  (dolist (source-file to-do t)
                    (unless (java-compile-file source-file)
                      (format t "Build failed.~%")
                      (return nil)))))))))

(defun make-jar ()
  (let ((*default-pathname-defaults* *build-root*)
        (jar-namestring (namestring *jar*)))
    (when (position #\space jar-namestring)
      (setf jar-namestring (concatenate 'string "\"" jar-namestring "\"")))
    (let ((substitutions-alist (acons "@JAR@" jar-namestring nil))
          (source-file (if (eq *platform* :windows) "make-jar.bat.in" "make-jar.in"))
          (target-file (if (eq *platform* :windows) "make-jar.bat"    "make-jar"))
          (command     (if (eq *platform* :windows) "make-jar.bat"    "sh make-jar")))
      (copy-with-substitutions source-file target-file substitutions-alist)
      (let ((status (run-shell-command command :directory *build-root*)))
        (unless (zerop status)
          (format t "~A returned ~S~%" command status))
        status))))

(defun do-compile-system (&key (zip t))
  (terpri)
  (finish-output)
  (let* ((java-namestring (safe-namestring *java*))
         status)
    (cond ((eq *platform* :windows)
           (with-open-file (stream
                            (merge-pathnames "compile-system.bat" *build-root*)
                            :direction :output
                            :if-exists :supersede)
             (princ java-namestring stream)
             (write-string " -cp " stream)
             (princ "src" stream)
             (write-char #\space stream)
             (write-string
              (if zip
                 "org.armedbear.lisp.Main --eval \"(compile-system :zip t :quit t)\""
                 "org.armedbear.lisp.Main --eval \"(compile-system :zip nil :quit t)\"")
              stream)
             (terpri stream))
           (setf status
                 (run-shell-command "compile-system.bat"
                                    :directory *build-root*)))
          (t ; Linux
           (let ((cmdline
                  (with-output-to-string (s)
                    (princ java-namestring s)
                    (write-string " -cp " s)
                    (princ "src" s)
                    (write-char #\space s)
                    (write-string
                     (if zip
                         "org.armedbear.lisp.Main --eval \"(compile-system :zip t :quit t)\""
                         "org.armedbear.lisp.Main --eval \"(compile-system :zip nil :quit t)\"")
                     s))))
             (setf status
                   (run-shell-command cmdline
                                      :directory *build-root*)))))
    status))

(defun make-libabcl ()
  (and (let* ((javah-namestring (namestring (probe-file (merge-pathnames "bin/javah" *jdk*))))
              (command
               (format nil "~A -o org/armedbear/lisp/native.h org.armedbear.lisp.Native"
                       javah-namestring))
              (status
               (run-shell-command command :directory (merge-pathnames "src/" *build-root*))))
         (unless (zerop status)
           (format t "~A returned ~S~%" command status))
         (zerop status))
       (let* ((jdk-namestring (namestring *jdk*))
              (command
               (format nil "gcc -shared -o libabcl.so -O -D_REENTRANT -fpic -I~Ainclude -I~Ainclude/~A native.c"
                       jdk-namestring jdk-namestring
                       (cond ((eq *platform* :linux)
                              "linux")
                             ((search "SunOS" (software-type))
                              "solaris")
                             ((search "FreeBSD" (software-type))
                              "freebsd"))))
              (status
               (run-shell-command command :directory *abcl-dir*)))
         (unless (zerop status)
           (format t "~A returned ~S~%" command status))
         (zerop status))))

;; abcl/abcl.bat
(defun make-launch-script ()
  ;; Use the -Xss4M and -Xmx256M flags so that the default launch script can be
  ;; used to build sbcl.
  (cond ((eq *platform* :windows)
         (with-open-file (s
                          (merge-pathnames "abcl.bat" *build-root*)
                          :direction :output
                          :if-exists :supersede)
           (format s "~A -Xss4M -Xmx256M -cp \"~A;~A\" org.armedbear.lisp.Main %1 %2 %3 %4 %5 %6 %7 %8 %9~%"
                   (safe-namestring *java*)
                   (namestring (merge-pathnames "src" *build-root*))
                   (namestring (merge-pathnames "abcl.jar" *build-root*)))))
        (t
         (let ((pathname (merge-pathnames "abcl" *build-root*)))
           (with-open-file (s pathname :direction :output :if-exists :supersede)
             (if (eq *platform* :linux)
                 ;; On Linux, set java.library.path for libabcl.so.
                 (format s "#!/bin/sh~%exec ~A -Xss4M -Xmx256M -Xrs -Djava.library.path=~A -cp ~A:~A org.armedbear.lisp.Main \"$@\"~%"
                         (safe-namestring *java*)
                         (safe-namestring *abcl-dir*)
                         (safe-namestring (merge-pathnames "src" *build-root*))
                         (safe-namestring (merge-pathnames "abcl.jar" *build-root*)))
                 ;; Not Linux.
                 (format s "#!/bin/sh~%exec ~A -Xss4M -Xmx256M -cp ~A:~A org.armedbear.lisp.Main \"$@\"~%"
                         (safe-namestring *java*)
                         (safe-namestring (merge-pathnames "src" *build-root*))
                         (safe-namestring (merge-pathnames "abcl.jar" *build-root*)))))
           (run-shell-command (format nil "chmod +x ~A" (safe-namestring pathname))
                              :directory *build-root*)))))

(defun build-stamp ()
  (multiple-value-bind
      (second minute hour date month year day daylight-p zone)
      (decode-universal-time (get-universal-time))
    (declare (ignore daylight-p))
    (setf day (nth day '("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun")))
    (setf month (nth (1- month) '("Jan" "Feb" "Mar" "Apr" "May" "Jun"
                                  "Jul" "Aug" "Sep" "Oct" "Nov" "Dec")))
    (setf zone (* zone 100)) ;; FIXME
    (format nil "~A ~A ~D ~D ~2,'0D:~2,'0D:~2,'0D -~4,'0D"
            day month date year hour minute second zone)))

(defun make-build-stamp ()
  (with-open-file (s
                   (merge-pathnames (make-pathname :name "build"
                                                   :defaults *abcl-dir*))
                   :direction :output
                   :if-exists :supersede)
    (format s "~A" (build-stamp))))

(defun delete-files (pathnames)
  (dolist (pathname pathnames)
    (let ((truename (probe-file pathname)))
      (when truename
        (delete-file truename)))))

(defun clean ()
  (with-current-directory (*build-root*)
    (delete-files (list "abcl.jar")))
  (with-current-directory (*abcl-dir*)
    (delete-files (directory "*.class"))
    (delete-files (directory "*.abcl"))
    (delete-files (directory "*.cls"))
    (delete-files '("native.h" "libabcl.so" "build")))
  (with-current-directory ((merge-pathnames "java/awt/" *abcl-dir*))
    (delete-files (directory "*.class"))))

(defun safe-namestring (pathname)
  (let ((string (namestring pathname)))
    (when (position #\space string)
      (setf string (concatenate 'string "\"" string "\"")))
    string))

(defun build-abcl (&key force
                        (batch t)
                        compile-system
                        jar
                        clean
                        libabcl
                        full)
  (let ((start (get-internal-real-time)))

    #+lispworks
    (when (eq *platform* :windows)
      (setf batch nil))

    (initialize-build)
    (format t "~&Platform: ~A~%"
            (case *platform*
              (:windows "Windows")
              (:linux   "Linux")
              (:darwin  "Mac OS X")
              (t        (software-type))))
    (finish-output)
    ;; clean
    (when clean
      (clean))
    ;; classes
    (unless (make-classes force batch)
      (format t "Build failed.~%")
      (return-from build-abcl nil))
    ;; COMPILE-SYSTEM
    (when (or full compile-system)
      (let* ((zip    (if (or full jar) nil t))
             (status (do-compile-system :zip zip)))
        (unless (zerop status)
          (format t "Build failed.~%")
          (return-from build-abcl nil))))
    ;; abcl.jar
    (when (or full jar)
      (let ((status (make-jar)))
        (unless (zerop status)
          (format t "Build failed.~%")
          (return-from build-abcl nil))))
    ;; libabcl.so
    (when (and (or full libabcl)
               (or (eq *platform* :linux)
                   (search "SunOS" (software-type))
                   (search "FreeBSD" (software-type))))
      ;; A failure here is not fatal.
      (make-libabcl))
    ;; abcl/abcl.bat
    (make-launch-script)
    (make-build-stamp)
    (let ((end (get-internal-real-time)))
      (format t "Build completed successfully in ~A seconds.~%"
              (/ (float (- end start)) internal-time-units-per-second)))
    t))

(defun build-abcl-executable ()
  (let* ((*default-pathname-defaults* *abcl-dir*)
         (source-files (directory "*.java"))
         (cmdline (with-output-to-string (s)
                    (princ "gcj -g -O0 " s)
                    (dolist (source-file source-files)
                      (unless (string= (pathname-name source-file) "Native")
                        (princ (pathname-name source-file) s)
                        (princ ".java" s)
                        (princ #\space s)))
                    (princ "--main=org.armedbear.lisp.Main -o lisp" s)))
         (result (run-shell-command cmdline :directory *abcl-dir*)))
    (zerop result)))

(defvar *copy-verbose* nil)

(defun copy-file (source target)
  (when *copy-verbose*
    (format t "~A -> ~A~%" source target))
  (let ((buffer (make-array 4096 :element-type '(unsigned-byte 8))))
    (with-open-file (in source :direction :input :element-type '(unsigned-byte 8))
      (with-open-file (out target :direction :output :element-type '(unsigned-byte 8)
                           :if-exists :supersede)
        (loop
          (let ((end (read-sequence buffer in)))
            (when (zerop end)
              (return))
            (write-sequence buffer out :end end)))))))

(defun copy-files (files source-dir target-dir)
  (ensure-directories-exist target-dir)
  (dolist (file files)
    (copy-file (merge-pathnames file source-dir)
               (merge-pathnames file target-dir))))

(defun make-dist-dir (version-string)
  (unless (eq *platform* :linux)
    (error "MAKE-DIST is only supported on Linux."))
  (let ((target-root (pathname (concatenate 'string "/var/tmp/" version-string "/"))))
    (when (probe-directory target-root)
      (error "Target directory ~S already exists." target-root))
    (let* ((source-dir *build-root*)
           (target-dir target-root)
           (files (list "README"
                        "COPYING"
                        "build-abcl.lisp"
                        "customizations.lisp"
                        "make-jar.bat.in"
                        "make-jar.in")))
      (copy-files files source-dir target-dir))
    (let* ((source-dir (merge-pathnames "examples/" *build-root*))
           (target-dir (merge-pathnames "examples/" target-root))
           (files '("hello.java")))
      (copy-files files source-dir target-dir))
    (let* ((source-dir (merge-pathnames "src/" *build-root*))
           (target-dir (merge-pathnames "src/" target-root))
           (files '("manifest-abcl")))
      (copy-files files source-dir target-dir))
    (let* ((source-dir *abcl-dir*)
           (target-dir (merge-pathnames "src/org/armedbear/lisp/" target-root))
           (*default-pathname-defaults* source-dir)
           (files (mapcar #'file-namestring (append (directory "*.java")
                                                    (directory "*.lisp")
                                                    (list "LICENSE" "native.c")))))
      (copy-files files source-dir target-dir))
    (let* ((source-dir (merge-pathnames "tests/" *abcl-dir*))
           (target-dir (merge-pathnames "src/org/armedbear/lisp/tests/" target-root))
           (*default-pathname-defaults* source-dir)
           (files (append (mapcar #'file-namestring (directory "*.lisp"))
                          (list "jl-config.cl"))))
      (copy-files files source-dir target-dir))
    (let* ((source-dir (merge-pathnames "java/awt/" *abcl-dir*))
           (target-dir (merge-pathnames "src/org/armedbear/lisp/java/awt/" target-root))
           (*default-pathname-defaults* source-dir)
           (files (mapcar #'file-namestring (directory "*.java"))))
      (copy-files files source-dir target-dir))
    target-root))

(defun make-dist (version-string)
  (let* ((dist-dir (make-dist-dir version-string))
         (parent-dir (merge-pathnames (make-pathname :directory '(:relative :back))
                                      dist-dir)))
    (let* ((command (format nil "tar czf ~A~A.tar.gz ~A"
                            (namestring parent-dir)
                            version-string version-string))
           (status (run-shell-command command :directory parent-dir)))
      (unless (zerop status)
        (format t "~A returned ~S~%" command status)))
    (let* ((command (format nil "zip -q -r ~A~A.zip ~A"
                            (namestring parent-dir)
                            version-string version-string))
           (status (run-shell-command command :directory parent-dir)))
      (unless (zerop status)
        (format t "~A returned ~S~%" command status)))))
