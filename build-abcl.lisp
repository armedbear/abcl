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

(defun comp (string char)
  "Chops off the character at the end of `string' if it matches char"
  (let ((len (length string)))
    (if (eql char (char string (1- len)))
        (subseq string 0 (1- len))
        string)))

(defun safe-namestring (pathname)
  (let ((string (namestring pathname)))
    (when (position #\space string)
      (setf string (concatenate 'string "\""
                                (comp string #\\)
                                "\"")))
    string))


(defun child-pathname (pathname parent)
  "Returns `pathname' relative to `parent', assuming that it
is infact a child of it while being rooted at the same root as `parent'."
  (let ((path-dir (pathname-directory pathname))
        (parent-dir (pathname-directory parent)))
    (do ((p1 path-dir (cdr p1))
         (p2 parent-dir (cdr p2)))
        ((or (endp p2) (not (equal (car p1) (car p2))))
         (when (endp p2)
           (make-pathname :directory (cons :relative p1)
                          :defaults pathname))))))


(defun file-newer (orig artifact)
  "Compares file date/time of `orig' and `artifact', returning
`NIL' if `orig' is newer than `artifact'."
  (or (null (probe-file artifact))
      (> (file-write-date orig)
         (file-write-date artifact))))



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
        ((equal 0 (ext:run-shell-command "uname | grep -i darwin" :output nil))
         :darwin)
        ((equal 0 (ext:run-shell-command "uname | grep -i linux" :output nil))
         :linux)
        (t
         :unknown)))

(defparameter *platform* (platform))

(defparameter *file-separator-char*
  (if (eq *platform* :windows) #\\ #\/))

(defparameter *path-separator-char*
  (if (eq *platform* :windows) #\; #\:))


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
          ((or (eq status t) (null status)) ;; clisp 2.47 returns NIL on success
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

#+ecl 
(defun run-shell-command (command &key directory (output *standard-output*))
  (when directory
    (if (member :windows *features*)
        (error "Unimplemented.")
        (setf command (concatenate 'string
                                   "\\cd \""
                                   (namestring (pathname directory))
                                   "\" && "
                                   command))))
  (ext:system command))
  ;; (multiple-value-bind (stream exit details)
  ;;     (ext:run-program 
  ;;      "/bin/sh" (list "-c" command)
  ;;      :input nil :output :stream :error :output)
  ;;   (declare (ignore details))
  ;;   (loop for line = (read-line stream nil)
  ;;      while line do (format output "~A~%" line))
  ;;   exit))


#+(or sbcl cmu lispworks openmcl ecl)
(defun probe-directory (pathspec)
  (let* ((truename (probe-file pathspec)) ; TRUENAME is a pathname.
         (namestring (and truename (namestring truename)))) ; NAMESTRING is a string.
    (and namestring
         (> (length namestring) 0)
         (eql (char namestring (1- (length namestring))) *file-separator-char*)
         truename)))

(defparameter *tree-root*
  (make-pathname :device (pathname-device *load-truename*)
                 :directory (pathname-directory *load-truename*)))
(defparameter *build-root*
  (merge-pathnames "build/classes/" *tree-root*))
(defparameter *source-root*
  (merge-pathnames "src/" *tree-root*))
(defparameter *dist-root*
  (merge-pathnames "dist/" *tree-root*))


(defparameter *customizations-file*
  (merge-pathnames "customizations.lisp" *tree-root*))

(defparameter *abcl-dir*
  (merge-pathnames "src/org/armedbear/lisp/" *tree-root*))

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
  (let ((classpath-components (list *source-root*
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
    (loop named replace
         for index = (search (car entry) string :test #'string=)
         do
         (unless index
           (return-from replace))
         (setf string (concatenate 'string
                                   (subseq string 0 index)
                                   (cdr entry)
                                   (subseq string (+ index (length (car entry))))))))
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
               " -d "
               (safe-namestring *build-root*)
               " "
               (namestring source-file)))

(defun java-compile-file (source-file)
  (let ((cmdline (build-javac-command-line source-file)))
    (equal 0 (run-shell-command cmdline :directory *abcl-dir*))))

(defun do-compile-classes (force batch)
  (let* ((source-files
          (remove-if-not
           #'(lambda (name)
               (let ((output-name
                      (merge-pathnames
                       (make-pathname :type "class"
                                      :defaults (child-pathname name
                                                                *source-root*))
                       *build-root*)))
                 (or force
                     (file-newer name output-name))))
           (directory (merge-pathnames "**/*.java" *source-root*)))))
    (format t "~&JDK: ~A~%" *jdk*)
    (format t "Java compiler: ~A~%" *java-compiler*)
    (format t "Compiler options: ~A~%~%" (if *java-compiler-options* *java-compiler-options* ""))
    (format t "~&Compiling Java sources...")
    (finish-output)
    (cond ((null source-files)
           (format t "Classes are up to date.~%")
           (finish-output)
           t)
          (t
           (cond (batch
                  (ensure-directories-exist *build-root*)
                  (let* ((cmdline (with-output-to-string (s)
                                    (princ *java-compiler-command-line-prefix* s)
                                    (princ " -d " s)
                                    (princ (safe-namestring *build-root*) s)
                                    (princ #\Space s)
                                    (dolist (source-file source-files)
                                      (princ (safe-namestring (namestring source-file)) s)
                                      (princ #\space s))))
                         (status (run-shell-command cmdline :directory *tree-root*)))
                    (format t "  done.~%")
                    (equal 0 status)))
                 (t
                  (ensure-directories-exist *build-root*)
                  (dolist (source-file source-files t)
                    (unless (java-compile-file (safe-namestring source-file))
                      (format t "Build failed.~%")
                      (return nil)))))))))

(defun make-jar ()
  (let ((*default-pathname-defaults* *tree-root*)
        (jar-namestring (namestring *jar*)))
    (when (position #\space jar-namestring)
      (setf jar-namestring (concatenate 'string "\"" jar-namestring "\"")))
    (let ((substitutions-alist (acons "@JAR@" jar-namestring nil))
          (source-file (if (eq *platform* :windows) "make-jar.bat.in" "make-jar.in"))
          (target-file (if (eq *platform* :windows) "make-jar.bat"    "make-jar"))
          (command     (if (eq *platform* :windows) "make-jar.bat"    "sh make-jar")))
      (copy-with-substitutions source-file target-file substitutions-alist)
      (ensure-directories-exist *dist-root*)
      (let ((status (run-shell-command command :directory *tree-root*)))
        (unless (equal 0 status)
          (format t "~A returned ~S~%" command status))
        status))))

(defun do-compile-system (&key (zip t))
  (format t "~&Compiling Lisp sources...")
  (terpri)
  (finish-output)
  (let* ((java-namestring (safe-namestring *java*))
         status
         (abcl-home (substitute-in-string
                     (namestring *abcl-dir*)
                     (when (eq *platform* :windows)
                       '(("\\" . "/")
                         ("/" . "\\\\")))))
         (output-path (substitute-in-string
                       (namestring
                        (merge-pathnames "build/classes/org/armedbear/lisp/"
                                         *tree-root*))
                       (when (eq *platform* :windows)
                         '(("\\" . "/")))))
         (cmdline (format nil
                          "~A -cp build/classes -Dabcl.home=\"~A\" ~
org.armedbear.lisp.Main --noinit --nosystem ~
--eval \"(compile-system :zip ~A :quit t :output-path \\\"~A\\\")\"~%"
                          java-namestring
                          abcl-home
                          (not (not zip)) ;; because that ensures T or NIL
                          output-path)))
    (ensure-directories-exist output-path)
    (setf status (run-shell-command cmdline :directory *tree-root*))
    (format t " done.~%")
    status))


;; abcl/abcl.bat
(defun make-launch-script ()
  ;; Use the -Xss4M and -Xmx256M flags so that the default launch script can be
  ;; used to build sbcl.
  (cond ((eq *platform* :windows)
         (with-open-file (s
                          (merge-pathnames "abcl.bat" *tree-root*)
                          :direction :output
                          :if-exists :supersede)
           (format s "~A -Xss4M -Xmx256M -cp \"~A\" org.armedbear.lisp.Main %1 %2 %3 %4 %5 %6 %7 %8 %9~%"
                   (safe-namestring *java*)
                   (namestring (merge-pathnames "dist\\abcl.jar" *tree-root*)))))
        (t
         (let ((pathname (merge-pathnames "abcl" *tree-root*)))
           (with-open-file (s pathname :direction :output :if-exists :supersede)
             (format s "#!/bin/sh~%exec ~A -Xss4M -Xmx256M -cp ~A org.armedbear.lisp.Main \"$@\"~%"
                     (safe-namestring *java*)
                     (safe-namestring (merge-pathnames "abcl.jar" *dist-root*))))
           (run-shell-command (format nil "chmod +x ~A" (safe-namestring pathname))
                              :directory *tree-root*)))))

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
  (format t "~&Cleaning compilation results."
  (dolist (f (list (list *tree-root* "abcl.jar" "abcl.bat" "make-jar.bat"
                         "compile-system.bat")
                   ;; as of 0.14 'compile-system.bat' isn't created anymore
                   ;; as of 0.14 'abcl.jar' is always created in dist/
                   (list *abcl-dir* "*.class" "*.abcl" "*.cls"
                                    "native.h" "libabcl.so" "build")
                   ;; as of 0.14, native.h and libabcl.so have been removed
                   (list (merge-pathnames "util/" *abcl-dir*) "*.class")
                   (list (merge-pathnames "build/classes/org/armedbear/lisp/"
                                          *tree-root*)
                                    "*.class" "*.abcl" "*.cls"
                                    "native.h" "libabcl.so" "build")
                   (list (merge-pathnames
                          "build/classes/org/armedbear/lisp/util/"
                          *tree-root*)
                                    "*.class" "*.abcl" "*.cls")
                   (list *dist-root* "*.jar" "*.class" "*.abcl" "*.cls")
                  (list (merge-pathnames "java/awt/" *abcl-dir*)
                         "*.class")))
    (let ((default (car f)))
      (when (probe-directory default)
        (delete-files (mapcan #'(lambda (name)
                                  (directory (merge-pathnames name default)))
                              (cdr f)))))))

(defun build-abcl (&key force
                        (batch t)
                        compile-system
                        jar
                        clean
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
    ;; Compile Java source into classes
    (unless (do-compile-classes force batch)
      (format t "Build failed.~%")
      (return-from build-abcl nil))
    ;; COMPILE-SYSTEM
    (when (or full compile-system)
      (let* ((zip    (if (or full jar) nil t))
             (status (do-compile-system :zip zip)))
        (unless (equal 0 status)
          (format t "Build failed.~%")
          (return-from build-abcl nil))))
    ;; abcl.jar
    (when (or full jar)
      (let ((status (make-jar)))
        (unless (equal 0 status)
          (format t "Build failed.~%")
          (return-from build-abcl nil))))
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
    (equal 0 result)))

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
    (let* ((source-dir *tree-root*)
           (target-dir target-root)
           (files (list "README"
                        "COPYING"
                        "build-abcl.lisp"
                        "customizations.lisp"
                        "make-jar.bat.in"
                        "make-jar.in")))
      (copy-files files source-dir target-dir))
    (let* ((source-dir (merge-pathnames "examples/" *tree-root*))
           (target-dir (merge-pathnames "examples/" target-root))
           (files '("hello.java")))
      (copy-files files source-dir target-dir))
    (let* ((target-dir (merge-pathnames "src/" target-root))
           (files '("manifest-abcl")))
      (copy-files files *source-root* target-dir))
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
      (unless (equal 0 status)
        (format t "~A returned ~S~%" command status)))
    (let* ((command (format nil "zip -q -r ~A~A.zip ~A"
                            (namestring parent-dir)
                            version-string version-string))
           (status (run-shell-command command :directory parent-dir)))
      (unless (equal 0 status)
        (format t "~A returned ~S~%" command status)))))
