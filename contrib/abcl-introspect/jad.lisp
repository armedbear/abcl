(defpackage :abcl-introspect/jvm/tools/jad
  (:use #:cl)
  (:nicknames #:jvm/tools/jad #:jad)
  (:export
   #:disassemble-class-bytes))

#|

<http://www.javadecompilers.com/jad/Jad%201.5.8g%20for%20Mac%20OS%20X%2010.4.6%20on%20Intel%20platform.zip>

|#

(in-package :abcl-introspect/jvm/tools/jad)

(defun introspect-jad-uri ()
  (uiop:os-cond
   ((uiop/os:os-macosx-p)
    "http://www.javadecompilers.com/jad/Jad%201.5.8g%20for%20Mac%20OS%20X%2010.4.6%20on%20Intel%20platform.zip")))

(defvar *working-jad-path* nil)
  
(defun ensure-jad ()
  (if (null *working-jad-path*)
      (let ((jad-path (abcl-build:introspect-path-for "jad")))
        (if (and jad-path
                 (handler-case
                     (uiop:run-program jad-path)
                   (uiop/run-program:subprocess-error (e) nil)))
            (setf *working-jad-path* jad-path))
        (progn
          (abcl-build:xdg/install (introspect-jad-uri) :type :unzip)
          (setf *working-jad-path* nil)))
      *working-jad-path*))

(defun disassemble-class-bytes (object)
  (ensure-jad)
  (let ((sys:::*disassembler*
          (format nil "~s -a -p" *working-jad-path*)))
    (cl:disassemble object)))
