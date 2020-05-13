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

(defvar *working-jad-executable* nil)
  
(defun ensure-jad ()
  (flet
      ((install-jad-returning-path (uri)
         (abcl-build:xdg/install (pathname uri) :type :unzip))
       (working-jad-p (jad-path)
         (handler-case
             (uiop:run-program jad-path)
           (uiop/run-program:subprocess-error (e) nil))))
      (if (null *working-jad-executable*)
          (let ((jad-path (abcl-build:introspect-path-for "jad")))
            (if (and jad-path
                     (working-jad-p jad-path))
                (setf *working-jad-executable* jad-path)
                (progn
                  (install-jad-returning-path (introspect-jad-uri))
                  (setf *working-jad-executable* jad-path))))
          (unless (working-jad-p *working-jad-executable*)
            (setf *working-jad-executable*
                  (install-jad-returning-path (introspect-jad-uri)))))))
          
(defun disassemble-class-bytes (object)
  (ensure-jad)
  (let ((sys::*disassembler*
          (format nil "~s -a -p" *working-jad-executable*)))
    (sys:disassemble-class-bytes object)))

