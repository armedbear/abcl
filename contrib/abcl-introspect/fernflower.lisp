(defpackage :abcl-introspect/jvm/tools/fernflower
  (:use :cl)
  (:export
   #:disassemble-class-bytes))
(in-package :abcl-introspect/jvm/tools/fernflower)

(defun fernflower-classpath ()
  ;; Very ugly.  How to make more intelligble?
  (slot-value (first (asdf:component-children (asdf:find-component  :fernflower "mvn-libs")))
              'asdf/interface::resolved-classpath))

(defun disassemble-class-bytes (object)
  (uiop/stream::with-temporary-file (:pathname p :type "class")
    (ext::write-class object p)
    (let* ((directory
             (namestring (truename (make-pathname :directory (pathname-directory p)))))
	   (path
             (namestring (truename p)))
           (command 
	     (format nil "java -cp ~a org.jetbrains.java.decompiler.main.decompiler.ConsoleDecompiler ~a ~a"
                     (fernflower-classpath) p directory))
           (output
             (namestring (make-pathname :defaults p :type "java"))))
      (uiop:run-program command)
      (let ((result (alexandria:read-file-into-string output)))
        (sys::print-lines-with-prefix result)))))

(eval-when (:load-toplevel :execute)
  (pushnew `(:fernflower . abcl-introspect/jvm/tools/fernflower::disassemble-class-bytes)
           sys::*disassemblers*)
  (format cl:*load-verbose* "~&; ~a: Successfully added fernflower decompiler.~%" *package*))


