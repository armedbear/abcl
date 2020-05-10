(defpackage :abcl-introspect/jvm/tools/javap
  (:use #:cl)
  (:export
   #:disassemble-class-bytes))

;;;; JDK javap <https://docs.oracle.com/javase/7/docs/technotes/tools/windows/javap.html>
#|
(let ((sys::*disassembler* "javap -c -verbose"))
  (disassemble 'cons))
|#

(in-package :abcl-introspect/jvm/tools/javap)

(defun disassemble-class-bytes (object)
  (let ((sys::*disassembler* "javap -c -verbose"))
    (sys:disassemble-class-bytes object)))

(eval-when (:load-toplevel :execute)
  (pushnew `(:javap . abcl-introspect/jvm/tools/javap::disassemble-class-bytes)
           sys::*disassemblers*)
  (format cl:*load-verbose* "~&; ~a ; Successfully added javap disassembler.~%" *package*))







