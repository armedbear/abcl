(defpackage :abcl-introspect/jvm/tools/javap
  (:use #:cl)
  (:export
   #:disassemble-class-bytes))

;; use javap
#|
(let ((sys::*disassembler* "javap -c -verbose"))
  (disassemble 'cons))
|#


(in-package :abcl-introspect/jvm/tools/javap)


(defun disassemble-class-bytes (object)
  (let ((sys::*disassembler* "javap -c -verbose"))
    (disassemble object)))


(eval-when (:load-toplevel :execute)
  (pushnew `(:javap . abcl-introspect/jvm/tools/javap::disassemble-class-bytes)
           sys::*disassemblers*)
  (format cl:*load-verbose* "~&; ~a ; Successfully added javap disassembler.~%" *package*))







