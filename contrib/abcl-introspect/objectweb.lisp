(in-package :cl-user)
(defpackage :abcl/build/jvm/tools/objectweb
  (:use :cl)
  (:export
   #:disassemble-class-bytes))
(in-package :abcl/build/jvm/tools/objectweb)

(defun disassemble-class-bytes (object)
  (let* ((reader (java:jnew "org.objectweb.asm.ClassReader" object))
         (writer (java:jnew "java.io.StringWriter"))
         (printer (java:jnew "java.io.PrintWriter" writer))
         (tracer (java:jnew "org.objectweb.asm.util.TraceClassVisitor" java:+null+ printer))
         ;; this is to support both the 1.X and subsequent releases
         (flags (ignore-errors (java:jfield "org.objectweb.asm.ClassReader" "SKIP_DEBUG"))))
    (java:jcall-raw "accept" reader tracer (or flags java:+false+))
    (java:jcall "toString" writer)))

(eval-when (:load-toplevel :execute)
  (pushnew `(:objectweb . abcl/build/jvm/tools/objectweb::disassemble-class-bytes)
           sys::*disassemblers*)
  (format cl:*load-verbose* "~&; ~a ; Successfully added Objectweb disassembler.~%" *package*))

