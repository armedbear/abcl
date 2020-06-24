(defpackage :abcl-introspect/jvm/tools/cfr
  (:use :cl)
  (:export
   #:disassemble-class-bytes))
(in-package :abcl-introspect/jvm/tools/cfr)

(defun cfr-jar-pathname ()
  ;; Very ugly.  How to make more intelligble?
  (slot-value (first (asdf:component-children (asdf:find-component  :cfr "mvn-libs")))
              'asdf/interface::resolved-classpath))

(defun disassemble-class-bytes (object)
  (let ((sys::*disassembler*
          ;; FIXME: use same java that is hosting ABCL
          (format nil "java -jar ~a" (cfr-jar-pathname))))
    (sys:disassemble-class-bytes object)))

(eval-when (:load-toplevel :execute)
  (pushnew `(:cfr . abcl-introspect/jvm/tools/cfr::disassemble-class-bytes)
           sys::*disassemblers*)
  (format cl:*load-verbose* "~&; ~a: Successfully added cfr disassembler.~%" *package*))



