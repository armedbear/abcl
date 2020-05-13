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
  (let ((sys::*disassembler*
          ;; FIXME: use same java that is hosting ABCL
          ;; !!! unclear options; wants to write output to filesystem
          (format nil "java -cp ~a org.jetbrains.java.decompiler.main.decompiler.BaseDecompiler"
                  (fernflower-classpath))))
    (sys:disassemble-class-bytes object)))

(eval-when (:load-toplevel :execute)
  (pushnew `(:fernflower . abcl-introspect/jvm/tools/fernflower::disassemble-class-bytes)
           sys::*disassemblers*)
  (format cl:*load-verbose* "~&; ~a: Successfully added fernflower disassembler.~%" *package*))


