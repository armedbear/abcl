(in-package :cl-user)
(defpackage :abcl/build/jvm/tools/procyon
    (:use :cl)
    (:export
     #:disassemble-class-bytes))

(defun disassemble-class-bytes (object)
  (error "Unimplemented use of procyon dissassembler."))
