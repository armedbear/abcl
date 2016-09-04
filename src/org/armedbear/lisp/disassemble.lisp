;;; disassemble.lisp
;;;
;;; Copyright (C) 2005 Peter Graves
;;; $Id$
;;;
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License
;;; as published by the Free Software Foundation; either version 2
;;; of the License, or (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
;;;
;;; As a special exception, the copyright holders of this library give you
;;; permission to link this library with independent modules to produce an
;;; executable, regardless of the license terms of these independent
;;; modules, and to copy and distribute the resulting executable under
;;; terms of your choice, provided that you also meet, for each linked
;;; independent module, the terms and conditions of the license of that
;;; module.  An independent module is a module which is not derived from
;;; or based on this library.  If you modify this library, you may extend
;;; this exception to your version of the library, but you are not
;;; obligated to do so.  If you do not wish to do so, delete this
;;; exception statement from your version.

(in-package #:system)

(require '#:clos)

(defvar *disassembler-function* NIL)

(defvar *disassemblers*
  `((:objectweb . objectweb-test)
    (:external . external-test)))

(defun choose-disassembler (&optional name)
  (setf *disassembler-function*
        (if name
            (or (funcall (cdr (assoc name *disassemblers*)))
                (error "Can't find suitable disassembler."))
            (loop
              for (NIL . test) in *disassemblers*
              for result = (funcall test)
              when result
                do (return result)
              finally (warn "Can't find suitable disassembler.")))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro with-open ((name value) &body body)
    `(let ((,name ,value))
       (unwind-protect
           (progn ,@body)
         (java:jcall-raw "close" ,name)))))

(defun read-byte-array-from-stream (stream)
  (let ((buffer (java:jnew-array (java:jclass "byte") 4096)))
    (with-open (output (java:jnew "java.io.ByteArrayOutputStream"))
      (loop
        for length = (java:jcall "read" stream buffer)
        until (eql length -1)
        do (java:jcall-raw "write" output buffer 0 length))
      (java:jcall-raw "flush" output)
      (java:jcall-raw "toByteArray" output))))

(defun class-resource-path (class)
  (format NIL "~A.class" (substitute #\/ #\. (java:jcall "getName" class))))

(defun class-bytes (class)
  (with-open (stream (java:jcall-raw
                      "getResourceAsStream"
                      (java:jcall-raw "getClassLoader" class)
                      (class-resource-path class)))
    (read-byte-array-from-stream stream)))

(defun disassemble (arg)
  (require-type arg '(OR FUNCTION
                      SYMBOL
                      (CONS (EQL SETF) (CONS SYMBOL NULL))
                      (CONS (EQL LAMBDA) LIST)))
  (let ((function (cond ((functionp arg)
                         arg)
                        ((symbolp arg)
                         (or (macro-function arg) (symbol-function arg))))))
    (when (typep function 'generic-function)
      (setf function (mop::funcallable-instance-function function)))
    (when (functionp function)
      (unless (compiled-function-p function)
        (setf function (compile nil function)))
      (let ((class-bytes (or (function-class-bytes function)
                             (class-bytes (java:jcall-raw "getClass" function)))))
        (if class-bytes
            (let ((disassembler (or *disassembler-function*
                                    (choose-disassembler))))
              (and disassembler (funcall disassembler class-bytes)))
            (%format t "; Disassembly is not available.~%"))))))

(defun print-lines-with-prefix (string)
  (with-input-from-string (stream string)
    (loop
      (let ((line (read-line stream nil)))
        (unless line (return))
        (write-string "; ")
        (write-string line)
        (terpri)))))

(defun external-disassemble (object)
  (print-lines-with-prefix (disassemble-class-bytes object)))

(defun external-test ()
  (ignore-errors
    (and (disassemble-class-bytes #'cons) #'external-disassemble)))

(defun objectweb-disassemble (object)
  (let* ((reader (java:jnew "org.objectweb.asm.ClassReader" object))
         (writer (java:jnew "java.io.StringWriter"))
         (printer (java:jnew "java.io.PrintWriter" writer))
         (tracer (java:jnew "org.objectweb.asm.util.TraceClassVisitor" java:+null+ printer))
         ;; this is to support both the 1.X and subsequent releases
         (flags (ignore-errors (java:jfield "org.objectweb.asm.ClassReader" "SKIP_DEBUG"))))
    (java:jcall-raw "accept" reader tracer (or flags java:+false+))
    (print-lines-with-prefix (java:jcall "toString" writer))))

(defun objectweb-test ()
  (ignore-errors
    (and (java:jclass "org.objectweb.asm.ClassReader") #'objectweb-disassemble)))
