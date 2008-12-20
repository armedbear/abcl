;;; dump-class.lisp
;;;
;;; Copyright (C) 2003-2005 Peter Graves
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

(require '#:opcodes)

(in-package #:jvm)

(defvar *pool* nil)

(defun read-u1 (stream)
  (read-byte stream))

(defun read-u2 (stream)
  (+ (ash (read-byte stream) 8) (read-byte stream)))

(defun read-u4 (stream)
  (+ (ash (read-u2 stream) 16) (read-u2 stream)))

(defun lookup-utf8 (index)
  (let ((entry (svref *pool* index)))
    (when (eql (car entry) 1)
      (caddr entry))))

(defun read-constant-pool-entry (stream)
  (let ((tag (read-u1 stream)))
    (case tag
      ((7 8)
       (list tag (read-u2 stream)))
      (1
       (let* ((len (read-u2 stream))
              (s (make-string len)))
         (dotimes (i len)
           (setf (char s i) (code-char (read-u1 stream))))
         (list tag len s)))
      ((3 4)
       (list tag (read-u4 stream)))
      ((5 6)
       (list tag (read-u4 stream) (read-u4 stream)))
      ((12 9 10 11)
       (list tag (read-u2 stream) (read-u2 stream)))
      (t
       (error "READ-CONSTANT-POOL-ENTRY unhandled tag ~D" tag)))))

(defvar *indent* 0)

(defparameter *spaces* (make-string 256 :initial-element #\space))

(defmacro out (&rest args)
  `(progn (format t (subseq *spaces* 0 *indent*)) (format t ,@args)))

(defun dump-code (code)
  (let ((code-length (length code)))
    (do ((i 0))
        ((>= i code-length))
      (let* ((opcode (svref code i))
             (size (opcode-size opcode)))
        (out "~D: ~D (#x~X) ~A~%" i opcode opcode (opcode-name opcode))
        (incf i)
        (dotimes (j (1- size))
          (let ((byte (svref code i)))
            (out "~D: ~D (#x~X)~%" i byte byte))
          (incf i))))))

(defun dump-code-attribute (stream)
  (let ((*indent* (+ *indent* 2)))
    (out "Stack: ~D~%" (read-u2 stream))
    (out "Locals: ~D~%" (read-u2 stream))
    (let* ((code-length (read-u4 stream))
           (code (make-array code-length)))
      (out "Code length: ~D~%" code-length)
      (out "Code:~%")
      (dotimes (i code-length)
        (setf (svref code i) (read-u1 stream)))
      (let ((*indent* (+ *indent* 2)))
        (dump-code code)))
    (let ((exception-table-length (read-u2 stream)))
      (out "Exception table length: ~D~%" exception-table-length)
      (let ((*indent* (+ *indent* 2)))
        (dotimes (i exception-table-length)
          (out "Start PC: ~D~%" (read-u2 stream))
          (out "End PC: ~D~%" (read-u2 stream))
          (out "Handler PC: ~D~%" (read-u2 stream))
          (out "Catch type: ~D~%" (read-u2 stream)))))
    (let ((attributes-count (read-u2 stream)))
      (out "Number of attributes: ~D~%" attributes-count)
      (let ((*indent* (+ *indent* 2)))
        (dotimes (i attributes-count)
          (read-attribute i stream))))))

(defun dump-exceptions (stream)
  (declare (ignore stream))
  )

(defun read-attribute (index stream)
  (let* ((name-index (read-u2 stream))
         (name (lookup-utf8 name-index))
         (length (read-u4 stream))
         (*indent* (+ *indent* 2)))
    (out "Attribute ~D: Name index: ~D (~S)~%" index name-index name)
    (out "Attribute ~D: Length: ~D~%" index length)
    (cond ((string= name "Code")
           (dump-code-attribute stream))
          ((string= name "Exceptions")
           (let ((count (read-u2 stream)))
             (out "Attribute ~D: Number of exceptions: ~D~%" index count)
             (let ((*indent* (+ *indent* 2)))
               (dotimes (i count)
                 (out "Exception ~D: ~D~%" i (read-u2 stream))))))
          ((string= name "SourceFile")
           (let ((source-file-index (read-u2 stream)))
             (out "Attribute ~D: Source file index: ~D (~S)~%"
                  index source-file-index (lookup-utf8 source-file-index))))
          (t
           (dotimes (i length)
             (read-u1 stream))))))

(defun read-info (index stream type)
  (let* ((access-flags (read-u2 stream))
         (name-index (read-u2 stream))
         (descriptor-index (read-u2 stream))
         (attributes-count (read-u2 stream))
         (*indent* (+ *indent* 2))
         (type (case type
                 ('field "Field")
                 ('method "Method"))))
    (out "~A ~D: Access flags: #x~X~%" type index access-flags)
    (out "~A ~D: Name index: ~D (~S)~%" type index name-index (lookup-utf8 name-index))
    (out "~A ~D: Descriptor index: ~D~%" type index descriptor-index)
    (out "~A ~D: Number of attributes: ~D~%" type index attributes-count)
    (let ((*indent* (+ *indent* 2)))
      (dotimes (i attributes-count)
        (read-attribute i stream)))))

(defun dump-class (filename)
  (let ((*indent* 0)
        (*pool* nil))
    (with-open-file (stream filename :direction :input :element-type 'unsigned-byte)
      (handler-bind ((end-of-file
                      #'(lambda (c) (return-from dump-class c))))
        (out "Magic number: #x~X~%" (read-u4 stream))
        (let ((minor (read-u2 stream))
              (major (read-u2 stream)))
          (out "Version: ~D.~D~%" major minor))
        ;; Constant pool.
        (let ((count (read-u2 stream))
              entry type)
          (out "Constant pool (~D entries):~%" count)
          (setq *pool* (make-array count))
          (let ((*indent* (+ *indent* 2)))
            (dotimes (index (1- count))
              (setq entry (read-constant-pool-entry stream))
              (setf (svref *pool* (1+ index)) entry)
              (setq type (case (car entry)
                           (7 'class)
                           (9 'field)
                           (10 'method)
                           (11 'interface)
                           (8 'string)
                           (3 'integer)
                           (4 'float)
                           (5 'long)
                           (6 'double)
                           (12 'name-and-type)
                           (1 'utf8)))
              (out "~D: ~A ~S~%" (1+ index) type entry))))
        (out "Access flags: #x~X~%" (read-u2 stream))
        (out "This class: ~D~%" (read-u2 stream))
        (out "Superclass: ~D~%" (read-u2 stream))
        ;; Interfaces.
        (let ((count (read-u2 stream)))
          (cond ((zerop count)
                 (out "No interfaces~%"))
                (t
                 (out "Interfaces (~D):~%" count)
                 (dotimes (i count)
                   (out "  ~D: ~D~%" i (read-u2 stream))))))
        ;; Fields.
        (let ((count (read-u2 stream)))
          (cond ((zerop count)
                 (out "No fields~%"))
                (t
                 (out "Fields (~D):~%" count)))
          (dotimes (index count)
            (read-info index stream 'field)))
        ;; Methods.
        (let ((count (read-u2 stream)))
          (cond ((zerop count)
                 (out "No methods~%"))
                (t
                 (out "Methods (~D):~%" count)))
          (dotimes (index count)
            (read-info index stream 'method)))
        ;; Attributes.
        (let ((count (read-u2 stream)))
          (cond ((zerop count)
                 (out "No attributes~%"))
                (t
                 (out "Attributes (~D):~%" count)))
          (dotimes (index count)
            (read-attribute index stream))))))
  t)
