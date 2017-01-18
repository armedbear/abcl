;;; describe.lisp
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
(require '#:format)

(defun describe-arglist (object stream)
  (multiple-value-bind
      (arglist known-p)
      (arglist object)
    (when known-p
      (format stream "~&The function's lambda list is:~%  ~A~%" arglist))))

(defun %describe-object (object stream)
  (format stream "~S is an object of type ~S.~%" object (type-of object)))

(defun describe (object &optional stream)
  (describe-object object (out-synonym-of stream))
  (values))

(defmethod describe-object ((object t) stream)
  (let ((*print-pretty* t))
    (typecase object
      (SYMBOL
       (let ((package (symbol-package object)))
         (if package
             (multiple-value-bind
                 (sym status)
                 (find-symbol (symbol-name object) package)
               (format stream "~S is an ~A symbol in the ~A package.~%"
                       object
                       (if (eq status :internal) "internal" "external")
                       (package-name package)))
             (format stream "~S is an uninterned symbol.~%" object))
         (cond ((special-variable-p object)
                (format stream "It is a ~A; "
                        (if (constantp object) "constant" "special variable"))
                (if (boundp object)
                    (format stream "its value is ~S.~%" (symbol-value object))
                    (format stream "it is unbound.~%")))
               ((boundp object)
                (format stream "It is an undefined variable; its value is ~S.~%"
                        (symbol-value object)))))
       (when (autoloadp object)
         (resolve object))
       (let ((function (and (fboundp object) (symbol-function object))))
         (when function
           (format stream "Its function binding is ~S.~%" function)
           (describe-arglist function stream)))
       (let ((doc (documentation object 'function)))
         (when doc
           (format stream "Function documentation:~%  ~A~%" doc)))
       (let ((doc (documentation object 'variable)))
         (when doc
           (format stream "Variable documentation:~%  ~A~%" doc)))
       (let ((plist (symbol-plist object)))
         (when plist
           (format stream "The symbol's property list contains these indicator/value pairs:~%")
           (loop
             (when (null plist) (return))
             (format stream "  ~S ~S~%" (car plist) (cadr plist))
             (setf plist (cddr plist))))))
      (FUNCTION
       (%describe-object object stream)
       (describe-arglist object stream)
       (let ((function-symbol (nth-value 2 (function-lambda-expression object))))
	 (if (and (consp function-symbol) (eq (car function-symbol) 'macro-function))
	     (setq function-symbol (second function-symbol)))
	 (when  function-symbol
	   (let ((doc (documentation function-symbol 'function)))
	     (when doc
	       (format stream "Function documentation:~%  ~A~%" doc)))
	   )))
      (INTEGER
       (%describe-object object stream)
       (format stream "~D.~%~
                       #x~X~%~
                       #o~O~%~
                       #b~B~%"
               object object object object))
      (t
       (%describe-object object stream))))
  (values))

(defmethod describe-object ((object pathname) stream)
  (format stream "~S is an object of type ~S:~%" object (type-of object))
  (format stream "  HOST         ~S~%" (pathname-host object))
  (format stream "  DEVICE       ~S~%" (pathname-device object))
  (format stream "  DIRECTORY    ~S~%" (pathname-directory object))
  (format stream "  NAME         ~S~%" (pathname-name object))
  (format stream "  TYPE         ~S~%" (pathname-type object))
  (format stream "  VERSION      ~S~%" (pathname-version object)))

(defun %describe-standard-object/funcallable (object stream)
  (let* ((class (class-of object))
         (slotds (mop:class-slots class))
         (max-slot-name-length 0)
         (instance-slotds ())
         (class-slotds ()))
    (format stream "~S is an instance of ~S.~%" object class)
    (dolist (slotd slotds)
      (let* ((name (mop:slot-definition-name slotd))
             (length (length (symbol-name name))))
        (when (> length max-slot-name-length)
          (setf max-slot-name-length length)))
      (case (mop:slot-definition-allocation slotd)
        (:instance (push slotd instance-slotds))
        (:class  (push slotd class-slotds))))
    (setf max-slot-name-length  (min (+ max-slot-name-length 3) 30))
    (flet ((describe-slot (slot-name)
             (if (slot-boundp object slot-name)
                 (format stream
                         "~&  ~A~VT  ~S"
                         slot-name max-slot-name-length (slot-value object slot-name))
                 (format stream
                         "~&  ~A~VT  unbound"
                         slot-name max-slot-name-length))))
      (when instance-slotds
        (format stream "The following slots have :INSTANCE allocation:~%")
        (dolist (slotd (nreverse instance-slotds))
          (describe-slot
           (mop:slot-definition-name slotd))))
        (format stream "~%")
      (when class-slotds
        (format stream "The following slots have :CLASS allocation:~%")
        (dolist (slotd (nreverse class-slotds))
          (describe-slot
           (mop:slot-definition-name slotd)))
        (format stream "~%")))))

(defmethod describe-object ((object standard-object) stream)
  (%describe-standard-object/funcallable object stream)
  (values))

(defmethod describe-object ((object mop:funcallable-standard-object) stream)
  (%describe-standard-object/funcallable object stream)
  (values))

(defmethod describe-object ((object java:java-object) stream)
  (java:describe-java-object object stream))
