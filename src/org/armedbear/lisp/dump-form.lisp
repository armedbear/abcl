;;; dump-form.lisp
;;;
;;; Copyright (C) 2004-2007 Peter Graves <peter@armedbear.org>
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
;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
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

(in-package "SYSTEM")

(export '(dump-form dump-uninterned-symbol-index))

(declaim (special *circularity* *circle-counter* *instance-forms*))


(defun get-instance-form (object)
  (multiple-value-bind
        (value presence)
      (gethash object *instance-forms*)
    (cond
      (presence value)
      (t
       (multiple-value-bind (creation-form initialization-form)
           (make-load-form object)
         (if initialization-form
             (let* ((instance (gensym))
                    load-form)
               (setf initialization-form
                     (subst instance object initialization-form))
               (setf initialization-form
                     (subst instance (list 'quote instance) initialization-form
                            :test #'equal))
               (setf load-form `(progn
                                  (let ((,instance ,creation-form))
                                    ,initialization-form
                                    ,instance)))
               (setf (gethash object *instance-forms*) load-form))
             (setf (gethash object *instance-forms*) creation-form)))))))

(defun df-register-circularity (object)
  (setf (gethash object *circularity*)
        (if (gethash object *circularity*)
            :circular
            t)))

(defun df-check-cons (object)
  (loop
     (df-check-object (car object))
     (setf object (cdr object))
     (when (atom object)
       (df-check-object object)
       (return))
     (when (null object)
       (return-from df-check-cons))
     (df-register-circularity object)))

(defun df-check-vector (object)
  (dotimes (index (length object))
    (df-check-object (aref object index))))

(defun df-check-instance (object)
  (df-check-object (get-instance-form object)))

(defun df-check-object (object)
  (unless (eq :circular (df-register-circularity object))
    (cond
      ((consp object) (df-check-cons object))
      ((vectorp object) (df-check-vector object))
      ((or (structure-object-p object)
           (standard-object-p object)
           (java:java-object-p object))
       (df-check-instance object)))))

(defun df-handle-circularity (object stream within-list)
  (let ((index (gethash object *circularity*)))
    (cond
      ((eq index :circular)
       (setf index
             (incf *circle-counter*))
       (setf (gethash object *circularity*) index)
       (when within-list
         (write-string " . " stream))
       (%stream-write-char #\# stream)
       (write index :stream stream)
       (%stream-write-char #\= stream)
       (when within-list
         (dump-cons object stream)  ;; ### *cough*
         (return-from df-handle-circularity t))
       (return-from df-handle-circularity))
      ((integerp index)
       (when within-list
         (write-string " . " stream))
       (%stream-write-char #\# stream)
       (write index :stream stream)
       (%stream-write-char #\# stream)
       (%stream-write-char #\Space stream)
       (return-from df-handle-circularity t))
      (t
       (unless *prevent-fasl-circle-detection*
         (assert (eq index t)))))))

(declaim (ftype (function (cons stream) t) dump-cons))
(defun dump-cons (object stream)
  (cond ((and (eq (car object) 'QUOTE) (= (length object) 2))
         (%stream-write-char #\' stream)
         (dump-object (%cadr object) stream))
        (t
         (%stream-write-char #\( stream)
         (loop
            (dump-object (%car object) stream)
            (setf object (%cdr object))
            (when (null object)
              (return)) ;; escape loop
            (%stream-write-char #\space stream)
            (when (atom object)
              (%stream-write-char #\. stream)
              (%stream-write-char #\space stream)
              (dump-object object stream)
              (return))
            (when (df-handle-circularity object stream t)
              (return))
            (when (> (charpos stream) 80)
              (%stream-terpri stream)))
         (%stream-write-char #\) stream))))

(declaim (ftype (function (t stream) t) dump-vector))
(defun dump-vector (object stream)
  (write-string "#(" stream)
  (let ((length (length object)))
    (when (> length 0)
      (dotimes (i (1- length))
        (declare (type index i))
        (dump-object (aref object i) stream)
        (when (> (charpos stream) 80)
          (%stream-terpri stream))
        (%stream-write-char #\space stream))
      (dump-object (aref object (1- length)) stream))
    (%stream-write-char #\) stream)))

(declaim (ftype (function (t stream) t) dump-instance))
(defun dump-instance (object stream)
  (write-string "#." stream)
  (dump-object (get-instance-form object) stream))

(declaim (ftype (function (symbol) integer) dump-uninterned-symbol-index))
(defun dump-uninterned-symbol-index (symbol)
  (let ((index (cdr (assoc symbol *fasl-uninterned-symbols*))))
    (unless index
      (setq index (1+ (or (cdar *fasl-uninterned-symbols*) -1)))
      (setq *fasl-uninterned-symbols*
            (acons symbol index *fasl-uninterned-symbols*)))
    index))

(declaim (ftype (function (t stream) t) dump-object))
(defun dump-object (object stream)
  (unless (df-handle-circularity object stream nil)
    (cond ((consp object)
           (dump-cons object stream))
          ((stringp object)
           (%stream-output-object object stream))
          ((bit-vector-p object)
           (%stream-output-object object stream))
          ((vectorp object)
           (dump-vector object stream))
          ((or (structure-object-p object) ;; FIXME instance-p
               (standard-object-p object)
               (java:java-object-p object))
           (dump-instance object stream))
          ((and (symbolp object) ;; uninterned symbol
                (null (symbol-package object)))
           (write-string "#" stream)
           (write (dump-uninterned-symbol-index object) :stream stream)
           (write-string "?" stream))
          (t
           (%stream-output-object object stream)))))

(defvar *the-fasl-printer-readtable*
  (copy-readtable (get-fasl-readtable))
  "This variable holds a copy of the FASL readtable which we need to bind
below, in order to prevent the current readtable from influencing the content
being written to the FASL: the READTABLE-CASE setting influences symbol printing.")

(defvar *prevent-fasl-circle-detection* nil)

(declaim (ftype (function (t stream) t) dump-form))
(defun dump-form (form stream)
  (let ((*print-fasl* t)
        (*print-array* t)
        (*print-base* 10)
        (*print-case* :upcase)
        (*print-circle* nil)
        (*print-escape* t)
        (*print-gensym* t)
        (*print-length* nil)
        (*print-level* nil)
        (*print-lines* nil)
        (*print-pretty* nil)
        (*print-radix* nil)
#+nil ;; XXX Some types (q.v. (UNSIGNED-BYTE 32)) don't have a
      ;; readable syntax because they don't roundtrip to the same
      ;; type, but still return a Lisp object that "works", albeit
      ;; perhaps inefficiently when READ from their DUMP-FORM
      ;; representation.
        (*print-readably* t)
        (*print-right-margin* nil)
        (*print-structure* t)
        (*readtable* *the-fasl-printer-readtable*)

        ;; make sure to write all floats with their exponent marker:
        ;; the dump-time default may not be the same at load-time
        (*read-default-float-format* nil)

        ;; these values are also bound by WITH-STANDARD-IO-SYNTAX,
        ;; but not used by our reader/printer, so don't bind them,
        ;; for efficiency reasons.
        ;;        (*read-eval* t)
        ;;        (*read-suppress* nil)
        ;;        (*print-miser-width* nil)
        ;;        (*print-pprint-dispatch* (copy-pprint-dispatch nil))
        ;;        (*read-base* 10)
        ;;        (*read-default-float-format* 'single-float)
        ;;        (*readtable* (copy-readtable nil))

        (*circularity* (make-hash-table :test #'eq))
        (*instance-forms* (make-hash-table :test #'eq))
        (*circle-counter* 0))
    (unless *prevent-fasl-circle-detection*
      (df-check-object form))
    (dump-object form stream)))

(provide 'dump-form)
