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

(export '(dump-form dump-uninterned-symbol-index
          %fasl-emit-toplevel-form
          %fasl-init-instance-tables
          *fasl-instance-count*
          *fasl-stream*))

(declaim (special *circularity* *circle-counter* *instance-forms*
                  *fasl-instance-table*
                  *fasl-instance-forms*
                  *fasl-instance-refs*
                  *fasl-instance-created-p*
                  *fasl-instance-initialized-p*
                  *fasl-instance-in-creation-p*
                  *fasl-instance-in-init-p*
                  *fasl-instance-count*
                  *fasl-emitting-to-fasl-stream*
                  *fasl-stream*))

(defvar *fasl-emitting-to-fasl-stream* nil
  "Bound to T while DUMP-FORM / %FASL-WRITE-RAW-FORM are writing
material destined for the fasl stream.  When NIL, DUMP-INSTANCE and
DF-CHECK-INSTANCE fall back to inline creation/initialization.")

;;;; MAKE-LOAD-FORM ordering for the file compiler.
;;;;
;;;; CLHS requires creation and initialization forms from MAKE-LOAD-FORM
;;;; to be dumped so that data-flow dependencies are honored: any object
;;;; referenced in a creation form must already exist, and initialization
;;;; forms run "as soon as possible" after their associated creation form
;;;; subject to the dependencies of the initialization form.
;;;;
;;;; We implement this by tracking referenced instances file-wide.  For
;;;; each literal instance we emit two separate fasl top-level forms:
;;;;
;;;;   (SETF (SVREF SYS::*FASL-INSTANCES* N) <creation-form>)
;;;;   <initialization-form>
;;;;
;;;; and replace the original inline reference to the instance with
;;;; "#.(SVREF SYS::*FASL-INSTANCES* N)".  The prologue allocates the
;;;; vector once the total count is known.

(defun %fasl-candidate-p (object)
  (or (structure-object-p object)
      (standard-object-p object)
      (java:java-object-p object)))

(defun %fasl-init-instance-tables ()
  (setq *fasl-instance-table* (make-hash-table :test 'eq)
        *fasl-instance-forms* (make-hash-table :test 'eq)
        *fasl-instance-refs* (make-hash-table :test 'eq)
        *fasl-instance-created-p* (make-hash-table :test 'eq)
        *fasl-instance-initialized-p* (make-hash-table :test 'eq)
        *fasl-instance-in-creation-p* (make-hash-table :test 'eq)
        *fasl-instance-in-init-p* (make-hash-table :test 'eq)
        *fasl-instance-count* 0))

(defun %fasl-register-instance (object)
  "Assign an index to OBJECT, caching its creation and initialization
forms.  Returns the index."
  (or (gethash object *fasl-instance-table*)
      (multiple-value-bind (creation-form initialization-form)
          (make-load-form object)
        (let ((index *fasl-instance-count*))
          (setf (gethash object *fasl-instance-table*) index)
          (setf (gethash object *fasl-instance-forms*)
                (cons creation-form initialization-form))
          (setf (gethash object *fasl-instance-refs*)
                (list 'svref 'sys::*fasl-instances* index))
          (incf *fasl-instance-count*)
          index))))

(declaim (ftype (function (t stream) t)
                %fasl-walk-for-deps
                %fasl-walk-creation-deps
                %fasl-walk-init-deps
                %fasl-ensure-created
                %fasl-ensure-initialized))

(defun %fasl-map-embedded-instances (form fn)
  "Call FN on every literal instance embedded in FORM, without
revisiting already-seen subobjects."
  (let ((seen (make-hash-table :test #'eq)))
    (labels ((walk (x)
               (unless (or (null x)
                           (symbolp x)
                           (numberp x)
                           (characterp x)
                           (stringp x)
                           (bit-vector-p x)
                           (gethash x seen))
                 (setf (gethash x seen) t)
                 (cond
                   ((consp x)
                    (walk (car x))
                    (walk (cdr x)))
                   ((vectorp x)
                    (dotimes (i (length x)) (walk (aref x i))))
                   ((%fasl-candidate-p x)
                    (funcall fn x))))))
      (walk form))))

(defun %fasl-walk-creation-deps (form stream)
  "Ensure creation forms are emitted for every instance embedded in
FORM (creation-dep transitive closure)."
  (%fasl-map-embedded-instances form
                                (lambda (x) (%fasl-ensure-created x stream))))

(defun %fasl-walk-init-deps (form stream)
  "Ensure full init forms (and their creation prerequisites) are
emitted for every instance embedded in FORM."
  (%fasl-map-embedded-instances form
                                (lambda (x) (%fasl-ensure-initialized x stream))))

(defun %fasl-walk-for-deps (form stream)
  "Drive the two-phase dep walk for a top-level FORM: ensure every
embedded literal instance is fully created *and* initialized before
FORM is emitted."
  (%fasl-walk-init-deps form stream))

(defun %fasl-init-deps-ready-p (object)
  "Return T if every literal instance embedded in OBJECT's init form
is already initialized (ignoring OBJECT itself).  This decides whether
we can run OBJECT's init form eagerly, right after its creation, per
the CLHS ASAP rule for init forms."
  (let ((init-form (cdr (gethash object *fasl-instance-forms*)))
        (ready t))
    (%fasl-map-embedded-instances
     init-form
     (lambda (y)
       (unless (or (eq y object)
                   (gethash y *fasl-instance-initialized-p*))
         (setf ready nil))))
    ready))

(defun %fasl-ensure-created (object stream)
  "Emit OBJECT's creation form (after its creation-dep transitive
closure) if not already emitted.  After emission, eagerly emit
OBJECT's init form when all its init-deps are already initialized, so
inits run ASAP after creation per CLHS."
  (unless (and *fasl-instance-table* (%fasl-candidate-p object))
    (return-from %fasl-ensure-created))
  (let ((index (%fasl-register-instance object)))
    (when (gethash object *fasl-instance-created-p*)
      (return-from %fasl-ensure-created))
    (when (gethash object *fasl-instance-in-creation-p*)
      (error "Circular creation dependency in MAKE-LOAD-FORM for ~S"
             object))
    (setf (gethash object *fasl-instance-in-creation-p*) t)
    (let ((creation-form (car (gethash object *fasl-instance-forms*))))
      (%fasl-walk-creation-deps creation-form stream)
      (%fasl-write-raw-form
       `(setf (svref sys::*fasl-instances* ,index) ,creation-form)
       stream))
    (setf (gethash object *fasl-instance-created-p*) t)
    (remhash object *fasl-instance-in-creation-p*)
    (when (%fasl-init-deps-ready-p object)
      (%fasl-ensure-initialized object stream))))

(defun %fasl-ensure-initialized (object stream)
  "Emit OBJECT's init form (after ensuring OBJECT is created and after
recursing through init-dep transitive closure) if not already emitted."
  (unless (and *fasl-instance-table* (%fasl-candidate-p object))
    (return-from %fasl-ensure-initialized))
  (%fasl-ensure-created object stream)
  (when (gethash object *fasl-instance-initialized-p*)
    (return-from %fasl-ensure-initialized))
  (when (gethash object *fasl-instance-in-init-p*)
    ;; In-progress init cycle: some earlier frame is already emitting
    ;; OBJECT's init form.  Per CLHS the ordering is unspecified for
    ;; init-level cycles; break here.
    (return-from %fasl-ensure-initialized))
  (setf (gethash object *fasl-instance-in-init-p*) t)
  (let ((init-form (cdr (gethash object *fasl-instance-forms*))))
    (when init-form
      (%fasl-walk-init-deps init-form stream)
      (%fasl-write-raw-form init-form stream)))
  (setf (gethash object *fasl-instance-initialized-p*) t)
  (remhash object *fasl-instance-in-init-p*))

(defun %fasl-ensure-created-and-initialized (object stream)
  "Compatibility shim retained for any external callers."
  (%fasl-ensure-initialized object stream))

(defun %fasl-write-raw-form (form stream)
  "Dump FORM as a complete fasl top-level expression without running
the dependency walk (the caller has already emitted deps)."
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
        (*print-right-margin* nil)
        (*print-structure* t)
        (*readtable* *the-fasl-printer-readtable*)
        (*read-default-float-format* nil)
        (*circularity* (make-hash-table :test #'eq))
        (*instance-forms* (make-hash-table :test #'eq))
        (*circle-counter* 0)
        (*fasl-emitting-to-fasl-stream* t))
    (unless *prevent-fasl-circle-detection*
      (df-check-object form))
    (dump-object form stream)
    (%stream-terpri stream)))

(defun %fasl-emit-toplevel-form (form stream)
  "Public entry: emit FORM to the fasl STREAM, first pre-emitting any
creation and initialization forms required by embedded literal
instances."
  (%fasl-walk-for-deps form stream)
  (%fasl-write-raw-form form stream))

(defun get-instance-form (object)
  "Legacy inline creation-plus-init expression for literal instances.
Used when dumping outside the fasl-stream context (e.g. embedding
constants into class files via SERIALIZE-OBJECT)."
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
     (when (eq :circular (df-register-circularity object))
       (return))))

(defun df-check-vector (object)
  (dotimes (index (length object))
    (df-check-object (aref object index))))

(defun df-check-instance (object)
  (let ((ref (and *fasl-emitting-to-fasl-stream*
                  *fasl-instance-refs*
                  (gethash object *fasl-instance-refs*))))
    (if ref
        ;; New fasl path: DUMP-INSTANCE will emit this exact ref cons;
        ;; walk it here so the same cons is registered in *circularity*
        ;; before DUMP-OBJECT reaches it.
        (df-check-object ref)
        (df-check-object (get-instance-form object)))))

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
         (assert (or (eq index t)
                     (integerp object)))))))) ;; strictly this should be 'long'

(declaim (ftype (function (cons stream) t) dump-cons))
(defun dump-cons (object stream)
  (cond ((and (eq (car object) 'QUOTE) (proper-list-of-length-p object 2))
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
  (let ((ref (and *fasl-emitting-to-fasl-stream*
                  *fasl-instance-refs*
                  (gethash object *fasl-instance-refs*))))
    (cond
      (ref
       ;; File-compiler path: emit a reference to the pre-populated
       ;; per-fasl instance table.
       (write-string "#." stream)
       (dump-object ref stream))
      (t
       ;; Legacy path: inline creation and initialization at the
       ;; point of reference.
       (write-string "#." stream)
       (dump-object (get-instance-form object) stream)))))

(declaim (ftype (function (symbol) integer) dump-uninterned-symbol-index))
(defun dump-uninterned-symbol-index (symbol)
  (let ((index (cdr (assoc symbol *fasl-uninterned-symbols*))))
    (unless index
      (setq index (1+ (or (cdar *fasl-uninterned-symbols*) -1)))
      (setq *fasl-uninterned-symbols*
            (acons symbol index *fasl-uninterned-symbols*)))
    index))

(declaim (ftype (function (pathname stream) t) dump-pathname))
(defun dump-pathname (pathname stream)
  (write-string "#P(" stream)
  (write-string ":HOST " stream)
  (dump-form (pathname-host pathname) stream)
  (write-string " :DEVICE " stream)
  (dump-form (pathname-device pathname) stream)
  (write-string " :DIRECTORY " stream)
  (dump-form (pathname-directory pathname) stream)
  (write-string " :NAME " stream)
  (dump-form (pathname-name pathname) stream)
  (write-string " :TYPE " stream)
  (dump-form (pathname-type pathname) stream)
  (write-string " :VERSION " stream)
  (dump-form (pathname-version pathname) stream)
  (write-string ")" stream))

(declaim (ftype (function (t stream) t) dump-object))
(defun dump-object (object stream)
  (unless (df-handle-circularity object stream nil)
    (cond ((consp object)
           (dump-cons object stream))
          ((stringp object)
           (%stream-output-object object stream))
          ((pathnamep object)
           (dump-pathname object stream))
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
;;    (print form)
    (unless *prevent-fasl-circle-detection*
      (df-check-object form))
    (dump-object form stream)))

(provide 'dump-form)
