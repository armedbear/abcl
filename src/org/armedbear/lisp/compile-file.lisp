;;; compile-file.lisp
;;;
;;; Copyright (C) 2004-2006 Peter Graves
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

(require "JVM")
;; (require "COMPILER-ERROR") already made accessible through JVM

(defvar *fbound-names*)

(defvar *class-number*)

(defvar *output-file-pathname*)

(declaim (ftype (function (t) t) compute-classfile-name))
(defun compute-classfile-name (n &optional (output-file-pathname
                                            *output-file-pathname*))
  "Computes the name of the class file associated with number `n'."
  (let ((name
         (%format nil "~A-~D"
                  (substitute #\_ #\.
                              (pathname-name output-file-pathname)) n)))
    (namestring (merge-pathnames (make-pathname :name name :type "cls")
                                 output-file-pathname))))

(declaim (ftype (function () t) next-classfile-name))
(defun next-classfile-name ()
  (compute-classfile-name (incf *class-number*)))

(defmacro report-error (&rest forms)
  `(handler-case (progn ,@forms)
     (compiler-unsupported-feature-error (condition)
       (fresh-line)
       (%format t "; UNSUPPORTED-FEATURE: ~A~%" condition)
       (values nil condition))))

;; Dummy function. Should never be called.
(defun dummy (&rest ignored)
  (declare (ignore ignored))
  (assert nil))

(declaim (ftype (function (t) t) verify-load))
(defun verify-load (classfile)
  (if (> *safety* 0)
    (and classfile
         (let ((*load-truename* *output-file-pathname*))
           (report-error
            (load-compiled-function classfile))))
    t))

(declaim (ftype (function (t) t) process-defconstant))
(defun process-defconstant (form)
  ;; "If a DEFCONSTANT form appears as a top level form, the compiler
  ;; must recognize that [the] name names a constant variable. An
  ;; implementation may choose to evaluate the value-form at compile
  ;; time, load time, or both. Therefore, users must ensure that the
  ;; initial-value can be evaluated at compile time (regardless of
  ;; whether or not references to name appear in the file) and that
  ;; it always evaluates to the same value."
  (eval form)
  (output-form form))

(declaim (ftype (function (t) t) note-toplevel-form))
(defun note-toplevel-form (form)
  (when *compile-print*
    (fresh-line)
    (princ "; ")
    (let ((*print-length* 2)
          (*print-level* 2)
          (*print-pretty* nil))
      (prin1 form))
    (terpri)))

(declaim (ftype (function (t stream t) t) process-toplevel-form))
(defun process-toplevel-form (form stream compile-time-too)
  (if (atom form)
      (when compile-time-too
        (eval form))
    (progn
      (let ((operator (%car form)))
        (case operator
          (MACROLET
           (process-toplevel-macrolet form stream compile-time-too)
           (return-from process-toplevel-form))
          ((IN-PACKAGE DEFPACKAGE)
           (note-toplevel-form form)
           (setf form (precompiler:precompile-form form nil *compile-file-environment*))
           (eval form)
           ;; Force package prefix to be used when dumping form.
           (let ((*package* +keyword-package+))
             (output-form form))
           (return-from process-toplevel-form))
          ((DEFVAR DEFPARAMETER)
           (note-toplevel-form form)
           (if compile-time-too
               (eval form)
               ;; "If a DEFVAR or DEFPARAMETER form appears as a top level form,
               ;; the compiler must recognize that the name has been proclaimed
               ;; special. However, it must neither evaluate the initial-value
               ;; form nor assign the dynamic variable named NAME at compile
               ;; time."
               (let ((name (second form)))
                 (%defvar name))))
          (DEFCONSTANT
           (note-toplevel-form form)
           (process-defconstant form)
           (return-from process-toplevel-form))
          (DEFUN
           (note-toplevel-form form)
           (let* ((name (second form))
                  (block-name (fdefinition-block-name name))
                  (lambda-list (third form))
                  (body (nthcdr 3 form)))
             (jvm::with-saved-compiler-policy
               (multiple-value-bind (body decls doc)
                   (parse-body body)
                 (let* ((expr `(lambda ,lambda-list
                                 ,@decls (block ,block-name ,@body)))
                        (classfile (next-classfile-name))
                        (internal-compiler-errors nil)
                        (result (with-open-file
				    (f classfile
				       :direction :output
				       :element-type '(unsigned-byte 8)
				       :if-exists :supersede)
                                  (handler-bind 
                                      ((internal-compiler-error
                                        #'(lambda (e)
                                            (push e internal-compiler-errors)
                                            (continue))))
                                    (report-error
                                     (jvm:compile-defun name expr nil
                                                        classfile f nil)))))
                        (compiled-function (if (not internal-compiler-errors)
                                               (verify-load classfile)
                                               nil)))
		   (declare (ignore result))
                   (cond
                     ((and (not internal-compiler-errors)
                           compiled-function)
                      (setf form
                            `(fset ',name
                                   (proxy-preloaded-function ',name ,(file-namestring classfile))
                                   ,*source-position*
                                   ',lambda-list
                                   ,doc))
                      (when compile-time-too
                        (fset name compiled-function)))
                     (t
                      ;; Add this warning when the stock ABCL compiles
                      ;; again, as all warnings in COMPILE-SYSTEM
                      ;; produce a non-zero exit status that stops
                      ;; build.xml in its tracks.
                      #+nil
                      (compiler-warn "Unable to compile function ~A.  Using interpreted form instead.~%" name)
                      (format *error-output*
                              "; Unable to compile function ~A.  Using interpreted form instead.~%" name)
                      (when internal-compiler-errors
                        (dolist (e internal-compiler-errors)
                          (format *error-output*
                                  "; ~A~%" e)))
                      (let ((precompiled-function
                             (precompiler:precompile-form expr nil
                                              *compile-file-environment*)))
                        (setf form
                              `(fset ',name
                                     ,precompiled-function
                                     ,*source-position*
                                     ',lambda-list
                                     ,doc)))
                      (when compile-time-too
                        (eval form)))))
                 (when (and (symbolp name) (eq (get name '%inline) 'INLINE))
                 ;; FIXME Need to support SETF functions too!
                   (setf (inline-expansion name)
                         (jvm::generate-inline-expansion block-name
                                                         lambda-list body))
                   (output-form `(setf (inline-expansion ',name)
                                       ',(inline-expansion name))))))
             (push name jvm::*functions-defined-in-current-file*)
             (note-name-defined name)
             ;; If NAME is not fbound, provide a dummy definition so that
             ;; getSymbolFunctionOrDie() will succeed when we try to verify that
             ;; functions defined later in the same file can be loaded correctly.
             (unless (fboundp name)
               (setf (fdefinition name) #'dummy)
               (push name *fbound-names*))))
          ((DEFGENERIC DEFMETHOD)
           (note-toplevel-form form)
           (note-name-defined (second form))
           (let ((*compile-print* nil))
             (process-toplevel-form (macroexpand-1 form *compile-file-environment*)
                                    stream compile-time-too))
             (return-from process-toplevel-form))
          (DEFMACRO
           (note-toplevel-form form)
           (let ((name (second form)))
             (eval form)
             (let* ((expr (function-lambda-expression (macro-function name)))
                    (classfile (next-classfile-name)))
	       (with-open-file
		   (f classfile
		      :direction :output
		      :element-type '(unsigned-byte 8)
		      :if-exists :supersede)
		 (ignore-errors
		   (jvm:compile-defun nil expr nil classfile f nil)))
               (if (null (verify-load classfile))
                   ;; FIXME error or warning
                   (format *error-output* "; Unable to compile macro ~A~%" name)
                 (progn
                   (setf form
                         (if (special-operator-p name)
                             `(put ',name 'macroexpand-macro
                                   (make-macro ',name
                                               (proxy-preloaded-function
                                                '(macro-function ,name)
                                                ,(file-namestring classfile))))
                             `(fset ',name
                                    (make-macro ',name
                                                (proxy-preloaded-function
                                                 '(macro-function ,name)
                                                 ,(file-namestring classfile)))
                                    ,*source-position*
                                    ',(third form)))))))))
          (DEFTYPE
           (note-toplevel-form form)
           (eval form))
          (EVAL-WHEN
           (multiple-value-bind (ct lt e)
               (parse-eval-when-situations (cadr form))
             (let ((new-compile-time-too (or ct (and compile-time-too e)))
                   (body (cddr form)))
               (if lt
                   (process-toplevel-progn body stream new-compile-time-too)
                 (when new-compile-time-too
                   (eval `(progn ,@body)))))
           (return-from process-toplevel-form)))
          (LOCALLY
           ;; FIXME Need to handle special declarations too!
           (jvm::with-saved-compiler-policy
             (multiple-value-bind (forms decls)
                 (parse-body (cdr form) nil)
               (process-optimization-declarations decls)
               (let* ((jvm::*visible-variables* jvm::*visible-variables*)
                      (specials (jvm::process-declarations-for-vars (cdr form)
                                                                    nil nil)))
                 (dolist (special specials)
                   (push special jvm::*visible-variables*))
                 (process-toplevel-progn forms stream compile-time-too))
               (return-from process-toplevel-form))))
          (PROGN
           (process-toplevel-progn (cdr form) stream compile-time-too)
           (return-from process-toplevel-form))
          (DECLARE
           (compiler-style-warn "Misplaced declaration: ~S" form))
          (t
           (when (and (symbolp operator)
                      (macro-function operator *compile-file-environment*))
             (note-toplevel-form form)
             ;; Note that we want MACROEXPAND-1 and not MACROEXPAND here, in
             ;; case the form being expanded expands into something that needs
             ;; special handling by PROCESS-TOPLEVEL-FORM (e.g. DEFMACRO).
             (let ((*compile-print* nil))
               (process-toplevel-form (macroexpand-1 form *compile-file-environment*)
                                      stream compile-time-too))
             (return-from process-toplevel-form))

           (cond ((eq operator 'QUOTE)
;;;                      (setf form (precompiler:precompile-form form nil
;;;                                                  *compile-file-environment*))
                  (when compile-time-too
                    (eval form))
                  (return-from process-toplevel-form))
                 ((eq operator 'PUT)
                  (setf form (precompiler:precompile-form form nil *compile-file-environment*)))
                 ((eq operator 'COMPILER-DEFSTRUCT)
                  (setf form (precompiler:precompile-form form nil *compile-file-environment*)))
                 ((eq operator 'PROCLAIM)
                  (setf form (precompiler:precompile-form form nil *compile-file-environment*)))
                 ((and (memq operator '(EXPORT REQUIRE PROVIDE SHADOW))
                       (or (keywordp (second form))
                           (and (listp (second form))
                                (eq (first (second form)) 'QUOTE))))
                  (setf form (precompiler:precompile-form form nil *compile-file-environment*)))
                 ((eq operator 'IMPORT)
                  (setf form (precompiler:precompile-form form nil *compile-file-environment*))
                  ;; Make sure package prefix is printed when symbols are imported.
                  (let ((*package* +keyword-package+))
                    (output-form form))
                  (when compile-time-too
                    (eval form))
                  (return-from process-toplevel-form))
                 ((and (eq operator '%SET-FDEFINITION)
                       (eq (car (second form)) 'QUOTE)
                       (consp (third form))
                       (eq (%car (third form)) 'FUNCTION)
                       (symbolp (cadr (third form))))
                  (setf form (precompiler:precompile-form form nil *compile-file-environment*)))
;;;                     ((memq operator '(LET LET*))
;;;                      (let ((body (cddr form)))
;;;                        (if (dolist (subform body nil)
;;;                              (when (and (consp subform) (eq (%car subform) 'DEFUN))
;;;                                (return t)))
;;;                            (setf form (convert-toplevel-form form))
;;;                            (setf form (precompiler:precompile-form form nil)))))
                 ((eq operator 'mop::ensure-method)
                  (setf form (convert-ensure-method form)))
                 ((and (symbolp operator)
                       (not (special-operator-p operator))
                       (null (cdr form)))
                  (setf form (precompiler:precompile-form form nil *compile-file-environment*)))
                 (t
;;;                      (setf form (precompiler:precompile-form form nil))
                  (note-toplevel-form form)
                  (setf form (convert-toplevel-form form nil)))))))))
  (when (consp form)
    (output-form form))
  ;; Make sure the compiled-function loader knows where
  ;; to load the compiled functions. Note that this trickery
  ;; was already used in verify-load before I used it,
  ;; however, binding *load-truename* isn't fully compliant, I think.
  (let ((*load-truename* *output-file-pathname*))
    (when compile-time-too
      (eval form))))

(declaim (ftype (function (t) t) convert-ensure-method))
(defun convert-ensure-method (form)
  (c-e-m-1 form :function)
  (c-e-m-1 form :fast-function)
  (precompiler:precompile-form form nil *compile-file-environment*))

(declaim (ftype (function (t t) t) c-e-m-1))
(defun c-e-m-1 (form key)
  (let* ((tail (cddr form))
         (function-form (getf tail key)))
    (when (and function-form (consp function-form)
               (eq (%car function-form) 'FUNCTION))
      (let ((lambda-expression (cadr function-form)))
        (jvm::with-saved-compiler-policy
          (let* ((classfile (next-classfile-name))
                 (result
		  (with-open-file
		      (f classfile
			 :direction :output
			 :element-type '(unsigned-byte 8)
			 :if-exists :supersede)
		    (report-error
		     (jvm:compile-defun nil lambda-expression nil classfile f nil))))
                 (compiled-function (verify-load classfile)))
	    (declare (ignore result))
            (cond (compiled-function
                   (setf (getf tail key)
                         `(load-compiled-function ,(file-namestring classfile))))
                  (t
                   ;; FIXME This should be a warning or error of some sort...
                   (format *error-output* "; Unable to compile method~%")))))))))

(declaim (ftype (function (t) t) simple-toplevel-form-p))
(defun simple-toplevel-form-p (form)
  "Returns NIL if the form is too complex to become an
interpreted toplevel form, non-NIL if it is 'simple enough'."
  (and (consp form)
             (every #'(lambda (arg)
                        (or (and (atom arg)
                                 (not (and (symbolp arg)
                                           (symbol-macro-p arg))))
                            (and (consp arg)
                                 (eq 'QUOTE (car arg)))))
              (cdr form))))

(declaim (ftype (function (t t) t) convert-toplevel-form))
(defun convert-toplevel-form (form declare-inline)
  (when (or (simple-toplevel-form-p form)
            (and (eq (car form) 'SETQ)
                 ;; for SETQ, look at the evaluated part
                 (simple-toplevel-form-p (third form))))
    ;; single form with simple or constant arguments
    ;; Without this exception, toplevel function calls
    ;; will be compiled into lambdas which get compiled to
    ;; compiled-functions. Those need to be loaded.
    ;; Conclusion: Top level interpreting the function call
    ;;  and its arguments may be (and should be) more efficient.
    (return-from convert-toplevel-form
      (precompiler:precompile-form form nil *compile-file-environment*)))
  (let* ((expr `(lambda () ,form))
         (classfile (next-classfile-name))
         (result
	  (with-open-file
	      (f classfile
		 :direction :output
		 :element-type '(unsigned-byte 8)
		 :if-exists :supersede)
	    (report-error (jvm:compile-defun nil expr nil classfile
                                             f declare-inline))))
         (compiled-function (verify-load classfile)))
    (declare (ignore result))
    (setf form
          (if compiled-function
              `(funcall (load-compiled-function ,(file-namestring classfile)))
              (precompiler:precompile-form form nil *compile-file-environment*)))))


(defun process-toplevel-macrolet (form stream compile-time-too)
  (let ((*compile-file-environment* (make-environment *compile-file-environment*)))
    (dolist (definition (cadr form))
      (environment-add-macro-definition *compile-file-environment*
                                        (car definition)
                                        (make-macro (car definition)
                                                    (make-expander-for-macrolet definition))))
    (dolist (body-form (cddr form))
      (process-toplevel-form body-form stream compile-time-too))))

(declaim (ftype (function (t stream t) t) process-toplevel-progn))
(defun process-toplevel-progn (forms stream compile-time-too)
  (dolist (form forms)
    (process-toplevel-form form stream compile-time-too)))

;;; Adapted from SBCL.
;;; Parse an EVAL-WHEN situations list, returning three flags,
;;; (VALUES COMPILE-TOPLEVEL LOAD-TOPLEVEL EXECUTE), indicating
;;; the types of situations present in the list.
(defun parse-eval-when-situations (situations)
  (when (or (not (listp situations))
	    (set-difference situations
			    '(:compile-toplevel
			      compile
			      :load-toplevel
			      load
			      :execute
			      eval)))
    (error "Bad EVAL-WHEN situation list: ~S." situations))
  (values (intersection '(:compile-toplevel compile) situations)
	  (intersection '(:load-toplevel load) situations)
	  (intersection '(:execute eval) situations)))


(defvar *binary-fasls* nil)
(defvar *forms-for-output* nil)
(defvar *fasl-stream* nil)

(defun output-form (form)
  (if *binary-fasls*
      (push form *forms-for-output*)
      (progn
        (dump-form form *fasl-stream*)
        (%stream-terpri *fasl-stream*))))

(defun finalize-fasl-output ()
  (when *binary-fasls*
    (let ((*package* (find-package :keyword))
          (*double-colon-package-separators* T))
      (dump-form (convert-toplevel-form (list* 'PROGN
                                               (nreverse *forms-for-output*))
                                        t)
                 *fasl-stream*))
    (%stream-terpri *fasl-stream*)))

(defun compile-file (input-file
                     &key
                     output-file
                     ((:verbose *compile-verbose*) *compile-verbose*)
                     ((:print *compile-print*) *compile-print*)
                     external-format)
  (declare (ignore external-format))    ; FIXME
  (unless (or (and (probe-file input-file) (not (file-directory-p input-file)))
              (pathname-type input-file))
    (let ((pathname (merge-pathnames (make-pathname :type "lisp") input-file)))
      (when (probe-file pathname)
        (setf input-file pathname))))
  (setf output-file (if output-file
                        (merge-pathnames output-file *default-pathname-defaults*)
                        (compile-file-pathname input-file)))
  (let* ((*output-file-pathname* output-file)
         (type (pathname-type output-file))
         (temp-file (merge-pathnames (make-pathname :type (concatenate 'string type "-tmp"))
                                     output-file))
         (temp-file2 (merge-pathnames (make-pathname :type (concatenate 'string type "-tmp2"))
                                     output-file))
         (warnings-p nil)
         (failure-p nil))
    (with-open-file (in input-file :direction :input)
      (let* ((*compile-file-pathname* (pathname in))
             (*compile-file-truename* (truename in))
             (*source* *compile-file-truename*)
             (*class-number* 0)
             (namestring (namestring *compile-file-truename*))
             (start (get-internal-real-time))
             elapsed
             *fasl-uninterned-symbols*)
        (when *compile-verbose*
          (format t "; Compiling ~A ...~%" namestring))
        (with-compilation-unit ()
          (with-open-file (out temp-file
                               :direction :output :if-exists :supersede
                               :external-format *fasl-external-format*)
            (let ((*readtable* *readtable*)
                  (*read-default-float-format* *read-default-float-format*)
                  (*read-base* *read-base*)
                  (*package* *package*)
                  (jvm::*functions-defined-in-current-file* '())
                  (*fbound-names* '())
                  (*fasl-stream* out)
                  *forms-for-output*)
              (jvm::with-saved-compiler-policy
                (jvm::with-file-compilation
                    (handler-bind ((style-warning 
                                    #'(lambda (c)
                                        (setf warnings-p t)
                                        ;; let outer handlers do their thing
                                        (signal c)
                                        ;; prevent the next handler
                                        ;; from running: we're a
                                        ;; WARNING subclass
                                        (continue)))
                                   ((or warning 
                                        compiler-error)
                                    #'(lambda (c)
                                        (declare (ignore c))
                                        (setf warnings-p t
                                              failure-p t))))
                      (loop
                         (let* ((*source-position* (file-position in))
                                (jvm::*source-line-number* (stream-line-number in))
                                (form (read in nil in))
                                (*compiler-error-context* form))
                           (when (eq form in)
                             (return))
                           (process-toplevel-form form out nil))))
                    (finalize-fasl-output)
                    (dolist (name *fbound-names*)
                      (fmakunbound name)))))))
        (with-open-file (in temp-file :direction :input)
          (with-open-file (out temp-file2 :direction :output
                               :if-does-not-exist :create
                               :if-exists :supersede)
            ;; write header
            (write "; -*- Mode: Lisp -*-" :escape nil :stream out)
            (%stream-terpri out)
            (let ((*package* (find-package '#:cl)))
              (write (list 'init-fasl :version *fasl-version*)
                     :stream out)
              (%stream-terpri out)
              (write (list 'setq '*source* *compile-file-truename*)
                     :stream out)
              (%stream-terpri out)
              ;; Note: Beyond this point, you can't use DUMP-FORM,
              ;; because the list of uninterned symbols has been fixed now.
              (when *fasl-uninterned-symbols*
                (write (list 'setq '*fasl-uninterned-symbols*
                             (coerce (mapcar #'car
                                             (nreverse *fasl-uninterned-symbols*))
                                     'vector))
                       :stream out))
              (%stream-terpri out)
              ;; we work with a fixed variable name here to work around the
              ;; lack of availability of the circle reader in the fasl reader
              ;; but it's a toplevel form anyway
              (write `(dotimes (i ,*class-number*)
                        (function-preload
                         (%format nil "~A-~D.cls"
                                  ,(substitute #\_ #\. (pathname-name output-file))
                                  (1+ i))))
                     :stream out
                     :circle t)
              (%stream-terpri out))


            ;; copy remaining content
            (loop for line = (read-line in nil :eof)
               while (not (eq line :eof))
               do (write-line line out))))
        (delete-file temp-file)
	(remove-zip-cache-entry output-file) ;; Necessary under windows
        (rename-file temp-file2 output-file)

        (when *compile-file-zip*
          (let* ((type ;; Don't use ".zip", it'll result in an extension
                  ;;  with a dot, which is rejected by NAMESTRING
                  (%format nil "~A~A" (pathname-type output-file) "-zip"))
                 (zipfile (namestring
                           (merge-pathnames (make-pathname :type type)
                                            output-file)))
                 (pathnames ()))
            (dotimes (i *class-number*)
              (let* ((pathname (compute-classfile-name (1+ i))))
                (when (probe-file pathname)
                  (push pathname pathnames))))
            (setf pathnames (nreverse pathnames))
            (let ((load-file (merge-pathnames (make-pathname :type "_")
                                              output-file)))
              (rename-file output-file load-file)
              (push load-file pathnames))
            (zip zipfile pathnames)
            (dolist (pathname pathnames)
              (let ((truename (probe-file pathname)))
                (when truename
                  (delete-file truename))))
            (rename-file zipfile output-file)))

        (setf elapsed (/ (- (get-internal-real-time) start) 1000.0))
        (when *compile-verbose*
          (format t "~&; Wrote ~A (~A seconds)~%"
                  (namestring output-file) elapsed))))
    (values (truename output-file) warnings-p failure-p)))

(defun compile-file-if-needed (input-file &rest allargs &key force-compile
                               &allow-other-keys)
  (setf input-file (truename input-file))
  (cond (force-compile
         (remf allargs :force-compile)
         (apply 'compile-file input-file allargs))
        (t
         (let* ((source-write-time (file-write-date input-file))
                (output-file       (or (getf allargs :output-file)
                                       (compile-file-pathname input-file)))
                (target-write-time (and (probe-file output-file)
                                        (file-write-date output-file))))
           (if (or (null target-write-time)
                   (<= target-write-time source-write-time))
               (apply 'compile-file input-file allargs)
               output-file)))))

(provide 'compile-file)
