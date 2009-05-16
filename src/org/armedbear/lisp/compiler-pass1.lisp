;;; compiler-pass1.lisp
;;;
;;; Copyright (C) 2003-2008 Peter Graves
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

(in-package "JVM")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "LOOP")
  (require "FORMAT")
  (require "CLOS")
  (require "PRINT-OBJECT")
  (require "COMPILER-TYPES")
  (require "KNOWN-FUNCTIONS")
  (require "KNOWN-SYMBOLS")
  (require "DUMP-FORM")
  (require "OPCODES")
  (require "JAVA"))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun generate-inline-expansion (block-name lambda-list body)
    (cond ((intersection lambda-list
                         '(&optional &rest &key &allow-other-keys &aux)
                         :test #'eq)
           nil)
          (t
           (setf body (copy-tree body))
           (list 'LAMBDA lambda-list
                 (precompile-form (list* 'BLOCK block-name body) t)))))
  ) ; EVAL-WHEN

;;; Pass 1.


;; Returns a list of declared free specials, if any are found.
(declaim (ftype (function (list list) list) process-declarations-for-vars))
(defun process-declarations-for-vars (body variables)
  (let ((free-specials '()))
    (dolist (subform body)
      (unless (and (consp subform) (eq (%car subform) 'DECLARE))
        (return))
      (let ((decls (%cdr subform)))
        (dolist (decl decls)
          (case (car decl)
            ((DYNAMIC-EXTENT FTYPE INLINE NOTINLINE OPTIMIZE)
             ;; Nothing to do here.
             )
            ((IGNORE IGNORABLE)
             (process-ignore/ignorable (%car decl) (%cdr decl) variables))
            (SPECIAL
             (dolist (name (%cdr decl))
               (let ((variable (find-variable name variables)))
                 (cond ((and variable
                             ;; see comment below (and DO-ALL-SYMBOLS.11)
                             (eq (variable-compiland variable)
                                 *current-compiland*))
                        (setf (variable-special-p variable) t))
                       (t
                        (dformat t "adding free special ~S~%" name)
                        (push (make-variable :name name :special-p t)
                              free-specials))))))
            (TYPE
             (dolist (name (cddr decl))
               (let ((variable (find-variable name variables)))
                 (when (and variable
                            ;; Don't apply a declaration in a local function to
                            ;; a variable defined in its parent. For an example,
                            ;; see CREATE-GREEDY-NO-ZERO-MATCHER in cl-ppcre.
                            ;; FIXME suboptimal, since we ignore the declaration
                            (eq (variable-compiland variable)
                                *current-compiland*))
                   (setf (variable-declared-type variable)
                         (make-compiler-type (cadr decl)))))))
            (t
             (dolist (name (cdr decl))
               (let ((variable (find-variable name variables)))
                 (when variable
                   (setf (variable-declared-type variable)
                         (make-compiler-type (%car decl)))))))))))
    free-specials))

(defun check-name (name)
  ;; FIXME Currently this error is signalled by the precompiler.
  (unless (symbolp name)
    (compiler-error "The variable ~S is not a symbol." name))
  (when (constantp name)
    (compiler-error "The name of the variable ~S is already in use to name a constant." name))
  name)

(declaim (ftype (function (t) t) p1-body))
(defun p1-body (body)
  (declare (optimize speed))
  (let ((tail body))
    (loop
      (when (endp tail)
        (return))
      (setf (car tail) (p1 (%car tail)))
      (setf tail (%cdr tail))))
  body)

(defknown p1-default (t) t)
(declaim (inline p1-default))
(defun p1-default (form)
  (setf (cdr form) (p1-body (cdr form)))
  form)

(defknown p1-if (t) t)
(defun p1-if (form)
  (let ((test (cadr form)))
    (cond ((unsafe-p test)
           (cond ((and (consp test)
                       (memq (%car test) '(GO RETURN-FROM THROW)))
                  (p1 test))
                 (t
                  (let* ((var (gensym))
                         (new-form
                          `(let ((,var ,test))
                             (if ,var ,(third form) ,(fourth form)))))
                    (p1 new-form)))))
          (t
           (p1-default form)))))


(defmacro p1-let/let*-vars 
    (varlist variables-var var body1 body2)
  (let ((varspec (gensym))
	(initform (gensym))
	(name (gensym)))
    `(let ((,variables-var ()))
       (dolist (,varspec ,varlist)
	 (cond ((consp ,varspec)
                ;; Even though the precompiler already signals this
                ;; error, double checking can't hurt; after all, we're
                ;; also rewriting &AUX into LET* bindings.
		(unless (<= 1 (length ,varspec) 2)
		  (compiler-error "The LET/LET* binding specification ~S is invalid."
				  ,varspec))
		(let* ((,name (%car ,varspec))
		       (,initform (p1 (%cadr ,varspec)))
		       (,var (make-variable :name (check-name ,name)
                                            :initform ,initform)))
		  (push ,var ,variables-var)
		  ,@body1))
	       (t
		(let ((,var (make-variable :name (check-name ,varspec))))
		  (push ,var ,variables-var)
		  ,@body1))))
       ,@body2)))

(defknown p1-let-vars (t) t)
(defun p1-let-vars (varlist)
  (p1-let/let*-vars 
   varlist vars var
   ()
   ((setf vars (nreverse vars))
    (dolist (variable vars)
      (push variable *visible-variables*)
      (push variable *all-variables*))
    vars)))

(defknown p1-let*-vars (t) t)
(defun p1-let*-vars (varlist)
  (p1-let/let*-vars 
   varlist vars var
   ((push var *visible-variables*)
    (push var *all-variables*))
   ((nreverse vars))))

(defun p1-let/let* (form)
  (declare (type cons form))
  (let* ((*visible-variables* *visible-variables*)
         (block (make-block-node '(LET)))
         (*blocks* (cons block *blocks*))
         (op (%car form))
         (varlist (cadr form))
         (body (cddr form)))
    (aver (or (eq op 'LET) (eq op 'LET*)))
    (when (eq op 'LET)
      ;; Convert to LET* if possible.
      (if (null (cdr varlist))
          (setf op 'LET*)
          (dolist (varspec varlist (setf op 'LET*))
            (or (atom varspec)
                (constantp (cadr varspec))
                (eq (car varspec) (cadr varspec))
                (return)))))
    (let ((vars (if (eq op 'LET)
                    (p1-let-vars varlist)
                    (p1-let*-vars varlist))))
      ;; Check for globally declared specials.
      (dolist (variable vars)
        (when (special-variable-p (variable-name variable))
          (setf (variable-special-p variable) t
                (block-environment-register block) t)))
      ;; For processing declarations, we want to walk the variable list from
      ;; last to first, since declarations apply to the last-defined variable
      ;; with the specified name.
      (setf (block-free-specials block)
            (process-declarations-for-vars body (reverse vars)))
      (setf (block-vars block) vars)
      ;; Make free specials visible.
      (dolist (variable (block-free-specials block))
        (push variable *visible-variables*)))
    (setf body (p1-body body))
    (setf (block-form block) (list* op varlist body))
    block))

(defun p1-locally (form)
  (let ((*visible-variables* *visible-variables*)
        (specials (process-special-declarations (cdr form))))
    (dolist (name specials)
;;       (format t "p1-locally ~S is special~%" name)
      (push (make-variable :name name :special-p t) *visible-variables*))
    (setf (cdr form) (p1-body (cdr form)))
    form))

(defknown p1-m-v-b (t) t)
(defun p1-m-v-b (form)
  (when (= (length (cadr form)) 1)
    (let ((new-form `(let* ((,(caadr form) ,(caddr form))) ,@(cdddr form))))
      (return-from p1-m-v-b (p1-let/let* new-form))))
  (let* ((*visible-variables* *visible-variables*)
         (block (make-block-node '(MULTIPLE-VALUE-BIND)))
         (*blocks* (cons block *blocks*))
         (varlist (cadr form))
         (values-form (caddr form))
         (body (cdddr form)))
    ;; Process the values-form first. ("The scopes of the name binding and
    ;; declarations do not include the values-form.")
    (setf values-form (p1 values-form))
    (let ((vars ()))
      (dolist (symbol varlist)
        (let ((var (make-variable :name symbol)))
          (push var vars)
          (push var *visible-variables*)
          (push var *all-variables*)))
      ;; Check for globally declared specials.
      (dolist (variable vars)
        (when (special-variable-p (variable-name variable))
          (setf (variable-special-p variable) t
                (block-environment-register block) t)))
      (setf (block-free-specials block)
            (process-declarations-for-vars body vars))
      (setf (block-vars block) (nreverse vars)))
    (setf body (p1-body body))
    (setf (block-form block)
          (list* 'MULTIPLE-VALUE-BIND varlist values-form body))
    block))

(defun p1-block (form)
  (let* ((block (make-block-node (cadr form)))
         (*blocks* (cons block *blocks*)))
    (setf (cddr form) (p1-body (cddr form)))
    (setf (block-form block) form)
    block))

(defun p1-catch (form)
  (let* ((tag (p1 (cadr form)))
         (body (cddr form))
         (block (make-block-node '(CATCH)))
         ;; our subform processors need to know
         ;; they're enclosed in a CATCH block
         (*blocks* (cons block *blocks*))
         (result '()))
    (dolist (subform body)
      (let ((op (and (consp subform) (%car subform))))
        (push (p1 subform) result)
        (when (memq op '(GO RETURN-FROM THROW))
          (return))))
    (setf result (nreverse result))
    (when (and (null (cdr result))
               (consp (car result))
               (eq (caar result) 'GO))
      (return-from p1-catch (car result)))
    (push tag result)
    (push 'CATCH result)
    (setf (block-form block) result)
    block))

(defun p1-unwind-protect (form)
  (if (= (length form) 2)
      (p1 (second form)) ; No cleanup forms: (unwind-protect (...)) => (...)

      ;; in order to compile the cleanup forms twice (see
      ;; p2-unwind-protect-node), we need to p1 them twice; p1 outcomes
      ;; can be compiled (in the same compiland?) only once.
      ;;
      ;; However, p1 transforms the forms being processed, so, we
      ;; need to copy the forms to create a second copy.
      (let* ((block (make-block-node '(UNWIND-PROTECT)))
             ;; a bit of jumping through hoops...
             (unwinding-forms (p1-body (copy-tree (cddr form))))
             (unprotected-forms (p1-body (cddr form)))
             ;; ... because only the protected form is
             ;; protected by the UNWIND-PROTECT block
             (*blocks* (cons block *blocks*))
             (protected-form (p1 (cadr form))))
        (setf (block-form block)
              `(unwind-protect ,protected-form
                 (progn ,@unwinding-forms)
                 ,@unprotected-forms))
        block)))

(defknown p1-return-from (t) t)
(defun p1-return-from (form)
  (let* ((name (second form))
         (block (find-block name)))
    (when (null block)
      (compiler-error "RETURN-FROM ~S: no block named ~S is currently visible."
                      name name))
    (dformat t "p1-return-from block = ~S~%" (block-name block))
    (setf (block-return-p block) t)
    (cond ((eq (block-compiland block) *current-compiland*)
           ;; Local case. If the RETURN is nested inside an UNWIND-PROTECT
           ;; which is inside the block we're returning from, we'll do a non-
           ;; local return anyway so that UNWIND-PROTECT can catch it and run
           ;; its cleanup forms.
           (dformat t "*blocks* = ~S~%" (mapcar #'block-name *blocks*))
           (let ((protected (enclosed-by-protected-block-p block)))
             (dformat t "p1-return-from protected = ~S~%" protected)
             (if protected
                 (setf (block-non-local-return-p block) t)
                 ;; non-local GO's ensure environment restoration
                 ;; find out about this local GO
                 (when (null (block-needs-environment-restoration block))
                   (setf (block-needs-environment-restoration block)
                         (enclosed-by-environment-setting-block-p block))))))
          (t
           (setf (block-non-local-return-p block) t)))
    (when (block-non-local-return-p block)
      (dformat t "non-local return from block ~S~%" (block-name block))))
  (list* 'RETURN-FROM (cadr form) (mapcar #'p1 (cddr form))))

(defun p1-tagbody (form)
  (let* ((block (make-block-node '(TAGBODY)))
         (*blocks* (cons block *blocks*))
         (*visible-tags* *visible-tags*)
         (local-tags '())
         (body (cdr form)))
    ;; Make all the tags visible before processing the body forms.
    (dolist (subform body)
      (when (or (symbolp subform) (integerp subform))
        (let* ((tag (make-tag :name subform :label (gensym) :block block)))
          (push tag local-tags)
          (push tag *visible-tags*))))
    (let ((new-body '())
          (live t))
      (dolist (subform body)
        (cond ((or (symbolp subform) (integerp subform))
               (push subform new-body)
               (push (find subform local-tags :key #'tag-name :test #'eql)
                     (block-tags block))
               (setf live t))
              ((not live)
               ;; Nothing to do.
               )
              (t
               (when (and (consp subform)
                          (memq (%car subform) '(GO RETURN-FROM THROW)))
                 ;; Subsequent subforms are unreachable until we see another
                 ;; tag.
                 (setf live nil))
               (push (p1 subform) new-body))))
      (setf (block-form block) (list* 'TAGBODY (nreverse new-body))))
    block))

(defknown p1-go (t) t)
(defun p1-go (form)
  (let* ((name (cadr form))
         (tag (find-tag name)))
    (unless tag
      (error "p1-go: tag not found: ~S" name))
    (setf (tag-used tag) t)
    (let ((tag-block (tag-block tag)))
      (cond ((eq (tag-compiland tag) *current-compiland*)
             ;; Does the GO leave an enclosing UNWIND-PROTECT or CATCH?
             (if (enclosed-by-protected-block-p tag-block)
                 (setf (block-non-local-go-p tag-block) t)
                 ;; non-local GO's ensure environment restoration
                 ;; find out about this local GO
                 (when (null (block-needs-environment-restoration tag-block))
                   (setf (block-needs-environment-restoration tag-block)
                         (enclosed-by-environment-setting-block-p tag-block)))))
            (t
             (setf (block-non-local-go-p tag-block) t)))))
  form)

(defun validate-function-name (name)
  (unless (or (symbolp name) (setf-function-name-p name))
    (compiler-error "~S is not a valid function name." name)))

(defmacro with-local-functions-for-flet/labels
    (form local-functions-var lambda-list-var name-var body-var body1 body2)
  `(progn (incf (compiland-children *current-compiland*) (length (cadr ,form)))
	  (let ((*visible-variables* *visible-variables*)
		(*local-functions* *local-functions*)
		(*current-compiland* *current-compiland*)
		(,local-functions-var '()))
	    (dolist (definition (cadr ,form))
	      (let ((,name-var (car definition))
		    (,lambda-list-var (cadr definition)))
		(validate-function-name ,name-var)
		(let* ((,body-var (cddr definition))
		       (compiland (make-compiland :name ,name-var
						  :parent *current-compiland*)))
		  ,@body1)))
	    (setf ,local-functions-var (nreverse ,local-functions-var))
	    ;; Make the local functions visible.
	    (dolist (local-function ,local-functions-var)
	      (push local-function *local-functions*)
	      (let ((variable (local-function-variable local-function)))
		(when variable
		  (push variable *visible-variables*))))
	    ,@body2)))

(defun split-decls (forms specific-vars)
  (let ((other-decls nil)
        (specific-decls nil))
    (dolist (form forms)
      (unless (and (consp form) (eq (car form) 'DECLARE)) ; shouldn't happen
        (return))
      (dolist (decl (cdr form))
        (case (car decl)
          ((OPTIMIZE DECLARATION DYNAMIC-EXTENT FTYPE INLINE NOTINLINE)
           (push (list 'DECLARE decl) other-decls))
          (SPECIAL
           (dolist (name (cdr decl))
             (if (memq name specific-vars)
                 (push `(DECLARE (SPECIAL ,name)) specific-decls)
                 (push `(DECLARE (SPECIAL ,name)) other-decls))))
          (TYPE
           (dolist (name (cddr decl))
             (if (memq name specific-vars)
                 (push `(DECLARE (TYPE ,(cadr decl) ,name)) specific-decls)
                 (push `(DECLARE (TYPE ,(cadr decl) ,name)) other-decls))))
          (t
           (dolist (name (cdr decl))
             (if (memq name specific-vars)
                 (push `(DECLARE (,(car decl) ,name)) specific-decls)
                 (push `(DECLARE (,(car decl) ,name)) other-decls)))))))
    (values (nreverse other-decls)
            (nreverse specific-decls))))

(defun rewrite-aux-vars (form)
  (let* ((lambda-list (cadr form))
         (aux-p (memq '&AUX lambda-list))
         (lets (cdr aux-p))
         aux-vars)
    (unless aux-p
      ;; no rewriting required
      (return-from rewrite-aux-vars form))
    (multiple-value-bind (body decls)
        (parse-body (cddr form))
      (dolist (form lets)
        (cond ((consp form)
               (push (car form) aux-vars))
              (t
               (push form aux-vars))))
      (setf lambda-list (subseq lambda-list 0 (position '&AUX lambda-list)))
      (multiple-value-bind (let-decls lambda-decls)
          (split-decls decls (lambda-list-names lambda-list))
        `(lambda ,lambda-list
           ,@lambda-decls
           (let* ,lets
             ,@let-decls
             ,@body))))))

(defun rewrite-lambda (form)
  (setf form (rewrite-aux-vars form))
  (let* ((lambda-list (cadr form)))
    (if (not (or (memq '&optional lambda-list)
                 (memq '&key lambda-list)))
        ;; no need to rewrite: no arguments with possible initforms anyway
        form
      (multiple-value-bind (body decls doc)
          (parse-body (cddr form))
        (let (state let-bindings new-lambda-list
                    (non-constants 0))
          (do* ((vars lambda-list (cdr vars))
                (var (car vars) (car vars)))
               ((endp vars))
            (push (car vars) new-lambda-list)
            (let ((replacement (gensym)))
              (flet ((parse-compound-argument (arg)
                       "Returns the values NAME, KEYWORD, INITFORM, INITFORM-P,
   SUPPLIED-P and SUPPLIED-P-P assuming ARG is a compound argument."
                       (destructuring-bind
                             (name &optional (initform nil initform-supplied-p)
                                   (supplied-p nil supplied-p-supplied-p))
                           (if (listp arg) arg (list arg))
                         (if (listp name)
                             (values (cadr name) (car name)
                                     initform initform-supplied-p
                                     supplied-p supplied-p-supplied-p)
                             (values name (make-keyword name)
                                     initform initform-supplied-p
                                     supplied-p supplied-p-supplied-p)))))
                (case var
                  (&optional (setf state :optional))
                  (&key (setf state :key))
                  ((&whole &environment &rest &body &allow-other-keys)
                   ;; do nothing special
                   )
                  (t
                   (cond
                     ((atom var)
                      (setf (car new-lambda-list)
                            (if (eq state :key)
                                (list (list (make-keyword var) replacement))
                                replacement))
                      (push (list var replacement) let-bindings))
                     ((constantp (second var))
                      ;; so, we must have a consp-type var we're looking at
                      ;; and it has a constantp initform
                      (multiple-value-bind
                            (name keyword initform initform-supplied-p
                                  supplied-p supplied-p-supplied-p)
                          (parse-compound-argument var)
                        (let ((var-form (if (eq state :key)
                                            (list keyword replacement)
                                            replacement))
                              (supplied-p-replacement (gensym)))
                          (setf (car new-lambda-list)
                                (cond
                                  ((not initform-supplied-p)
                                   (list var-form))
                                  ((not supplied-p-supplied-p)
                                   (list var-form initform))
                                  (t
                                   (list var-form initform
                                         supplied-p-replacement))))
                          (push (list name replacement) let-bindings)
                          ;; if there was a 'supplied-p' variable, it might
                          ;; be used in the declarations. Since those will be
                          ;; moved below the LET* block, we need to move the
                          ;; supplied-p parameter too.
                          (when supplied-p-supplied-p
                            (push (list supplied-p supplied-p-replacement)
                                  let-bindings)))))
                     (t
                      (incf non-constants)
                      ;; this is either a keyword or an optional argument
                      ;; with a non-constantp initform
                      (multiple-value-bind
                            (name keyword initform initform-supplied-p
                                  supplied-p supplied-p-supplied-p)
                          (parse-compound-argument var)
                        (declare (ignore initform-supplied-p))
                        (let ((var-form (if (eq state :key)
                                            (list keyword replacement)
                                            replacement))
                              (supplied-p-replacement (gensym)))
                          (setf (car new-lambda-list)
                                (list var-form nil supplied-p-replacement))
                          (push (list name `(if ,supplied-p-replacement
                                                ,replacement ,initform))
                                let-bindings)
                          (when supplied-p-supplied-p
                            (push (list supplied-p supplied-p-replacement)
                                  let-bindings)))))))))))
          (if (zerop non-constants)
              ;; there was no reason to rewrite...
              form
              (let ((rv
                     `(lambda ,(nreverse new-lambda-list)
                        ,@(when doc (list doc))
                        (let* ,(nreverse let-bindings)
                          ,@decls ,@body))))
                rv)))))))

(defun p1-flet (form)
  (with-local-functions-for-flet/labels
      form local-functions lambda-list name body
      ((let ((local-function (make-local-function :name name
                                                  :compiland compiland)))
	 (multiple-value-bind (body decls) (parse-body body)
	   (let* ((block-name (fdefinition-block-name name))
		  (lambda-expression
                   (rewrite-lambda
		   `(lambda ,lambda-list ,@decls (block ,block-name ,@body))))
		  (*visible-variables* *visible-variables*)
		  (*local-functions* *local-functions*)
		  (*current-compiland* compiland))
	     (setf (compiland-lambda-expression compiland) lambda-expression)
	     (setf (local-function-inline-expansion local-function)
		   (generate-inline-expansion block-name lambda-list body))
	     (p1-compiland compiland)))
	 (push local-function local-functions)))
      ((with-saved-compiler-policy
	   (process-optimization-declarations (cddr form))
	 (list* (car form) local-functions (p1-body (cddr form)))))))


(defun p1-labels (form)
  (with-local-functions-for-flet/labels
      form local-functions lambda-list name body
      ((let* ((variable (make-variable :name (gensym)))
	      (local-function (make-local-function :name name
						   :compiland compiland
						   :variable variable)))
	 (multiple-value-bind (body decls) (parse-body body)
	   (setf (compiland-lambda-expression compiland)
                 (rewrite-lambda
		 `(lambda ,lambda-list ,@decls (block ,name ,@body)))))
	 (push variable *all-variables*)
	 (push local-function local-functions)))
      ((dolist (local-function local-functions)
	 (let ((*visible-variables* *visible-variables*)
	       (*current-compiland* (local-function-compiland local-function)))
	   (p1-compiland (local-function-compiland local-function))))
       (list* (car form) local-functions (p1-body (cddr form))))))

(defknown p1-funcall (t) t)
(defun p1-funcall (form)
  (unless (> (length form) 1)
    (compiler-warn "Wrong number of arguments for ~A." (car form))
    (return-from p1-funcall form))
  (let ((function-form (%cadr form)))
    (when (and (consp function-form)
               (eq (%car function-form) 'FUNCTION))
      (let ((name (%cadr function-form)))
;;         (format t "p1-funcall name = ~S~%" name)
        (let ((source-transform (source-transform name)))
          (when source-transform
;;             (format t "found source transform for ~S~%" name)
;;             (format t "old form = ~S~%" form)
;;             (let ((new-form (expand-source-transform form)))
;;               (when (neq new-form form)
;;                 (format t "new form = ~S~%" new-form)
;;                 (return-from p1-funcall (p1 new-form))))
            (let ((new-form (expand-source-transform (list* name (cddr form)))))
;;               (format t "new form = ~S~%" new-form)
              (return-from p1-funcall (p1 new-form)))
            )))))
  ;; Otherwise...
  (p1-function-call form))

(defun p1-function (form)
  (let ((form (copy-tree form))
        local-function)
    (cond ((and (consp (cadr form))
                (or (eq (caadr form) 'LAMBDA)
                    (eq (caadr form) 'NAMED-LAMBDA)))
           (let* ((named-lambda-p (eq (caadr form) 'NAMED-LAMBDA))
                  (named-lambda-form (when named-lambda-p
                                       (cadr form)))
                  (name (when named-lambda-p
                          (cadr named-lambda-form)))
                  (lambda-form (if named-lambda-p
                                   (cons 'LAMBDA (cddr named-lambda-form))
                                   (cadr form)))
                  (lambda-list (cadr lambda-form))
                  (body (cddr lambda-form))
                  (compiland (make-compiland :name (if named-lambda-p
                                                       name (gensym "ANONYMOUS-LAMBDA-"))
                                             :lambda-expression lambda-form
                                             :parent *current-compiland*)))
             (when *current-compiland*
               (incf (compiland-children *current-compiland*)))
             (multiple-value-bind (body decls)
                 (parse-body body)
               (setf (compiland-lambda-expression compiland)
                     ;; if there still was a doc-string present, remove it
                     (rewrite-lambda
                      `(lambda ,lambda-list ,@decls ,@body)))
               (let ((*visible-variables* *visible-variables*)
                     (*current-compiland* compiland))
                 (p1-compiland compiland)))
             (list 'FUNCTION compiland)))
          ((setf local-function (find-local-function (cadr form)))
           (dformat t "p1-function local function ~S~%" (cadr form))
           (let ((variable (local-function-variable local-function)))
             (when variable
                 (dformat t "p1-function ~S used non-locally~%"
                          (variable-name variable))
                 (setf (variable-used-non-locally-p variable) t)))
           form)
          (t
           form))))

(defun p1-lambda (form)
  (setf form (rewrite-lambda form))
  (let* ((lambda-list (cadr form)))
    (when (or (memq '&optional lambda-list)
              (memq '&key lambda-list))
      (let ((state nil))
        (dolist (arg lambda-list)
          (cond ((memq arg lambda-list-keywords)
                 (setf state arg))
                ((memq state '(&optional &key))
                 (when (and (consp arg)
                            (not (constantp (second arg))))
                   (compiler-unsupported
                    "P1-LAMBDA: can't handle optional argument with non-constant initform.")))))))
    (p1-function (list 'FUNCTION form))))

(defun p1-eval-when (form)
  (list* (car form) (cadr form) (mapcar #'p1 (cddr form))))

(defknown p1-progv (t) t)
(defun p1-progv (form)
  ;; We've already checked argument count in PRECOMPILE-PROGV.

  ;; ### FIXME: we need to return a block here, so that
  ;;  (local) GO in p2 can restore the lastSpecialBinding environment
  (let ((new-form (rewrite-progv form)))
    (when (neq new-form form)
      (return-from p1-progv (p1 new-form))))
  (let* ((symbols-form (p1 (cadr form)))
         (values-form (p1 (caddr form)))
         (block (make-block-node '(PROGV)))
         (*blocks* (cons block *blocks*))
         (body (cdddr form)))
    (setf (block-form block)
          `(progv ,symbols-form ,values-form ,@(p1-body body))
          (block-environment-register block) t)
    block))

(defknown rewrite-progv (t) t)
(defun rewrite-progv (form)
  (let ((symbols-form (cadr form))
        (values-form (caddr form))
        (body (cdddr form)))
    (cond ((or (unsafe-p symbols-form) (unsafe-p values-form))
           (let ((g1 (gensym))
                 (g2 (gensym)))
             `(let ((,g1 ,symbols-form)
                    (,g2 ,values-form))
                (progv ,g1 ,g2 ,@body))))
          (t
           form))))

(defun p1-quote (form)
  (unless (= (length form) 2)
    (compiler-error "Wrong number of arguments for special operator ~A (expected 1, but received ~D)."
                    'QUOTE
                    (1- (length form))))
  (let ((arg (%cadr form)))
    (if (or (numberp arg) (characterp arg))
        arg
        form)))

(defun p1-setq (form)
  (unless (= (length form) 3)
    (error "Too many arguments for SETQ."))
  (let ((arg1 (%cadr form))
        (arg2 (%caddr form)))
    (let ((variable (find-visible-variable arg1)))
      (if variable
          (progn
            (when (variable-ignore-p variable)
              (compiler-style-warn
               "Variable ~S is assigned even though it was declared to be ignored."
               (variable-name variable)))
            (incf (variable-writes variable))
            (cond ((eq (variable-compiland variable) *current-compiland*)
                   (dformat t "p1-setq: write ~S~%" arg1))
                  (t
                   (dformat t "p1-setq: non-local write ~S~%" arg1)
                   (setf (variable-used-non-locally-p variable) t))))
          (dformat t "p1-setq: unknown variable ~S~%" arg1)))
    (list 'SETQ arg1 (p1 arg2))))

(defun p1-the (form)
  (unless (= (length form) 3)
    (compiler-error "Wrong number of arguments for special operator ~A (expected 2, but received ~D)."
                    'THE
                    (1- (length form))))
  (let ((type (%cadr form))
        (expr (%caddr form)))
    (cond ((and (listp type) (eq (car type) 'VALUES))
           ;; FIXME
           (p1 expr))
          ((= *safety* 3)
           (let* ((sym (gensym))
                  (new-expr `(let ((,sym ,expr))
                               (require-type ,sym ',type)
                               ,sym)))
             (p1 new-expr)))
          (t
           (list 'THE type (p1 expr))))))

(defun p1-truly-the (form)
  (unless (= (length form) 3)
    (compiler-error "Wrong number of arguments for special operator ~A (expected 2, but received ~D)."
                    'TRULY-THE
                    (1- (length form))))
  (list 'TRULY-THE (%cadr form) (p1 (%caddr form))))

(defknown unsafe-p (t) t)
(defun unsafe-p (args)
  (cond ((node-p args)
         (unsafe-p (node-form args)))
        ((atom args)
         nil)
        (t
         (case (%car args)
           (QUOTE
            nil)
           (LAMBDA
            nil)
           ((RETURN-FROM GO CATCH THROW UNWIND-PROTECT BLOCK)
            t)
           (t
            (dolist (arg args)
              (when (unsafe-p arg)
                (return t))))))))

(defknown rewrite-throw (t) t)
(defun rewrite-throw (form)
  (let ((args (cdr form)))
    (if (unsafe-p args)
        (let ((syms ())
              (lets ()))
          ;; Tag.
          (let ((arg (first args)))
            (if (constantp arg)
                (push arg syms)
                (let ((sym (gensym)))
                  (push sym syms)
                  (push (list sym arg) lets))))
          ;; Result. "If the result-form produces multiple values, then all the
          ;; values are saved."
          (let ((arg (second args)))
            (if (constantp arg)
                (push arg syms)
                (let ((sym (gensym)))
                  (cond ((single-valued-p arg)
                         (push sym syms)
                         (push (list sym arg) lets))
                        (t
                         (push (list 'VALUES-LIST sym) syms)
                         (push (list sym
                                     (list 'MULTIPLE-VALUE-LIST arg))
                               lets))))))
          (list 'LET* (nreverse lets) (list* 'THROW (nreverse syms))))
        form)))

(defknown p1-throw (t) t)
(defun p1-throw (form)
  (let ((new-form (rewrite-throw form)))
    (when (neq new-form form)
      (return-from p1-throw (p1 new-form))))
  (list* 'THROW (mapcar #'p1 (cdr form))))

(defknown rewrite-function-call (t) t)
(defun rewrite-function-call (form)
  (let ((args (cdr form)))
    (if (unsafe-p args)
        (let ((arg1 (car args)))
          (cond ((and (consp arg1) (eq (car arg1) 'GO))
                 arg1)
                (t
                 (let ((syms ())
                       (lets ()))
                   ;; Preserve the order of evaluation of the arguments!
                   (dolist (arg args)
                     (cond ((constantp arg)
                            (push arg syms))
                           ((and (consp arg) (eq (car arg) 'GO))
                            (return-from rewrite-function-call
                                         (list 'LET* (nreverse lets) arg)))
                           (t
                            (let ((sym (gensym)))
                              (push sym syms)
                              (push (list sym arg) lets)))))
                   (list 'LET* (nreverse lets)
                         (list* (car form) (nreverse syms)))))))
        form)))

(defknown p1-function-call (t) t)
(defun p1-function-call (form)
  (let ((new-form (rewrite-function-call form)))
    (when (neq new-form form)
;;       (let ((*print-structure* nil))
;;         (format t "old form = ~S~%" form)
;;         (format t "new form = ~S~%" new-form))
      (return-from p1-function-call (p1 new-form))))
  (let* ((op (car form))
         (local-function (find-local-function op)))
    (cond (local-function
;;            (format t "p1 local call to ~S~%" op)
;;            (format t "inline-p = ~S~%" (inline-p op))

           (when (and *enable-inline-expansion* (inline-p op))
             (let ((expansion (local-function-inline-expansion local-function)))
               (when expansion
                 (let ((explain *explain*))
                   (when (and explain (memq :calls explain))
                     (format t ";   inlining call to local function ~S~%" op)))
                 (return-from p1-function-call
                   (p1 (expand-inline form expansion))))))

           ;; FIXME
           (dformat t "local function assumed not single-valued~%")
           (setf (compiland-%single-valued-p *current-compiland*) nil)

           (let ((variable (local-function-variable local-function)))
             (when variable
               (dformat t "p1 ~S used non-locally~%" (variable-name variable))
               (setf (variable-used-non-locally-p variable) t))))
          (t
           ;; Not a local function call.
           (dformat t "p1 non-local call to ~S~%" op)
           (unless (single-valued-p form)
;;                (format t "not single-valued op = ~S~%" op)
             (setf (compiland-%single-valued-p *current-compiland*) nil)))))
  (p1-default form))

(defknown p1 (t) t)
(defun p1 (form)
  (cond ((symbolp form)
         (let (value)
           (cond ((null form)
                  form)
                 ((eq form t)
                  form)
                 ((keywordp form)
                  form)
                 ((and (constantp form)
                       (progn
                         (setf value (symbol-value form))
                         (or (numberp value)
                             (stringp value)
                             (pathnamep value))))
                  (setf form value))
                 (t
                  (let ((variable (find-visible-variable form)))
                    (when (null variable)
		      (unless (or (special-variable-p form)
                                  (memq form *undefined-variables*))
			(compiler-style-warn
                         "Undefined variable ~S assumed special" form)
			(push form *undefined-variables*))
                      (setf variable (make-variable :name form :special-p t))
                      (push variable *visible-variables*))
                    (let ((ref (make-var-ref variable)))
                      (unless (variable-special-p variable)
                        (when (variable-ignore-p variable)
                          (compiler-style-warn
                           "Variable ~S is read even though it was declared to be ignored."
                           (variable-name variable)))
                        (push ref (variable-references variable))
                        (incf (variable-reads variable))
                        (cond ((eq (variable-compiland variable) *current-compiland*)
                               (dformat t "p1: read ~S~%" form))
                              (t
                               (dformat t "p1: non-local read ~S variable-compiland = ~S current compiland = ~S~%"
                                        form
                                        (compiland-name (variable-compiland variable))
                                        (compiland-name *current-compiland*))
                               (setf (variable-used-non-locally-p variable) t))))
                      (setf form ref)))
                  form))))
        ((atom form)
         form)
        (t
         (let ((op (%car form))
               handler)
           (cond ((symbolp op)
                  (when (compiler-macro-function op)
                    (unless (notinline-p op)
                      (multiple-value-bind (expansion expanded-p)
                          (compiler-macroexpand form)
                        ;; Fall through if no change...
                        (when expanded-p
                          (return-from p1 (p1 expansion))))))
                  (cond ((setf handler (get op 'p1-handler))
                         (funcall handler form))
                        ((macro-function op *compile-file-environment*)
                         (p1 (macroexpand form *compile-file-environment*)))
                        ((special-operator-p op)
                         (compiler-unsupported "P1: unsupported special operator ~S" op))
                        (t
                         (p1-function-call form))))
                 ((and (consp op) (eq (%car op) 'LAMBDA))
                  (p1 (list* 'FUNCALL form)))
                 (t
                  form))))))

(defun install-p1-handler (symbol handler)
  (setf (get symbol 'p1-handler) handler))

(defun initialize-p1-handlers ()
  (dolist (pair '((AND                  p1-default)
                  (BLOCK                p1-block)
                  (CATCH                p1-catch)
                  (DECLARE              identity)
                  (EVAL-WHEN            p1-eval-when)
                  (FLET                 p1-flet)
                  (FUNCALL              p1-funcall)
                  (FUNCTION             p1-function)
                  (GO                   p1-go)
                  (IF                   p1-if)
                  (LABELS               p1-labels)
                  (LAMBDA               p1-lambda)
                  (LET                  p1-let/let*)
                  (LET*                 p1-let/let*)
                  (LOAD-TIME-VALUE      identity)
                  (LOCALLY              p1-locally)
                  (MULTIPLE-VALUE-BIND  p1-m-v-b)
                  (MULTIPLE-VALUE-CALL  p1-default)
                  (MULTIPLE-VALUE-LIST  p1-default)
                  (MULTIPLE-VALUE-PROG1 p1-default)
                  (OR                   p1-default)
                  (PROGN                p1-default)
                  (PROGV                p1-progv)
                  (QUOTE                p1-quote)
                  (RETURN-FROM          p1-return-from)
                  (SETQ                 p1-setq)
                  (SYMBOL-MACROLET      identity)
                  (TAGBODY              p1-tagbody)
                  (THE                  p1-the)
                  (THROW                p1-throw)
                  (TRULY-THE            p1-truly-the)
                  (UNWIND-PROTECT       p1-unwind-protect)))
    (install-p1-handler (%car pair) (%cadr pair))))

(initialize-p1-handlers)

(defun p1-compiland (compiland)
;;   (format t "p1-compiland name = ~S~%" (compiland-name compiland))
  (let ((form (compiland-lambda-expression compiland)))
    (aver (eq (car form) 'LAMBDA))
    (setf form (rewrite-lambda form))
    (process-optimization-declarations (cddr form))

    (let* ((lambda-list (cadr form))
           (body (cddr form))
           (*visible-variables* *visible-variables*)
           (closure (make-closure `(lambda ,lambda-list nil) nil))
           (syms (sys::varlist closure))
           (vars nil))
      (dolist (sym syms)
        (let ((var (make-variable :name sym
                                  :special-p (special-variable-p sym))))
          (push var vars)
          (push var *all-variables*)
          (push var *visible-variables*)))
      (setf (compiland-arg-vars compiland) (nreverse vars))
      (let ((free-specials (process-declarations-for-vars body vars)))
        (setf (compiland-free-specials compiland) free-specials)
        (dolist (var free-specials)
          (push var *visible-variables*)))
      (setf (compiland-p1-result compiland)
            (list* 'LAMBDA lambda-list (p1-body body))))))

(provide "COMPILER-PASS1")