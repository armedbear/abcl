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

(require "LOOP")
(require "FORMAT")
(require "CLOS")
(require "PRINT-OBJECT")
(require "COMPILER-TYPES")
(require "KNOWN-FUNCTIONS")
(require "KNOWN-SYMBOLS")
(require "DUMP-FORM")
(require "JAVA")


(defun generate-inline-expansion (name lambda-list body
                                  &optional (args nil args-p))
  "Generates code that can be used to expand a named local function inline.
It can work either per-function (no args provided) or per-call."
  (if args-p
      (multiple-value-bind
            (body decls)
          (parse-body body)
        (expand-function-call-inline nil lambda-list
                                     ;; the forms below get wrapped
                                     ;; in a LET, making the decls
                                     ;; part of the decls of the LET.
                                     (copy-tree `(,@decls (block ,name ,@body)))
                                     args))
      (cond ((intersection lambda-list
                           '(&optional &rest &key &allow-other-keys &aux)
                           :test #'eq)
             nil)
            (t
             (multiple-value-bind
                   (body decls)
                 (parse-body body)
               (setf body (copy-tree body))
               `(lambda ,lambda-list ,@decls
                        (block ,name ,@body)))))))


;;; Pass 1.

(defun parse-lambda-list (lambda-list)
  "Breaks the lambda list into the different elements, returning the values

 required-vars
 optional-vars
 key-vars
 key-p
 rest-var
 allow-other-keys-p
 aux-vars
 whole-var
 env-var

where each of the vars returned is a list with these elements:

 var      - the actual variable name
 initform - the init form if applicable; optional, keyword and aux vars
 p-var    - variable indicating presence
 keyword  - the keyword argument to match against

"
  (let ((remaining lambda-list)
        (state :req)
        keyword-required
        req opt key rest whole env aux key-p allow-others-p)
    (when (eq (car lambda-list) '&WHOLE)
      (let ((var (second lambda-list)))
        (when (memq var lambda-list-keywords)
          (error 'program-error
                 :format-control "Lambda list keyword ~A found where &WHOLE ~
                                  variable expected in lambda list ~A."
                 :format-arguments (list var lambda-list)))
        (setf whole (list var))
        (setf remaining (nthcdr 2 lambda-list))))

    (do* ((arg (pop remaining) (pop tail))
          (tail remaining tail))
         ((and (null arg)
               (endp tail)))
      (let* ((allowable-previous-states
              ;; even if the arglist could theoretically contain the
              ;; keyword :req, this still works, because the cdr will
              ;; be NIL, meaning that the code below thinks we DIDN'T
              ;; find a new state. Which happens to be true.
              (cdr (member arg '(&whole &environment &aux &allow-other-keys
                                 &key &rest &optional :req)))))
        (cond
          (allowable-previous-states
           (setf keyword-required nil) ;; we have a keyword...
           (case arg
             (&key
              (setf key-p t))
             (&rest
              (when (endp tail)
                (error 'program-error
                       :format-control "&REST without variable in lambda list ~A."
                       :format-arguments (list lambda-list)))
              (setf rest (list (pop tail))
                    keyword-required t))
             (&allow-other-keys
              (unless (eq state '&KEY)
                (error 'program-error
                       :format-control "&ALLOW-OTHER-KEYS outside of &KEY ~
                                        section in lambda list ~A"
                       :format-arguments (list lambda-list)))
              (setf allow-others-p t
                    keyword-required t
                    arg nil))
             (&environment
              (setf env (list (pop tail))
                    keyword-required t
                    ;; &ENVIRONMENT can appear anywhere; retain our last
                    ;; state so we know what next keywords are valid
                    arg state))
             (&whole
              (error 'program-error
                     :format-control "&WHOLE must appear first in lambda list ~A."
                     :format-arguments (list lambda-list))))
           (when arg
             ;; ### verify that the next state is valid
             (unless (or (null state)
                         (member state allowable-previous-states))
               (error 'program-error
                      :format-control "~A not allowed after ~A ~
                                       in lambda-list ~S"
                      :format-arguments (list arg state lambda-list)))
             (setf state arg)))
          (keyword-required
           ;; a keyword was required, but none was found...
           (error 'program-error
                  :format-control "Lambda list keyword expected, but found ~
                                   ~A in lambda list ~A"
                  :format-arguments (list arg lambda-list)))
          (t ;; a variable specification
           (case state
             (:req (push (list arg) req))
             (&optional
              (cond ((symbolp arg)
                     (push (list arg) opt))
                    ((consp arg)
                     (push (list (car arg) (cadr arg)
                                 (caddr arg)) opt))
                    (t
                     (error "Invalid &OPTIONAL variable."))))
             (&key
              (cond ((symbolp arg)
                     (push (list arg nil nil (sys::keywordify arg)) key))
                    ((consp arg)
                     (push (list (if (consp (car arg))
                                     (cadar arg) (car arg))
                                 (cadr arg) (caddr arg)
                                 (if (consp (car arg))
                                     (caar arg)
                                     (sys::keywordify (car arg)))) key))
                    (t
                     (error "Invalid &KEY variable."))))
             (&aux
              (cond ((symbolp arg)
                     (push (list arg nil nil nil) aux))
                    ((consp arg)
                     (push (list (car arg) (cadr arg) nil nil) aux))
                    (t
                     (error "Invalid &aux state."))))
             (t
              (error 'program-error
                     :format-control "Invalid state found: ~A."
                     :format-arguments (list state))))))))
    (values
     (nreverse req)
     (nreverse opt)
     (nreverse key)
     key-p
     rest allow-others-p
     (nreverse aux) whole env)))

(define-condition lambda-list-mismatch (error)
  ((mismatch-type :reader lambda-list-mismatch-type :initarg :mismatch-type)))

(defmacro push-argument-binding (var form temp-bindings bindings)
  (let ((g (gensym)))
    `(let ((,g (gensym (symbol-name '#:temp))))
       (push (list ,g ,form) ,temp-bindings)
       (push (list ,var ,g) ,bindings))))

(defun match-lambda-list (parsed-lambda-list arguments)
  (flet ((pop-required-argument ()
           (if (null arguments)
               (error 'lambda-list-mismatch :mismatch-type :too-few-arguments)
               (pop arguments)))
         (var (var-info) (car var-info))
         (initform (var-info) (cadr var-info))
         (p-var (var-info) (caddr var-info)))
    (destructuring-bind (req opt key key-p rest allow-others-p aux whole env)
        parsed-lambda-list
      (declare (ignore whole env))
      (let (req-bindings temp-bindings bindings ignorables)
        ;;Required arguments.
        (setf req-bindings
              (loop :for (var) :in req
                 :collect `(,var ,(pop-required-argument))))

        ;;Optional arguments.
        (when opt
          (dolist (var-info opt)
            (if arguments
                (progn
                  (push-argument-binding (var var-info) (pop arguments)
                                         temp-bindings bindings)
                  (when (p-var var-info)
                    (push `(,(p-var var-info) t) bindings)))
                (progn
                  (push `(,(var var-info) ,(initform var-info)) bindings)
                  (when (p-var var-info)
                    (push `(,(p-var var-info) nil) bindings)))))
          (setf bindings (nreverse bindings)))
        
        (unless (or key-p rest (null arguments))
          (error 'lambda-list-mismatch :mismatch-type :too-many-arguments))

        ;;Keyword and rest arguments.
        (if key-p
            (multiple-value-bind (kbindings ktemps kignor)
                (match-keyword-and-rest-args 
                 key allow-others-p rest arguments)
              (setf bindings (append bindings kbindings)
                    temp-bindings (append temp-bindings ktemps)
                    ignorables (append kignor ignorables)))
            (when rest
              (let (rest-binding)
                (push-argument-binding (var rest) `(list ,@arguments)
                                       temp-bindings rest-binding)
                (setf bindings (append bindings rest-binding)))))
        ;;Aux parameters.
        (when aux
          (setf bindings
                `(,@bindings
                  ,@(loop
                       :for var-info :in aux
                       :collect `(,(var var-info) ,(initform var-info))))))
        (values (append req-bindings temp-bindings bindings)
                ignorables)))))

(defun match-keyword-and-rest-args (key allow-others-p rest arguments)
  (flet ((var (var-info) (car var-info))
         (initform (var-info) (cadr var-info))
         (p-var (var-info) (caddr var-info))
         (keyword (var-info) (cadddr var-info)))
    (when (oddp (list-length arguments))
      (error 'lambda-list-mismatch
             :mismatch-type :odd-number-of-keyword-arguments))
    
    (let (temp-bindings bindings other-keys-found-p ignorables already-seen
          args)
      ;;If necessary, make up a fake argument to hold :allow-other-keys,
      ;;needed later. This also handles nicely:
      ;;  3.4.1.4.1 Suppressing Keyword Argument Checking
      ;;third statement.
      (unless (find :allow-other-keys key :key #'keyword)
        (let ((allow-other-keys-temp (gensym (symbol-name :allow-other-keys))))
          (push allow-other-keys-temp ignorables)
          (push (list allow-other-keys-temp nil nil :allow-other-keys) key)))
      
      ;;First, let's bind the keyword arguments that have been passed by
      ;;the caller. If we encounter an unknown keyword, remember it.
      ;;As per the above, :allow-other-keys will never be considered
      ;;an unknown keyword.
      (loop
         :for var :in arguments :by #'cddr
         :for value :in (cdr arguments) :by #'cddr
         :do (let ((var-info (find var key :key #'keyword)))
               (if (and var-info (not (member var already-seen)))
                   ;;var is one of the declared keyword arguments
                   (progn
                     (push-argument-binding (var var-info) value
                                            temp-bindings bindings)
                     (when (p-var var-info)
                       (push `(,(p-var var-info) t) bindings))
                     (push var args)
                     (push (var var-info) args)
                     (push var already-seen))
                   (let ((g (gensym)))
                     (push `(,g ,value) temp-bindings)
                     (push var args)
                     (push g args)
                     (push g ignorables)
                     (unless var-info
                       (setf other-keys-found-p t))))))
      
      ;;Then, let's bind those arguments that haven't been passed in
      ;;to their default value, in declaration order.
      (let (defaults)
        (loop
           :for var-info :in key
           :do (unless (find (var var-info) bindings :key #'car)
                 (push `(,(var var-info) ,(initform var-info)) defaults)
                 (when (p-var var-info)
                   (push `(,(p-var var-info) nil) defaults))))
        (setf bindings (append (nreverse defaults) bindings)))
      
      ;;If necessary, check for unrecognized keyword arguments.
      (when (and other-keys-found-p (not allow-others-p))
        (if (loop
               :for var :in arguments :by #'cddr
               :if (eq var :allow-other-keys)
               :do (return t))
            ;;We know that :allow-other-keys has been passed, so we
            ;;can access the binding for it and be sure to get the
            ;;value passed by the user and not an initform.
            (let* ((arg (var (find :allow-other-keys key :key #'keyword)))
                   (binding (find arg bindings :key #'car))
                   (form (cadr binding)))
              (if (constantp form)
                  (unless (eval form)
                    (error 'lambda-list-mismatch
                           :mismatch-type :unknown-keyword))
                  (setf (cadr binding)
                        `(or ,(cadr binding)
                             (error 'program-error
                                    "Unrecognized keyword argument")))))
            ;;TODO: it would be nice to report *which* keyword
            ;;is unknown
            (error 'lambda-list-mismatch :mismatch-type :unknown-keyword)))
      (when rest
        (setf bindings (append bindings `((,(var rest) (list ,@(nreverse args)))))))
      (values bindings temp-bindings ignorables))))

#||test for the above
(handler-case
    (let ((lambda-list
           (multiple-value-list
            (jvm::parse-lambda-list
             '(a b &optional (c 42) &rest foo &key (bar c) baz ((kaz kuz) bar))))))
      (jvm::match-lambda-list
       lambda-list
       '((print 1) 3 (print 32) :bar 2)))
  (jvm::lambda-list-mismatch (x) (jvm::lambda-list-mismatch-type x)))
||#

(defun expand-function-call-inline (form lambda-list body args)
  (handler-case
      (multiple-value-bind (bindings ignorables)
          (match-lambda-list (multiple-value-list
                              (parse-lambda-list lambda-list))
                             args)
        `(let* ,bindings
           ,@(when ignorables
                   `((declare (ignorable ,@ignorables))))
           ,@body))
    (lambda-list-mismatch (x)
      (compiler-warn "Invalid function call: ~S (mismatch type: ~A)"
                     form (lambda-list-mismatch-type x))
      form)))

;; Returns a list of declared free specials, if any are found.
(declaim (ftype (function (list list block-node) list)
                process-declarations-for-vars))
(defun process-declarations-for-vars (body variables block)
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
                        (push (make-variable :name name :special-p t
                                             :block block)
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
  (loop for form in body
     collect (p1 form)))

(defknown p1-default (t) t)
(declaim (inline p1-default))
(defun p1-default (form)
  (cons (car form) (p1-body (cdr form))))

(defun let/let*-variables (block bindings)
  (loop for binding in bindings
     if (consp binding)
     collect (make-variable :name (check-name (car binding))
                            :initform (cadr binding)
                            :block block)
     else
     collect (make-variable :name (check-name binding)
                            :block block)))

(defun valid-let/let*-binding-p (varspec)
  (when (consp varspec)
    (unless (<= 1 (length varspec) 2)
      (compiler-error "The LET/LET* binding specification ~
                       ~S is invalid." varspec)))
  T)

(defun check-let/let*-bindings (bindings)
  (every #'valid-let/let*-binding-p bindings))

(defknown p1-let-vars (t) t)
(defun p1-let-vars (block varlist)
  (check-let/let*-bindings varlist)
  (let ((vars (let/let*-variables block varlist)))
    (dolist (variable vars)
      (setf (variable-initform variable)
            (p1 (variable-initform variable))))
    (dolist (variable vars)
      (push variable *visible-variables*)
      (push variable *all-variables*))
    vars))

(defknown p1-let*-vars (t) t)
(defun p1-let*-vars (block varlist)
  (check-let/let*-bindings varlist)
  (let ((vars (let/let*-variables block varlist)))
    (dolist (variable vars)
      (setf (variable-initform variable)
            (p1 (variable-initform variable)))
      (push variable *visible-variables*)
      (push variable *all-variables*))
    vars))

(defun p1-let/let* (form)
  (declare (type cons form))
  (let* ((*visible-variables* *visible-variables*)
         (block (make-let/let*-node))
         (*block* block)
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
                    (p1-let-vars block varlist)
                    (p1-let*-vars block varlist))))
      ;; Check for globally declared specials.
      (dolist (variable vars)
        (when (special-variable-p (variable-name variable))
          (setf (variable-special-p variable) t
                (let-environment-register block) t)))
      ;; For processing declarations, we want to walk the variable list from
      ;; last to first, since declarations apply to the last-defined variable
      ;; with the specified name.
      (setf (let-free-specials block)
            (process-declarations-for-vars body (reverse vars) block))
      (setf (let-vars block) vars)
      ;; Make free specials visible.
      (dolist (variable (let-free-specials block))
        (push variable *visible-variables*)))
    (with-saved-compiler-policy
      (process-optimization-declarations body)
      (let ((*blocks* (cons block *blocks*)))
        (setf body (p1-body body)))
      (setf (let-form block) (list* op varlist body))
      block)))

(defun p1-locally (form)
  (let* ((*visible-variables* *visible-variables*)
         (block (make-locally-node))
         (*block* block)
         (free-specials (process-declarations-for-vars (cdr form) nil block)))
    (setf (locally-free-specials block) free-specials)
    (dolist (special free-specials)
;;       (format t "p1-locally ~S is special~%" name)
      (push special *visible-variables*))
    (let ((*blocks* (cons block *blocks*)))
      (with-saved-compiler-policy
        (process-optimization-declarations (cdr form))
        (setf (locally-form block)
              (list* 'LOCALLY (p1-body (cdr form))))
        block))))

(defknown p1-m-v-b (t) t)
(defun p1-m-v-b (form)
  (when (= (length (cadr form)) 1)
    (let ((new-form `(let* ((,(caadr form) ,(caddr form))) ,@(cdddr form))))
      (return-from p1-m-v-b (p1-let/let* new-form))))
  (let* ((*visible-variables* *visible-variables*)
         (block (make-m-v-b-node))
         (*block* block)
         (varlist (cadr form))
         ;; Process the values-form first. ("The scopes of the name binding and
         ;; declarations do not include the values-form.")
         (values-form (p1 (caddr form)))
         (*blocks* (cons block *blocks*))
         (body (cdddr form)))
    (let ((vars ()))
      (dolist (symbol varlist)
        (let ((var (make-variable :name symbol :block block)))
          (push var vars)
          (push var *visible-variables*)
          (push var *all-variables*)))
      ;; Check for globally declared specials.
      (dolist (variable vars)
        (when (special-variable-p (variable-name variable))
          (setf (variable-special-p variable) t
                (m-v-b-environment-register block) t)))
      (setf (m-v-b-free-specials block)
            (process-declarations-for-vars body vars block))
      (dolist (special (m-v-b-free-specials block))
        (push special *visible-variables*))
      (setf (m-v-b-vars block) (nreverse vars)))
    (with-saved-compiler-policy
      (process-optimization-declarations body)
      (setf body (p1-body body))
      (setf (m-v-b-form block)
            (list* 'MULTIPLE-VALUE-BIND varlist values-form body))
      block)))

(defun p1-block (form)
  (let* ((block (make-block-node (cadr form)))
         (*block* block)
         (*blocks* (cons block *blocks*))
         (form (list* (car form) (cadr form) (p1-body (cddr form)))))
    (setf (block-form block) form)
    (when (block-non-local-return-p block)
      ;; Add a closure variable for RETURN-FROM to use
      (push (setf (block-id-variable block)
                  (make-variable :name (gensym)
                                 :block block
                                 :used-non-locally-p t))
            *all-variables*))
    block))

(defun p1-catch (form)
  (let* ((tag (p1 (cadr form)))
         (body (cddr form))
         (block (make-catch-node))
         (*block* block)
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
    (setf (catch-form block) result)
    block))

(defun p1-threads-synchronized-on (form)
  (let* ((synchronized-object (p1 (cadr form)))
         (body (cddr form))
         (block (make-synchronized-node))
         (*block* block)
         (*blocks* (cons block *blocks*))
         result)
    (dolist (subform body)
      (let ((op (and (consp subform) (%car subform))))
        (push (p1 subform) result)
        (when (memq op '(GO RETURN-FROM THROW))
          (return))))
    (setf (synchronized-form block)
          (list* 'threads:synchronized-on synchronized-object
                 (nreverse result)))
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
      (let* ((block (make-unwind-protect-node))
             (*block* block)
             ;; a bit of jumping through hoops...
             (unwinding-forms (p1-body (copy-tree (cddr form))))
             (unprotected-forms (p1-body (cddr form)))
             ;; ... because only the protected form is
             ;; protected by the UNWIND-PROTECT block
             (*blocks* (cons block *blocks*))
             (protected-form (p1 (cadr form))))
        (setf (unwind-protect-form block)
              `(unwind-protect ,protected-form
                 (progn ,@unwinding-forms)
                 ,@unprotected-forms))
        block)))

(defknown p1-return-from (t) t)
(defun p1-return-from (form)
  (let* ((name (second form))
         (block (find-block name))
         non-local-p)
    (when (null block)
      (compiler-error "RETURN-FROM ~S: no block named ~S is currently visible."
                      name name))
    (dformat t "p1-return-from block = ~S~%" (block-name block))
    (cond ((eq (block-compiland block) *current-compiland*)
           ;; Local case. If the RETURN is nested inside an UNWIND-PROTECT
           ;; which is inside the block we're returning from, we'll do a non-
           ;; local return anyway so that UNWIND-PROTECT can catch it and run
           ;; its cleanup forms.
           ;;(dformat t "*blocks* = ~S~%" (mapcar #'node-name *blocks*))
           (let ((protected (enclosed-by-protected-block-p block)))
             (dformat t "p1-return-from protected = ~S~%" protected)
             (if protected
                 (setf (block-non-local-return-p block) t
                       non-local-p t)
                 ;; non-local GO's ensure environment restoration
                 ;; find out about this local GO
                 (when (null (block-needs-environment-restoration block))
                   (setf (block-needs-environment-restoration block)
                         (enclosed-by-environment-setting-block-p block))))))
          (t
           (setf (block-non-local-return-p block) t
                 non-local-p t)))
    (when (block-non-local-return-p block)
      (dformat t "non-local return from block ~S~%" (block-name block)))
    (let ((value-form (p1 (caddr form))))
      (push value-form (block-return-value-forms block))
      (make-jump-node (list 'RETURN-FROM name value-form)
                      non-local-p block))))

(defun p1-tagbody (form)
  (let* ((block (make-tagbody-node))
         (*block* block)
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
                     (tagbody-tags block))
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
      (setf (tagbody-form block) (list* 'TAGBODY (nreverse new-body))))
    (when (some #'tag-used-non-locally (tagbody-tags block))
      (push (setf (tagbody-id-variable block)
                  (make-variable :name (gensym)
                                 :block block
                                 :used-non-locally-p t))
            *all-variables*))
    block))

(defknown p1-go (t) t)
(defun p1-go (form)
  (let* ((name (cadr form))
         (tag (find-tag name)))
    (unless tag
      (error "p1-go: tag not found: ~S" name))
    (setf (tag-used tag) t)
    (let ((tag-block (tag-block tag))
          non-local-p)
      (cond ((eq (tag-compiland tag) *current-compiland*)
             ;; Does the GO leave an enclosing UNWIND-PROTECT or CATCH?
             (if (enclosed-by-protected-block-p tag-block)
                 (setf (tagbody-non-local-go-p tag-block) t
                       (tag-used-non-locally tag) t
                       non-local-p t)
                 ;; non-local GO's ensure environment restoration
                 ;; find out about this local GO
                 (when (null (tagbody-needs-environment-restoration tag-block))
                   (setf (tagbody-needs-environment-restoration tag-block)
                         (enclosed-by-environment-setting-block-p tag-block)))))
            (t
             (setf (tagbody-non-local-go-p tag-block) t
                   (tag-used-non-locally tag) t
                   non-local-p t)))
      (make-jump-node form non-local-p tag-block tag))))

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

(defun lambda-list-names (lambda-list)
  "Returns a list of variable names extracted from `lambda-list'."
  (multiple-value-bind
        (req opt key key-p rest allow-key-p aux whole env)
      (parse-lambda-list lambda-list)
    (declare (ignore key-p allow-key-p))
    (mapcan (lambda (x)
              (mapcar #'first x))
            (list req opt key aux (list rest) (list whole) (list env)))))

(defun lambda-list-keyword-p (x)
  (memq x lambda-list-keywords))

(defun rewrite-aux-vars (form)
  (let* ((lambda-list (cadr form))
         (aux-p (memq '&AUX lambda-list))
         (post-aux-&environment (memq '&ENVIRONMENT aux-p))
         (lets (ldiff (cdr aux-p) post-aux-&environment)) ; strip trailing &environment
         aux-vars)
    (unless aux-p
      ;; no rewriting required
      (return-from rewrite-aux-vars form))
    (dolist (var lets)
      (when (lambda-list-keyword-p var)
        (error 'program-error
               :format-control "Lambda list keyword ~A not allowed after &AUX in ~A."
               :format-arguments (list var lambda-list))))
    (multiple-value-bind (body decls)
        (parse-body (cddr form))
      (dolist (form lets)
        (cond ((consp form)
               (push (car form) aux-vars))
              (t
               (push form aux-vars))))
      (setf lambda-list
            (append (subseq lambda-list 0 (position '&AUX lambda-list))
                    post-aux-&environment))
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

(defun validate-function-name (name)
  (unless (or (symbolp name) (setf-function-name-p name))
    (compiler-error "~S is not a valid function name." name))
  name)

(defun construct-flet/labels-function (definition)
  (let* ((name (car definition))
         (block-name (fdefinition-block-name (validate-function-name name)))
         (lambda-list (cadr definition))
         (compiland (make-compiland :name name :parent *current-compiland*))
         (local-function (make-local-function :name name :compiland compiland)))
    (push local-function (compiland-children *current-compiland*))
    (multiple-value-bind
          (body decls)
        (parse-body (cddr definition))
      (setf (local-function-definition local-function)
            (copy-tree (cdr definition)))
      (setf (compiland-lambda-expression compiland)
            (rewrite-lambda `(lambda ,lambda-list
                               ,@decls
                               (block ,block-name
                                 ,@body)))))
    local-function))

(defun p1-flet (form)
  (let* ((local-functions
          (mapcar #'(lambda (definition)
                      (construct-flet/labels-function definition))
                  (cadr form)))
         (*local-functions* *local-functions*))
    (dolist (local-function local-functions)
      (p1-compiland (local-function-compiland local-function)))
    (dolist (local-function local-functions)
      (push local-function *local-functions*))
    (with-saved-compiler-policy
      (process-optimization-declarations (cddr form))
      (let* ((block (make-flet-node))
             (*block* block)
             (*blocks* (cons block *blocks*))
             (body (cddr form))
             (*visible-variables* *visible-variables*))
        (setf (flet-free-specials block)
              (process-declarations-for-vars body nil block))
        (dolist (special (flet-free-specials block))
          (push special *visible-variables*))
        (setf body (p1-body body) ;; affects the outcome of references-needed-p
              (flet-form block)
              (list* (car form)
                     (remove-if #'(lambda (fn)
                                    (and (inline-p (local-function-name fn))
                                         (not (local-function-references-needed-p fn))))
                                local-functions)
                     body))
        block))))


(defun p1-labels (form)
  (let* ((local-functions
          (mapcar #'(lambda (definition)
                      (construct-flet/labels-function definition))
                  (cadr form)))
         (*local-functions* *local-functions*)
         (*visible-variables* *visible-variables*))
    (dolist (local-function local-functions)
      (push local-function *local-functions*))
    (dolist (local-function local-functions)
      (p1-compiland (local-function-compiland local-function)))
    (let* ((block (make-labels-node))
           (*block* block)
           (*blocks* (cons block *blocks*))
           (body (cddr form))
           (*visible-variables* *visible-variables*))
      (setf (labels-free-specials block)
            (process-declarations-for-vars body nil block))
      (dolist (special (labels-free-specials block))
        (push special *visible-variables*))
      (with-saved-compiler-policy
        (process-optimization-declarations (cddr form))
        (setf (labels-form block)
              (list* (car form) local-functions (p1-body (cddr form))))
        block))))

(defknown p1-funcall (t) t)
(defun p1-funcall (form)
  (unless (> (length form) 1)
    (compiler-warn "Wrong number of arguments for ~A." (car form))
    (return-from p1-funcall form))
  (let ((function-form (%cadr form)))
    (when (and (consp function-form)
               (eq (%car function-form) 'FUNCTION))
      (let ((name (%cadr function-form)))
        (let ((source-transform (source-transform name)))
          (when source-transform
            (let ((new-form (expand-source-transform (list* name (cddr form)))))
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
                                             :parent *current-compiland*))
                  (local-function (make-local-function :compiland compiland)))
             (push local-function (compiland-children *current-compiland*))
             (multiple-value-bind (body decls)
                 (parse-body body)
               (setf (compiland-lambda-expression compiland)
                     ;; if there still was a doc-string present, remove it
                     (rewrite-lambda
                      `(lambda ,lambda-list ,@decls ,@body)))
               (let ((*visible-variables* *visible-variables*)
                     (*current-compiland* compiland))
                 (p1-compiland compiland)))
             (list 'FUNCTION local-function)))
          ((setf local-function (find-local-function (cadr form)))
           (dformat "p1-function local function ~S~%" (cadr form))
           ;;we found out that the function needs a reference
           (setf (local-function-references-needed-p local-function) t)
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
  (let* ((symbols-form (p1 (cadr form)))
         (values-form (p1 (caddr form)))
         (block (make-progv-node))
         (*block* block)
         (*blocks* (cons block *blocks*))
         (body (cdddr form)))
;;  The (commented out) block below means to detect compile-time
;;  enumeration of bindings to be created (a quoted form in the symbols
;;  position).
;;    (when (and (quoted-form-p symbols-form)
;;               (listp (second symbols-form)))
;;      (dolist (name (second symbols-form))
;;        (let ((variable (make-variable :name name :special-p t)))
;;          (push 
    (setf (progv-environment-register block) t
          (progv-form block)
          `(progv ,symbols-form ,values-form ,@(p1-body body)))
    block))

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
          ((and (<= 1 *safety* 2) ;; at safety 1 or 2 check relatively
                (symbolp type))   ;; simple types (those specified by a single symbol)
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

(defknown p1-throw (t) t)
(defun p1-throw (form)
  (list* 'THROW (mapcar #'p1 (cdr form))))

(defknown rewrite-function-call (t) t)
(defun rewrite-function-call (form)
  (let ((op (car form)) (args (cdr form)))
    (cond
      ((and (eq op 'funcall) (listp (car args)) (eq (caar args) 'lambda))
       ;;(funcall (lambda (...) ...) ...)
       (let ((op (car args)) (args (cdr args)))
         (expand-function-call-inline form (cadr op) (copy-tree (cddr op))
                                      args)))
      ((and (listp op) (eq (car op) 'lambda))
       ;;((lambda (...) ...) ...)
       (expand-function-call-inline form (cadr op) (copy-tree (cddr op)) args))
      (t form))))

(defknown p1-function-call (t) t)
(defun p1-function-call (form)
  (let ((new-form (rewrite-function-call form)))
    (when (neq new-form form)
      (return-from p1-function-call (p1 new-form))))
  (let* ((op (car form))
         (local-function (find-local-function op)))
    (when local-function
      (when (and *enable-inline-expansion* (inline-p op)
                 (local-function-definition local-function))
        (let* ((definition (local-function-definition local-function))
               (lambda-list (car definition))
               (body (cdr definition))
               (expansion (generate-inline-expansion op lambda-list body
                                                     (cdr form))))
          (when expansion
            (let ((explain *explain*))
              (when (and explain (memq :calls explain))
                (format t ";   inlining call to local function ~S~%" op)))
            (return-from p1-function-call
                         (let ((*inline-declarations*
                                (remove op *inline-declarations* :key #'car :test #'equal)))
                           (p1 expansion))))))))
  (p1-default form))

(defun %funcall (fn &rest args)
  "Dummy FUNCALL wrapper to force p1 not to optimize the call."
  (apply fn args))

(defun p1-variable-reference (var)
  (let ((variable (find-visible-variable var)))
    (when (null variable)
      (unless (or (special-variable-p var)
                  (memq var *undefined-variables*))
        (compiler-style-warn
         "Undefined variable ~S assumed special" var)
        (push var *undefined-variables*))
      (setf variable (make-variable :name var :special-p t))
      (push variable *visible-variables*))
    (let ((ref (make-var-ref variable)))
      (unless (variable-special-p variable)
        (when (variable-ignore-p variable)
          (compiler-style-warn
           "Variable ~S is read even though it was declared to be ignored."
           (variable-name variable)))
        (push ref (variable-references variable))
        (incf (variable-reads variable))
        (cond
          ((eq (variable-compiland variable) *current-compiland*)
           (dformat t "p1: read ~S~%" var))
          (t
           (dformat t "p1: non-local read ~S variable-compiland = ~S current compiland = ~S~%"
                    var
                    (compiland-name (variable-compiland variable))
                    (compiland-name *current-compiland*))
           (setf (variable-used-non-locally-p variable) t))))
      ref)))

(defknown p1 (t) t)
(defun p1 (form)
  (cond
    ((symbolp form)
     (let (value)
       (cond
         ((null form)
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
          (p1-variable-reference form)))))
    ((atom form)
     form)
    (t
     (let ((op (%car form))
           handler)
       (cond
         ((symbolp op)
          (when (find-local-function op)
            ;; local functions shadow macros and functions in
            ;; the global environment as well as compiler macros
            (return-from p1
              (p1-function-call form)))
          (when (compiler-macro-function op)
            (unless (notinline-p op)
              (multiple-value-bind (expansion expanded-p)
                  (compiler-macroexpand form)
                ;; Fall through if no change...
                (when expanded-p
                  (return-from p1 (p1 expansion))))))
          (cond
            ((setf handler (get op 'p1-handler))
             (funcall handler form))
            ((macro-function op *compile-file-environment*)
             (p1 (macroexpand form *compile-file-environment*)))
            ((special-operator-p op)
             (compiler-unsupported "P1: unsupported special operator ~S" op))
            (t
             (p1-function-call form))))
         ((and (consp op) (eq (%car op) 'LAMBDA))
          (let ((maybe-optimized-call (rewrite-function-call form)))
            (if (eq maybe-optimized-call form)
                (p1 `(%funcall (function ,op) ,@(cdr form)))
                (p1 maybe-optimized-call))))
         (t
          (compiler-unsupported "P1 unhandled case ~S" form)))))))

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
                  (IF                   p1-default)
                  ;; used to be p1-if, which was used to rewrite the test
                  ;; form to a LET-binding; that's not necessary, because
                  ;; the test form doesn't lead to multiple operands on the
                  ;; operand stack
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
                  (UNWIND-PROTECT       p1-unwind-protect)
                  (THREADS:SYNCHRONIZED-ON
                                        p1-threads-synchronized-on)
                  (JVM::WITH-INLINE-CODE identity)))
    (install-p1-handler (%car pair) (%cadr pair))))

(initialize-p1-handlers)

(defun p1-compiland (compiland)
  (let ((*current-compiland* compiland)
        (*local-functions* *local-functions*)
        (*visible-variables* *visible-variables*)
        (form (compiland-lambda-expression compiland)))
    (aver (eq (car form) 'LAMBDA))
    (setf form (rewrite-lambda form))
    (with-saved-compiler-policy
      (process-optimization-declarations (cddr form))

      (let* ((lambda-list (cadr form))
             (body (cddr form))
             (closure (make-closure `(lambda ,lambda-list nil) nil))
             (syms (sys::varlist closure))
             (vars nil)
             compiland-result)
        (dolist (sym syms)
          (let ((var (make-variable :name sym
                                    :special-p (special-variable-p sym))))
            (push var vars)
            (push var *all-variables*)
            (push var *visible-variables*)))
        (setf (compiland-arg-vars compiland) (nreverse vars))
        (let ((free-specials (process-declarations-for-vars body vars nil)))
          (setf (compiland-free-specials compiland) free-specials)
          (dolist (var free-specials)
            (push var *visible-variables*)))
        (setf compiland-result
              (list* 'LAMBDA lambda-list (p1-body body)))
        (setf (compiland-%single-valued-p compiland)
              (single-valued-p compiland-result))
        (setf (compiland-p1-result compiland)
              compiland-result)))))

(provide "COMPILER-PASS1")
