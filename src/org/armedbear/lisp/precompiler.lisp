;;; precompiler.lisp
;;;
;;; Copyright (C) 2003-2008 Peter Graves <peter@armedbear.org>
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


(export '(process-optimization-declarations
          inline-p notinline-p inline-expansion expand-inline
          *defined-functions* *undefined-functions* note-name-defined))

(declaim (ftype (function (t) t) process-optimization-declarations))
(defun process-optimization-declarations (forms)
  (dolist (form forms)
    (unless (and (consp form) (eq (%car form) 'DECLARE))
      (return))
    (dolist (decl (%cdr form))
      (case (car decl)
        (OPTIMIZE
         (dolist (spec (%cdr decl))
           (let ((val 3)
                 (quality spec))
             (when (consp spec)
               (setf quality (%car spec)
                     val (cadr spec)))
             (when (and (fixnump val)
                        (<= 0 val 3))
               (case quality
                 (speed
                  (setf *speed* val))
                 (safety
                  (setf *safety* val))
                 (debug
                  (setf *debug* val))
                 (space
                  (setf *space* val))
                 (compilation-speed) ;; Ignored.
                 (t
                  (compiler-warn "Ignoring unknown optimization quality ~S in ~S." quality decl)))))))
        ((INLINE NOTINLINE)
         (dolist (symbol (%cdr decl))
           (push (cons symbol (%car decl)) *inline-declarations*)))
        (:explain
         (dolist (spec (%cdr decl))
           (let ((val t)
                 (quality spec))
             (when (consp spec)
               (setf quality (%car spec))
               (when (= (length spec) 2)
                 (setf val (%cadr spec))))
             (if val
                 (pushnew quality *explain*)
                 (setf *explain* (remove quality *explain*)))))))))
  t)

(declaim (ftype (function (t) t) inline-p))
(defun inline-p (name)
  (declare (optimize speed))
  (let ((entry (assoc name *inline-declarations* :test #'equal)))
    (if entry
        (eq (cdr entry) 'INLINE)
        (and (symbolp name) (eq (get name '%inline) 'INLINE)))))

(declaim (ftype (function (t) t) notinline-p))
(defun notinline-p (name)
  (declare (optimize speed))
  (let ((entry (assoc name *inline-declarations* :test #'equal)))
    (if entry
        (eq (cdr entry) 'NOTINLINE)
        (and (symbolp name) (eq (get name '%inline) 'NOTINLINE)))))

(defun expand-inline (form expansion)
;;   (format t "expand-inline form = ~S~%" form)
;;   (format t "expand-inline expansion = ~S~%" expansion)
  (let* ((op (car form))
         (proclaimed-ftype (proclaimed-ftype op))
         (args (cdr form))
         (vars (cadr expansion))
         (varlist ())
         new-form)
;;     (format t "op = ~S proclaimed-ftype = ~S~%" op (proclaimed-ftype op))
    (do ((vars vars (cdr vars))
         (args args (cdr args)))
        ((null vars))
      (push (list (car vars) (car args)) varlist))
    (setf new-form (list* 'LET (nreverse varlist)
                          (copy-tree (cddr expansion))))
    (when proclaimed-ftype
      (let ((result-type (ftype-result-type proclaimed-ftype)))
        (when (and result-type
                   (neq result-type t)
                   (neq result-type '*))
          (setf new-form (list 'TRULY-THE result-type new-form)))))
;;     (format t "expand-inline new form = ~S~%" new-form)
    new-form))

(define-compiler-macro assoc (&whole form &rest args)
  (cond ((and (= (length args) 4)
              (eq (third args) :test)
              (or (equal (fourth args) '(quote eq))
                  (equal (fourth args) '(function eq))))
         `(assq ,(first args) ,(second args)))
        ((= (length args) 2)
         `(assql ,(first args) ,(second args)))
        (t form)))

(define-compiler-macro member (&whole form &rest args)
  (let ((arg1 (first args))
        (arg2 (second args)))
    (case (length args)
      (2
       `(memql ,arg1 ,arg2))
      (4
       (let ((arg3 (third args))
             (arg4 (fourth args)))
         (cond ((and (eq arg3 :test)
                     (or (equal arg4 '(quote eq))
                         (equal arg4 '(function eq))))
                `(memq ,arg1 ,arg2))
               ((and (eq arg3 :test)
                     (or (equal arg4 '(quote eql))
                         (equal arg4 '(function eql))
                         (equal arg4 '(quote char=))
                         (equal arg4 '(function char=))))
                `(memql ,arg1 ,arg2))
               (t
                form))))
      (t
       form))))

(define-compiler-macro search (&whole form &rest args)
  (if (= (length args) 2)
      `(simple-search ,@args)
      form))

(define-compiler-macro identity (&whole form &rest args)
  (if (= (length args) 1)
      `(progn ,(car args))
      form))

(defun quoted-form-p (form)
  (and (consp form) (eq (%car form) 'QUOTE) (= (length form) 2)))

(define-compiler-macro eql (&whole form &rest args)
  (let ((first (car args))
        (second (cadr args)))
    (if (or (and (quoted-form-p first) (symbolp (cadr first)))
            (and (quoted-form-p second) (symbolp (cadr second))))
        `(eq ,first ,second)
        form)))

(define-compiler-macro not (&whole form arg)
  (if (atom arg)
      form
      (let ((op (case (car arg)
                  (>= '<)
                  (<  '>=)
                  (<= '>)
                  (>  '<=)
                  (t  nil))))
        (if (and op (= (length arg) 3))
            (cons op (cdr arg))
            form))))

(defun predicate-for-type (type)
  (cdr (assq type '((ARRAY             . arrayp)
                    (ATOM              . atom)
                    (BIT-VECTOR        . bit-vector-p)
                    (CHARACTER         . characterp)
                    (COMPLEX           . complexp)
                    (CONS              . consp)
                    (FIXNUM            . fixnump)
                    (FLOAT             . floatp)
                    (FUNCTION          . functionp)
                    (HASH-TABLE        . hash-table-p)
                    (INTEGER           . integerp)
                    (LIST              . listp)
                    (NULL              . null)
                    (NUMBER            . numberp)
                    (NUMBER            . numberp)
                    (PACKAGE           . packagep)
                    (RATIONAL          . rationalp)
                    (REAL              . realp)
                    (SIMPLE-BIT-VECTOR . simple-bit-vector-p)
                    (SIMPLE-STRING     . simple-string-p)
                    (SIMPLE-VECTOR     . simple-vector-p)
                    (STREAM            . streamp)
                    (STRING            . stringp)
                    (SYMBOL            . symbolp)))))

(define-compiler-macro typep (&whole form &rest args)
  (if (= (length args) 2) ; no environment arg
      (let* ((object (%car args))
             (type-specifier (%cadr args))
             (type (and (consp type-specifier)
                        (eq (%car type-specifier) 'QUOTE)
                        (%cadr type-specifier)))
             (predicate (and type (predicate-for-type type))))
        (if predicate
            `(,predicate ,object)
            `(%typep ,@args)))
      form))

(define-compiler-macro subtypep (&whole form &rest args)
  (if (= (length args) 2)
      `(%subtypep ,@args)
      form))

(define-compiler-macro funcall (&whole form
                                &environment env &rest args)
  (let ((callee (car args)))
    (if (and (>= *speed* *debug*)
             (consp callee)
             (eq (%car callee) 'function)
             (symbolp (cadr callee))
             (not (special-operator-p (cadr callee)))
             (not (macro-function (cadr callee) env))
             (memq (symbol-package (cadr callee))
                   (list (find-package "CL") (find-package "SYS"))))
        `(,(cadr callee) ,@(cdr args))
        form)))

(define-compiler-macro byte (size position)
  `(cons ,size ,position))

(define-compiler-macro byte-size (bytespec)
  `(car ,bytespec))

(define-compiler-macro byte-position (bytespec)
  `(cdr ,bytespec))

(define-source-transform concatenate (&whole form result-type &rest sequences)
  (if (equal result-type '(quote STRING))
      `(sys::concatenate-to-string (list ,@sequences))
      form))

(define-source-transform ldb (&whole form bytespec integer)
  (if (and (consp bytespec)
           (eq (%car bytespec) 'byte)
           (= (length bytespec) 3))
      (let ((size (%cadr bytespec))
            (position (%caddr bytespec)))
        `(%ldb ,size ,position ,integer))
      form))

(define-source-transform find (&whole form item sequence &key from-end test test-not start end key)
  (cond ((and (>= (length form) 3) (null start) (null end))
         (cond ((and (stringp sequence)
                     (null from-end)
                     (member test '(#'eql #'char=) :test #'equal)
                     (null test-not)
                     (null key))
                `(string-find ,item ,sequence))
               (t
                (let ((item-var (gensym))
                      (seq-var (gensym)))
                  `(let ((,item-var ,item)
                         (,seq-var ,sequence))
                     (if (listp ,seq-var)
                         (list-find* ,item-var ,seq-var ,from-end ,test ,test-not 0 (length ,seq-var) ,key)
                         (vector-find* ,item-var ,seq-var ,from-end ,test ,test-not 0 (length ,seq-var) ,key)))))))
        (t
         form)))

(define-source-transform adjoin (&whole form &rest args)
  (if (= (length args) 2)
      `(adjoin-eql ,(first args) ,(second args))
      form))

(define-source-transform format (&whole form &rest args)
  (if (stringp (second args))
      `(format ,(pop args) (formatter ,(pop args)) ,@args)
      form))

(define-compiler-macro catch (&whole form tag &rest args)
  (declare (ignore tag))
  (if (and (null (cdr args))
           (constantp (car args)))
      (car args)
      form))

(define-compiler-macro string= (&whole form &rest args)
  (if (= (length args) 2)
      `(sys::%%string= ,@args)
      form))

(define-compiler-macro <= (&whole form &rest args)
  (cond ((and (= (length args) 3)
              (numberp (first args))
              (numberp (third args))
              (= (first args) (third args)))
         `(= ,(second args) ,(first args)))
        (t
         form)))


(in-package "PRECOMPILER")

;; No source-transforms and inlining in precompile-function-call
;; No macro expansion in precompile-dolist and precompile-dotimes
;; No macro expansion in precompile-do/do*
;; No macro expansion in precompile-defun
;; Special precompilation in precompile-case and precompile-cond
;; Special precompilation in precompile-when and precompile-unless
;; No precompilation in precompile-nth-value
;; Special precompilation in precompile-return
;; Special precompilation in expand-macro
;;
;; if *in-jvm-compile* is false

(defvar *in-jvm-compile* nil)
(defvar *precompile-env* nil)


(declaim (ftype (function (t) t) precompile1))
(defun precompile1 (form)
  (cond ((symbolp form)
         (multiple-value-bind
               (expansion expanded)
             (expand-macro form)
           (if expanded
               (precompile1 expansion)
               form)))
        ((atom form)
         form)
        (t
         (let ((op (%car form))
               handler)
           (when (symbolp op)
             (cond ((setf handler (get op 'precompile-handler))
                    (return-from precompile1 (funcall handler form)))
                   ((macro-function op *precompile-env*)
                    (return-from precompile1 (precompile1 (expand-macro form))))
                   ((special-operator-p op)
                    (error "PRECOMPILE1: unsupported special operator ~S." op))))
           (precompile-function-call form)))))

(defun precompile-identity (form)
  (declare (optimize speed))
  form)

(declaim (ftype (function (t) cons) precompile-cons))
(defun precompile-cons (form)
  (cons (car form) (mapcar #'precompile1 (cdr form))))

(declaim (ftype (function (t t) t) precompile-function-call))
(defun precompile-function-call (form)
  (let ((op (car form)))
    (when (and (consp op) (eq (%car op) 'LAMBDA))
      (return-from precompile-function-call
                   (cons (precompile-lambda op)
                         (mapcar #'precompile1 (cdr form)))))
    (when (or (not *in-jvm-compile*) (notinline-p op))
      (return-from precompile-function-call (precompile-cons form)))
    (when (source-transform op)
      (let ((new-form (expand-source-transform form)))
        (when (neq new-form form)
          (return-from precompile-function-call (precompile1 new-form)))))
    (when *enable-inline-expansion*
      (let ((expansion (inline-expansion op)))
        (when expansion
          (let ((explain *explain*))
            (when (and explain (memq :calls explain))
              (format t ";   inlining call to ~S~%" op)))
          (return-from precompile-function-call (precompile1 (expand-inline form expansion))))))
    (cons op (mapcar #'precompile1 (cdr form)))))

(defun precompile-locally (form)
  (let ((*inline-declarations* *inline-declarations*))
    (process-optimization-declarations (cdr form))
  (cons 'LOCALLY (mapcar #'precompile1 (cdr form)))))

(defun precompile-block (form)
  (let ((args (cdr form)))
    (if (null (cdr args))
        nil
        (list* 'BLOCK (car args) (mapcar #'precompile1 (cdr args))))))

(defun precompile-dolist (form)
  (if *in-jvm-compile*
      (precompile1 (macroexpand form *precompile-env*))
      (cons 'DOLIST (cons (mapcar #'precompile1 (cadr form))
                          (mapcar #'precompile1 (cddr form))))))

(defun precompile-dotimes (form)
  (if *in-jvm-compile*
      (precompile1 (macroexpand form *precompile-env*))
      (cons 'DOTIMES (cons (mapcar #'precompile1 (cadr form))
                           (mapcar #'precompile1 (cddr form))))))

(defun precompile-do/do*-vars (varlist)
  (let ((result nil))
    (dolist (varspec varlist)
      (if (atom varspec)
          (push varspec result)
          (case (length varspec)
            (1
             (push (%car varspec) result))
            (2
             (let* ((var (%car varspec))
                    (init-form (%cadr varspec)))
               (unless (symbolp var)
                 (error 'type-error))
               (push (list var (precompile1 init-form))
                     result)))
            (3
             (let* ((var (%car varspec))
                    (init-form (%cadr varspec))
                    (step-form (%caddr varspec)))
               (unless (symbolp var)
                 (error 'type-error))
               (push (list var (precompile1 init-form) (precompile1 step-form))
                     result))))))
    (nreverse result)))

(defun precompile-do/do*-end-form (end-form)
  (let ((end-test-form (car end-form))
        (result-forms (cdr end-form)))
    (list* end-test-form (mapcar #'precompile1 result-forms))))

(defun precompile-do/do* (form)
  (if *in-jvm-compile*
      (precompile1 (macroexpand form *precompile-env*))
      (list* (car form)
             (precompile-do/do*-vars (cadr form))
             (precompile-do/do*-end-form (caddr form))
             (mapcar #'precompile1 (cdddr form)))))

(defun precompile-do-symbols (form)
  (list* (car form) (cadr form) (mapcar #'precompile1 (cddr form))))

(defun precompile-load-time-value (form)
  form)

(defun precompile-progn (form)
  (let ((body (cdr form)))
    (if (eql (length body) 1)
        (let ((res (precompile1 (%car body))))
          ;; If the result turns out to be a bare symbol, leave it wrapped
          ;; with PROGN so it won't be mistaken for a tag in an enclosing
          ;; TAGBODY.
          (if (symbolp res)
              (list 'progn res)
              res))
        (cons 'PROGN (mapcar #'precompile1 body)))))

(defun precompile-threads-synchronized-on (form)
  (cons 'threads:synchronized-on (mapcar #'precompile1 (cdr form))))

(defun precompile-progv (form)
  (if (< (length form) 3)
      (error "Not enough arguments for ~S." 'progv)
      (list* 'PROGV (mapcar #'precompile1 (%cdr form)))))

(defun precompile-setf (form)
  (let ((place (second form)))
    (cond ((and (consp place)
                (eq (%car place) 'VALUES))
	   (setf form
		 (list* 'SETF
			(list* 'VALUES
			       (mapcar #'precompile1 (%cdr place)))
			(cddr form)))
	   (precompile1 (expand-macro form)))
	  ((symbolp place)
           (multiple-value-bind
                 (expansion expanded)
               (expand-macro place)
             (if expanded
                 (precompile1 (list* 'SETF expansion
                                     (cddr form)))
                 (precompile1 (expand-macro form)))))
          (t
           (precompile1 (expand-macro form))))))

(defun precompile-setq (form)
  (let* ((args (cdr form))
         (len (length args)))
    (when (oddp len)
      (error 'simple-program-error
             :format-control "Odd number of arguments to SETQ."))
    (if (= len 2)
        (let* ((sym (%car args))
               (val (%cadr args)))
          (multiple-value-bind
                (expansion expanded)
              (expand-macro sym)
            (if expanded
                (precompile1 (list 'SETF expansion val))
                (list 'SETQ sym (precompile1 val)))))
        (let ((result ()))
          (loop
            (when (null args)
              (return))
            (push (precompile-setq (list 'SETQ (car args) (cadr args))) result)
            (setq args (cddr args)))
          (setq result (nreverse result))
          (push 'PROGN result)
          result))))

(defun precompile-psetf (form)
  (setf form
        (list* 'PSETF
               (mapcar #'precompile1 (cdr form))))
  (precompile1 (expand-macro form)))

(defun precompile-psetq (form)
  ;; Make sure all the vars are symbols.
  (do* ((rest (cdr form) (cddr rest))
        (var (car rest)))
       ((null rest))
    (unless (symbolp var)
      (error 'simple-error
             :format-control "~S is not a symbol."
             :format-arguments (list var))))
  ;; Delegate to PRECOMPILE-PSETF so symbol macros are handled correctly.
  (precompile-psetf form))


(defun precompile-lambda-list (form)
  (let (new aux-tail)
    (dolist (arg form (nreverse new))
       (if (or (atom arg) (> 2 (length arg)))
           (progn
             (when (eq arg '&aux)
               (setf aux-tail t))
             (push arg new))
          ;; must be a cons of more than 1 cell
          (let ((new-arg (copy-list arg)))
            (unless (<= 1 (length arg) (if aux-tail 2 3))
              ;; the aux-vars have a maximum length of 2 conses
              ;; optional and key vars may have 3
              (error 'program-error
                     :format-control
                     "The ~A binding specification ~S is invalid."
                     :format-arguments (list (if aux-tail "&AUX"
                                                 "&OPTIONAL/&KEY") arg)))
             (setf (second new-arg)
                   (precompile1 (second arg)))
             (push new-arg new))))))

(defun extract-lambda-vars (lambda-list)
  (let ((state :required)
        vars)
    (dolist (var/key lambda-list vars)
      (cond
        ((eq '&aux var/key)       (setf state :aux))
        ((eq '&key var/key)       (setf state :key))
        ((eq '&optional var/key)  (setf state :optional))
        ((eq '&rest var/key)      (setf state :rest))
        ((symbolp var/key)        (unless (eq var/key '&allow-other-keys)
                                    (push var/key vars)))
        ((and (consp var/key)
              (member state '(:optional :key)))
         (setf var/key (car var/key))
         (when (and (consp var/key) (eq state :key))
           (setf var/key (second var/key)))
         (if (symbolp var/key)
             (push var/key vars)
             (error 'program-error
                    :format-control
                    "Unexpected ~A variable specifier ~A."
                    :format-arguments (list state var/key))))
        ((and (consp var/key) (eq state :aux))
         (if (symbolp (car var/key))
             (push (car var/key) vars)
             (error 'program-error
                    :format-control "Unexpected &AUX format for ~A."
                    :format-arguments (list var/key))))
        (t
         (error 'program-error
                :format-control "Unexpected lambda-list format: ~A."
                :format-arguments (list lambda-list)))))))

(defun precompile-lambda (form)
  (let ((body (cddr form))
        (precompiled-lambda-list
           (precompile-lambda-list (cadr form)))
        (*inline-declarations* *inline-declarations*)
        (*precompile-env* (make-environment *precompile-env*)))
    (process-optimization-declarations body)
    (dolist (var (extract-lambda-vars precompiled-lambda-list))
      (environment-add-symbol-binding *precompile-env* var nil))
    (list* 'LAMBDA precompiled-lambda-list
           (mapcar #'precompile1 body))))

(defun precompile-named-lambda (form)
  (let ((lambda-form (list* 'LAMBDA (caddr form) (cdddr form))))
    (let ((body (cddr lambda-form))
          (precompiled-lambda-list
           (precompile-lambda-list (cadr lambda-form)))
          (*inline-declarations* *inline-declarations*)
          (*precompile-env* (make-environment *precompile-env*)))
      (process-optimization-declarations body)
      (dolist (var (extract-lambda-vars precompiled-lambda-list))
        (environment-add-symbol-binding *precompile-env* var nil))
      (list* 'NAMED-LAMBDA (cadr form) precompiled-lambda-list
             (mapcar #'precompile1 body)))))

(defun precompile-defun (form)
  (if *in-jvm-compile*
      (precompile1 (expand-macro form))
      form))

(defun precompile-macrolet (form)
  (let ((*precompile-env* (make-environment *precompile-env*)))
    (dolist (definition (cadr form))
      (environment-add-macro-definition
       *precompile-env*
       (car definition)
       (make-macro (car definition)
                   (make-closure
                    (make-expander-for-macrolet definition)
                    NIL))))
    (multiple-value-bind (body decls)
        (parse-body (cddr form) nil)
      `(locally ,@decls ,@(mapcar #'precompile1 body)))))

(defun precompile-symbol-macrolet (form)
  (let ((*precompile-env* (make-environment *precompile-env*))
        (defs (cadr form)))
    (dolist (def defs)
      (let ((sym (car def))
            (expansion (cadr def)))
        (when (special-variable-p sym)
          (error 'program-error
                 :format-control
                 "Attempt to bind the special variable ~S with SYMBOL-MACROLET."
                 :format-arguments (list sym)))
        (environment-add-symbol-binding *precompile-env*
                                        sym
                                        (sys::make-symbol-macro expansion))))
    (multiple-value-bind (body decls)
        (parse-body (cddr form) nil)
      (when decls
        (let ((specials ()))
          (dolist (decl decls)
            (when (eq (car decl) 'DECLARE)
              (dolist (declspec (cdr decl))
                (when (eq (car declspec) 'SPECIAL)
                  (setf specials (append specials (cdr declspec)))))))
          (when specials
            (let ((syms (mapcar #'car (cadr form))))
              (dolist (special specials)
                (when (memq special syms)
                  (error 'program-error
                         :format-control
                         "~S is a symbol-macro and may not be declared special."
                         :format-arguments (list special))))))))
      `(locally ,@decls ,@(mapcar #'precompile1 body)))))

(defun precompile-the (form)
  (list 'THE
        (second form)
        (precompile1 (third form))))

(defun precompile-truly-the (form)
  (list 'TRULY-THE
        (second form)
        (precompile1 (third form))))

(defun precompile-let/let*-vars (vars)
  (let ((result nil))
    (dolist (var vars)
      (cond ((consp var)
             (unless (<= 1 (length var) 2)
               (error 'program-error
                       :format-control
                       "The LET/LET* binding specification ~S is invalid."
                       :format-arguments (list var)))
             (let ((v (%car var))
                   (expr (cadr var)))
               (unless (symbolp v)
                 (error 'simple-type-error
                        :format-control "The variable ~S is not a symbol."
                        :format-arguments (list v)))
               (push (list v (precompile1 expr)) result)
               (environment-add-symbol-binding *precompile-env* v nil)))
               ;; any value will do: we just need to shadow any symbol macros
            (t
             (push var result)
             (environment-add-symbol-binding *precompile-env* var nil))))
    (nreverse result)))

(defun precompile-let (form)
  (let ((*precompile-env* (make-environment *precompile-env*)))
    (list* 'LET
           (precompile-let/let*-vars (cadr form))
           (mapcar #'precompile1 (cddr form)))))

;; (LET* ((X 1)) (LET* ((Y 2)) (LET* ((Z 3)) (+ X Y Z)))) =>
;; (LET* ((X 1) (Y 2) (Z 3)) (+ X Y Z))
(defun maybe-fold-let* (form)
  (if (and (= (length form) 3)
           (consp (%caddr form))
           (eq (%car (%caddr form)) 'LET*))
      (let ((third (maybe-fold-let* (%caddr form))))
        (list* 'LET* (append (%cadr form) (cadr third)) (cddr third)))
      form))

(defun precompile-let* (form)
  (setf form (maybe-fold-let* form))
  (let ((*precompile-env* (make-environment *precompile-env*)))
    (list* 'LET*
           (precompile-let/let*-vars (cadr form))
           (mapcar #'precompile1 (cddr form)))))

(defun precompile-case (form)
  (if *in-jvm-compile*
      (precompile1 (macroexpand form *precompile-env*))
      (let* ((keyform (cadr form))
             (clauses (cddr form))
             (result (list (precompile1 keyform))))
        (dolist (clause clauses)
          (push (precompile-case-clause clause) result))
        (cons (car form) (nreverse result)))))

(defun precompile-case-clause (clause)
  (let ((keys (car clause))
        (forms (cdr clause)))
    (cons keys (mapcar #'precompile1 forms))))

(defun precompile-cond (form)
  (if *in-jvm-compile*
      (precompile1 (macroexpand form *precompile-env*))
      (let ((clauses (cdr form))
            (result nil))
        (dolist (clause clauses)
          (push (precompile-cond-clause clause) result))
        (cons 'COND (nreverse result)))))

(defun precompile-cond-clause (clause)
  (let ((test (car clause))
        (forms (cdr clause)))
    (cons (precompile1 test) (mapcar #'precompile1 forms))))

(defun precompile-local-function-def (def)
  (let ((name (car def))
        (body (cddr def)))
    ;; Macro names are shadowed by local functions.
    (environment-add-function-definition *precompile-env* name body)
    (cdr (precompile-named-lambda (list* 'NAMED-LAMBDA def)))))

(defun precompile-local-functions (defs)
  (let ((result nil))
    (dolist (def defs (nreverse result))
      (push (precompile-local-function-def def) result))))

(defun find-use (name expression)
  (cond ((atom expression)
         nil)
        ((eq (%car expression) name)
         t)
        ((consp name)
         t) ;; FIXME Recognize use of SETF functions!
        (t
         (or (find-use name (%car expression))
             (find-use name (%cdr expression))))))

(defun precompile-flet/labels (form)
  (let ((*precompile-env* (make-environment *precompile-env*))
        (operator (car form))
        (locals (cadr form))
        (body (cddr form)))
    (dolist (local locals)
      (let* ((name (car local))
             (used-p (find-use name body)))
        (unless used-p
          (when (eq operator 'LABELS)
            (dolist (local locals)
              (when (neq name (car local))
                (when (find-use name (cddr local))
                  (setf used-p t)
                  (return))
                ;; Scope of defined function names includes
                ;; &OPTIONAL, &KEY and &AUX parameters
                ;; (LABELS.7B, LABELS.7C and LABELS.7D).
                (let ((vars (or
                             (cdr (memq '&optional (cadr local)))
                             (cdr (memq '&key (cadr local)))
                             (cdr (memq '&aux (cadr local))))))
                  (when (and vars (find-use name vars)
                             (setf used-p t)
                             (return))))))))
        (unless used-p
          (format t "; Note: deleting unused local function ~A ~S~%"
                  operator name)
          (let* ((new-locals (remove local locals :test 'eq))
                 (new-form
                  (if new-locals
                      (list* operator new-locals body)
                      (list* 'LOCALLY body))))
            (return-from precompile-flet/labels (precompile1 new-form))))))
    (list* (car form)
           (precompile-local-functions locals)
           (mapcar #'precompile1 body))))

(defun precompile-function (form)
  (if (and (consp (cadr form)) (eq (caadr form) 'LAMBDA))
      (list 'FUNCTION (precompile-lambda (%cadr form)))
      form))

(defun precompile-if (form)
  (let ((args (cdr form)))
    (case (length args)
      (2
       (let ((test (precompile1 (%car args))))
         (cond ((null test)
                nil)
               (;;(constantp test)
                (eq test t)
                (precompile1 (%cadr args)))
               (t
                (list 'IF
                      test
                      (precompile1 (%cadr args)))))))
      (3
       (let ((test (precompile1 (%car args))))
         (cond ((null test)
                (precompile1 (%caddr args)))
               (;;(constantp test)
                (eq test t)
                (precompile1 (%cadr args)))
               (t
                (list 'IF
                      test
                      (precompile1 (%cadr args))
                      (precompile1 (%caddr args)))))))
      (t
       (error "wrong number of arguments for IF")))))

(defun precompile-when (form)
  (if *in-jvm-compile*
      (precompile1 (macroexpand form *precompile-env*))
      (precompile-cons form)))

(defun precompile-unless (form)
  (if *in-jvm-compile*
      (precompile1 (macroexpand form *precompile-env*))
      (precompile-cons form)))

;; MULTIPLE-VALUE-BIND is handled explicitly by the JVM compiler.
(defun precompile-multiple-value-bind (form)
  (let ((vars (cadr form))
        (values-form (caddr form))
        (body (cdddr form))
        (*precompile-env* (make-environment *precompile-env*)))
    (dolist (var vars)
      (environment-add-symbol-binding *precompile-env* var nil))
    (list* 'MULTIPLE-VALUE-BIND
           vars
           (precompile1 values-form)
           (mapcar #'precompile1 body))))

;; MULTIPLE-VALUE-LIST is handled explicitly by the JVM compiler.
(defun precompile-multiple-value-list (form)
  (list 'MULTIPLE-VALUE-LIST (precompile1 (cadr form))))

(defun precompile-nth-value (form)
  (if *in-jvm-compile*
      (precompile1 (macroexpand form *precompile-env*))
      form))

(defun precompile-return (form)
  (if *in-jvm-compile*
      (precompile1 (macroexpand form *precompile-env*))
      (list 'RETURN (precompile1 (cadr form)))))

(defun precompile-return-from (form)
  (list 'RETURN-FROM (cadr form) (precompile1 (caddr form))))

(defun precompile-tagbody (form)
  (do ((body (cdr form) (cdr body))
       (result ()))
      ((null body) (cons 'TAGBODY (nreverse result)))
    (if (atom (car body))
        (push (car body) result)
        (push (let* ((first-form (car body))
                     (expanded (precompile1 first-form)))
                (if (and (symbolp expanded)
                         (neq expanded first-form))
                    ;; Workaround:
                    ;;  Since our expansion/compilation order
                    ;;   is out of sync with the definition of
                    ;;   TAGBODY (which requires the compiler
                    ;;   to look for tags before expanding),
                    ;;   we need to disguise anything which might
                    ;;   look like a tag. We do this by wrapping
                    ;;   it in a PROGN form.
                    (list 'PROGN expanded)
                    expanded)) result))))

(defun precompile-eval-when (form)
  (list* 'EVAL-WHEN (cadr form) (mapcar #'precompile1 (cddr form))))

(defun precompile-unwind-protect (form)
  (list* 'UNWIND-PROTECT
         (precompile1 (cadr form))
         (mapcar #'precompile1 (cddr form))))

;; EXPAND-MACRO is like MACROEXPAND, but EXPAND-MACRO quits if *IN-JVM-COMPILE*
;; is false and a macro is encountered that is also implemented as a special
;; operator, so interpreted code can use the special operator implementation.
(defun expand-macro (form)
  (let (exp)
    (loop
       (unless *in-jvm-compile*
         (when (and (consp form)
                    (symbolp (%car form))
                    (special-operator-p (%car form)))
           (return-from expand-macro (values form exp))))
       (multiple-value-bind (result expanded)
           (macroexpand-1 form *precompile-env*)
         (unless expanded
           (return-from expand-macro (values result exp)))
         (setf form result
               exp t)))))

(declaim (ftype (function (t t) t) precompile-form))
(defun precompile-form (form in-jvm-compile
                        &optional precompile-env)
  (let ((*in-jvm-compile* in-jvm-compile)
        (*inline-declarations* *inline-declarations*)
        (pre::*precompile-env* precompile-env))
    (precompile1 form)))

(defun install-handler (symbol &optional handler)
  (declare (type symbol symbol))
  (let ((handler (or handler
                     (find-symbol (sys::%format nil "PRECOMPILE-~A"
                                                (symbol-name symbol))
                                  'precompiler))))
    (unless (and handler (fboundp handler))
      (error "No handler for ~S." (let ((*package* (find-package :keyword)))
				    (format nil "~S" symbol))))
    (setf (get symbol 'precompile-handler) handler)))

(defun install-handlers ()
  (mapcar #'install-handler '(BLOCK
                              CASE
                              COND
                              DOLIST
                              DOTIMES
                              EVAL-WHEN
                              FUNCTION
                              IF
                              LAMBDA
                              MACROLET
                              MULTIPLE-VALUE-BIND
                              MULTIPLE-VALUE-LIST
                              NAMED-LAMBDA
                              NTH-VALUE
                              PROGN
                              PROGV
                              PSETF
                              PSETQ
                              RETURN
                              RETURN-FROM
                              SETF
                              SETQ
                              SYMBOL-MACROLET
                              TAGBODY
                              UNWIND-PROTECT
                              UNLESS
                              WHEN))

  (dolist (pair '((ECASE                precompile-case)

                  (AND                  precompile-cons)
                  (OR                   precompile-cons)

                  (CATCH                precompile-cons)
                  (MULTIPLE-VALUE-CALL  precompile-cons)
                  (MULTIPLE-VALUE-PROG1 precompile-cons)

                  (DO                   precompile-do/do*)
                  (DO*                  precompile-do/do*)

                  (LET                  precompile-let)
                  (LET*                 precompile-let*)

                  (LOCALLY              precompile-locally)

                  (FLET                 precompile-flet/labels)
                  (LABELS               precompile-flet/labels)

                  (LOAD-TIME-VALUE      precompile-load-time-value)

                  (DECLARE              precompile-identity)
                  (DEFUN                precompile-defun)
                  (GO                   precompile-identity)
                  (QUOTE                precompile-identity)
                  (THE                  precompile-the)
                  (THROW                precompile-cons)
                  (TRULY-THE            precompile-truly-the)

                  (THREADS:SYNCHRONIZED-ON
                                        precompile-threads-synchronized-on)
		  
		  (JVM::WITH-INLINE-CODE precompile-identity)))
    (install-handler (first pair) (second pair))))

(install-handlers)

(export '(precompile-form))

(in-package #:ext)
(defun macroexpand-all (form &optional env)
  (precompiler:precompile-form form t env))

(in-package #:lisp)

(defmacro compiler-let (bindings &body forms &environment env)
  (let ((bindings (mapcar #'(lambda (binding)
                              (if (atom binding) (list binding) binding))
                          bindings)))
    (progv (mapcar #'car bindings)
           (mapcar #'(lambda (binding)
                       (eval (cadr binding))) bindings)
      (macroexpand-all `(progn ,@forms) env))))

(in-package #:system)

(defun set-function-definition (name new old)
  (let ((*warn-on-redefinition* nil))
    (sys::%set-lambda-name new name)
    (sys:set-call-count new (sys:call-count old))
    (sys::%set-arglist new (sys::arglist old))
    (when (macro-function name)
      (setf new (make-macro name new)))
    (if (typep old 'standard-generic-function)
        (mop:set-funcallable-instance-function old new)
        (setf (fdefinition name) new))))

(defun precompile (name &optional definition)
  (unless definition
    (setq definition (or (and (symbolp name) (macro-function name))
                         (fdefinition name))))
  (let ((expr definition)
        env result
        (pre::*precompile-env* nil))
    (when (functionp definition)
      (multiple-value-bind (form closure-p)
          (function-lambda-expression definition)
        (unless form
          (return-from precompile (values nil t t)))
        (setq env closure-p)
        (setq expr form)))
    (unless (and (consp expr) (eq (car expr) 'lambda))
      (format t "Unable to precompile ~S.~%" name)
      (return-from precompile (values nil t t)))
    (setf result
          (sys:make-closure (precompiler:precompile-form expr nil env) env))
    (when (and name (functionp result))
      (sys::set-function-definition name result definition))
    (values (or name result) nil nil)))

(defun precompile-package (pkg &key verbose)
  (dolist (sym (package-symbols pkg))
    (when (fboundp sym)
      (unless (special-operator-p sym)
        (let ((f (fdefinition sym)))
          (unless (compiled-function-p f)
            (when verbose
              (format t "Precompiling ~S~%" sym)
              (finish-output))
            (precompile sym))))))
  t)

(defun %compile (name definition)
  (if (and name (fboundp name) (%typep (symbol-function name) 'generic-function))
      (values name nil nil)
      (precompile name definition)))

;; ;; Redefine EVAL to precompile its argument.
;; (defun eval (form)
;;   (%eval (precompile-form form nil)))

;; ;; Redefine DEFMACRO to precompile the expansion function on the fly.
;; (defmacro defmacro (name lambda-list &rest body)
;;   (let* ((form (gensym "WHOLE-"))
;;          (env (gensym "ENVIRONMENT-")))
;;     (multiple-value-bind (body decls)
;;         (parse-defmacro lambda-list form body name 'defmacro :environment env)
;;       (let ((expander `(lambda (,form ,env) ,@decls (block ,name ,body))))
;;         `(progn
;;            (let ((macro (make-macro ',name
;;                                     (or (precompile nil ,expander) ,expander))))
;;              ,@(if (special-operator-p name)
;;                    `((put ',name 'macroexpand-macro macro))
;;                    `((fset ',name macro)))
;;              (%set-arglist macro ',lambda-list)
;;              ',name))))))

;; Make an exception just this one time...
(when (get 'defmacro 'macroexpand-macro)
  (fset 'defmacro (get 'defmacro 'macroexpand-macro))
  (remprop 'defmacro 'macroexpand-macro))

(defvar *defined-functions*)

(defvar *undefined-functions*)

(defun note-name-defined (name)
  (when (boundp '*defined-functions*)
    (push name *defined-functions*))
  (when (and (boundp '*undefined-functions*) (not (null *undefined-functions*)))
    (setf *undefined-functions* (remove name *undefined-functions*))))

;; Redefine DEFUN to precompile the definition on the fly.
(defmacro defun (name lambda-list &body body &environment env)
  (note-name-defined name)
  (multiple-value-bind (body decls doc)
      (parse-body body)
    (let* ((block-name (fdefinition-block-name name))
           (lambda-expression
            `(named-lambda ,name ,lambda-list
                           ,@decls
                           ,@(when doc `(,doc))
                           (block ,block-name ,@body))))
      (cond ((and (boundp 'jvm::*file-compilation*)
                  ;; when JVM.lisp isn't loaded yet, this variable isn't bound
                  ;; meaning that we're not trying to compile to a file:
                  ;; Both COMPILE and COMPILE-FILE bind this variable.
                  ;; This function is also triggered by MACROEXPAND, though
                  jvm::*file-compilation*)
             `(fset ',name ,lambda-expression))
            (t
             (when (and env (empty-environment-p env))
               (setf env nil))
             (when (null env)
               (setf lambda-expression (precompiler:precompile-form lambda-expression nil)))
             `(progn
                (%defun ',name ,lambda-expression)
                ,@(when doc
                   `((%set-documentation ',name 'function ,doc)))))))))

(export '(precompile))

;;(provide "PRECOMPILER")