;;;; -*- Mode:Lisp -*-
;;;;
;;;; Copyright (c) 2007 - 2009 Tobias C. Rittweiler <tcr@freebits.de>
;;;; Copyright (c) 2007, Robert P. Goldman <rpgoldman@sift.info> and SIFT, LLC
;;;;
;;;; All rights reserved.
;;;;
;;;; See LICENSE for details.
;;;;

(in-package :editor-hints.named-readtables)

;;;
;;;  ``This is enough of a foothold to implement a more elaborate
;;;    facility for using readtables in a localized way.''
;;;
;;;                               (X3J13 Cleanup Issue IN-SYNTAX)
;;;

;;;;;; DEFREADTABLE &c.

(defmacro defreadtable (name &body options)
  "Define a new named readtable, whose name is given by the symbol NAME.
  Or, if a readtable is already registered under that name, redefine
  that one.

  The readtable can be populated using the following OPTIONS:

  - `(:MERGE READTABLE-DESIGNATORS+)`

      Merge the readtables designated into the new readtable being
      defined as per MERGE-READTABLES-INTO.

      If no :MERGE clause is given, an empty readtable is used. See
      MAKE-READTABLE.

  - `(:FUSE READTABLE-DESIGNATORS+)`

      Like :MERGE except:

      Error conditions of type READER-MACRO-CONFLICT that are signaled
      during the merge operation will be silently _continued_. It
      follows that reader macros in earlier entries will be
      overwritten by later ones. For backward compatibility, :FUZE is
      accepted as an alias of :FUSE.

  - `(:DISPATCH-MACRO-CHAR MACRO-CHAR SUB-CHAR FUNCTION)`

      Define a new sub character `SUB-CHAR` for the dispatching macro
      character `MACRO-CHAR`, per SET-DISPATCH-MACRO-CHARACTER. You
      probably have to define `MACRO-CHAR` as a dispatching macro
      character by the following option first.

  - `(:MACRO-CHAR MACRO-CHAR FUNCTION [NON-TERMINATING-P])`

      Define a new macro character in the readtable, per
      SET-MACRO-CHARACTER. If `FUNCTION` is the keyword :DISPATCH,
      `MACRO-CHAR` is made a dispatching macro character, per
      MAKE-DISPATCH-MACRO-CHARACTER.

  - `(:SYNTAX-FROM FROM-READTABLE-DESIGNATOR FROM-CHAR TO-CHAR)`

      Set the character syntax of TO-CHAR in the readtable being
      defined to the same syntax as FROM-CHAR as per
      SET-SYNTAX-FROM-CHAR.

  - `(:CASE CASE-MODE)`

      Defines the _case sensitivity mode_ of the resulting readtable.

  Any number of option clauses may appear. The options are grouped by
  their type, but in each group the order the options appeared
  textually is preserved. The following groups exist and are executed
  in the following order: :MERGE and :FUSE (one
  group), :CASE, :MACRO-CHAR and :DISPATCH-MACRO-CHAR (one group),
  finally :SYNTAX-FROM.

  Notes:

  The readtable is defined at load-time. If you want to have it
  available at compilation time -- say to use its reader-macros in the
  same file as its definition -- you have to wrap the DEFREADTABLE
  form in an explicit EVAL-WHEN.

  On redefinition, the target readtable is made empty first before
  it's refilled according to the clauses.

  NIL, :STANDARD, :COMMON-LISP, :MODERN, and :CURRENT are
  preregistered readtable names."
  (check-type name symbol)
  (when (reserved-readtable-name-p name)
    (error "~A is the designator for a predefined readtable. ~
            Not acceptable as a user-specified readtable name." name))
  (flet ((process-option (option var)
           (destructure-case option
             ((:merge &rest readtable-designators)
	      `(merge-readtables-into ,var ,@(mapcar #'(lambda (x) `',x)
                                                     readtable-designators)))
             ((:fuse &rest readtable-designators)
	      `(handler-bind ((reader-macro-conflict #'continue))
                 (merge-readtables-into ,var ,@(mapcar #'(lambda (x) `',x)
                                                       readtable-designators))))
             ;; alias for :FUSE
             ((:fuze &rest readtable-designators)
	      `(handler-bind ((reader-macro-conflict #'continue))
                 (merge-readtables-into ,var ,@(mapcar #'(lambda (x) `',x)
                                                       readtable-designators))))
             ((:dispatch-macro-char disp-char sub-char function)
              `(set-dispatch-macro-character ,disp-char ,sub-char
                                             ,function ,var))
             ((:macro-char char function &optional non-terminating-p)
	      (if (eq function :dispatch)
		  `(make-dispatch-macro-character ,char ,non-terminating-p ,var)
		  `(set-macro-character ,char ,function
                                        ,non-terminating-p ,var)))
	     ((:syntax-from from-rt-designator from-char to-char)
	      `(set-syntax-from-char ,to-char ,from-char
				     ,var (find-readtable ,from-rt-designator)))
	     ((:case mode)
	      `(setf (readtable-case ,var) ,mode))))
	 (remove-clauses (clauses options)
	   (setq clauses (if (listp clauses) clauses (list clauses)))
	   (remove-if-not #'(lambda (x) (member x clauses))
			  options :key #'first)))
    (let* ((merge-clauses (remove-clauses '(:merge :fuze :fuse) options))
	   (case-clauses (remove-clauses :case  options))
	   (macro-clauses (remove-clauses '(:macro-char :dispatch-macro-char)
                                          options))
	   (syntax-clauses (remove-clauses :syntax-from options))
	   (other-clauses
             (set-difference options
                             (append merge-clauses case-clauses
                                     macro-clauses syntax-clauses))))
      (cond
	((not (null other-clauses))
	 (error "Bogus DEFREADTABLE clauses: ~/PPRINT-LINEAR/" other-clauses))
	(t
	 `(eval-when (:load-toplevel :execute)
            ;; The (FIND-READTABLE ...) isqrt important for proper
            ;; redefinition semantics, as redefining has to modify the
            ;; already existing readtable object.
            (let ((readtable (find-readtable ',name)))
              (cond ((not readtable)
                     (setq readtable (make-readtable ',name)))
                    (t
                     (setq readtable (%clear-readtable readtable))
                     (simple-style-warn
                      "Overwriting already existing readtable ~S."
                      readtable)))
              ,@(loop for option in merge-clauses
                      collect (process-option option 'readtable))
              ,@(loop for option in case-clauses
                      collect (process-option option 'readtable))
              ,@(loop for option in macro-clauses
                      collect (process-option option 'readtable))
              ,@(loop for option in syntax-clauses
                      collect (process-option option 'readtable))
              readtable)))))))

(defmacro in-readtable (name)
  "Set *READTABLE* to the readtable referred to by the symbol NAME."
  (check-type name symbol)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     ;; NB. The :LOAD-TOPLEVEL is needed for cases like (DEFVAR *FOO*
     ;; (GET-MACRO-CHARACTER #\"))
     (setf *readtable* (ensure-readtable ',name))
     (when (find-package :swank)
       (%frob-swank-readtable-alist *package* *readtable*))))

;;; KLUDGE: [interim solution]
;;;
;;;   We need support for this in Slime itself, because we want IN-READTABLE
;;;   to work on a per-file basis, and not on a per-package basis.
;;;
(defun %frob-swank-readtable-alist (package readtable)
  (let ((readtable-alist (find-symbol (string '#:*readtable-alist*)
				      (find-package :swank))))
    (when (boundp readtable-alist)
      (pushnew (cons (package-name package) readtable)
	       (symbol-value readtable-alist)
	       :test #'(lambda (entry1 entry2)
			 (destructuring-bind (pkg-name1 . rt1) entry1
			   (destructuring-bind (pkg-name2 . rt2) entry2
			     (and (string= pkg-name1 pkg-name2)
				  (eq rt1 rt2)))))))))

(deftype readtable-designator ()
  `(or null readtable))

(deftype named-readtable-designator ()
  "Either a symbol or a readtable itself."
  `(or readtable-designator symbol))

;;;;; Compiler macros

;;; Since the :STANDARD readtable is interned, and we can't enforce
;;; its immutability, we signal a style-warning for suspicious uses
;;; that may result in strange behaviour:

;;; Modifying the standard readtable would, obviously, lead to a
;;; propagation of this change to all places which use the :STANDARD
;;; readtable (and thus rendering this readtable to be non-standard,
;;; in fact.)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun constant-standard-readtable-expression-p (thing)
    (or (null thing)
        (eq thing :standard)
        (and (consp thing)
             (find thing
                   '((find-readtable nil)
                     (find-readtable :standard)
                     (ensure-readtable nil)
                     (ensure-readtable :standard))
                   :test #'equal))))

  (defun signal-suspicious-registration-warning (name-expr readtable-expr)
    (when (constant-standard-readtable-expression-p readtable-expr)
      (simple-style-warn
       "Caution: ~<You're trying to register the :STANDARD readtable ~
    under a new name ~S. As modification of the :STANDARD readtable ~
    is not permitted, subsequent modification of ~S won't be ~
    permitted either. You probably want to wrap COPY-READTABLE ~
    around~@:>~%             ~S"
       (list name-expr name-expr) readtable-expr))))

(define-compiler-macro register-readtable (&whole form name readtable)
  (signal-suspicious-registration-warning name readtable)
  form)

(define-compiler-macro ensure-readtable (&whole form name &optional
                                                (default nil default-p))
  (when default-p
    (signal-suspicious-registration-warning name default))
  form)

(declaim (special *standard-readtable* *empty-readtable*))

(define-api make-readtable
    (&optional (name nil name-supplied-p) &key merge)
    (&optional named-readtable-designator &key (:merge list) => readtable)
  "Creates and returns a new readtable under the specified
  NAME.

  MERGE takes a list of NAMED-READTABLE-DESIGNATORS and specifies the
  readtables the new readtable is created from. (See the :MERGE clause
  of DEFREADTABLE for details.)

  If MERGE is NIL, an empty readtable is used instead.

  If NAME is not given, an anonymous empty readtable is returned.

  Notes:

  An empty readtable is a readtable where each character's syntax is
  the same as in the _standard readtable_ except that each macro
  character has been made a constituent. Basically: whitespace stays
  whitespace, everything else is constituent."
  (cond ((not name-supplied-p)
         (copy-readtable *empty-readtable*))
        ((reserved-readtable-name-p name)
         (error "~A is the designator for a predefined readtable. ~
                Not acceptable as a user-specified readtable name." name))
        ((let ((rt (find-readtable name)))
           (and rt (prog1 nil
                     (cerror "Overwrite existing entry."
                             'readtable-does-already-exist :readtable-name name)
                     ;; Explicitly unregister to make sure that we do
                     ;; not hold on of any reference to RT.
                     (unregister-readtable rt)))))
        (t (let ((result (apply #'merge-readtables-into
                                ;; The first readtable specified in
                                ;; the :merge list is taken as the
                                ;; basis for all subsequent
                                ;; (destructive!) modifications (and
                                ;; hence it's copied.)
                                (copy-readtable (if merge
                                                    (ensure-readtable
                                                     (first merge))
                                                    *empty-readtable*))
                                (rest merge))))

             (register-readtable name result)))))

(define-api rename-readtable
    (old-name new-name)
    (named-readtable-designator symbol => readtable)
  "Replaces the associated name of the readtable designated by
  OLD-NAME with NEW-NAME. If a readtable is already registered under
  NEW-NAME, an error of type READTABLE-DOES-ALREADY-EXIST is
  signaled."
  (when (find-readtable new-name)
    (cerror "Overwrite existing entry."
            'readtable-does-already-exist :readtable-name new-name))
  (let* ((readtable (ensure-readtable old-name))
	 (readtable-name (readtable-name readtable)))
    ;; We use the internal functions directly to omit repeated
    ;; type-checking.
    (%unassociate-name-from-readtable readtable-name readtable)
    (%unassociate-readtable-from-name readtable-name readtable)
    (%associate-name-with-readtable new-name readtable)
    (%associate-readtable-with-name new-name readtable)
    readtable))

(define-api merge-readtables-into
    (result-readtable &rest named-readtables)
    (named-readtable-designator &rest named-readtable-designator => readtable)
  "Copy the contents of each readtable in NAMED-READTABLES into
  RESULT-READTABLE.

  If a macro character appears in more than one of the readtables,
  i.e. if a conflict is discovered during the merge, an error of type
  READER-MACRO-CONFLICT is signaled."
  (flet ((merge-into (to from)
	   (do-readtable ((char reader-fn non-terminating-p disp? table) from)
             (check-reader-macro-conflict from to char)
             (cond ((not disp?)
                    (set-macro-character char reader-fn non-terminating-p to))
                   (t
                    (ensure-dispatch-macro-character char non-terminating-p to)
                    (loop for (subchar . subfn) in table do
                      (check-reader-macro-conflict from to char subchar)
                      (set-dispatch-macro-character char subchar
                                                    subfn to)))))
	   to))
    (let ((result-table (ensure-readtable result-readtable)))
      (dolist (table (mapcar #'ensure-readtable named-readtables))
        (merge-into result-table table))
      result-table)))

(defun ensure-dispatch-macro-character (char &optional non-terminating-p
                                                       (readtable *readtable*))
  (if (dispatch-macro-char-p char readtable)
      t
      (make-dispatch-macro-character char non-terminating-p readtable)))

(define-api copy-named-readtable
    (named-readtable)
    (named-readtable-designator => readtable)
  "Like COPY-READTABLE but takes a NAMED-READTABLE-DESIGNATOR as argument."
  (copy-readtable (ensure-readtable named-readtable)))

(define-api list-all-named-readtables () (=> list)
  "Returns a list of all registered readtables. The returned list is
  guaranteed to be fresh, but may contain duplicates."
  (mapcar #'ensure-readtable (%list-all-readtable-names)))


(define-condition readtable-error (error) ())

(define-condition readtable-does-not-exist (readtable-error)
  ((readtable-name :initarg :readtable-name
	           :initform (required-argument)
	           :accessor missing-readtable-name
                   :type named-readtable-designator))
  (:report (lambda (condition stream)
             (format stream "A readtable named ~S does not exist."
                     (missing-readtable-name condition)))))

(define-condition readtable-does-already-exist (readtable-error)
  ((readtable-name :initarg :readtable-name
                   :initform (required-argument)
                   :accessor existing-readtable-name
                   :type named-readtable-designator))
  (:report (lambda (condition stream)
             (format stream "A readtable named ~S already exists."
                     (existing-readtable-name condition))))
  (:documentation "Continuable."))

(define-condition reader-macro-conflict (readtable-error)
  ((macro-char
    :initarg :macro-char
    :initform (required-argument)
    :accessor conflicting-macro-char
    :type character)
   (sub-char
    :initarg :sub-char
    :initform nil
    :accessor conflicting-dispatch-sub-char
    :type (or null character))
   (from-readtable
    :initarg :from-readtable
    :initform (required-argument)
    :accessor from-readtable
    :type readtable)
   (to-readtable
    :initarg :to-readtable
    :initform (required-argument)
    :accessor to-readtable
    :type readtable))
  (:report
   (lambda (condition stream)
     (format stream "~@<Reader macro conflict while trying to merge the ~
                    ~:[macro character~;dispatch macro characters~] ~
                    ~@C~@[ ~@C~] from ~A into ~A.~@:>"
             (conflicting-dispatch-sub-char condition)
             (conflicting-macro-char condition)
             (conflicting-dispatch-sub-char condition)
             (from-readtable condition)
             (to-readtable condition))))
  (:documentation "Continuable.

  This condition is signaled during the merge process if a reader
  macro (be it a macro character or the sub character of a dispatch
  macro character) is present in the both source and the target
  readtable and the two respective reader macro functions differ."))

(defun check-reader-macro-conflict (from to char &optional subchar)
  (flet ((conflictp (from-fn to-fn)
           (assert from-fn ()
                   "Bug in readtable iterators or concurrent access?")
           (and to-fn (not (function= to-fn from-fn)))))
    (when (if subchar
              (conflictp (%get-dispatch-macro-character char subchar from)
                         (%get-dispatch-macro-character char subchar to))
              (conflictp (%get-macro-character char from)
                         (%get-macro-character char to)))
      (cerror (format nil "Overwrite ~@C in ~A." char to)
              'reader-macro-conflict
              :from-readtable from
              :to-readtable to
              :macro-char char
              :sub-char subchar))))


;;; Although there is no way to get at the standard readtable in
;;; Common Lisp (cf. /standard readtable/, CLHS glossary), we make
;;; up the perception of its existence by interning a copy of it.
;;;
;;; We do this for reverse lookup (cf. READTABLE-NAME), i.e. for
;;;
;;;   (equal (readtable-name (find-readtable :standard)) "STANDARD")
;;;
;;; holding true.
;;;
;;; We, however, inherit the restriction that the :STANDARD
;;; readtable _must not be modified_ (cf. CLHS 2.1.1.2), although it'd
;;; technically be feasible (as *STANDARD-READTABLE* will contain a
;;; mutable copy of the implementation-internal standard readtable.)
;;; We cannot enforce this restriction without shadowing
;;; CL:SET-MACRO-CHARACTER and CL:SET-DISPATCH-MACRO-FUNCTION which
;;; is out of scope of this library, though. So we just threaten
;;; with nasal demons.
;;;
(defvar *standard-readtable*
  (%standard-readtable))

(defvar *empty-readtable*
  (%clear-readtable (copy-readtable nil)))

(defvar *case-preserving-standard-readtable*
  (let ((readtable (copy-readtable nil)))
    (setf (readtable-case readtable) :preserve)
    readtable))

(defparameter *reserved-readtable-names*
  '(nil :standard :common-lisp :modern :current))

(defun reserved-readtable-name-p (name)
  (and (member name *reserved-readtable-names*) t))

;;; In principle, we could DEFREADTABLE some of these. But we do
;;; reserved readtable lookup seperately, since we can't register a
;;; readtable for :CURRENT anyway.

(defun find-reserved-readtable (reserved-name)
  (cond ((eq reserved-name nil)          *standard-readtable*)
	((eq reserved-name :standard)    *standard-readtable*)
        ((eq reserved-name :common-lisp) *standard-readtable*)
        ((eq reserved-name :modern)      *case-preserving-standard-readtable*)
	((eq reserved-name :current)     *readtable*)
	(t (error "Bug: no such reserved readtable: ~S" reserved-name))))

(define-api find-readtable
    (name)
    (named-readtable-designator => (or readtable null))
  "Looks for the readtable specified by NAME and returns it if it is
  found. Returns NIL otherwise."
  (cond ((readtablep name) name)
        ((reserved-readtable-name-p name)
         (find-reserved-readtable name))
        ((%find-readtable name))))

;;; FIXME: This doesn't take a NAMED-READTABLE-DESIGNATOR, but only a
;;; STRING-DESIGNATOR. (When fixing, heed interplay with compiler
;;; macros below.)
(defsetf find-readtable register-readtable)

(define-api ensure-readtable
    (name &optional (default nil default-p))
    (named-readtable-designator &optional (or named-readtable-designator null)
      => readtable)
  "Looks up the readtable specified by NAME and returns it if it's found.
  If it is not found, it registers the readtable designated by DEFAULT
  under the name represented by NAME; or if no default argument is
  given, it signals an error of type READTABLE-DOES-NOT-EXIST
  instead."
  (cond ((find-readtable name))
        ((not default-p)
         (error 'readtable-does-not-exist :readtable-name name))
        (t (setf (find-readtable name) (ensure-readtable default)))))


(define-api register-readtable
    (name readtable)
    (symbol readtable => readtable)
  "Associate READTABLE with NAME. Returns the readtable."
  (assert (typep name '(not (satisfies reserved-readtable-name-p))))
  (%associate-readtable-with-name name readtable)
  (%associate-name-with-readtable name readtable)
  readtable)

(define-api unregister-readtable
    (named-readtable)
    (named-readtable-designator => boolean)
  "Remove the association of NAMED-READTABLE. Returns T if successfull,
  NIL otherwise."
  (let* ((readtable (find-readtable named-readtable))
	 (readtable-name (and readtable (readtable-name readtable))))
    (if (not readtable-name)
	nil
	(prog1 t
	  (check-type readtable-name
                      (not (satisfies reserved-readtable-name-p)))
          (%unassociate-readtable-from-name readtable-name readtable)
          (%unassociate-name-from-readtable readtable-name readtable)))))

(define-api readtable-name
    (named-readtable)
    (named-readtable-designator => symbol)
  "Returns the name of the readtable designated by NAMED-READTABLE,
  or NIL."
   (let ((readtable (ensure-readtable named-readtable)))
    (cond ((%readtable-name readtable))
          ((eq readtable *readtable*) :current)
	  ((eq readtable *standard-readtable*) :common-lisp)
          ((eq readtable *case-preserving-standard-readtable*) :modern)
	  (t nil))))
