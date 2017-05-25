;;;;
;;;; Copyright (c) 2008 - 2009 Tobias C. Rittweiler <tcr@freebits.de>
;;;;
;;;; All rights reserved.
;;;;
;;;; See LICENSE for details.
;;;;

(in-package :editor-hints.named-readtables)

(defmacro define-cruft (name lambda-list &body (docstring . alternatives))
  (assert (typep docstring 'string) (docstring) "Docstring missing!")
  (assert (not (null alternatives)))
  `(progn
     (declaim (inline ,name))
     (defun ,name ,lambda-list ,docstring ,(first alternatives))))

(eval-when (:compile-toplevel :execute)
  #+sbcl (when (find-symbol "ASSERT-NOT-STANDARD-READTABLE"
                            (find-package "SB-IMPL"))
           (pushnew :sbcl+safe-standard-readtable *features*)))


;;;;; Implementation-dependent cruft

;;;; Mapping between a readtable object and its readtable-name.

(defvar *readtable-names* (make-hash-table :test 'eq))

(define-cruft %associate-readtable-with-name (name readtable)
  "Associate READTABLE with NAME for READTABLE-NAME to work."
  #+ :common-lisp (setf (gethash readtable *readtable-names*) name))

(define-cruft %unassociate-readtable-from-name (name readtable)
  "Remove the association between READTABLE and NAME."
  #+ :common-lisp (progn (assert (eq name (gethash readtable *readtable-names*)))
                         (remhash readtable *readtable-names*)))

(define-cruft %readtable-name (readtable)
  "Return the name associated with READTABLE."
  #+ :common-lisp (values (gethash readtable *readtable-names*)))

(define-cruft %list-all-readtable-names ()
  "Return a list of all available readtable names."
  #+ :common-lisp (list* :standard :current
                         (loop for name being each hash-value of *readtable-names*
                               collect name)))


;;;; Mapping between a readtable-name and the actual readtable object.

;;; On Allegro we reuse their named-readtable support so we work
;;; nicely on their infrastructure.

#-allegro
(defvar *named-readtables* (make-hash-table :test 'eq))

#+allegro
(defun readtable-name-for-allegro (symbol)
  (multiple-value-bind (kwd status)
        (if (keywordp symbol)
            (values symbol nil)
            ;; Kludge: ACL uses keywords to name readtables, we allow
            ;; arbitrary symbols.
            (intern (format nil "~A.~A"
                            (package-name (symbol-package symbol))
                            (symbol-name symbol))
                    :keyword))
    (prog1 kwd
      (assert (or (not status) (get kwd 'named-readtable-designator)))
      (setf (get kwd 'named-readtable-designator) t))))

(define-cruft %associate-name-with-readtable (name readtable)
  "Associate NAME with READTABLE for FIND-READTABLE to work."
  #+ :allegro     (setf (excl:named-readtable (readtable-name-for-allegro name)) readtable)
  #+ :common-lisp (setf (gethash name *named-readtables*) readtable))

(define-cruft %unassociate-name-from-readtable (name readtable)
  "Remove the association between NAME and READTABLE"
  #+ :allegro     (let ((n (readtable-name-for-allegro name)))
                    (assert (eq readtable (excl:named-readtable n)))
                    (setf (excl:named-readtable n) nil))
  #+ :common-lisp (progn (assert (eq readtable (gethash name *named-readtables*)))
                         (remhash name *named-readtables*)))

(define-cruft %find-readtable (name)
  "Return the readtable named NAME."
  #+ :allegro     (excl:named-readtable (readtable-name-for-allegro name) nil)
  #+ :common-lisp (values (gethash name *named-readtables* nil)))


;;;; Reader-macro related predicates

;;; CLISP creates new function objects for standard reader macros on
;;; each readtable copy.
(define-cruft function= (fn1 fn2)
  "Are reader-macro function-designators FN1 and FN2 the same?"
  #+ :clisp
  (let* ((fn1 (ensure-function fn1))
         (fn2 (ensure-function fn2))
         (n1 (system::function-name fn1))
         (n2 (system::function-name fn2)))
    (if (and (eq n1 :lambda) (eq n2 :lambda))
        (eq fn1 fn2)
        (equal n1 n2)))
  #+ :sbcl
  (let ((fn1 (ensure-function fn1))
        (fn2 (ensure-function fn2)))
    (or (eq fn1 fn2)
        ;; After SBCL 1.1.18, for dispatch macro characters
        ;; GET-MACRO-CHARACTER returns closures whose name is:
        ;;
        ;; (LAMBDA (STREAM CHAR) :IN SB-IMPL::%MAKE-DISPATCH-MACRO-CHAR)
        ;;
        ;; Treat all these closures equivalent.
        (flet ((internal-dispatch-macro-closure-name-p (name)
                 (find "SB-IMPL::%MAKE-DISPATCH-MACRO-CHAR" name
                       :key #'prin1-to-string :test #'string-equal)))
          (let ((n1 (sb-impl::%fun-name fn1))
                (n2 (sb-impl::%fun-name fn2)))
            (and (listp n1) (listp n2)
                 (internal-dispatch-macro-closure-name-p n1)
                 (internal-dispatch-macro-closure-name-p n2))))))
  #+ :common-lisp
  (eq (ensure-function fn1) (ensure-function fn2)))

;;; CLISP will incorrectly fold the call to G-D-M-C away
;;; if not declared inline.
(define-cruft dispatch-macro-char-p (char rt)
  "Is CHAR a dispatch macro character in RT?"
  #+ :common-lisp
  (handler-case (locally
                    #+clisp (declare (notinline get-dispatch-macro-character))
                  (get-dispatch-macro-character char #\x rt)
                  t)
    (error () nil)))

;; (defun macro-char-p (char rt)
;;   (let ((reader-fn (%get-macro-character char rt)))
;;     (and reader-fn t)))

;; (defun standard-macro-char-p (char rt)
;;   (multiple-value-bind (rt-fn rt-flag) (get-macro-character char rt)
;;     (multiple-value-bind (std-fn std-flag) (get-macro-character char *standard-readtable*)
;;       (and (eq rt-fn std-fn)
;; 	   (eq rt-flag std-flag)))))

;; (defun standard-dispatch-macro-char-p (disp-char sub-char rt)
;;   (flet ((non-terminating-p (ch rt) (nth-value 1 (get-macro-character ch rt))))
;;     (and (eq (non-terminating-p disp-char rt)
;; 	     (non-terminating-p disp-char *standard-readtable*))
;; 	 (eq (get-dispatch-macro-character disp-char sub-char rt)
;; 	     (get-dispatch-macro-character disp-char sub-char *standard-readtable*)))))


;;;; Readtables Iterators

(defmacro with-readtable-iterator ((name readtable) &body body)
  (let ((it (gensym)))
    `(let ((,it (%make-readtable-iterator ,readtable)))
       (macrolet ((,name () `(funcall ,',it)))
         ,@body))))

#+sbcl
(defun %make-readtable-iterator (readtable)
  (let ((char-macro-array (sb-impl::character-macro-array readtable))
        (char-macro-ht    (sb-impl::character-macro-hash-table readtable))
        (dispatch-tables  (sb-impl::dispatch-tables readtable))
        (char-code 0))
    (with-hash-table-iterator (ht-iterator char-macro-ht)
      (labels ((grovel-base-chars ()
                 (if (>= char-code sb-int:base-char-code-limit)
                     (grovel-unicode-chars)
                     (let ((reader-fn (svref char-macro-array char-code))
                           (char (code-char (shiftf char-code (1+ char-code)))))
                       (if reader-fn
                           (yield char)
                           (grovel-base-chars)))))
               (grovel-unicode-chars ()
                 (multiple-value-bind (more? char) (ht-iterator)
                   (if (not more?)
                       (values nil nil nil nil nil)
                       (yield char))))
               (yield (char)
                 (let ((disp-fn (get-macro-character char readtable))
                       (disp-ht))
                   (cond
                     ((setq disp-ht (cdr (assoc char dispatch-tables)))
                      (let ((sub-char-alist))
                        (maphash (lambda (k v)
                                   (push (cons k v) sub-char-alist))
                                 disp-ht)
                        (values t char disp-fn t sub-char-alist)))
                     (t
                      (values t char disp-fn nil nil))))))
        #'grovel-base-chars))))
#+clozure
(defun %make-readtable-iterator (readtable)
  (flet ((ensure-alist (x)
           #.`(etypecase x
                (list x)
                ,@(uiop:if-let (sv (uiop:find-symbol* '#:sparse-vector :ccl nil))
                    `((,sv
                       (let ((table (uiop:symbol-call :ccl '#:sparse-vector-table x)))
                         (uiop:while-collecting (c)
                           (loop for i below (length table) do
                             (uiop:if-let ((v (svref table i)))
                               (loop with i8 = (ash i 8)
                                     for j below (length v) do
                                       (uiop:if-let ((datum (svref v j)))
                                         (c (cons (code-char (+ i8 j)) datum))))))))))))))
    (let ((char-macros
            (ensure-alist
             (#.(or (uiop:find-symbol* '#:rdtab.macros :ccl nil) (uiop:find-symbol* '#:rdtab.alist :ccl)) readtable))))
      (lambda ()
        (if char-macros
            (destructuring-bind (char . defn) (pop char-macros)
              (if (consp defn)
                  (values t char (car defn) t (ensure-alist (cdr defn)))
                  (values t char defn nil nil)))
            (values nil nil nil nil nil))))))

;;; Written on ACL 8.0.
#+allegro
(defun %make-readtable-iterator (readtable)
  (declare (optimize speed))            ; for TCO
  (check-type readtable readtable)
  (let* ((macro-table     (first (excl::readtable-macro-table readtable)))
         (dispatch-tables (excl::readtable-dispatch-tables readtable))
         (table-length    (length macro-table))
         (idx 0))
    (labels ((grovel-macro-chars ()
               (if (>= idx table-length)
                   (grovel-dispatch-chars)
                   (let ((read-fn (svref macro-table idx))
			 (oidx idx))
                     (incf idx)
                     (if (or (eq read-fn #'excl::read-token)
                             (eq read-fn #'excl::read-dispatch-char)
                             (eq read-fn #'excl::undefined-macro-char))
                         (grovel-macro-chars)
                         (values t (code-char oidx) read-fn nil nil)))))
             (grovel-dispatch-chars ()
               (if (null dispatch-tables)
                   (values nil nil nil nil nil)
                   (destructuring-bind (disp-char sub-char-table)
                       (first dispatch-tables)
                     (setf dispatch-tables (rest dispatch-tables))
                     ;;; Kludge. We can't fully clear dispatch tables
                     ;;; in %CLEAR-READTABLE.
                     (when (eq (svref macro-table (char-code disp-char))
                               #'excl::read-dispatch-char)
                       (values t
                               disp-char
                               (svref macro-table (char-code disp-char))
                               t
                               (loop for subch-fn   across sub-char-table
                                     for subch-code from 0
                                     when subch-fn
                                       collect (cons (code-char subch-code)
                                                     subch-fn))))))))
      #'grovel-macro-chars)))


#-(or sbcl clozure allegro)
(eval-when (:compile-toplevel)
  (let ((*print-pretty* t))
    (simple-style-warn
     "~&~@<  ~@;~A has not been ported to ~A. ~
       We fall back to a portable implementation of readtable iterators. ~
       This implementation has to grovel through all available characters. ~
       On Unicode-aware implementations this may come with some costs.~@:>"
     (package-name '#.*package*) (lisp-implementation-type))))

#-(or sbcl clozure allegro)
(defun %make-readtable-iterator (readtable)
  (check-type readtable readtable)
  (let ((char-code 0))
    #'(lambda ()
        (prog ()
         :GROVEL
           (when (< char-code char-code-limit)
             (let ((char (code-char char-code)))
               (incf char-code)
               (when (not char) (go :GROVEL))
               (let ((fn (get-macro-character char readtable)))
                 (when (not fn) (go :GROVEL))
                 (multiple-value-bind (disp? alist)
                     (handler-case ; grovel dispatch macro characters.
                         (values
                          t
                          ;; Only grovel upper case characters to
                          ;; avoid duplicates.
                          (loop for code from 0 below char-code-limit
                                for subchar = (non-lowercase-code-char code)
                                for disp-fn = (and subchar
                                                   (get-dispatch-macro-character
                                                    char subchar readtable))
                                when disp-fn
                                  collect (cons subchar disp-fn)))
                       (error () nil))
                   (return (values t char fn disp? alist))))))))))

#-(or sbcl clozure allegro)
(defun non-lowercase-code-char (code)
  (let ((ch (code-char code)))
    (when (and ch (or (not (alpha-char-p ch))
                      (upper-case-p ch)))
      ch)))

(defmacro do-readtable ((entry-designator readtable &optional result)
                        &body body)
  "Iterate through a readtable's macro characters, and dispatch macro characters."
  (destructuring-bind (char &optional reader-fn non-terminating-p disp? table)
      (if (symbolp entry-designator)
          (list entry-designator)
          entry-designator)
    (let ((iter (gensym "ITER+"))
          (more? (gensym "MORE?+"))
          (rt (gensym "READTABLE+")))
      `(let ((,rt ,readtable))
         (with-readtable-iterator (,iter ,rt)
           (loop
             (multiple-value-bind (,more?
                                   ,char
                                   ,@(when reader-fn (list reader-fn))
                                   ,@(when disp? (list disp?))
                                   ,@(when table (list table)))
                 (,iter)
               (unless ,more? (return ,result))
               (let ,(when non-terminating-p
                       ;; FIXME: N-T-P should be incorporated in iterators.
                       `((,non-terminating-p
                          (nth-value 1 (get-macro-character ,char ,rt)))))
                 ,@body))))))))

;;;; Misc

;;; This should return an implementation's actual standard readtable
;;; object only if the implementation makes the effort to guard against
;;; modification of that object. Otherwise it should better return a
;;; copy.
(define-cruft %standard-readtable ()
  "Return the standard readtable."
  #+ :sbcl+safe-standard-readtable sb-impl::*standard-readtable*
  #+ :common-lisp                  (copy-readtable nil))

;;; On SBCL, SET-SYNTAX-FROM-CHAR does not get rid of a
;;; readtable's dispatch table properly.
;;; Same goes for Allegro but that does not seem to provide a
;;; setter for their readtable's dispatch tables. Hence this ugly
;;; workaround.
(define-cruft %clear-readtable (readtable)
  "Make all macro characters in READTABLE be constituents."
  #+ :sbcl
  (prog1 readtable
    (do-readtable (char readtable)
      (set-syntax-from-char char #\A readtable))
    (setf (sb-impl::dispatch-tables readtable) nil))
  #+ :allegro
  (prog1 readtable
    (do-readtable (char readtable)
      (set-syntax-from-char char #\A readtable))
    (let ((dispatch-tables (excl::readtable-dispatch-tables readtable)))
      (setf (cdr   dispatch-tables) nil)
      (setf (caar  dispatch-tables) #\Backspace)
      (setf (cadar dispatch-tables) (fill (cadar dispatch-tables) nil))))
  #+ :common-lisp
  (do-readtable (char readtable readtable)
    (set-syntax-from-char char #\A readtable)))

;;; See Clozure Trac Ticket 601. This is supposed to be removed at
;;; some point in the future.
(define-cruft %get-dispatch-macro-character (char subchar rt)
  "Ensure ANSI behaviour for GET-DISPATCH-MACRO-CHARACTER."
  #+ :ccl         (ignore-errors
                    (get-dispatch-macro-character char subchar rt))
  #+ :common-lisp (get-dispatch-macro-character char subchar rt))

;;; Allegro stores READ-TOKEN as reader macro function of each
;;; constituent character.
(define-cruft %get-macro-character (char rt)
  "Ensure ANSI behaviour for GET-MACRO-CHARACTER."
  #+ :allegro     (let ((fn (get-macro-character char rt)))
                    (cond ((not fn) nil)
                          ((function= fn #'excl::read-token) nil)
                          (t fn)))
  #+ :common-lisp (get-macro-character char rt))


;;;; Specialized PRINT-OBJECT for named readtables.

;;; As per #19 in CLHS 11.1.2.1.2 defining a method for PRINT-OBJECT
;;; that specializes on READTABLE is actually forbidden. It's quite
;;; likely to work (modulo package-locks) on most implementations,
;;; though.

;;; We don't need this on Allegro CL's as we hook into their
;;; named-readtable facility, and they provide such a method already.
#-allegro
(without-package-lock (:common-lisp #+lispworks :implementation)
  (defmethod print-object :around ((rt readtable) stream)
    (let ((name (readtable-name rt)))
      (if name
          (print-unreadable-object (rt stream :type nil :identity t)
            (format stream "~A ~S" :named-readtable name))
          (call-next-method)))))
