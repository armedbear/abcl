(in-package :common-lisp-user)

;;; This is is basically MGL-PAX:DEFINE-PACKAGE but we don't have it
;;; defined yet. The package variance stuff is because we export
;;; documentation from the NAMED-READTABLES-DOC system.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (locally
      (declare #+sbcl
               (sb-ext:muffle-conditions sb-kernel::package-at-variance))
    (handler-bind
        (#+sbcl (sb-kernel::package-at-variance #'muffle-warning))
      (defpackage :editor-hints.named-readtables
        (:use :common-lisp)
        (:nicknames :named-readtables)
        (:export
         #:defreadtable
         #:in-readtable
         #:make-readtable
         #:merge-readtables-into
         #:find-readtable
         #:ensure-readtable
         #:rename-readtable
         #:readtable-name
         #:register-readtable
         #:unregister-readtable
         #:copy-named-readtable
         #:list-all-named-readtables
         ;; Types
         #:named-readtable-designator
         ;; Conditions
         #:reader-macro-conflict
         #:readtable-does-already-exist
         #:readtable-does-not-exist)
        (:documentation "See NAMED-READTABLES:@NAMED-READTABLES-MANUAL.")))))

(pushnew :named-readtables *features*)
