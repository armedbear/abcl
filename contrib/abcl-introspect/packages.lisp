(defpackage abcl-introspect/system
  (:nicknames #:abcl-introspect/sys)
  (:use :common-lisp)
  (:export
   #:*debugging-locals-p*
   #:find-locals))

;;; Import and externalize all external symbols of
;;; ABCL-INTROSPECT/SYSTEM from the SYSTEM package.  Following this
;;; discipline will allow us to sanely determine what symbols
;;; ABCL-INTROSPECT adds to SYSTEM.
;;;
;;; TODO: do this for the rest of abcl-introspect.lisp and
;;; stacktrace.lisp
(eval-when (:compile-toplevel :load-toplevel)
  (loop :for symbol :being :the :external-symbols :of :abcl-introspect/system
        :doing
           (import symbol :system)
           (export symbol :system)))

