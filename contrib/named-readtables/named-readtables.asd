;;;; -*- mode: Lisp -*-

(in-package :asdf)

(defclass named-readtables-source-file (cl-source-file) ())

#+sbcl
(defmethod perform :around ((o compile-op)
                            (c named-readtables-source-file))
  (let ((sb-ext:*derive-function-types* t))
    (call-next-method)))

(defsystem "named-readtables"
  :description "Library that creates a namespace for named readtable
  akin to the namespace of packages."
  :author "Tobias C. Rittweiler <trittweiler@common-lisp.net>"
  :maintainer "Gábor Melis"
  :mailto "mega@retes.hu"
  :version "0.9"
  :licence "BSD, see LICENSE"
  :default-component-class named-readtables-source-file
  :pathname "src"
  :serial t
  :components ((:file "package")
               (:file "utils")
               (:file "define-api")
               (:file "cruft")
               (:file "named-readtables"))
  :in-order-to ((test-op (test-op "named-readtables/test"))))

(defsystem "named-readtables/test"
  :description "Test suite for the Named-Readtables library."
  :author "Tobias C. Rittweiler <trittweiler@common-lisp.net>"
  :maintainer "Gábor Melis"
  :mailto "mega@retes.hu"
  :depends-on ("named-readtables")
  :pathname "test"
  :serial t
  :default-component-class named-readtables-source-file
  :components
  ((:file "package")
   (:file "rt")
   (:file "tests"))
  :perform (test-op (o c) (symbol-call :named-readtables-test '#:do-tests)))


;;; MGL-PAX depends on NAMED-READTABLES so we must put documentation
;;; in a separate system in order to be able to use MGL-PAX.
(defsystem "named-readtables/doc"
  :depends-on ("named-readtables" "mgl-pax")
  :pathname "src"
  :components ((:file "doc")))
