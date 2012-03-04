;;;; -*- Mode: LISP -*-
(require :asdf)
(require :abcl-asdf)
;; Quicklisp defines:
;;(defvar *setup-url* "http://beta.quicklisp.org/quickstart/setup.lisp")
(asdf:defsystem :quicklisp-abcl
    :version "0.1.0"
    :components ((:iri "http://beta.quicklisp.org/quicklisp.lisp"))
    #+nil ;;; FIXME tickle the internal Quicklisp setup 
    :in-order-to ((asdf:compile-op (ql::install))))
                 

