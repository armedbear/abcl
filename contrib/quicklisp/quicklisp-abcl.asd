;;;; -*- Mode: LISP -*-
(require :asdf)
(require :abcl-contrib)
(require :abcl-asdf)
(in-package :asdf)
;; Quicklisp defines:
;;(defvar *setup-url* "http://beta.quicklisp.org/quickstart/setup.lisp")
(defsystem :quicklisp-abcl
    :version "0.2.0"
    :description "Convenience stubs to load locally installed Quicklisp."
;;    #+nil::defsystem-depends-on (abcl-asdf)
    :components nil)
;; #+nil::needs-abcl-asdf((:iri "http://beta.quicklisp.org/quicklisp.lisp"))
;;    #+nil::in-order-to ((asdf:compile-op (ql::install)))  ;;; FIXME tickle the internal Quicklisp setup 

(defmethod perform ((o load-op) (c (eql (find-system 'quicklisp-abcl))))
  ;;; Load local Quicklisp if it has been an installed
  (let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" 
                                         (user-homedir-pathname))))
    (if (probe-file quicklisp-init)
        (load quicklisp-init)
        (progn
          (load "http://beta.quicklisp.org/quicklisp.lisp")
          (funcall (intern "install" "QUICKLISP-QUICKSTART"))))))
        
    
  


