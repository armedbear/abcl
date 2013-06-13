;;;; -*- Mode: LISP -*-
(require :asdf)
(require :abcl-contrib)
(require :abcl-asdf)
(in-package :asdf)

;; Quicklisp defines:
;;(defvar *setup-url* "http://beta.quicklisp.org/quickstart/setup.lisp")

(defsystem :quicklisp-abcl
    :description 
    "Load Quicklisp from the network if it isn't already installed. <urn:abcl.org/release/1.3.0-dev/contrib/quicklisp-abcl#0.2.0>"
    :version "0.2.0"
    :components nil)

;; #+nil::needs-abcl-asdf((:iri "http://beta.quicklisp.org/quicklisp.lisp"))
;;    #+nil::in-order-to ((asdf:compile-op (ql::install)))  ;;; FIXME tickle the internal Quicklisp setup 

(defmethod perform ((o load-op) (c (eql (find-system 'quicklisp-abcl))))
  ;;; Load local Quicklisp if it has been an installed
  (let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" 
                                         (user-homedir-pathname))))
    (if (probe-file quicklisp-init)
        (load quicklisp-init)
        (handler-case 
            (load "https://beta.quicklisp.org/quicklisp.lisp")
          (error (e)
            (warn "Using insecure transport for remote installation
              of Quicklisp:~&~A~&." e)
            (load "http://beta.quicklisp.org/quicklisp.lisp"))))
    (unless (find-package :quicklisp)
      (funcall (intern "INSTALL" "QUICKLISP-QUICKSTART")))))

        
    
  


