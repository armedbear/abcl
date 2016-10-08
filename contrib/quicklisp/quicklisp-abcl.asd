;;;; -*- Mode: LISP -*-
(require :asdf)
(require :abcl-contrib)
(require :abcl-asdf)
(in-package :asdf)

;; Quicklisp defines:
;;(defvar *setup-url* "http://beta.quicklisp.org/quickstart/setup.lisp")

(defsystem :quicklisp-abcl
    :description 
    "Load Quicklisp from the network if it isn't already installed. <urn:abcl.org/release/1.4.0/contrib/quicklisp-abcl#0.3.0>"
    :version "0.3.0"
    :components nil)

;; #+nil::needs-abcl-asdf((:iri "http://beta.quicklisp.org/quicklisp.lisp"))

(defmethod perform ((o load-op) (c (eql (find-system 'quicklisp-abcl))))
  (let* ((setup (merge-pathnames "quicklisp/setup.abcl" 
                                 (user-homedir-pathname)))
         (setup-source (merge-pathnames (make-pathname :type "lisp") setup)))
    (let ((it (or (probe-file setup)
                  (probe-file setup-source))))
      (if it
           ;;; First try loaded Quicklisp artifacts if it has been an installed for this user
          (prog1
              (load it)
            ;; compilation only succeeds after QUICKLISP has been loaded fully
            (unless (probe-file setup)
              (compile-file setup-source)))
          (progn 
            (handler-case 
                (load "https://beta.quicklisp.org/quicklisp.lisp")
              (error (e)
                (warn "Using insecure transport for remote installation of Quicklisp:~&~A~&." e)
                (load "http://beta.quicklisp.org/quicklisp.lisp")))
            (unless (find-package :quicklisp)
              (funcall (intern "INSTALL" "QUICKLISP-QUICKSTART"))))))))



        
    
  


