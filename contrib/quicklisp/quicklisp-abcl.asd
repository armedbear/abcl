;;;; -*- Mode: LISP -*-
(require :asdf)

(in-package :cl-user)

(asdf:defsystem quicklisp-abcl
  :description "Load Quicklisp from the network if it isn't already installed."
  :long-name "<urn:abcl.org/release/1.5.0/contrib/quicklisp-abcl#>"
  :version "0.5.1"
  :components nil)

(defvar cl-user::*quicklisp-parent-dir* (user-homedir-pathname)
  "Pathname reference to the parent directory of the local Quicklisp installation")
  
(defmethod asdf:perform ((o asdf:load-op) (c (eql (asdf:find-system :quicklisp-abcl))))
  (let* ((setup-base (merge-pathnames "quicklisp/setup" 
                                      cl-user::*quicklisp-parent-dir*))
         (setup-source (probe-file (make-pathname :defaults setup-base
                                                  :type "lisp")))
         (setup-fasl (probe-file (make-pathname :defaults setup-base
                                                  :type "abcl"))))
      (if setup-source
           ;;; First try loading the Quicklisp setup as compiled fasl if it exists
          (if setup-fasl
                (handler-case
                    (load setup-fasl)
                  ;; Sometimes the fasl is invalid; if so, load source, and recompile
                  (error (e)
                    (when setup-source
                      (load setup-source)
                      (compile-file setup-source))))
                ;; compilation only succeeds after QUICKLISP has been loaded fully
                (when setup-source
                  (load setup-source)
                  (compile-file setup-source)))
          ;;; Otherwise execute the quicklisp startup sequence
          (progn 
            (handler-case 
                (load "https://beta.quicklisp.org/quicklisp.lisp")
              (error (e)
                (warn "Using insecure transport for remote installation of Quicklisp:~&~A~&." e)
                (load "http://beta.quicklisp.org/quicklisp.lisp")))))
      (unless (find-package :quicklisp)
        (uiop:symbol-call :quicklisp-quickstart '#:install
                          :path (merge-pathnames "quicklisp/" cl-user::*quicklisp-parent-dir*)))))




        
    
  


