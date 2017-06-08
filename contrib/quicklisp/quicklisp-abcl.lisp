(in-package :cl-user)

(defpackage quicklisp-abcl
  (:nicknames :quicklisp-abcl)
  (:use :cl
        :asdf)
  (:export :*quicklisp-parent-dir*))

(in-package :quicklisp-abcl)

(defvar *quicklisp-parent-dir* (user-homedir-pathname)
  "Pathname reference to the parent directory of the local Quicklisp installation")

(defmethod asdf:perform ((o asdf:load-op) (c (eql (asdf:find-system :quicklisp-abcl))))
  (when (find :quicklisp *features*)
    (return-from asdf:perform))
  (let* ((setup-base
          (merge-pathnames "quicklisp/setup" 
                           *quicklisp-parent-dir*))
         (setup-source
          (probe-file (make-pathname :defaults setup-base
                                     :type "lisp")))
         (setup-fasl
          (probe-file (make-pathname :defaults setup-base
                                     :type "abcl"))))
    (if setup-source
           ;;; First try loading the Quicklisp setup as a compiled fasl if it exists
        (if setup-fasl
            (handler-case
                (load setup-fasl)
              ;; The fasl may be invalid (i.e. between abcl versions); if so, load source, and recompile
              (error (e)
                (declare (ignore e))
                (when setup-source
                  (load setup-source)
                  (compile-file setup-source))))
            ;; compilation only succeeds after QUICKLISP has been loaded fully
            (when setup-source
              (load setup-source)
              (compile-file setup-source)))
          ;;; Otherwise download Quicklisp and run its installation sequence
        (progn 
          (handler-case 
              (load "https://beta.quicklisp.org/quicklisp.lisp")
            (error (e)
              (warn "Using insecure transport for remote installation of Quicklisp:~&~A~&." e)
              (load "http://beta.quicklisp.org/quicklisp.lisp")))
          (uiop:symbol-call :quicklisp-quickstart '#:install
                            :path (merge-pathnames "quicklisp/" *quicklisp-parent-dir*))))))

