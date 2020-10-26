(in-package :cl-user)

(defpackage quicklisp-abcl
  (:nicknames :quicklisp-abcl)
  (:use :cl
        :asdf)
  (:export
   #:quicklisp/boot/fasls
   #:ensure-installation
   #:*quicklisp-parent-dir*))

(in-package :quicklisp-abcl)

;;;;
;;;;  1. (ABCL) Download setup.lisp if necessary from the network,
;;;;     running the Quicklisp setup routine 
;;;;
;;;;  2.  Ensure that we cache the and use the fasl for
;;;;        (merge-pathnames "setup.lisp" *quicklisp-parent-dir*
;;;;

(defvar *quicklisp-parent-dir* (user-homedir-pathname)
  "Pathname reference to the parent directory of the local Quicklisp installation")

(defun quicklisp/boot/fasls (&key (remove nil))
  "Enumerate all Quicklisp fasls, including the one we shim for the loader"
  ;;; TODO: ensure that this works for other implementations
    (let* ((setup-base
             (merge-pathnames "quicklisp/setup" *quicklisp-parent-dir*))
           (setup-source
             (make-pathname :defaults setup-base :type "lisp"))
           (setup-fasl
             (make-pathname :defaults setup-base :type "abcl"))
           (asdf-output-root
             (when (ignore-errors (asdf:find-system :quicklisp))
               (asdf:apply-output-translations
                (asdf:system-source-directory (asdf:find-system :quicklisp))))))
      (let ((all-fasls (append (list setup-fasl)
                               (when asdf-output-root
                                 (directory 
                                  (merge-pathnames "**/*" asdf-output-root))))))
        (when remove
          (format *load-verbose* "~&;;quicklisp-abcl: deleting ~{~a ~}~%" all-fasls)
          (mapcar #'delete-file all-fasls))
        (values all-fasls
                setup-base setup-source setup-fasl))))

;;; After we have loaded this system, ensure Quicklisp is loaded
(defun ensure-installation ()
  (when (find :quicklisp *features*)
    (return-from ensure-installation))
  (multiple-value-bind (fasls
                        setup-base setup-source setup-fasl)
      (quicklisp/boot/fasls)
    (if (probe-file setup-source)
        ;; First try loading the Quicklisp setup as a compiled fasl if it exists
        (if (probe-file setup-fasl)
            (handler-case
                (load setup-fasl)
              ;; The fasl may be invalid (i.e. between abcl versions); if so, load source, and recompile
              (error (e)
                (format *load-verbose* "~&Failed to load Quicklisp setup fasl ~%~t~a~%because:~%~t~a~%" setup-fasl e)
                (when setup-source
                  (format *load-verbose* "Removing Quicklisp setup fasl and recompiling...")
                  (quicklisp/boot/fasls :remove t)
                  (load setup-source)
                  (compile-file setup-source :output-file setup-fasl))))
            ;; compilation only succeeds after Quicklisp has been fully loaded 
            (when (probe-file setup-source)
              (load setup-source)
              (compile-file setup-source :output-file setup-fasl)))
          ;;; Otherwise download Quicklisp and run its installation sequence
        (progn 
          (handler-case 
              (load "https://beta.quicklisp.org/quicklisp.lisp")
            (error (e)
              (warn "Using insecure transport for remote installation of Quicklisp:~&~A~&." e)
              (load "http://beta.quicklisp.org/quicklisp.lisp")))
          (uiop:symbol-call :quicklisp-quickstart '#:install
                            :path (merge-pathnames "quicklisp/"
                                                   *quicklisp-parent-dir*))))))


