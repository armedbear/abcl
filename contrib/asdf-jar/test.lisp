(defun init-test ()
  (require :quicklisp)
  (ql:quickload :cl-ppcre))
  
(defun package-test ()
  (package :cl-ppcre))

(defun load-test ()
  (push "jar:file:/var/tmp/cl-ppcre.jar!/cl-ppcre-2.0.3/" 
	asdf:*central-registry*)
  (asdf:disable-output-translations)
  (setf asdf::*verbose-out* t)
  (asdf:load-system :cl-ppcre))

