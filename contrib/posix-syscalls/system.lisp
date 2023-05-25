(in-package :system/posix-syscalls)

;;; Currently unused, but save original symbol functions
(defvar *ext--getenv*
  #'ext:getenv)
(defvar *uiop/os--getenv*
  #'uiop/os:getenv)

(eval-when (:load-toplevel)
  (when
      (c-library-reference)
    (defun ext:getenv (variable)
      (getenv variable))
    (defsetf ext:getenv (variable) (value)
      `(progn
         (when (= (putenv ,variable ,value)
                0)
           ,value)))
    (setf (symbol-function 'uiop/os:getenv)
          'getenv)
    ;;; XXX figure out how to reuse the ext:getenv expander?
    (defsetf uiop/os:getenv (variable) (value)
      `(progn
         (when (= (putenv ,variable ,value)
                  0)
           ,value)))))

         




      

    
