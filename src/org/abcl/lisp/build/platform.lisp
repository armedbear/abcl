(require :asdf)

(in-package abcl/build)
;; Platform detection.

(defun platform ()
  #-clisp
  (let ((software-type (software-type)))
    (cond ((search "Linux" software-type)
           :linux)
          ((or (search "Mac OS X" software-type) ; abcl
               (search "Darwin" software-type))  ; sbcl
           :darwin)
          ((search "Windows" software-type)
           :windows)
          (t
           :unknown)))
  #+clisp
  (cond ((member :win32 *features*)
         :windows)
        ((equal 0 (ext:run-shell-command "uname | grep -i darwin" :output nil))
         :darwin)
        ((equal 0 (ext:run-shell-command "uname | grep -i linux" :output nil))
         :linux)
        (t
         :unknown)))

(defparameter *platform* (platform))

;; *PLATFORM* will be either :WINDOWS, :DARWIN, :LINUX, or :UNKNOWN.
(case *platform*
  (:windows
   (setq *jdk*           "C:\\Program Files\\Java\\jdk1.5.0_16\\")
   #+nil  (setq *java-compiler* "jikes")
   )
  (:darwin
   (setq *jdk*           "/usr/")
   #+nil (setq *java-compiler* "jikes")
   #+nil (setq *jar*    "jar"))
  ((:linux :unknown)
   (setq *jdk*           "/home/peter/sun/jdk1.5.0_16/")
   (setq *jar*           "fastjar")))

