(in-package :cl-user)

(defun test-LOG4J.2 ()
  "Output a message to the Console. 

Note:  for users of SLIME, this will appear in the associated *inferior-lisp* buffer."
  (#"configure" 'log4j.BasicConfigurator)
  (#"info" (#"getRootLogger" 'log4j.Logger) "Kilroy wuz here."))







