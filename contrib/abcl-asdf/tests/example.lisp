(in-package :abcl-asdf-test)

(defun test-LOG4J.2 ()
  "Output a message to the Console. 

Note:  for users of SLIME, this will appear in the associated *inferior-lisp* buffer."
  (#"configure" 'log4j.BasicConfigurator)
  (#"info" (#"getRootLogger" 'log4j.Logger) "Kilroy wuz here."))

(rt:deftest LOG4j.2 
    (test-LOG4J.2)
  t)







