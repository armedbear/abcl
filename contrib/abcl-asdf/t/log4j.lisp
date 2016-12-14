(in-package :abcl-asdf/test)

(diag
  "Output a message to the Console. 

Note:  for users of SLIME, this will appear in the associated *inferior-lisp* buffer.")

(plan 1)

(ok
 (require :log4j)
 (#"configure" 'log4j.BasicConfigurator)
 (#"info" (#"getRootLogger" 'log4j.Logger) "Kilroy wuz here."))

(finalize)





