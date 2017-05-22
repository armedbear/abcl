(in-package :abcl-asdf/test)

(prove:diag
  "Output a message to the Console. 

Note:  for users of SLIME, this will appear in the associated *inferior-lisp* buffer.")

(prove:plan 1)

(require :log4j)
(#"configure" 'log4j.BasicConfigurator)
(ok 
 (#"info" (#"getRootLogger" 'log4j.Logger) "Kilroy wuz here."))

(prove:finalize)





