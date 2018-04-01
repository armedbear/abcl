(in-package :cl-user)

(prove:diag
  "Output a message to the Console.

Note:  for users of SLIME, this will appear in the associated *inferior-lisp* buffer.")

(prove:plan 2)

(progn
  (when (find "log4j" (asdf:already-loaded-systems) :test 'equal)
    (prove:diag "Log4j was already loaded.  Explicitly clearing it from ASDF.")
    (asdf:clear-system :log4j))
  (prove:ok (asdf:load-system :log4j)
            "Testing loading the log4j system…")
  (#"configure" 'log4j.BasicConfigurator)
  (#"info" (#"getRootLogger" 'log4j.Logger) "Kilroy wuz here.")
  (prove:pass "No error occured while testing logging to *standard-output*"))

(prove:finalize)





