(in-package :abcl-asdf-test)

(defun run (&rest args)
  (abcl-rt:do-test 'test-log4j.2))

  
