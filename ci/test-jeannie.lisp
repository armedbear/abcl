;;; Jeannie is not in Quicklisp: see <file:install-jeannie.bash>
;;; But we possibly need to install dependencies from Quicklisp
(unless (ignore-errors (asdf:make :jeannie/test))
  (ql:quickload :jeannie/test))

(time 
 (asdf:test-system :jeannie))



