;;; Jeannie is not in Quicklisp: see <file:install-jeannie.bash>
;;; But we possibly need to install dependencies from Quicklisp
(ql:quickload :jeannie)

(time 
 (asdf:test-system :jeannie))



