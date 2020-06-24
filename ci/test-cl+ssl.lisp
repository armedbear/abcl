#+abcl
(progn 
  (require :asdf)
  (require :abcl-contrib))

(ql:quickload
 '(:cl+ssl :cl+ssl.test))

(time
 (fiveam:run-all-tests))

