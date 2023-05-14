(require :asdf)
(require :abcl-contrib)
(asdf:make :quicklisp-abcl)

(dolist (system '(:jss-tests :asdf-jar-test))
  (unless
      (ignore-errors (asdf:make system))
    (ql:quickload system))
  (time 
   (asdf:test-system system)))



