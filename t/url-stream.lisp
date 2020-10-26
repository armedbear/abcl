(in-package :cl-user)

(prove:plan 1)
(require :abcl-contrib)
(require :jss)


(let ((uri #P"https://raw.githubusercontent.com/parallele-at/covid/master/eg/covid.n3"))
  (prove:ok
    (#"getInputStream"
     (system:make-file-stream uri 
                              'character :input nil :default))
    (format nil "Building a Java file stream~%~tfrom~t'~a'~%" uri)))

(prove:finalize)


