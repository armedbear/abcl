(in-package :jss)

(format t "~%~%")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setq *inhibit-jss-optimization* nil)
  (format t "With optimization: ~s~%" (macroexpand (precompiler::precompile-form '(#"compile" 'regex.Pattern ".*") t))))

(defun optimized-jss (count)
  (loop repeat count do (#"compile" 'regex.Pattern ".*")))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setq *inhibit-jss-optimization* t)
  (format t "Without optimization: ~s~%" (precompiler::precompile-form '(#"compile" 'regex.Pattern ".*") t)))

(defun unoptimized-jss (count)
  (loop repeat count do (#"compile" 'regex.Pattern ".*")))

(defun just-loop (count)
  (loop repeat count))

(print 'just-loop)
(time (just-loop 10000))
(print 'first)
(time (optimized-jss 10000))
(print 'second)
(time (unoptimized-jss 10000))

#|
With optimization: (INVOKE-RESTARGS-MACRO "compile" (QUOTE REGEX.PATTERN) (LIST ".*") NIL T)
Without optimization: ((LAMBDA (#:G85648 &REST #:G85649) (INVOKE-RESTARGS "compile" #:G85648 #:G85649 NIL)) (QUOTE REGEX.PATTERN) ".*")

JUST-LOOP 
0.0 seconds real time
0 cons cells

OPTIMIZED-JSS
0.011 seconds real time
0 cons cells

UNOPTIMIZED-JSS
0.325 seconds real time
800156 cons cells
|#
