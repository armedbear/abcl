;;; tests for FORMAT wackiness <https://abcl.org/trac/ticket/190>

(prove:plan 15)

(prove:ok
 (format t "~,vf" 3 -0.1768522)
 "reported with -0.177 NIL")

(prove:ok
 (format t "~,vf" 2 -0.1768522)
 "reported with -0.18 NIL")

(prove:ok 
 (format t "~,vf" 1 -0.1768522)
 "reported with -0.2 NIL")

(prove:ok 
 (format t "~,vf" 0 -0.1768522)
 "-0. NIL")

(prove:ok 
 (format t "~$" -0.1768522)
 "--1.6 NIL")

(prove:ok
 (format t "~v$" 3 -0.1768522)
 "reported as --1.75 NIL")

(prove:ok
 (format t "~v$" 2 -0.1768522)
 "--1.6 NIL")

(prove:ok
 (format t "~v$" 1 -0.1768522)
 "reported type error:  Array index out of bounds: 2")

(prove:ok
 (format t "~v$" 0 -0.1768522)
 "--0. NIL")

(prove:ok 
 (format t "~$" 0.1768522)
 "0.18 NIL")

(prove:ok 
 (format t "~v$" 3 0.1768522)
 "0.177 NIL")

(prove:ok
 (format nil "~v$" 2 0.1768522)
 "0.18 NIL")

(prove:ok 
 (format nil "~v$" 1 0.1768522)
 "0.2 NIL")

(prove:ok
 (format nil "~v$" 0 0.1768522)
 "0. NIL")

;; dingd
(prove:ok
 (format nil "~$" -0.0)
 "-0.00")

(prove:finalize)
