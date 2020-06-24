;;; tests for FORMAT wackiness <https://abcl.org/trac/ticket/190>

(prove:plan 15)

(prove:is
 (format nil "~,vf" 3 -0.1768522)
 "-0.177") ;; sbcl

(prove:is
 (format nil "~,vf" 2 -0.1768522)
 "-0.18")

(prove:is 
 (format nil "~,vf" 1 -0.1768522)
 "-0.2")

(prove:is
 (format nil "~,vf" 0 -0.1768522)
 "-0.")

(prove:is
 (format nil "~$" -0.1768522)
 "-0.18")

(prove:is
 (format nil "~v$" 3 -0.1768522)
 "-0.177")

(prove:is
 (format nil "~v$" 2 -0.1768522)
 "-0.18")

(prove:is
 (format nil "~v$" 1 -0.1768522)
 "-0.2") ;;  reported type error:  Array index out of bounds: 

(prove:is
 (format nil "~v$" 0 -0.1768522)
 "-0.")

(prove:is
 (format nil "~$" 0.1768522)
 "0.18")

(prove:is
 (format nil "~v$" 3 0.1768522)
 "0.177")

(prove:is
 (format nil "~v$" 2 0.1768522)
 "0.18")

(prove:is
 (format nil "~v$" 1 0.1768522)
 "0.2")

(prove:is
 (format nil "~v$" 0 0.1768522)
 "0.")

;; dingd
(prove:is
 (format nil "~$" -0.0)
 "0.00") ;; sbcl

(prove:finalize)
