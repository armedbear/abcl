(in-package :cl-user)

;; <https://mailman.common-lisp.net/pipermail/armedbear-devel/2019-November/004017.html>
(prove:plan 1)
(prove:isnt
 (handler-case 
     (java:jstatic-raw "reverseBytes" "java.lang.Short" #x7f)
   (error (e) (format *standard-output* "error: ~a~%" e)))
 nil
 "Calling java.lang.Short.reverseBytes(short)")

;; <http://trac.common-lisp.net/armedbear/ticket/259>
(prove:plan 1)
(prove:isnt
 (handler-case 
     (java:jstatic-raw "asList" "java.util.Arrays" 
                       (java:jnew-array (java:jclass "int") 1))
   (error (e) (format *standard-output* "error: ~a~%" e)))
 nil
 "Calling java.util.Arrays.asList(int[1])")

(prove:finalize)

 
