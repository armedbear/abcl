(in-package :cl-user)

(prove:plan 1)
(prove:isnt
 (handler-case 
     (java:jstatic-raw "reverseBytes" "java.lang.Short" #x7f)
   (error (e) (format *standard-output* "error: ~a~%" e)))
 nil
 "Calling java.lang.Short.reverseBytes(short)")

(prove:plan 1)
(prove:isnt
 (handler-case 
     (java:jstatic-raw "valueOf" "java.lang.Byte" #x7f)
   (error (e) (format *standard-output* "error: ~a~%" e)))
 nil
 "Calling java.lang.Byte.valueOf(byte)")

;; <http://trac.common-lisp.net/armedbear/ticket/259>
(prove:plan 1)
(prove:isnt
 (handler-case 
     (java:jstatic-raw "asList" "java.util.Arrays" 
                       (java:jnew-array (java:jclass "int") 1))
   (error (e) (format *standard-output* "error: ~a~%" e)))
 nil
 "Calling java.util.Arrays.asList(int[1])")

;;; Begin <https://mailman.common-lisp.net/pipermail/armedbear-devel/2020-February/004037.html>
;; the following signals the "no such method" error,
;; consistent with my understanding of the intent of the new patch
(prove:plan 1)
(prove:is
 (handler-case
     (jstatic "reverseBytes" "java.lang.Short" 6300000)
   (error (e)
     t))
   t
   "Dynamic static method invocation with large argument signals error")

;; however, this version does not throw [an] error, and silently narrows
;; 63000[0] the value to a short
(prove:plan 1)
(prove:is
 (handler-case 
     (let ((method
            (jmethod "java.lang.Short" "reverseBytes" "short")))
       (jstatic method nil 6300000))
   (error (e)
     t))
   t
   "Explicit static method invocation with large argument signals error")

(prove:plan 1)
(prove:is
 (handler-case
     (let ((buffer (jstatic-raw "allocate" "java.nio.ByteBuffer" 1))
           (value #xf0)) ;; a "byte" between 127 and 255
       (jcall "put" buffer value)
       (= value
          (+ 256 (jcall "get" buffer 0))))
   (error (e)
     nil))
 t
 "Invoking method with a byte value between 127 and 255")

;;; end <https://mailman.common-lisp.net/pipermail/armedbear-devel/2020-February/004037.html>

(prove:finalize)

 
