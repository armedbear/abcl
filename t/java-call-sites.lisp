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


;;; <https://github.com/armedbear/abcl/pull/379>
(prove:plan 1)
(prove:isnt
 (handler-case 
     (let* ((parameter
              (jnew-array-from-list "java.lang.String" '("tmp" "passwd")))
            (class
              (jclass "java.nio.file.Path"))
            ;;; N.b. Path.of() only exists starting from openjdk11.  How to test earlier?
            (method
              (jmethod "java.nio.file.Path" "of" "java.lang.String" (jclass-of parameter)))
            (result
              (jstatic method class "/chroot/" parameter)))
       result)
   (error (e)
     (values :error)))
 :error
 "Invoking a method parameterized on String String[].")

(prove:finalize)

 
