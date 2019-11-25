(in-package :cl-user)

(prove:plan 1)

;;; Test for <https://github.com/armedbear/abcl/commit/dbba85a710ecc1baaafda38e820cf7fce40b05e5>
(prove:is 
 (let ((*read-suppress* t)) 
   (with-input-from-string (s "") 
     (read s nil :eof)))
 :eof
 "Testing CL:READ when *READ-SUPPRESS* is t")

(prove:finalize)

