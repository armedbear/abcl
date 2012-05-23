(require :abcl)
(require :abcl-test-lisp) 
(require :abcl-contrib)
(require :jss)

(in-package :abcl-test-lisp)

;;; http://trac.common-lisp.net/armedbear/ticket/205
(deftest jss.with-constant-signature.1 
    (progn 
      (jss:with-constant-signature ((substring "substring")) 
        (substring "01234" 2)))
  "234")


