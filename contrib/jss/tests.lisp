(require :abcl)
(require :abcl-test-lisp) 
(require :abcl-contrib)
(require :jss)

(in-package :abcl-test-lisp)

;;; http://abcl.org/trac/ticket/205
(deftest jss.with-constant-signature.1 
    (progn 
      (jss:with-constant-signature ((substring "substring")) 
        (substring "01234" 2)))
  "234")

;;; http://abcl.org/trac/ticket/229
(deftest jss.jcall.1
    (let* ((headers (#"getHeaderFields" 
                    (#"openConnection" 
                     (jss::new 'java.net.url "http://google.com"))))

           (second-header (#"get" *headers*
                                  (second (jss::set-to-list (#"keySet"
                                  *headers*))))))
      (#"size" *ural*))
-1)


