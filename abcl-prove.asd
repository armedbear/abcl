(defsystem :abcl-prove
  :version "1.7.0"
  :depends-on (prove))

(defsystem :abcl-prove/t
  :depends-on (abcl-prove abcl)
  :perform
    (test-op (op c)
      (declare (ignore op c))
      (dolist (p (directory
                  (merge-pathnames "*.lisp"
                                   (asdf:system-relative-pathname :abcl
                                                                  "t/"))))
        (ignore-errors
         (uiop:symbol-call :prove :run p)))))





  
  
