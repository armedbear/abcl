;;;; FIXME: resolve this Quicklisp/ASDF schism so that this is not necessary


(defsystem :abcl-prove
  :depends-on (prove))

(defsystem :abcl-prove/t
  :depends-on (:abcl-prove :abcl)
  :perform
  (asdf:test-op (op c)
     ;; (ignore c) ;;; any reference triggers all source artifacts           
    (ignore-errors
      (dolist (p (directory
        (merge-pathnames "*.lisp"
                         (asdf:system-relative-pathname :abcl "t/"))))
        (uiop:symbol-call :prove :run p)))))



  
  
