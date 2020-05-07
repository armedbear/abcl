(prove:plan 1)

;;; FIXME test shouldn't signal error
(prove:plan 1)
(let ((file (asdf:system-relative-pathname :abcl
                                           "t/eg/compiler-fails-on-inline-recursion.lisp")))
  (prove:ok
   (handler-case 
       (compile-file file)
     ;;; anything signalled as error is a failure
     (t (e) (prove:diag (format nil "Compilation failed signalling ~a" e))))
   (format nil "Compiling '~a'~%" file)))
     
;; <https://mailman.common-lisp.net/pipermail/armedbear-devel/2020-May/004054.html>
(prove:plan 1)
(prove:ok
  (handler-case
      (multiple-value-bind (compiled-function warnings failure)
          (compile nil '(lambda () ((lambda () ((lambda () 0))))))
        (equal
         (compiled-function warnings failure)
         (t nil nil)))
    (t (e) (prove:diag (format nil "Compilation failed signalling ~a" e))))
  "Able to compile nested lambda expression")

;; <https://mailman.common-lisp.net/pipermail/armedbear-devel/2020-May/004055.html>
(prove:plan 1)
(prove:ok
 (handler-case
     (multiple-value-bind (compiled-function warnings failure)
         (compile nil '(lambda () (dotimes (i 1 2) (catch 'c 3))))
       (equal
        (compiled-function warnings failure)
        (t nil nil)))
   (t (e) (prove:diag (format nil "Compilation failed signalling ~a" e))))
 "Able to compile tagbody not found form.")

(prove:finalize)

