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

(prove:plan 1)
(prove:ok
 (let* ((instructions
          (list
           (jvm::make-instruction 58 '(5))
           (jvm::make-instruction 58 '(2 4 :wide-prefix))))
        (code
          (make-array (length instructions)
                      :initial-contents instructions))
        (bytes
          (jvm::code-bytes code)))
   (eq (length bytes) 6))
 "Compilation of wide ASTORE instruction.")

;;; <https://github.com/armedbear/abcl/issues/541>
(prove:plan 1)
(let ((file (asdf:system-relative-pathname :abcl
                                           "t/eg/compiler-fails-top-level-lambda.lisp")))
  (prove:ok
   (handler-case 
       (compile-file file)
     ;;; anything signalled as error is a failure
     (t (e) (prove:diag (format nil "Compilation failed signalling ~a" e))))
   (format nil "Compiling '~a'~%" file)))

 
   

(prove:finalize)

