(in-package :cl-user)

(defmacro env-parts (&environment env)
  `(sys::environment-parts ,env))

(prove:plan 1)
(prove:is
   (eval 
    '(let ((a 10))
      (env-parts)))
   '((:lexical-variable a 10))
   "Lexical let binding captures local")

(prove:plan 1)
(prove:ok
 (let ((env-parts-information
         (eval '(let ((b 20))
                 (defun bar ()
                   (let ((a 10)) (env-parts)))
                 (bar))))
       (expected-clauses
         #| Testing envionment actually contains:
           ((:LEXICAL-VARIABLE A 10) (:LEXICAL-VARIABLE B 20)
             (:BLOCK BAR #<org.armedbear.lisp.Primitives$BlockMarker@ca46a46>))
|#
         '((:LEXICAL-VARIABLE A 10) (:LEXICAL-VARIABLE B 20))))
   ;;; FIXME find a more idiomatic way to do this that also reports
   ;;; what fails.  Use CL:INTERSECTION
   (reduce
    (lambda (a b) (and a b))
    (mapcar
     (lambda (item)
       (member item env-parts-information :test #'equalp))
     expected-clauses)))
   "Nested lexical bindings captures locals")

(prove:finalize)



