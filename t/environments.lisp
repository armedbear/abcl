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
(prove:is
 (eval '(let ((b 20))
         (defun bar ()
           (let ((a 10)) (env-parts)))
         (bar)))
 '((:LEXICAL-VARIABLE A 10) (:LEXICAL-VARIABLE B 20))
 "Nested lexical bindings captures locals")


