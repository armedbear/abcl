(require :asdf)
(require :abcl-contrib)
(asdf:load-system :quicklisp-abcl)

(asdf:load-system :prove)

(prove:run #p"~/work/abcl/t/sys-run-program.lisp")


