(require :asdf)
(require :abcl-contrib)
(asdf:load-system :quicklisp-abcl)

(unless (ignore-errors (asdf:load-system :prove))
  (ql:quickload :prove))

(prove:run
 (asdf:system-relative-pathname :abcl
				"t/sys-run-program.lisp"))


