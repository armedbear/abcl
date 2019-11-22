(require :asdf)

(ql:quickload :prove) ;; FIXME shouldn't need this
(asdf:load-system :abcl/test/lisp)

(asdf:test-system :abcl/test/lisp)
