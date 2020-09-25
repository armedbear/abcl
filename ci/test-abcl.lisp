(require :asdf)

(asdf:load-system :abcl/test/lisp)
(time 
 (asdf:test-system :abcl/test/lisp))

