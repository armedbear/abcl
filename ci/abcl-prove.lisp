(ql:quickload :abcl-prove)

(asdf:load-system :abcl-prove)
(asdf:test-system :abcl-prove/t)

;;; also works as :abcl/t under some definitions
