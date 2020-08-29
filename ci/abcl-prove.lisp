(ql:quickload :abcl-prove)

(asdf:load-system :abcl-prove)
(time 
 (asdf:test-system :abcl-prove/t))

