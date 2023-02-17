(ql:quickload :abcl-prove)

(time 
 (asdf:test-system :abcl-prove))

(ql:quickload :closer-mop)
(time
 (asdf:test-system :abcl-prove/closer-mop))

