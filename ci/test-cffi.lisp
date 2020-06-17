(require :asdf)
(require :abcl-contrib)

(ql:quickload
 '(:cffi :cffi-tests))

(time 
 (asdf:test-system :cffi))



