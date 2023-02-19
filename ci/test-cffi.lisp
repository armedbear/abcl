(require :asdf)
(require :abcl-contrib)

(asdf:make :abcl-asdf)
(asdf:make :jna)  

(ql:quickload
 '(:cffi :cffi-tests))

(time 
 (asdf:test-system :cffi))



