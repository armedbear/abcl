(require :asdf)
(require :abcl-contrib)

(ql:quickload
 '(:static-vectors :static-vectors/test))

(time 
 (asdf:test-system :static-vectors))

