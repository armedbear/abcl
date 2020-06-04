(require :asdf)
(require :abcl-contrib)

(ql:quickload :static-vectors)
(ql:quickload :static-vectors/test)

(asdf:test-system :static-vectors)
