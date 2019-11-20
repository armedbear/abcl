(require :asdf)
(require :abcl-contrib)

(ql:quickload :cffi-tests)

(asdf:test-system :cffi)


