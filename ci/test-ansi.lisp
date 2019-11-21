(require :asdf)

(ql:quickload :prove) ;; FIXME
(ql:quickload :abcl)

(asdf:load-system :abcl)

(asdf:test-system :abcl/test/ansi/compiled)
