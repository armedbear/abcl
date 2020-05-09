(ql:quickload :abcl-introspect)
;;; shouldn't be necessary…
(ql:quickload :prove)
(ql:quickload :prove-asdf)
(ql:quickload :abcl-introspect-tests)
(asdf:test-system :abcl-introspect)
