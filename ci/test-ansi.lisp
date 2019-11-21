(require :asdf)

;;; Won't work without configuring the ASDF registry?
(asdf:load-system :abcl)

(asdf:test-system :abcl/test/ansi/compiled)
