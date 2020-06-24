(require :asdf)

(asdf:load-system :abcl)

(time 
 (asdf:test-system :abcl/test/ansi/compiled))

