(require 'asdf)
(handler-case 
    (asdf:test-system :abcl/test/lisp :force t)
  (t (e) (warn "Exiting after catching ~A" e)))
(ext:exit)
