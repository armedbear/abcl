(require 'asdf)
(asdf:oos 'asdf:load-op :abcl)
(asdf:oos 'asdf:test-op :abcl :force t)
(ext:exit)