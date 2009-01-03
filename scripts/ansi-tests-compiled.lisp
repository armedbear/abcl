(require 'asdf)
(asdf:oos 'asdf:load-op :abcl)
(abcl.tests.ansi-tests:run :compile-tests t)
(ext:exit)