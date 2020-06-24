;;; Install Quicklisp under ABCL via the QUICKLISP-ABCL contrib
(require :asdf)
(require :abcl-contrib)
(asdf:load-system :quicklisp-abcl :force t)
(let ((ql-util::*do-not-prompt* t))
  (ql:add-to-init-file))

