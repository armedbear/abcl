(require :asdf)
(require :abcl-contrib)
(asdf:load-system :quicklisp-abcl)

(or
 (ignore-errors (asdf:make :swank))
 (ql:quickload :swank))
(swank:create-server :dont-close t)


