(require :abcl-contrib)
(asdf:make :quicklisp-abcl)

(unless (ignore-errors
         (asdf:make :abcl-introspect-test))
  (ql:quickload :abcl-introspect-test))

(time 
 (asdf:test-system :abcl-introspect))

