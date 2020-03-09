(eval-when (:load-toplevel :execute)
  (require :abcl-contrib)
  (require :jss)
  (require :abcl-asdf))

(prove:plan 1)
(abcl-asdf:with-aether ()
  (prove:ok
   (abcl-asdf:ensure-mvn-version)))


(prove:finalize)

