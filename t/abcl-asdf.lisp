(eval-when (:load-toplevel :execute)
  (require :abcl-contrib)
  (require :jss)
  (require :abcl-asdf))

(prove:plan 1)
(let (ignorable)
  (abcl-asdf:with-aether (ignorable)
    (prove:ok
     (abcl-asdf:ensure-mvn-version))))

(prove:finalize)

