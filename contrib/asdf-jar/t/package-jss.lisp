;;(asdf:clear-system :jss)

(prove:plan 1)
(asdf:make :asdf-jar)
(asdf:make :jss)
(prove:ok
 (asdf-jar:package :jss)
 "Able to package JSS")

(prove:finalize)


