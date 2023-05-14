(asdf:make :asdf-jar)

(prove:plan 1)
;;;; Systems like JSS are part of the ABCL-CONTRIB, therefore usually
;;;; reside in jar files, for which copying fasls is currently broken
;;;; <https://github.com/armedbear/abcl/issues/476>
(asdf:clear-system :jss)
(asdf:make :jss)
(prove:ok
 (asdf-jar:package :jss :verbose t :fasls nil)
 "Able to package JSS")

(asdf:make :quicklisp-abcl)

(prove:plan 1)
(prove:ok
 (asdf-jar:package :hunchentoot :verbose t :fasls nil)
 (format nil "Able to package HUNCHENTOOT"))

(prove:finalize)



