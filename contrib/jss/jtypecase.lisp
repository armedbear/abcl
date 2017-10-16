(in-package :jss)

(defmacro jtypecase (keyform &body cases)
  "JTYPECASE Keyform {(Type Form*)}*
  Evaluates the Forms in the first clause for which Type names a class that Keyform #"isInstance" of
  is true."
  (sys::case-body 'jtypecase keyform cases t `(lambda(i c) (jcall "isInstance" (find-java-class c) i))  nil nil nil))

