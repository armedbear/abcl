(in-package :jss)

(defvar *jtypecache* (make-hash-table :test 'eq))

(defun jtypep (object type)
  (declare (optimize (speed 3) (safety 0)))
  (let ((class (or (gethash type *jtypecache*)
                   (ignore-errors (setf (gethash type *jtypecache*) (find-java-class type)))))
        (method (load-time-value (jmethod "java.lang.Class" "isInstance" "java.lang.Object"))))
    (and class 
         (jcall method class object))))
    
(defmacro jtypecase (keyform &body cases)
  "JTYPECASE Keyform {(Type Form*)}*
  Evaluates the Forms in the first clause for which Type names a class that Keyform isInstance of
  is true."
  (sys::case-body 'jtypecase keyform cases t 'jtypep nil nil nil))

