(in-package :jss)

(defparameter *cl-user-compatibility* nil
  "Whether backwards compatiblity with JSS's use of CL-USER has been enabled.")

(defun ensure-compatiblity ()
  (setf *cl-user-compatibility* t)
  (dolist (symbol '(get-java-field new))
    (unintern symbol :cl-user)
    (import symbol :cl-user)))

    
