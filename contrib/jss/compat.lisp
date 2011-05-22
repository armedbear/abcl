(in-package :jss)

(defparameter *cl-user-compatibility* nil
  "Whether backwards compatiblity with JSS's use of CL-USER has been enabled.")

(defun ensure-compatiblity ()
  (setf *cl-user-compatibility* t)
  (let ((dont-export '(add-to-classpath *cl-user-compatibility*)))
    (loop :for symbol :being :each :external-symbol :in :jss 
       :when (not (find symbol dont-export))
       :do 
         (unintern symbol :cl-user)
       :and :do
         (import symbol :cl-user))))

    
