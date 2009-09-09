(defun eek ()
  (format t "Another EEK."))

(defun ook ()
  (let ((*load-verbose* t))
    (load (merge-pathnames #p"bar" *load-truename*))))

(defun aak ()
  (format t "*LOAD-TRUENAME* is '~A'" *load-truename*))
