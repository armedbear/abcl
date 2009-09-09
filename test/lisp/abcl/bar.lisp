(defun bar () 
  (labels 
      ((output () (format t "Some BAR")))
    (output)))

(defvar *bar* t)

(defun baz ()
  (format t "Some BAZ"))


