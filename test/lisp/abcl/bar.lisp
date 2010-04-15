(defvar *pathname* *load-pathname*)
(defvar *truename* *load-truename*)

(defun bar () 
  (labels 
      ((output () 
         (format t "Some BAR~%*load-pathname* ~S~%*load-truename* ~S~%"
                 *pathname* *truename*)))
    (output)))

(defvar *bar* t)

(defun baz ()
  (format t "Some BAZ"))


