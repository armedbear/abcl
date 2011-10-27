#-abcl We're only grovelling ABCL docstrings here.
(defun grovel-docstrings-as-tex (&optional (package (find-package :java)))
  (with-open-file (stream "java.tex" :direction :output)
    (loop :for symbol :being :each :external-symbol :of package 
       :collecting (symbol-tex symbol))))

(asdf:load-system 'swank) ;; XXX Does this load the SWANK-BACKEND package as well

(defun symbol-as-tex (symbol)
  "Return the TeX representation of a SYMBOL as a string."
  (let (type documentation arglist 
             (doc (swank-backend:describe-symbol-for-emacs symbol)))
    (cond ((find :function doc)
           (setf type :function
                 documentation (second doc)))
          ((find :variable doc)
           (setf type :variable 
                 documentation (second doc))))
    (warn "Unfinished implementation.")))
  
          
    
     