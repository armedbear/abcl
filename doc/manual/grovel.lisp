#-abcl We're only grovelling ABCL docstrings here.
(defun grovel-docstrings-as-tex (&optional (package (find-package :java)))
  (with-open-file (stream "java.tex" :direction :output)
    (loop :for symbol :being :each :external-symbol :of package 
       :collecting (symbol-as-tex symbol))))

(asdf:load-system 'swank) ;; XXX Does this load the SWANK-BACKEND package as well

(defun arglist-as-string (symbol)
  (loop :for arg :in (arglist symbol)
     :collecting (format nil "~A" (symbol-name arg))))

(defvar *type-alist* 
  '((:function . "Function")
    (:macro . "Macro")
    (:variable . "Variable")
    (:generic-function . "Generic Function")))

(defun symbol-as-tex (symbol)
  "Return the TeX representation of a SYMBOL as Tex."
  (let (type documentation arglist doc)
    (when (setf doc (swank-backend:describe-symbol-for-emacs symbol))
        (cond 
          ((find :function doc)
           (setf type :function
                 documentation (second doc)
                 arglist (arglist-as-string symbol)))
          ((find :variable doc)
           (setf type :variable 
                 documentation (second doc)))
          ((find :macro doc)
           (setf type :macro
                 documentation (second doc)))
          ((find :generic-function doc)
           (setf type :generic-function
                 documentation (second doc))))
        (format nil "\\ref{~A:~A}~&--- ~A [\\textbf{~A}]: ~A"
                (symbol-name symbol)
                (package-name (symbol-package symbol))
                (cdr (assoc type *type-alist*))
                (symbol-name symbol)
                (package-name (symbol-package symbol))))))
                



  
          
    
     