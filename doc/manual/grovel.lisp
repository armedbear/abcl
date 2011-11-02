#-abcl We're only grovelling ABCL docstrings here.
(defun grovel-docstrings-as-tex (&optional (package (find-package :java)))
  (let ((output-file (format nil "~A.tex" (string-downcase (package-name package)))))
    (with-open-file (stream output-file :direction :output)
      (format t "Writing output to ~A.~%" output-file)
      (loop :for symbol :being :each :external-symbol :of package 
         :doing (format stream "~&~A~%~%" (symbol-as-tex symbol))))))

(asdf:load-system 'swank) ;; XXX Does this load the SWANK-BACKEND package as well

(defun arglist-as-tex (symbol)
  (handler-case 
      (loop :for arg :in (arglist symbol)
         :collecting
         (format nil 
                 ;;; XXX should really check the entire input for TeX escapes
                 (if (and (symbolp arg)
                          (or (string= (subseq (symbol-name arg) 0 1) #\&)
                              (string= (subseq (symbol-name arg) 0 1) #\%)))
                     "\\~A"
                     "~A")
                 (if (symbolp arg)
                     (string-downcase (symbol-name arg))
                     (format nil "~(~A~)" arg))))
    (t (e) 
      (progn (warn "Failed to form arglist for ~A: ~A" symbol e)
             (list "")))))
             

(defvar *type-alist* 
  '((:function 
     . "Function")
    (:macro 
     . "Macro")
    (:variable 
     . "Variable")
    (:class 
     . "Class")
    (:special-operator
     . "Special Operator")
    (:generic-function 
     . "Generic Function")))

(defun symbol-as-tex (symbol)
  "Return the TeX representation of a SYMBOL as Tex."
  (let (type documentation arglist doc symbol-name package-name)
    (when (setf doc (swank-backend:describe-symbol-for-emacs symbol))
        (cond 
          ((find :function doc)
           (setf type :function
                 documentation (second doc)
                 arglist (format nil "~{~A~^ ~}" (arglist-as-tex symbol))))
          ((find :variable doc)
           (setf type :variable 
                 documentation (second doc)))
          ((find :macro doc)
           (setf type :macro
                 documentation (second doc)))
          ((find :generic-function doc)
           (setf type :generic-function
                 documentation (second doc)))
          ((find :class doc)
           (setf type :class
                 documentation (second doc)))
          ((find :special-operator doc)
           (setf type :special-operator
                 documentation (second doc)))
          (t 
           (warn "Unknown type of documentation for symbol ~A: ~A"
                 symbol doc)))
        (setf symbol-name (string-downcase 
                           symbol)
              package-name (string-downcase 
                            (package-name (find-package (symbol-package symbol)))))
        (format nil "~&\\paragraph{}~&\\label{~A:~A}~&\\index{~A}~&--- ~A: \\textbf{~A} [\\textbf{~A}] \\textit{~A}~%~%\\begin{adjustwidth}{5em}{5em}~&~A~&\\end{adjustwidth}"
                (package-name (find-package (symbol-package symbol)))
                (symbol-name symbol)
                (symbol-name symbol)
                (cdr (assoc type *type-alist*))
                symbol-name
                package-name
                (if arglist arglist "")
                (if documentation documentation "")))))
                
                



  
          
    
     