(in-package :abcl/documentation)

(defun grovel-docstrings-as-tex (&key
                                   (package
                                    (find-package :java))
                                   (directory
                                    (asdf:component-pathname
                                     (asdf:find-component :abcl/documentation 'grovel))))
  "Transform exported symbols documentation in PACKAGE to DIRECTORY."
  (let ((output-file (merge-pathnames
                      (format nil "~A.tex" (string-downcase (package-name package)))
                      directory))
        (symbols (loop :for symbol :being :each :external-symbol :of package :collecting symbol)))
    (with-open-file (stream output-file :direction :output)
      (format t "Writing output to '~A'.~%" output-file)
      (loop :for symbol :in (sort symbols
                                  (lambda (a b)
                                    (string-lessp (symbol-name a) (symbol-name b))))
         :doing (let ((documentation (symbol-as-tex symbol)))
                  (when documentation
                    (format stream "~&~A~%~%" documentation)))))))

(defun texify-string (string &optional remove)
  (with-output-to-string (s)
    (loop for char across string
         do (if (find char '(#\& #\% #\# #\_))
                (unless remove
                  (write-char #\\ s)
                  (write-char char s))
                (write-char char s)))))

(defun texify (thing)
  "Return STRING with LaTeX-sensitive characters escaped.
Downcase symbol names but leave strings alone."
  (cond ((listp thing)
         (format nil "~A" (mapcar #'texify thing)))
        ((stringp thing) (texify-string thing))
        ((symbolp thing) (texify-string (string-downcase (symbol-name thing))))))

(defun arglist-as-tex (symbol)
  (handler-case 
      (loop :for arg :in (ext:arglist symbol)
         :collecting (texify arg))
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
        (format nil "~&\\paragraph{}
\\label{~A:~A}
\\index{~A}
--- ~A: \\textbf{~A} [\\textbf{~A}] \\textit{~A}

\\begin{adjustwidth}{5em}{5em}
~A
\\end{adjustwidth}"
                (texify-string (package-name (find-package (symbol-package symbol))) t)
                (texify-string (symbol-name symbol) t)
                (texify-string (symbol-name symbol) t)
                (cdr (assoc type *type-alist*))
                (texify symbol-name)
                (texify package-name)
                (if arglist arglist "")
                (if documentation (texify documentation) "")))))
                
                



  
          
    
     
