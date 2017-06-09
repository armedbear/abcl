(in-package abcl/documentation)

#+elisp ;; huh?
(slime-apropos-package "JAVA")

(defun index ()
  "Regenerate TeX markup from symbol introspection."
  (dolist (package '(:extensions
                     :system
                     :threads
                     :mop
                     :java
                     :jss ))
    (grovel-docstrings-as-tex :package package)))

