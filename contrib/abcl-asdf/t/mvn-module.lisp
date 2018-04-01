(in-package :cl-user)

;;; TODO: restore original ASDF configuration after running test
(defun asdf-add-test-mvn-module ()
  (asdf:initialize-source-registry
   `(:source-registry
     (:directory ,(asdf:system-relative-pathname :asdf-mvn-module "t/eg/"))
     :inherit-configuration)))

(unless (ignore-errors (asdf:find-system :test-mvn-module))
  (asdf-add-test-mvn-module))

(prove:plan 3)
(prove:ok (asdf:load-system :test-mvn-module)
          "Testing loading of ASDF:MVN-MODULE definition…")
(prove:ok (asdf:load-system :soot-only-repositories)
          "Testing loading with only repositories list…")
(prove:ok (asdf:load-system :soot-mixed-repositories)
          "Testing loading with both single and list of repositories…")
(prove:finalize)
