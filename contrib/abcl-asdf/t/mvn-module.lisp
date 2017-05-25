(in-package :cl-user)

;;; TODO: restore original ASDF configuration after running test
(defun asdf-add-test-mvn-module ()
  (asdf:initialize-source-registry
   `(:source-registry
     (:directory ,(asdf:system-relative-pathname :asdf-mvn-module "t/eg/"))
     :inherit-configuration)))

(unless (ignore-errors (asdf:find-system :test-mvn-module))
  (asdf-add-test-mvn-module))

(prove:plan 1)
(prove:ok (asdf:load-system :test-mvn-module)
          "Testing loading of ASDF:MVN-MODULE definition…")

(prove:finalize)
