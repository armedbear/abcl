(in-package :abcl-asdf-test)

(defun run (&rest args)
  (abcl-rt:do-tests))

(rt:deftest ABCL-ASDF.MAVEN.1 
    (multiple-value-list (ensure-mvn-version))
  (t (3 0 4)))

  
