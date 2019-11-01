(in-package :cl-user)

(prove:plan 1)
(prove:ok 
 (abcl/build:xdg/abcl-install-root #P"http://archive.apache.org/dist/ant/binaries/apache-ant-1.9.4-bin.zip"))

(prove:plan 2)
(let ((path (ext:make-temp-directory))
      (uri #p"http://www-eu.apache.org/dist/ant/binaries/apache-ant-1.10.7-bin.zip"))
  (prove:diag
   (format nil "Testing binary unzip installation of~%~,2t<~a>~%to~%~,2t '~a'." uri path))
  (multiple-value-bind (root contents)
      (abcl/build:xdg/install uri)
    (prove:ok (and root
                   (probe-file root)))
    (prove:ok (and
               (consp contents)
               (> (length contents) 0)))))
(prove:finalize)

