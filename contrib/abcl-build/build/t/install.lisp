(in-package :cl-user)

(let ((uri #p"https://downloads.apache.org/ant/binaries/apache-ant-1.10.9-bin.zip"))
  (prove:plan 1)
  (prove:ok 
   (abcl/build:xdg/abcl-install-root uri)
   (format nil "Suitable install root for <~a>" uri))

   (prove:plan 2)
   (let ((path (ext:make-temp-directory)))
         (prove:diag
           (format nil "Testing binary unzip installation of~%~,2t<~a>~%to~%~,2t '~a'." uri path))
         (multiple-value-bind (root contents)
             (abcl/build:xdg/install uri)
           (prove:ok (and root
                          (probe-file root)))
           (prove:ok (and
                      (consp contents)
                      (> (length contents) 0))))))
(prove:finalize)

