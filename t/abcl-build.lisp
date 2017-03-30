(in-package abcl/test/t)

(plan 1)

(let ((path (ext:make-temp-directory))
      (uri #p"http://www-eu.apache.org/dist/ant/binaries/apache-ant-1.10.1-bin.zip"))
  (diag (format nil "Testing binary unzip installation of <~a> to '~a'." uri path))
  (build-abcl:install uri :type :unpack/zip)
  (ok
   (directory (merge-pathnames "*.*" path))))

(finalize)


