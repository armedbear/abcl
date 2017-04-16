(in-package abcl-build)

(defun install/ant ()
  (let ((uri
         #p"http://archive.apache.org/dist/ant/binaries/apache-ant-1.9.4-bin.zip"

         #+nil ;; https on OPEN fails; probably attempting to upgrade
         #p"https://archive.apache.org/dist/ant/binaries/apache-ant-1.9.4-bin.zip"

         #+nil ;; need apache-ant-1.9 for JVM version 49.0 
         #p"http://www-eu.apache.org/dist/ant/binaries/apache-ant-1.10.1-bin.zip"))
    (let ((root (install uri :type :unpack/zip)))
      (values
       (merge-pathnames "bin/ant" root)
       root
       (directory (merge-pathnames "*.*" root))))))
