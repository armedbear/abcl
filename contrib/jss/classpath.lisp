(in-package :java)

(defmethod add-to-classpath :after ((uri-or-uris t) &optional classloader)
  (declare (ignore classloader))
  (let ((paths (if (listp uri-or-uris)
                   uri-or-uris
                   (list uri-or-uris))))
    (dolist (path paths)
      (let ((absolute (namestring (truename path))))
        (cond ((equal (pathname-type absolute) "jar")
               (jss:jar-import absolute))
              ((ext:file-directory-p absolute)
               (jss:classfiles-import absolute)))))))

