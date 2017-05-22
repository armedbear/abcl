(in-package :abcl/build)

(defun abcl/build ()
  (abcl-build:ant/call (asdf:system-relative-pathname :abcl "build.xml")
                      "abcl"))

(defun abcl/dist ()
  (abcl-build:ant/call (asdf:system-relative-pathname :abcl "build.xml")
                      "abcl.release"))

(defun abcl/test ()
  (abcl-build:ant/call (asdf:system-relative-pathname :abcl "build.xml")
                      "abcl.test"))

