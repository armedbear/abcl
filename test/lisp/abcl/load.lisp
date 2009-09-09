(in-package #:abcl.test.lisp)

#-:unix (error "Load test setup currently needs UNIX shell script support.")

(defun load-init ()
  (let* ((*default-pathname-defaults* *this-directory*)
         (asdf::*verbose-out* *standard-output*)
         (package-command (format nil "sh ~A" (merge-pathnames "package-load.sh"))))
    (compile-file "foo.lisp")
    (compile-file "bar.lisp")
    (compile-file "eek.lisp")
    (asdf:run-shell-command package-command)))

(load-init)

(deftest load.1
    (let ((*default-pathname-defaults* *this-directory*))
      (load "foo"))
  t)

(deftest load.2
    (let ((*default-pathname-defaults* *this-directory*))
      (load "foo.lisp"))
  t)

(deftest load.3
    (let ((*default-pathname-defaults* *this-directory*))
      (load "foo.abcl"))
  t)

(deftest load.4
    (let ((*default-pathname-defaults* *this-directory*))
      (load "jar:file:baz.jar!/foo"))
  t)

(deftest load.6
    (let ((*default-pathname-defaults* *this-directory*))
      (load "jar:file:baz.jar!/bar"))
  t)

(deftest load.7
    (let ((*default-pathname-defaults* *this-directory*))
      (load "jar:file:baz.jar!/bar.abcl"))
  t)

(deftest load.8
    (let ((*default-pathname-defaults* *this-directory*))
      (load "jar:file:baz.jar!/eek"))
  t)

(deftest load.9
    (let ((*default-pathname-defaults* *this-directory*))
      (load "jar:file:baz.jar!/eek.lisp"))
  t)





  
