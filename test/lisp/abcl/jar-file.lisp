(in-package #:abcl.test.lisp)

#-:unix (error "Load test setup currently needs UNIX shell script support.")

(defun load-init ()
  (let* ((*default-pathname-defaults* *this-directory*)
         (asdf::*verbose-out* *standard-output*)
         (package-command (format nil "cd ~A; sh ~A" 
                                  *this-directory*
                                  (merge-pathnames "package-load.sh"))))
    (compile-file "foo.lisp")
    (compile-file "bar.lisp")
    (compile-file "eek.lisp")
    (asdf:run-shell-command package-command))
  (setf *jar-file-init* t))

(defvar *jar-file-init* nil)


(defmacro with-jar-file-init (&rest body)
  `(let ((*default-pathname-defaults* *this-directory*))
     (progn
       (unless *jar-file-init*
         (load-init))
       ,@body)))
  

(deftest jar-file-load.1
    (with-jar-file-init
        (load "foo"))
  t)

(deftest jar-file-load.2
    (with-jar-file-init
      (load "foo.lisp"))
  t)

(deftest jar-file-load.3
    (with-jar-file-init
      (load "foo.abcl"))
  t)

(deftest jar-file-load.4
    (with-jar-file-init
      (load "jar:file:baz.jar!/foo"))
  t)

(deftest jar-file-load.6
    (with-jar-file-init
      (load "jar:file:baz.jar!/bar"))
  t)

(deftest jar-file-load.7
    (with-jar-file-init
      (load "jar:file:baz.jar!/bar.abcl"))
  t)

(deftest jar-file-load.8
    (with-jar-file-init
      (load "jar:file:baz.jar!/eek"))
  t)

(deftest jar-file-load.9
    (with-jar-file-init
      (load "jar:file:baz.jar!/eek.lisp"))
  t)


(deftest jar-file-probe-file.1
    (with-jar-file-init
        (probe-file "jar:file:baz.jar!/eek.lisp"))
  #p"jar:file:baz.jar!/eek.lisp")


(deftest jar-file-merge-pathnames.1
    (merge-pathnames 
     "!/foo" #p"jar:file:baz.jar")
  #p"jar:file:baz.jar!/foo")





  
