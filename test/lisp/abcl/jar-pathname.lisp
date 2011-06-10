(in-package #:abcl.test.lisp)

(defparameter *tmp-directory* nil)
(defparameter *tmp-directory-whitespace* nil)
(defparameter *tmp-jar-path* nil)
(defparameter *tmp-jar-path-whitespace* nil)

(eval-when (:compile-toplevel :load-toplevel)
  (let ((temp-file (java:jcall "getAbsolutePath" 
                               (java:jstatic "createTempFile" "java.io.File" "jar" "tmp"))))
    (setf *tmp-directory*
          (truename (make-pathname :directory 
                                   (append 
                                    (pathname-directory (pathname temp-file))
                                    '("jar-pathname-tests"))))
          *tmp-directory-whitespace*
          (merge-pathnames "a/directory with/s p a/" *tmp-directory*))))

(defvar *foo.lisp*
  `((defun foo ()
      (labels ((output ()
                 (format t "FOO here.")))
        (output)))))

(defvar *bar.lisp*
  `((defvar *pathname* *load-pathname*)
    (defvar *truename* *load-truename*)

    (defun bar () 
      (labels 
          ((output () 
             (format t "Some BAR~%*load-pathname* ~S~%*load-truename* ~S~%"
                     *pathname* *truename*)))
        (output)))
    (defvar *bar* t)

    (defun baz ()
      (format t "Some BAZ"))))

(defvar *eek.lisp* 
  `((defun eek ()
      (format t "Another EEK."))
    (defun ook ()
      (let ((*load-verbose* t))
        (load (merge-pathnames #p"bar" *load-truename*))))
    (defun aak ()
      (format t "*LOAD-TRUENAME* is '~A'" *load-truename*))))

(defun write-forms (forms path)
  (with-open-file (s path :direction :output :if-exists :supersede)
    (with-standard-io-syntax
      (dolist (form forms)
        (print form s)))))

(defun jar-file-init ()
  (format t "~&Using ~A to create files for testing jar-pathnames.~%" *tmp-directory*)
  (ensure-directories-exist *tmp-directory*)
  (let* ((*default-pathname-defaults*  *tmp-directory*)
         (asdf::*verbose-out* *standard-output*))
    (write-forms *foo.lisp* "foo.lisp")
    (compile-file "foo.lisp")
    (write-forms *foo.lisp* "foo bar.lisp")
    (compile-file "foo bar.lisp")
    (write-forms *bar.lisp* "bar.lisp")
    (compile-file "bar.lisp")
    (write-forms *eek.lisp* "eek.lisp")
    (compile-file "eek.lisp")
    (let* ((tmpdir (merge-pathnames "tmp/" *tmp-directory*))
           (subdirs 
            (mapcar (lambda (p) (merge-pathnames p tmpdir))
                    '("a/b/" "d/e+f/" "path/with a couple/spaces/in it/")))
           (sub1 (first subdirs))
           (sub2 (second subdirs))
           (sub3 (third subdirs)))
      (when (probe-directory tmpdir)
        (delete-directory-and-files tmpdir))
      (mapcar (lambda (p) (ensure-directories-exist p)) subdirs)
      (sys:unzip (merge-pathnames "foo.abcl") tmpdir)
      (sys:unzip (merge-pathnames "foo.abcl") sub1)
      (sys:unzip (merge-pathnames "foo.abcl") sub3)
      (sys:unzip (merge-pathnames "foo bar.abcl") sub3)
      (cl-fad-copy-file (merge-pathnames "bar.abcl")
                        (merge-pathnames "bar.abcl" tmpdir))
      (cl-fad-copy-file (merge-pathnames "bar.abcl")
                        (merge-pathnames "bar.abcl" sub1))
      (cl-fad-copy-file (merge-pathnames "foo bar.abcl")
                        (merge-pathnames "foo bar.abcl" sub1))
      (cl-fad-copy-file (merge-pathnames "bar.abcl")
                        (merge-pathnames "bar.abcl" sub2))
      (cl-fad-copy-file (merge-pathnames "bar.abcl")
                        (merge-pathnames "bar.abcl" sub3))
      (cl-fad-copy-file (merge-pathnames "foo bar.abcl")
                        (merge-pathnames "foo bar.abcl" sub3))
      (cl-fad-copy-file (merge-pathnames "eek.lisp")
                        (merge-pathnames "eek.lisp" tmpdir))
      (cl-fad-copy-file (merge-pathnames "eek.lisp")
                        (merge-pathnames "eek.lisp" sub1))
      (setf *tmp-jar-path*
           (sys:zip (merge-pathnames "baz.jar")
                    (loop :for p :in (list tmpdir sub1 sub2 sub3)
                       :appending (directory (merge-pathnames "*" p)))
                    tmpdir))
      (ensure-directories-exist *tmp-directory-whitespace*)
      (setf *tmp-jar-path-whitespace*
            (merge-pathnames "baz.jar" *tmp-directory-whitespace*))
      (cl-fad-copy-file *tmp-jar-path* *tmp-jar-path-whitespace*))))

(defun clean-jar-tests () 
  (when (probe-file *tmp-directory*)
    (delete-directory-and-files *tmp-directory*)))

(defmacro with-jar-file-init (&rest body)
  `(let ((*default-pathname-defaults* *tmp-directory*))
     (progn
       (unless (and *tmp-jar-path* (probe-file *tmp-jar-path*))
         (jar-file-init))
       ,@body)))

(defmacro load-from-jar (jar path) 
  `(with-jar-file-init 
       (load (format nil "jar:file:~A!/~A" ,jar ,path))))

(deftest jar-pathname.load.1
    (load-from-jar *tmp-jar-path* "foo")
  t)

(deftest jar-pathname.load.2
    (load-from-jar *tmp-jar-path* "bar")
  t)

(deftest jar-pathname.load.3
    (load-from-jar *tmp-jar-path* "bar.abcl")
  t)

(deftest jar-pathname.load.4
    (load-from-jar *tmp-jar-path* "eek")
  t)

(deftest jar-pathname.load.5
    (load-from-jar *tmp-jar-path* "eek.lisp")
  t)

(deftest jar-pathname.load.6
    (load-from-jar *tmp-jar-path* "foo")
  t)

(deftest jar-pathname.load.7
    (load-from-jar *tmp-jar-path* "a/b/bar")
  t)

(deftest jar-pathname.load.8
    (load-from-jar *tmp-jar-path* "a/b/bar.abcl")
  t)

(deftest jar-pathname.load.9
    (load-from-jar *tmp-jar-path* "a/b/eek")
  t)

(deftest jar-pathname.load.10
    (load-from-jar *tmp-jar-path* "a/b/eek.lisp")
  t)

(deftest jar-pathname.load.11
    (load-from-jar *tmp-jar-path* "d/e+f/bar.abcl")
  t)

(deftest jar-pathname.load.12
    (load-from-jar *tmp-jar-path* "a/b/foo%20bar.abcl")
  t)

(deftest jar-pathname.load.13
    (load-from-jar *tmp-jar-path* "a/b/foo bar.abcl")
  t)

(deftest jar-pathname.load.14
    (load-from-jar *tmp-jar-path-whitespace* "a/b/foo.abcl")
  t)

(deftest jar-pathname.load.15
    (load-from-jar *tmp-jar-path-whitespace* "a/b/foo bar.abcl")
  t)

(deftest jar-pathname.load.16
    (load-from-jar *tmp-jar-path-whitespace* "a/b/foo%20bar.abcl")
  t)

(defparameter *url-jar-pathname-base*
  "jar:http://abcl-dynamic-install.googlecode.com/files/baz-20110610a.jar!/")

(defmacro load-url-relative (path) 
  `(load (format nil "~A~A" *url-jar-pathname-base* ,path)))

;;; wrapped in PROGN for easy disabling without a network connection
;;; XXX come up with a better abstraction

(progn 
  (deftest jar-pathname.load.http.1
      (load-url-relative "foo")
    t)

  (deftest jar-pathname.load.http.2
      (load-url-relative "bar")
    t)

  (deftest jar-pathname.load.http.3
      (load-url-relative "bar.abcl")
    t)

  (deftest jar-pathname.load.http.4
      (load-url-relative "eek")
    t)

  (deftest jar-pathname.load.http.5
      (load-url-relative "eek.lisp")
    t)

  (deftest jar-pathname.load.http.6
      (load-url-relative "a/b/foo")
    t)

  (deftest jar-pathname.load.http.7
      (load-url-relative "a/b/bar")
    t)

  (deftest jar-pathname.load.http.8
      (load-url-relative "a/b/bar.abcl")
    t)

  (deftest jar-pathname.load.http.9
      (load-url-relative "a/b/eek")
    t)

  (deftest jar-pathname.load.http.10
      (load-url-relative "a/b/eek.lisp")
    t))

(deftest jar-pathname.probe-file.1
    (with-jar-file-init
        (probe-file "jar:file:baz.jar!/eek.lisp"))
  #p#.(format nil "jar:file:~A/baz.jar!/eek.lisp" 
              (namestring *tmp-directory*)))

(deftest jar-pathname.probe-file.2
    (with-jar-file-init
        (probe-file "jar:file:baz.jar!/a/b/bar.abcl"))
  #p#.(format nil "jar:file:~A/baz.jar!/a/b/bar.abcl"
              (namestring *tmp-directory*)))

(deftest jar-pathname.probe-file.3
    (with-jar-file-init
        (probe-file "jar:jar:file:baz.jar!/a/b/bar.abcl!/bar._"))
   #p#.(format nil "jar:jar:file:~Abaz.jar!/a/b/bar.abcl!/bar._"
                       (namestring *tmp-directory*)))

(push 'jar-pathname.probe-file.4 *expected-failures*)
(deftest jar-pathname.probe-file.4
    (with-jar-file-init
        (probe-file "jar:file:baz.jar!/a/b"))
  #p#.(format nil "jar:file:~Abaz.jar!/a/b/"
                       (namestring *tmp-directory*)))

(push 'jar-pathname.probe-file.5 *expected-failures*)
(deftest jar-pathname.probe-file.5
    (with-jar-file-init
        (probe-file "jar:file:baz.jar!/a/b/"))
  #p#.(format nil "jar:file:~Abaz.jar!/a/b/"
                       (namestring *tmp-directory*)))

(deftest jar-pathname.probe-file.6
    (with-jar-file-init
        (probe-file "jar:file:baz.jar!/d/e+f/bar.abcl"))
  #p#.(format nil "jar:file:~Abaz.jar!/d/e+f/bar.abcl"
                       (namestring *tmp-directory*)))

(deftest jar-pathname.merge-pathnames.1
    (merge-pathnames 
     "/bar.abcl" #p"jar:file:baz.jar!/foo")
  #p"jar:file:baz.jar!/bar.abcl")

(deftest jar-pathname.merge-pathnames.2
    (merge-pathnames 
     "bar.abcl" #p"jar:file:baz.jar!/foo/")
  #p"jar:file:baz.jar!/foo/bar.abcl")

(deftest jar-pathname.merge-pathnames.3
    (merge-pathnames 
     "jar:file:baz.jar!/foo" "bar")
  #p"jar:file:baz.jar!/foo")

(deftest jar-pathname.merge-pathnames.4
    (merge-pathnames 
     "jar:file:baz.jar!/foo" "/a/b/c")
  #p"jar:file:/a/b/baz.jar!/foo")


;;; Under win32, we get the device in the merged path
#+windows 
(push 'jar-pathname.merge-pathnames.5 *expected-failures*)

(deftest jar-pathname.merge-pathnames.5
    (merge-pathnames "jar:file:/a/b/c/foo.jar!/bar/baz.lisp")
  #p"jar:file:/a/b/c/foo.jar!/bar/baz.lisp")

(deftest jar-pathname.truename.1
    (signals-error (truename "jar:file:baz.jar!/foo")
                   'file-error)
  t)

(deftest jar-pathname.1
    (let* ((p #p"jar:file:foo/baz.jar!/")
           (d (first (pathname-device p))))
      (values
       (pathname-directory d) (pathname-name d) (pathname-type d)))
  (:relative "foo") "baz" "jar")

(deftest jar-pathname.2
    (let* ((p #p"jar:file:baz.jar!/foo.abcl")
           (d (first (pathname-device p))))
      (values
       (pathname-name d) (pathname-type d) 
       (pathname-directory p) (pathname-name p) (pathname-type p)))
  "baz" "jar"
   (:absolute) "foo" "abcl")
   
(deftest jar-pathname.3
    (let* ((p #p"jar:jar:file:baz.jar!/foo.abcl!/")
           (d0 (first (pathname-device p)))
           (d1 (second (pathname-device p))))
      (values 
       (pathname-name d0) (pathname-type d0)
       (pathname-name d1) (pathname-type d1)))
  "baz" "jar"
  "foo" "abcl")

(deftest jar-pathname.4
    (let* ((p #p"jar:jar:file:a/baz.jar!/b/c/foo.abcl!/this/that/foo-20.cls")
           (d0 (first (pathname-device p)))
           (d1 (second (pathname-device p))))
      (values 
       (pathname-directory d0) (pathname-name d0) (pathname-type d0)
       (pathname-directory d1) (pathname-name d1) (pathname-type d1)
       (pathname-directory p) (pathname-name p) (pathname-type p)))
  (:relative "a") "baz" "jar"
  (:relative "b" "c") "foo" "abcl"
  (:absolute "this" "that") "foo-20" "cls")

(deftest jar-pathname.5
    (let* ((p #p"jar:jar:file:a/foo/baz.jar!/b/c/foo.abcl!/armed/bear/bar-1.cls")
           (d0 (first (pathname-device p)))
           (d1 (second (pathname-device p))))
      (values 
       (pathname-directory d0) (pathname-name d0) (pathname-type d0)
       (pathname-directory d1) (pathname-name d1) (pathname-type d1)
       (pathname-directory p) (pathname-name p) (pathname-type p)))
  (:relative "a" "foo" ) "baz" "jar"
  (:relative "b" "c") "foo" "abcl"
  (:absolute "armed" "bear") "bar-1" "cls")

(deftest jar-pathname.6
    (let* ((p #p"jar:http://example.org/abcl.jar!/org/armedbear/lisp/Version.class")
           (d (first (pathname-device p))))
      (values 
       (ext:pathname-url-p d)
       (namestring d)
       (pathname-directory p) (pathname-name p) (pathname-type p)))
  t
  "http://example.org/abcl.jar" 
  (:absolute "org" "armedbear" "lisp") "Version" "class")

(deftest jar-pathname.7
    (let* ((p #p"jar:jar:http://example.org/abcl.jar!/foo.abcl!/foo-1.cls")
           (d (pathname-device p))
           (d0 (first d))
           (d1 (second d)))
      (values
       (ext:pathname-url-p d0)
       (namestring d0)
       (pathname-name d1) (pathname-type d1)
       (pathname-name p) (pathname-type p)))
  t
  "http://example.org/abcl.jar"
  "foo" "abcl"
  "foo-1" "cls")

(deftest jar-pathname.8
    (let* ((p #p"jar:file:/a/b/foo.jar!/")
           (d (first (pathname-device p))))
      (values
       (pathname-directory d) (pathname-name d) (pathname-type d)))
  (:ABSOLUTE "a" "b") "foo" "jar")

(deftest jar-pathname.9
    (let* ((p #p"jar:file:a/b/foo.jar!/c/d/foo.lisp")
           (d (first (pathname-device p))))
      (values
       (pathname-directory d) (pathname-name d) (pathname-type d)
       (pathname-directory p) (pathname-name p) (pathname-type p)))
  (:relative "a" "b") "foo" "jar"
  (:absolute "c" "d") "foo" "lisp")

;;; 'jar:file:' forms must be URI encoded, meaning whitespace is not allowed
(deftest jar-pathname.10
    (signals-error 
     (let ((s "jar:file:/foo/bar/a space/that!/this"))
       (equal s
              (namestring (pathname s))))
     'file-error)
  t)

(deftest jar-pathname.11
    (let ((s (string-downcase "jar:file:/foo/bar/a%20space%3f/that!/this")))
      (string= s
               (string-downcase (namestring (pathname s)))))
  t)

;;; We allow jar-pathname to be contructed without a device to allow
;;; MERGE-PATHNAMES to work, even though #p"file:" is illegal.
(deftest jar-pathname.12
    (string= (namestring (first (pathname-device #p"jar:file:!/foo.bar")))
             "")
  t)

(deftest jar-pathname.match-p.1
    (pathname-match-p "jar:file:/a/b/some.jar!/a/system/def.asd"
                      "jar:file:/**/*.jar!/**/*.asd")
  t)

(deftest jar-pathname.match-p.2
    (pathname-match-p "/a/system/def.asd"
                      "jar:file:/**/*.jar!/**/*.asd")
  nil)

(deftest jar-pathname.match-p.3
    (pathname-match-p "jar:file:/a/b/some.jar!/a/system/def.asd"
                      "/**/*.asd")
  nil)

(deftest jar-pathname.translate.1
    (translate-pathname "jar:file:/a/b/c.jar!/d/e/f.lisp" 
			"jar:file:/**/*.jar!/**/*.*" 
			"/foo/**/*.*")
  #p"/foo/d/e/f.lisp")

      

        

  
