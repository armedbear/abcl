(in-package #:abcl/test/lisp)

(defparameter *tmp-directory* nil)
(defparameter *tmp-directory-whitespace* nil)
(defparameter *tmp-jar-path* nil)
(defparameter *tmp-jar-path-whitespace* nil)

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

(defun create-jar ()
  (let* ((temp-file (java:jcall "getAbsolutePath" 
                                (java:jstatic "createTempFile" "java.io.File" "jar" "tmp")))
         (temp-dir (make-pathname :directory (append 
                                              (pathname-directory (pathname temp-file))
                                              '("jar-pathname-tests")))))
    (jar-file-init temp-dir)))

(defun jar-file-init (temp-dir)
  "Create the jar archives used for testing.
Returns the two values of the pathnames of the created archives."
  (ensure-directories-exist temp-dir)
  (setf *tmp-directory*
        (truename temp-dir)
        *tmp-directory-whitespace*
        (merge-pathnames "a/directory with/s p a/" *tmp-directory*))
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
      (cl-fad-copy-file *tmp-jar-path* *tmp-jar-path-whitespace* :overwrite t)))
  (values *tmp-jar-path* *tmp-jar-path-whitespace*))

(defun clean-jar-tests () 
  (when (probe-file *tmp-directory*)
    (delete-directory-and-files *tmp-directory*)))

(defmacro with-jar-file-init (&rest body)
  `(progn 
     (unless (and *tmp-jar-path* (probe-file *tmp-jar-path*))
       (create-jar))
     (let ((*default-pathname-defaults* *tmp-directory*)) ;; why do we need this?
       ,@body)))

(defun load-from-jar (jar entry) 
  (load (merge-jar-entry jar entry)))

(defun merge-jar-entry (jar entry)
  (let ((jar-pathname (if (ext:pathname-jar-p jar)
                          jar
                          (make-pathname :device (list jar)))))
    (merge-pathnames entry jar-pathname)))


(deftest jar-pathname.load.2
  (with-jar-file-init
      (load-from-jar *tmp-jar-path* "bar"))
  t)

(deftest jar-pathname.load.3
  (with-jar-file-init
      (load-from-jar *tmp-jar-path* "bar.abcl"))
  t)

(deftest jar-pathname.load.4
  (with-jar-file-init
      (load-from-jar *tmp-jar-path* "eek"))
  t)

(deftest jar-pathname.load.5
  (with-jar-file-init
      (load-from-jar *tmp-jar-path* "eek.lisp"))
  t)

(deftest jar-pathname.load.6
  (signals-error 
   (load-from-jar *tmp-jar-path* "this doesn't exist")
   'file-error)
  t)

(deftest jar-pathname.load.7
  (with-jar-file-init
      (load-from-jar *tmp-jar-path* "a/b/bar"))
  t)

(deftest jar-pathname.load.8
  (with-jar-file-init
      (load-from-jar *tmp-jar-path* "a/b/bar.abcl"))
  t)

(deftest jar-pathname.load.9
  (with-jar-file-init
      (load-from-jar *tmp-jar-path* "a/b/eek"))
  t)

(deftest jar-pathname.load.10
  (with-jar-file-init
      (load-from-jar *tmp-jar-path* "a/b/eek.lisp"))
  t)

(deftest jar-pathname.load.11
  (with-jar-file-init
      (load-from-jar *tmp-jar-path* "d/e+f/bar.abcl"))
  t)

  
  #+(or) ;; URI encodings in namestring are not currently interpolated 
(deftest jar-pathname.load.12
    (with-jar-file-init
        (load-from-jar *tmp-jar-path* "a/b/foo%20bar.abcl"))
  t)

(deftest jar-pathname.load.13
    (with-jar-file-init 
        (load-from-jar *tmp-jar-path* "a/b/foo bar.abcl"))
  t)

#+(or) ;; URI encodings in namestring are not currently interpolated 
(deftest jar-pathname.load.14
    (with-jar-file-init
	(load-from-jar *tmp-jar-path-whitespace* "a/b/bar.abcl"))  
  t)
#+(or) ;; URI encodings in namestring are not currently interpolated 
(deftest jar-pathname.load.15
    (load-from-jar *tmp-jar-path-whitespace* "a/b/foo bar.abcl")  
  t)

  #+(or) ;; URI encodings in namestring are not currently interpolated 
(deftest jar-pathname.load.16
    (load-from-jar *tmp-jar-path-whitespace* "a/b/foo%20bar.abcl")
  t)

(defparameter *url-jar-pathname-base*
  #p"jar:https://abcl.org/releases/1.7.1/abcl-contrib.jar!/")

(deftest jar-pathname.url.https.1
    (equalp
     *url-jar-pathname-base*
     (probe-file *url-jar-pathname-base*))
  t)

(deftest jar-pathname.url.https.2
     (namestring (merge-pathnames "**" "jar:https://abcl.org/releases/1.7.1/abcl-contrib.jar!/"))
  "jar:https://abcl.org/releases/1.7.1/abcl-contrib.jar!/**")

(deftest jar-pathname.url.https.3
    (not (null (probe-file #p"jar:https://abcl.org/releases/1.7.1/abcl-contrib.jar!/README.markdown")))
  t)

(deftest jar-pathname.url.https.4
    (< 1 (length (directory #p"jar:https://abcl.org/releases/1.7.1/abcl-contrib.jar!/**/")))
  t)
    
(deftest jar-pathname.probe-file.1
    (with-jar-file-init
        (let ((p (merge-jar-entry  *tmp-jar-path* "eek.lisp")))
          (not (null (probe-file p)))))
  t)

(deftest jar-pathname.probe-file.2
  (with-jar-file-init
      (let ((p (merge-jar-entry *tmp-jar-path* "a/b/bar.abcl")))
        (not (null (probe-file p)))))
  t)

(deftest jar-pathname.probe-file.3
    (with-jar-file-init
        (let ((p (make-pathname :device (list (pathname *tmp-jar-path*) #p"a/b/bar.abcl")
                                :directory '(:absolute)
                                :name "bar_1"
                                :type "cls")))
          (not (null (probe-file p)))))
  t)

(deftest jar-pathname.probe-file.4
    (with-jar-file-init
        (let ((p (merge-jar-entry *tmp-jar-path* "a/b/bar.abcl")))
          (not (null (probe-file p)))))
  t)

(deftest jar-pathname.probe-file.5
  (with-jar-file-init
      (let ((p (merge-jar-entry *tmp-jar-path* "a/b/" )))
        (not (null (probe-file p)))))
  t)

(deftest jar-pathname.probe-file.6
  (with-jar-file-init
      (let ((p (merge-jar-entry *tmp-jar-path* "d/e+f/bar.abcl")))
        (not (null (probe-file p)))))
  t)

(deftest jar-pathname.probe-file.7
  (with-jar-file-init 
      (not (null (probe-file (merge-jar-entry *tmp-jar-path* "__loader__._")))))
  t)


#+(or) ;; abcl-1.8.0 behavior is not to merge absolute pathname with JAR-PATHNAME defaults
(deftest jar-pathname.merge-pathnames.1
  (merge-pathnames "/bar.abcl" #p"jar:file:/baz.jar!/foo")
  #p"jar:file:/baz.jar!/bar.abcl")

(deftest jar-pathname.merge-pathnames.2
  (namestring (merge-pathnames "bar.abcl" #p"jar:file:///baz.jar!/foo/baz"))
  "jar:file:///baz.jar!/foo/bar.abcl")

(deftest jar-pathname.merge-pathnames.3
  (namestring (merge-pathnames "jar:file:///baz.jar!/foo" "bar"))
  "jar:file:///baz.jar!/foo")

(deftest jar-pathname.merge-pathnames.4
    (namestring (merge-pathnames "jar:file:///baz.jar!/foo" "/a/b/c"))
  "jar:file:///baz.jar!/foo")

;;; Under win32, we get the device in the merged path
#+windows 
(push 'jar-pathname.merge-pathnames.5 *expected-failures*)
(deftest jar-pathname.merge-pathnames.5
  (namestring (merge-pathnames "jar:file:///a/b/c/foo.jar!/bar/baz.lisp"))
  "jar:file:///a/b/c/foo.jar!/bar/baz.lisp")

(deftest jar-pathname.truename.1
  (signals-error (truename "jar:file:baz.jar!/foo")
                 'file-error)
  t)

(deftest jar-pathname.1
    (let* ((p #p"jar:file:/foo/baz.jar!/")
           (d (first (pathname-device p))))
      (values
       (pathname-directory d) (pathname-name d) (pathname-type d)))
  (:absolute "foo") "baz" "jar")

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

;;; 'jar:file:' forms currently (abcl-1.8.0) can't be URI encoded, meaning whitespace is not allowed
(deftest jar-pathname.10
    (signals-error 
     (let ((s "jar:file:/foo/bar/a space/that!/this"))
       (equal s
              (namestring (pathname s))))
     'error)
  t)

#+(or) ;; URI escaping not returned 
(deftest jar-pathname.11
    (let ((s (string-downcase "jar:file:///foo/bar/a%20space%3f/that!/this")))
      (string= s
               (string-downcase (namestring (pathname s)))))
  t)

;;; We allow jar-pathname to be contructed without a device to allow
;;; MERGE-PATHNAMES to work, even though #p"file:" is illegal.
#+(or)
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

;;; ticket #181
;;; TODO Make reasons for failure more clear
(deftest jar-pathname.truename.1
    (let* ((abcl 
            (slot-value (asdf:find-system 'abcl) 'asdf::absolute-pathname))
           (jar-entry 
            (pathname (format nil "jar:file:~A/dist/abcl-contrib.jar!/jss/jss.asd" (namestring abcl))))
           (jar-entry-dir 
            (make-pathname :defaults jar-entry :name nil :type nil))
           (defaults 
            *default-pathname-defaults*))
      (let ((*default-pathname-defaults* jar-entry-dir))
        (not (probe-file (merge-pathnames jar-entry)))))
  nil)
  
