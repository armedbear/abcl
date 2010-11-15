(in-package #:abcl.test.lisp)

(defvar *jar-file-init* nil)

;;; From CL-FAD
(defvar *stream-buffer-size* 8192)
(defun cl-fad-copy-stream (from to &optional (checkp t))
  "Copies into TO \(a stream) from FROM \(also a stream) until the end
of FROM is reached, in blocks of *stream-buffer-size*.  The streams
should have the same element type.  If CHECKP is true, the streams are
checked for compatibility of their types."
  (when checkp
    (unless (subtypep (stream-element-type to) (stream-element-type from))
      (error "Incompatible streams ~A and ~A." from to)))
  (let ((buf (make-array *stream-buffer-size*
                         :element-type (stream-element-type from))))
    (loop
     (let ((pos (read-sequence buf from)))
       (when (zerop pos) (return))
       (write-sequence buf to :end pos))))
  (values))

(defun cl-fad-copy-file (from to &key overwrite)
  "Copies the file designated by the non-wild pathname designator FROM
to the file designated by the non-wild pathname designator TO.  If
OVERWRITE is true overwrites the file designtated by TO if it exists."
  (let ((element-type '(unsigned-byte 8)))
    (with-open-file (in from :element-type element-type)
      (with-open-file (out to :element-type element-type
                              :direction :output
                              :if-exists (if overwrite
                                           :supersede :error))
        (cl-fad-copy-stream in out))))
  (values))

(defun jar-file-init ()
  (let* ((*default-pathname-defaults*  *abcl-test-directory*)
         (asdf::*verbose-out* *standard-output*))
    (compile-file "foo.lisp")
    (compile-file "bar.lisp")
    (compile-file "eek.lisp")
    (let* ((tmpdir (merge-pathnames "tmp/" *abcl-test-directory*))
           (subdirs 
            (mapcar (lambda (p) (merge-pathnames p tmpdir))
                    '("a/b/" "d/e+f/")))
           (sub1 (first subdirs))
           (sub2 (second subdirs)))
      (when (probe-directory tmpdir)
        (delete-directory-and-files tmpdir))
      (mapcar (lambda (p) (ensure-directories-exist p)) subdirs)
      (sys:unzip (merge-pathnames "foo.abcl") tmpdir)
      (sys:unzip (merge-pathnames "foo.abcl") sub1)
      (cl-fad-copy-file (merge-pathnames "bar.abcl")
                        (merge-pathnames "bar.abcl" tmpdir))
      (cl-fad-copy-file (merge-pathnames "bar.abcl")
                        (merge-pathnames "bar.abcl" sub1))
      (cl-fad-copy-file (merge-pathnames "bar.abcl")
                        (merge-pathnames "bar.abcl" sub2))
      (cl-fad-copy-file (merge-pathnames "eek.lisp")
                        (merge-pathnames "eek.lisp" tmpdir))
      (cl-fad-copy-file (merge-pathnames "eek.lisp")
                        (merge-pathnames "eek.lisp" sub1))
      (sys:zip (merge-pathnames "baz.jar")
               (loop :for p :in (list tmpdir sub1 sub2)
                  :appending (directory (merge-pathnames "*" p)))
               tmpdir)
      #+nil (delete-directory-and-files dir)))
  (setf *jar-file-init* t))

(defmacro with-jar-file-init (&rest body)
  `(let ((*default-pathname-defaults* *abcl-test-directory*))
     (progn
       (unless *jar-file-init*
         (jar-file-init))
       ,@body)))

(deftest jar-pathname.load.1
    (with-jar-file-init
      (load "jar:file:baz.jar!/foo"))
  t)

(deftest jar-pathname.load.2
    (with-jar-file-init
      (load "jar:file:baz.jar!/bar"))
  t)

(deftest jar-pathname.load.3
    (with-jar-file-init
      (load "jar:file:baz.jar!/bar.abcl"))
  t)

(deftest jar-pathname.load.4
    (with-jar-file-init
      (load "jar:file:baz.jar!/eek"))
  t)

(deftest jar-pathname.load.5
    (with-jar-file-init
      (load "jar:file:baz.jar!/eek.lisp"))
  t)

(deftest jar-pathname.load.6
    (with-jar-file-init
      (load "jar:file:baz.jar!/a/b/foo"))
  t)

(deftest jar-pathname.load.7
    (with-jar-file-init
      (load "jar:file:baz.jar!/a/b/bar"))
  t)

(deftest jar-pathname.load.8
    (with-jar-file-init
      (load "jar:file:baz.jar!/a/b/bar.abcl"))
  t)

(deftest jar-pathname.load.9
    (with-jar-file-init
      (load "jar:file:baz.jar!/a/b/eek"))
  t)

(deftest jar-pathname.load.10
    (with-jar-file-init
      (load "jar:file:baz.jar!/a/b/eek.lisp"))
  t)

(deftest jar-pathname.load.11
    (with-jar-file-init
        (load "jar:file:baz.jar!/d/e+f/bar.abcl"))
  t)

;;; wrapped in PROGN for easy disabling without a network connection
;;; XXX come up with a better abstraction

(defvar *url-jar-pathname-base*
  "jar:http://abcl-dynamic-install.googlecode.com/files/baz-20100505a.jar!/")

(defmacro load-url-relative (path) 
  `(load (format nil "~A~A" *url-jar-pathname-base* ,path)))

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
              (namestring *abcl-test-directory*)))

(deftest jar-pathname.probe-file.2
    (with-jar-file-init
        (probe-file "jar:file:baz.jar!/a/b/bar.abcl"))
  #p#.(format nil "jar:file:~A/baz.jar!/a/b/bar.abcl"
              (namestring *abcl-test-directory*)))

(deftest jar-pathname.probe-file.3
    (with-jar-file-init
        (probe-file "jar:jar:file:baz.jar!/a/b/bar.abcl!/bar._"))
   #p#.(format nil "jar:jar:file:~Abaz.jar!/a/b/bar.abcl!/bar._"
                       (namestring *abcl-test-directory*)))

(deftest jar-pathname.probe-file.4
    (with-jar-file-init
        (probe-file "jar:file:baz.jar!/a/b"))
  #p#.(format nil "jar:file:~Abaz.jar!/a/b/"
                       (namestring *abcl-test-directory*)))

(deftest jar-pathname.probe-file.5
    (with-jar-file-init
        (probe-file "jar:file:baz.jar!/a/b/"))
  #p#.(format nil "jar:file:~Abaz.jar!/a/b/"
                       (namestring *abcl-test-directory*)))

(deftest jar-pathname.probe-file.6
    (with-jar-file-init
        (probe-file "jar:file:baz.jar!/d/e+f/bar.abcl"))
  #p#.(format nil "jar:file:~Abaz.jar!/d/e+f/bar.abcl"
                       (namestring *abcl-test-directory*)))

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

      

        

  
