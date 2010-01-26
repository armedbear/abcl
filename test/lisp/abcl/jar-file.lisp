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
    (let* ((dir (merge-pathnames "tmp/" *abcl-test-directory*))
           (sub (merge-pathnames "a/b/" dir)))
      (when (probe-directory dir)
        (delete-directory-and-files dir))
      (ensure-directories-exist sub)
      (sys:unzip (merge-pathnames "foo.abcl")
                 dir)
      (sys:unzip (merge-pathnames "foo.abcl")
                 sub)
      (cl-fad-copy-file (merge-pathnames "bar.abcl")
                 (merge-pathnames "bar.abcl" dir))
      (cl-fad-copy-file (merge-pathnames "bar.abcl")
                 (merge-pathnames "bar.abcl" sub))
      (cl-fad-copy-file (merge-pathnames "eek.lisp")
                 (merge-pathnames "eek.lisp" dir))
      (cl-fad-copy-file (merge-pathnames "eek.lisp")
                 (merge-pathnames "eek.lisp" sub))
      (sys:zip (merge-pathnames "baz.jar")
               (append
                (directory (merge-pathnames "*" dir))
                (directory (merge-pathnames "*" sub)))
               dir)
      (delete-directory-and-files dir)))
  (setf *jar-file-init* t))

(defmacro with-jar-file-init (&rest body)
  `(let ((*default-pathname-defaults* *abcl-test-directory*))
     (progn
       (unless *jar-file-init*
         (jar-file-init))
       ,@body)))

#+nil
(defmacro with-jar-file-init (&rest body)
  `(progv '(*default-pathname-defaults*) '(,*abcl-test-directory*)
    (unless *jar-file-init*
      (load-init))
    ,@body))

(deftest jar-file.load.1
    (with-jar-file-init
      (load "jar:file:baz.jar!/foo"))
  t)

(deftest jar-file.load.2
    (with-jar-file-init
      (load "jar:file:baz.jar!/bar"))
  t)

(deftest jar-file.load.3
    (with-jar-file-init
      (load "jar:file:baz.jar!/bar.abcl"))
  t)

(deftest jar-file.load.4
    (with-jar-file-init
      (load "jar:file:baz.jar!/eek"))
  t)

(deftest jar-file.load.5
    (with-jar-file-init
      (load "jar:file:baz.jar!/eek.lisp"))
  t)

(deftest jar-file.load.6
    (with-jar-file-init
      (load "jar:file:baz.jar!/a/b/foo"))
  t)

(deftest jar-file.load.7
    (with-jar-file-init
      (load "jar:file:baz.jar!/a/b/bar"))
  t)

(deftest jar-file.load.8
    (with-jar-file-init
      (load "jar:file:baz.jar!/a/b/bar.abcl"))
  t)

(deftest jar-file.load.9
    (with-jar-file-init
      (load "jar:file:baz.jar!/a/b/eek"))
  t)

(deftest jar-file.load.10
    (with-jar-file-init
      (load "jar:file:baz.jar!/a/b/eek.lisp"))
  t)

(deftest jar-file.probe-file.1
    (with-jar-file-init
        (probe-file "jar:file:baz.jar!/eek.lisp"))
  #p#.(format nil "jar:file:~A/baz.jar!/eek.lisp" 
              (namestring *abcl-test-directory*)))

(deftest jar-file.probe-file.2
    (with-jar-file-init
        (probe-file "jar:file:baz.jar!/a/b/bar.abcl"))
  #p#.(format nil "jar:file:~A/baz.jar!/a/b/bar.abcl"
              (namestring *abcl-test-directory*)))

(deftest jar-file.probe-file.3
    (with-jar-file-init
        (probe-file "jar:jar:file:baz.jar!/a/b/bar.abcl!/bar._"))
   #p#.(format nil "jar:jar:file:~Abaz.jar!/a/b/bar.abcl!/bar._"
                       (namestring *abcl-test-directory*)))

(deftest jar-file.probe-file.4
    (with-jar-file-init
        (probe-file "jar:file:baz.jar!/a/b"))
  nil)

(deftest jar-file.probe-file.5
    (with-jar-file-init
        (probe-file "jar:file:baz.jar!/a/b/"))
  #p#.(format nil "jar:file:~Abaz.jar!/a/b/"
                       (namestring *abcl-test-directory*)))

(deftest jar-file.merge-pathnames.1
    (merge-pathnames 
     "/bar.abcl" #p"jar:file:baz.jar!/foo")
  #p"jar:file:baz.jar!/bar.abcl")

(deftest jar-file.merge-pathnames.2
    (merge-pathnames 
     "/bar.abcl" #p"jar:file:baz.jar!/foo/")
  #p"jar:file:baz.jar!/foo/bar.abcl")

(deftest jar-file.merge-pathnames.3
    (merge-pathnames 
     "jar:file:baz.jar!/foo" "bar")
  #p"jar:file:baz.jar!/foo")

(deftest jar-file.truename.1
    (signals-error (truename "jar:file:baz.jar!/foo")
                   'file-error)
  t)


(deftest jar-file.pathname.1
    (let* ((p #p"jar:file:foo/baz.jar!/")
           (d (first (pathname-device p))))
      (values
       (pathname-directory d) (pathname-name d) (pathname-type d)))
  (:relative "foo") "baz" "jar")

(deftest jar-file.pathname.2
    (let* ((p #p"jar:file:baz.jar!/foo.abcl")
           (d (first (pathname-device p))))
      (values
       (pathname-name d) (pathname-type d) 
       (pathname-directory p) (pathname-name p) (pathname-type p)))
  "baz" "jar"
   nil "foo" "abcl")
   
(deftest jar-file.pathname.3
    (let* ((p #p"jar:jar:file:baz.jar!/foo.abcl!/")
           (d0 (first (pathname-device p)))
           (d1 (second (pathname-device p))))
      (values 
       (pathname-name d0) (pathname-type d0)
       (pathname-name d1) (pathname-type d1)))
  "baz" "jar"
  "foo" "abcl")

(deftest jar-file.pathname.4
    (let* ((p #p"jar:jar:file:a/baz.jar!/b/c/foo.abcl!/this/that/foo-20.cls")
           (d0 (first (pathname-device p)))
           (d1 (second (pathname-device p))))
      (values 
       (pathname-directory d0) (pathname-name d0) (pathname-type d0)
       (pathname-directory d1) (pathname-name d1) (pathname-type d1)
       (pathname-directory p) (pathname-name p) (pathname-type p)))
  (:relative "a") "baz" "jar"
  (:relative "b" "c") "foo" "abcl"
  (:relative "this" "that") "foo-20" "cls")

(deftest jar-file.pathname.5
    (let* ((p #p"jar:jar:file:a/foo/baz.jar!/b/c/foo.abcl!/armed/bear/bar-1.cls")
           (d0 (first (pathname-device p)))
           (d1 (second (pathname-device p))))
      (values 
       (pathname-directory d0) (pathname-name d0) (pathname-type d0)
       (pathname-directory d1) (pathname-name d1) (pathname-type d1)
       (pathname-directory p) (pathname-name p) (pathname-type p)))
  (:relative "a" "foo" ) "baz" "jar"
  (:relative "b" "c") "foo" "abcl"
  (:relative "armed" "bear") "bar-1" "cls")

(deftest jar-file.pathname.6
    (let* ((p #p"jar:http://example.org/abcl.jar!/org/armedbear/lisp/Version.class")
           (d (first (pathname-device p))))

      (values 
       d
       (pathname-directory p) (pathname-name p) (pathname-type p)))
  "http://example.org/abcl.jar" 
  (:relative "org" "armedbear" "lisp") "Version" "class")

(deftest jar-file.pathname.7
    (let* ((p #p"jar:jar:http://example.org/abcl.jar!/foo.abcl!/foo-1.cls")
           (d (pathname-device p))
           (d0 (first d))
           (d1 (second d)))
      (values
       d0 
       (pathname-name d1) (pathname-type d1)
       (pathname-name p) (pathname-type p)))
  "http://example.org/abcl.jar"
  "foo" "abcl"
  "foo-1" "cls")

(deftest jar-file.pathname.8
    (let* ((p #p"jar:file:/a/b/foo.jar!/")
           (d (first (pathname-device p))))
      (values
       (pathname-directory d) (pathname-name d) (pathname-type d)))
  (:ABSOLUTE "a" "b") "foo" "jar")

(deftest jar-file.pathname.9
    (let* ((p #p"jar:file:a/b/foo.jar!/c/d/foo.lisp")
           (d (first (pathname-device p))))
      (values
       (pathname-directory d) (pathname-name d) (pathname-type d)
       (pathname-directory p) (pathname-name p) (pathname-type p)))
  (:RELATIVE "a" "b") "foo" "jar"
  (:RELATIVE "c" "d") "foo" "lisp")

      
      
             

       
        

  
