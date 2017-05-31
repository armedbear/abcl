(in-package :abcl.test.lisp)

;;; When these bugs get fixed, they should be moved elsewhere in the
;;; testsuite so they remain fixed.

(deftest bugs.logical-pathname.1
    #|
Date: Mon, 18 Jan 2010 10:51:07 -0500
Message-ID: <29af5e2d1001180751l7cf79a3ay929cef1deb9ed063@mail.gmail.com>
Subject: Re: [armedbear-devel] translate-logical-pathname and :wild-inferiors 
regression
From: Alan Ruttenberg <alanruttenberg@gmail.com>
    |#
    (progn
      (setf (logical-pathname-translations "ido") 
            '(("IDO:IDO-CORE;**;*.*" 
               "/Users/alanr/repos/infectious-disease-ontology/trunk/src/ontology/ido-core/**/*.*") 
              ("IDO:IMMUNOLOGY;**;*.*"
               "/Users/alanr/repos/infectious-disease-ontology/trunk/src/ontology/immunology/**/*.*") 
              ("IDO:TOOLS;**;*.*" 
               "/Users/alanr/repos/infectious-disease-ontology/trunk/src/tools/**/*.*") 
              ("IDO:LIB;**;*.*"
               "/Users/alanr/repos/infectious-disease-ontology/trunk/lib/**/*.*")))
      (translate-pathname "IDO:IMMUNOLOGY;" "IDO:IMMUNOLOGY;**;*.*" 
                          "/Users/alanr/repos/infectious-disease-ontology/trunk/src/ontology/**/*.*"))
  #P"/users/alanr/repos/infectious-disease-ontology/trunk/src/ontology/")

(deftest bugs.logical-pathname.2
    #|
Message-Id: <BBE9D0E5-5166-4D24-9A8A-DC4E766976D1@ISI.EDU>
From: Thomas Russ <tar@ISI.EDU>
To: armedbear-devel@common-lisp.net
Subject: [armedbear-devel] Bug in translate-logical-pathname.
    |#
    (progn 
      (setf (logical-pathname-translations "L")
            '(("L:NATIVE;**;*.*" "/usr/lisp/abcl/native/**/*.*")))
      (translate-logical-pathname "L:NATIVE;TEST;FOO.FASL"))
  #p"/usr/lisp/abcl/native/test/foo.fasl")

      
(deftest bugs.pathname.1
    (namestring (make-pathname :directory '(:relative) :name "file" 
                   :type :unspecific 
                   :host nil :device nil))
  "./file")

(deftest bugs.pathname.2
    (TRANSLATE-PATHNAME 
     #P"/Users/evenson/work/bordeaux-threads/src/bordeaux-threads.abcl" 
     #P"/**/**/*.*" 
     #P"/Users/evenson/.cache/common-lisp/armedbear-0.20.0-dev-darwin-unknown/**/*.*")
  #P"/Users/evenson/.cache/common-lisp/armedbear-0.20.0-dev-darwin-unknown/bordeaux-threads.abcl")

(deftest bugs.pathname.3  
    (namestring (MAKE-PATHNAME :HOST NIL :DEVICE NIL 
                               :DIRECTORY '(:RELATIVE :WILD-INFERIORS) 
                               :DEFAULTS "/**/"))
  "**/")

#+abcl
(deftest bugs.java.1
    (let* ((a (java:jnew-array "byte" 1))
           (b (let ((array-list (java:jnew (java:jconstructor
                                       "java.util.ArrayList"))))
                (java:jcall (java:jmethod "java.util.AbstractList" "add"
                                          "java.lang.Object")
                            array-list a)
                (java:jcall (java:jmethod "java.util.AbstractList" "get" "int")
                            array-list 0))))
      (type-of (sys::%make-byte-array-input-stream b)))
  stream)
                

(deftest bugs.readtable-case.1 
  (let (original-case result)
    (setf original-case (readtable-case *readtable*)
          (readtable-case *readtable*) :invert
          result (list (string (read-from-string "lower"))
                       (string (read-from-string "UPPER"))
                       (string (read-from-string "#:lower"))
                       (string (read-from-string "#:UPPER")))
          (readtable-case *readtable*) original-case)
    (values-list result))
  "LOWER" "upper" "LOWER" "upper")

;;; http://abcl.org/trac/ticket/165
(deftest bugs.pprint.1
    (let ((result (make-array '(0) :element-type 'base-char :fill-pointer t)))
      (with-output-to-string (s result)
        (pprint-logical-block (s nil :per-line-prefix "---") 
          (format s "~(~A~)" '(1 2 3 4))))
      result)
  "---(1 2 3 4)")

(deftest bugs.defgeneric.1
    (let ((symbol (gensym))
          (docstring "Ipso est genericus")
          result)
      (eval `(defgeneric ,symbol nil
                 (:documentation ,docstring)))
      (setf result (documentation symbol 'function))
      (fmakunbound symbol)
      (string= result docstring))
  t)

;;; http://abcl.org/trac/ticket/199
(deftest bugs.clos.aux.1
    ((lambda (a &aux (b (+ a 1))) 
       b)
     2)
  3)
      
;;; http://abcl.org/trac/ticket/243
(deftest bugs.pathname.make-pathname.1
    (signals-error 
     (make-pathname :device (list "foo"))
     'error)
  t)
;; Dunno about this one.  Maybe we should signal an error when
;; constructed a pathname that we *know* can never refer to any resource.
(push 'bugs.pathname.make-pathname.1 *expected-failures*)


(deftest bugs.pathname.make-pathname.2
  (probe-file (make-pathname :device (list "foo")))
nil)

;; http://abcl.org/trac/ticket/293
(deftest bugs.loop.1
    (loop :with x :of-type (float 0) = 0.0
       :for y :upto 1
       :collecting (cons x y))
  ((0.0 . 0) (0.0 . 1)))

;; http://abcl.org/trac/ticket/444
;; https://trac.clozure.com/ccl/ticket/1085
(deftest bugs.loop.2
    (loop for x = #'(lambda ()) for y = 10 then 20 return y)
  10)

;; http://abcl.org/trac/ticket/294
(deftest bugs.reader.1
    (let ((*readtable* *readtable*))
      (set-macro-character #\? (lambda (stream char) (code-char (read stream nil nil t))))
      '(a .?0))
  (A . #\Null))
      
;;; http://abcl.org/trac/ticket/311
(deftest bugs.export.1
   (let ((a (symbol-name (gensym "PACKAGE-")))
         (b (symbol-name (gensym "PACKAGE-")))
         result)
     (make-package a)
     (intern "FOO" a)
     (export (find-symbol "FOO" a) a)
     (make-package b :use (list a))
     (export (find-symbol "FOO" b) b)
     (unexport (find-symbol "FOO" a) a)
     (setf result (unexport (find-symbol "FOO" b) b))
     (delete-package a)
     (delete-package b)
     result)
  t)

        
