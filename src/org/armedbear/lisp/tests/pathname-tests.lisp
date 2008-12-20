;;; pathname-tests.lisp
;;;
;;; Copyright (C) 2005 Peter Graves
;;; $Id$
;;;
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License
;;; as published by the Free Software Foundation; either version 2
;;; of the License, or (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

(load (merge-pathnames "test-utilities.lisp" *load-truename*))

(in-package #:test)

(defun check-physical-pathname (pathname expected-directory expected-name expected-type)
  (let* ((directory (pathname-directory pathname))
         (name (pathname-name pathname))
         (type (pathname-type pathname))
         (ok t))
    (unless (and (pathnamep pathname)
                 (not (typep pathname 'logical-pathname)))
      (setf ok nil))
    (unless (and (equal directory expected-directory)
                 (equal name expected-name)
                 (equal type expected-type))
      (setf ok nil))
    ok))

(defun check-windows-pathname (pathname expected-host expected-device
                                        expected-directory expected-name
                                        expected-type)
  (let* ((host (pathname-host pathname))
         (device (pathname-device pathname))
         (directory (pathname-directory pathname))
         (name (pathname-name pathname))
         (type (pathname-type pathname))
         (ok t))
    (unless (and (pathnamep pathname)
                 (not (typep pathname 'logical-pathname)))
      (setf ok nil))
    (unless (and (equal host expected-host)
                 (equal device expected-device)
                 (equal directory expected-directory)
                 (equal name expected-name)
                 (equal type expected-type))
      (setf ok nil))
    ok))

(defun check-logical-pathname (pathname expected-host expected-directory
                                        expected-name expected-type
                                        expected-version)
  (let* ((host (pathname-host pathname))
         (directory (pathname-directory pathname))
         (name (pathname-name pathname))
         (type (pathname-type pathname))
         (version (pathname-version pathname))
         ;; Allegro's logical pathnames don't canonicalize their string
         ;; components to upper case.
         (test #-allegro 'equal
               #+allegro 'equalp)
         (ok t))
    (unless (typep pathname 'logical-pathname)
      (setf ok nil))
    ;; "The device component of a logical pathname is always :UNSPECIFIC..." 19.3.2.1
    #-allegro ;; Except on Allegro, where it's NIL.
    (unless (eq (pathname-device pathname) :unspecific)
      (setf ok nil))
    (unless (and (funcall test (if (stringp host) host
                                    (host-namestring pathname))
                          expected-host)
                 (funcall test directory expected-directory)
                 (funcall test name expected-name)
                 (funcall test type expected-type)
                 (eql version expected-version))
      (setf ok nil))
    ok))

(defun check-merge-pathnames (pathname default-pathname expected-result)
  (let* ((result (merge-pathnames pathname default-pathname))
         (test #-allegro 'equal
               #+allegro (if (typep result 'logical-pathname)
                             'equalp
                             'equal)))
    (and (funcall test (pathname-host result) (pathname-host expected-result))
         (funcall test (pathname-directory result) (pathname-directory expected-result))
         (funcall test (pathname-name result) (pathname-name expected-result))
         (funcall test (pathname-type result) (pathname-type expected-result)))))

(defun check-translate-pathname (args expected)
  (declare (optimize safety))
  (declare (type list args))
  (declare (type string expected))
  (let ((result (namestring (apply 'translate-pathname args))))
    (equal result
           #-windows expected
           #+windows (substitute #\\ #\/ expected))))

(defmacro check-readable (pathname)
  `(equal ,pathname (read-from-string (write-to-string ,pathname :readably t))))

(defun check-readable-or-signals-error (pathname)
  (handler-case
      (equal pathname (read-from-string (write-to-string pathname :readably t)))
    (print-not-readable () t)))

(defmacro check-namestring (pathname namestring)
  `(string= (namestring ,pathname)
            #+windows (substitute #\\ #\/ ,namestring)
            #-windows ,namestring))

;; Define a logical host.
(setf (logical-pathname-translations "effluvia")
      '(("**;*.*.*" "/usr/local/**/*.*")))

(deftest equal.1
  (equal (make-pathname :name "foo" :type "bar")
         (make-pathname :name "foo" :type "bar"))
  t)

(deftest equal.2
  (equal (make-pathname :name "foo" :type "bar" :version nil)
         (make-pathname :name "foo" :type "bar" :version :newest))
  #+(or clisp lispworks) nil
  #-(or clisp lispworks) t)

(deftest sxhash.1
  (let* ((p (make-pathname :name "foo" :type "bar" :version nil))
         (s (sxhash p)))
    (values (typep s 'fixnum)
            (>= s 0)))
  t t)

;; "(equal x y) implies (= (sxhash x) (sxhash y))"
(deftest sxhash.2
  (let ((p1 (make-pathname :name "foo" :type "bar" :version nil))
        (p2 (make-pathname :name "foo" :type "bar" :version :newest)))
    (if (equal p1 p2)
        (= (sxhash p1) (sxhash p2))
        t))
  t)

;; It's suboptimal if all pathnames return the same SXHASH, but that happens
;; with SBCL.
(deftest sxhash.3
  (= (sxhash #p"/usr/local/bin/sbcl") (sxhash #p"") (sxhash #p"foo.bar"))
  #+sbcl t
  #-sbcl nil)

;; "Parsing a null string always succeeds, producing a pathname with all
;; components (except the host) equal to nil."
(deftest physical.1
  (check-physical-pathname #p"" nil nil nil)
  t)

(deftest physical.2
  (check-physical-pathname #p"/" '(:absolute) nil nil)
  t)

(deftest physical.3
  (check-physical-pathname #p"/foo" '(:absolute) "foo" nil)
  t)

(deftest physical.4
  #-lispworks
  (check-physical-pathname #p"/foo." '(:absolute) "foo" "")
  #+lispworks
  (check-physical-pathname #p"/foo." '(:absolute) "foo." nil)
  t)

(deftest physical.5
  (check-physical-pathname #p"/foo.bar" '(:absolute) "foo" "bar")
  t)

(deftest physical.6
  #-lispworks
  (check-physical-pathname #p"/foo.bar." '(:absolute) "foo.bar" "")
  #+lispworks
  (check-physical-pathname #p"/foo.bar." '(:absolute) "foo.bar." nil)
  t)

(deftest physical.7
  (check-physical-pathname #p"/foo.bar.baz" '(:absolute) "foo.bar" "baz")
  t)

(deftest physical.8
  (check-physical-pathname #p"/foo/bar" '(:absolute "foo") "bar" nil)
  t)

(deftest physical.9
  (check-physical-pathname #p"/foo..bar" '(:absolute) "foo." "bar")
  t)

(deftest physical.10
  (check-physical-pathname #p"foo.bar" nil "foo" "bar")
  t)

(deftest physical.11
  (check-physical-pathname #p"foo.bar.baz" nil "foo.bar" "baz")
  t)

(deftest physical.12
  (check-physical-pathname #p"foo/" '(:relative "foo") nil nil)
  t)

(deftest physical.13
  (check-physical-pathname #p"foo/bar" '(:relative "foo") "bar" nil)
  t)

(deftest physical.14
  (check-physical-pathname #p"foo/bar/baz" '(:relative "foo" "bar") "baz" nil)
  t)

(deftest physical.15
  (check-physical-pathname #p"foo/bar/" '(:relative "foo" "bar") nil nil)
  t)

#+allegro
(deftest physical.16
  ;; This reduction is wrong.
  (check-physical-pathname #p"foo/bar/.." '(:relative "foo") nil nil)
  t)

#+allegro
(deftest physical.17
  (check-physical-pathname #p"/foo/../" '(:absolute) nil nil)
  t)

(deftest physical.18
  #-lispworks
  (check-physical-pathname #p".lisprc" nil ".lisprc" nil)
  #+lispworks
  (check-physical-pathname #p".lisprc" nil "" "lisprc")
  t)

(deftest physical.19
  (check-physical-pathname #p"x.lisprc" nil "x" "lisprc")
  t)

(deftest physical.20
  #-allegro
  (check-physical-pathname (make-pathname :name ".") nil "." nil)
  #+allegro
  (check-physical-pathname (make-pathname :name ".") '(:relative) nil nil)
  t)

(deftest physical.21
  #-cmu
  (check-readable (make-pathname :name "."))
  #+cmu
  (check-readable-or-signals-error (make-pathname :name "."))
  t)
#+(or cmu lispworks (and allegro windows))
(pushnew 'physical.21 *expected-failures*)

;; #p"."
(deftest physical.22
  #+(or allegro abcl cmu)
  (check-physical-pathname #p"." '(:relative) nil nil)
  #-(or allegro abcl cmu)
  ;; No trailing separator character means it's a file.
  (check-physical-pathname #p"." nil "." nil)
  t)
#+lispworks
(pushnew 'physical.22 *expected-failures*)

(deftest namestring.1
  (check-namestring #p"."
                    #+(or abcl allegro cmu) "./"
                    #-(or abcl allegro cmu) ".")
  t)
#+lispworks
(pushnew 'namestring.1 *expected-failures*)

(deftest physical.23
  (equal #p"." #p"")
  nil)
#+lispworks
(pushnew 'physical.23 *expected-failures*)

;; #p"./"
;; Trailing separator character means it's a directory.
(deftest physical.24
  (let ((pathname #-windows #p"./"
                  #+windows #p".\\"))
    #-(or sbcl)
    (check-physical-pathname pathname '(:relative) nil nil)
    #+(or sbcl)
    ;; Is this more exact?
    (check-physical-pathname pathname '(:relative ".") nil nil))
  t)
#+(or lispworks (and allegro windows))
(pushnew 'physical.24 *expected-failures*)

(deftest namestring.2
  (check-namestring #-windows #p"./"
                    #+windows #p".\\"
                    "./")
  t)
#+lispworks
(pushnew 'namestring.2 *expected-failures*)

(deftest directory-namestring.1
  (equal (directory-namestring #-windows #p"./"
                               #+windows #p".\\")
         #-windows "./"
         #+windows ".\\")
  t)
#+lispworks
(pushnew 'directory-namestring.1 *expected-failures*)

(deftest physical.25
  (equal #-windows #p"./"
         #+windows #p".\\"
         #p"")
  nil)
#+(or lispworks (and allegro windows))
(pushnew 'physical.25 *expected-failures*)

(deftest physical.26
  #-allegro
  (check-physical-pathname (make-pathname :name "..") nil ".." nil)
  #+allegro
  (check-physical-pathname (make-pathname :name "..") '(:relative :back) nil nil)
  t)

#-(or sbcl)
(deftest physical.27
  #-cmu
  (check-readable (make-pathname :name ".."))
  #+cmu
  (check-readable-or-signals-error (make-pathname :name ".."))
  t)
#+(or clisp cmu lispworks)
(pushnew 'physical.27 *expected-failures*)

;; #p".."
(deftest physical.28
  #+(or allegro (and lispworks windows))
  (check-physical-pathname #p".." '(:relative :back) nil nil)
  #+(or abcl cmu (and lispworks unix))
  (check-physical-pathname #p".." '(:relative :up) nil nil)
  ;; Other implementations think it's a file.
  #+(or)
  ;; If it's a file, to a human its name would be "..". No implementation gets
  ;; this right.
  (check-physical-pathname #p".." nil ".." nil)
  #+(or sbcl clisp)
  ;; These implementations parse ".." as the name "." followed by another dot and
  ;; the type string "", which no human would do.
  (check-physical-pathname #p".." nil "." "")
  t)
#+cmu
(pushnew 'physical.28 *expected-failures*)

(deftest namestring.3
  (check-namestring #p".."
                    #+(or abcl allegro cmu lispworks) "../"
                    #-(or abcl allegro cmu lispworks) "..")
  t)

;; #p"../"
(deftest physical.29
  (let ((pathname #-windows #p"../"
                  #+windows #p"..\\"))
    #+(or allegro (and lispworks windows))
    (check-physical-pathname pathname '(:relative :back) nil nil)
    #+(or abcl sbcl cmu clisp (and lispworks unix))
    (check-physical-pathname pathname '(:relative :up) nil nil))
  t)

(deftest namestring.4
  (check-namestring #-windows #p"../"
                    #+windows #p"..\\"
                    "../")
  t)

(deftest directory-namestring.2
  (equal (directory-namestring #-windows #p"../"
                               #+windows #p"..\\")
         #-windows "../"
         #+windows "..\\")
  t)

#-sbcl
(deftest physical.30
  #-(or allegro cmu)
  (string= (namestring (make-pathname :name "..")) "..")
  #+allegro
  (string= (namestring (make-pathname :name ".."))
           #-windows "../"
           #+windows "..\\")
  #+cmu
  (signals-error (make-pathname :name "..") 'warning)
  t)

(deftest physical.31
  (string= (namestring (make-pathname :directory '(:relative :up)))
           #+windows "..\\"
           #-windows "../")
  t)

#+windows
(deftest windows.1
  (equal #p"/foo/bar/baz" #p"\\foo\\bar\\baz")
  t)

#+windows
(deftest windows.2
  (let ((pathname #p"foo.bar"))
    (check-windows-pathname pathname nil nil nil "foo" "bar"))
  t)

#+windows
(deftest windows.3
  (let ((pathname #p"\\foo.bar"))
    (check-windows-pathname pathname nil nil '(:absolute) "foo" "bar"))
  t)

#+windows
(deftest windows.4
  (let ((pathname #p"c:\\foo.bar"))
    #+(or abcl allegro)
    (check-windows-pathname pathname nil "c" '(:absolute) "foo" "bar")
    #+clisp
    (check-windows-pathname pathname nil "C" '(:absolute) "foo" "bar")
    #+lispworks
    (check-windows-pathname pathname "c" nil '(:absolute) "foo" "bar"))
  t)

#+windows
(deftest windows.5
  (equal #p"c:\\foo.bar" #p"C:\\FOO.BAR")
  t)

(deftest wild.1
  (check-physical-pathname #p"foo.*" nil "foo" :wild)
  t)

(deftest wild.2
  (check-physical-pathname #p"*.*" nil :wild :wild)
  t)

(deftest wild.3
  #-(or cmu sbcl)
  (check-physical-pathname #p"abc*" nil "abc*" nil)
  #+(or cmu sbcl)
  (wild-pathname-p #p"abc*")
  t)

(deftest wild.4
  #-(or cmu sbcl)
  (check-physical-pathname #p"abc?" nil "abc?" nil)
  #+(or cmu sbcl)
  (wild-pathname-p #p"abc?")
  t)

(deftest wild.5
  #-(or cmu sbcl)
  (check-physical-pathname #p"abc[d-h]" nil "abc[d-h]" nil)
  #+(or cmu sbcl)
  (wild-pathname-p #p"abc[d-h]")
  t)

;; Lots of dots.
#+(or allegro abcl cmu)
(deftest lots-of-dots.1
  (check-physical-pathname #p"..." nil "..." nil)
  t)
#+cmu
(pushnew 'lots-of-dots.1 *expected-failures*)

#+(or allegro abcl cmu)
(deftest lots-of-dots.2
  (check-physical-pathname #p"......" nil "......" nil)
  t)
#+cmu
(pushnew 'lots-of-dots.2 *expected-failures*)

;; Silly names.
#-(or allegro sbcl)
(deftest silly.1
  #+(or abcl clisp)
  (signals-error (make-pathname :name "abc/def") 'error)
  #-(or abcl clisp)
  (check-readable (make-pathname :name "abc/def"))
  t)
#+(or cmu lispworks)
(pushnew 'silly.1 *expected-failures*)

(deftest silly.2
  (signals-error (make-pathname :name "abc/def")
                 #-cmu 'error
                 #+cmu 'warning)
  t)

(deftest silly.3
  (check-readable-or-signals-error (make-pathname :name ".foo"))
  t)

(deftest silly.4
  (check-readable-or-signals-error (make-pathname :type ".foo"))
  t)

(deftest silly.5
  (check-readable-or-signals-error (make-pathname :name "abc.def"))
  t)

(deftest silly.6
  (check-readable-or-signals-error (make-pathname :type "abc.def"))
  t)

;; LOGICAL-PATHNAME-TRANSLATIONS
#-allegro
(deftest logical-pathname-translations.1
  #+(or sbcl cmu lispworks)
  (equal (logical-pathname-translations "effluvia")
         '(("**;*.*.*" "/usr/local/**/*.*")))
  #+clisp
  (equal (logical-pathname-translations "effluvia")
         '((#p"EFFLUVIA:**;*.*.*" "/usr/local/**/*.*")))
  #+abcl
  (equal (logical-pathname-translations "effluvia")
         '((#p"EFFLUVIA:**;*.*.*" #p"/usr/local/**/*.*")))
  t)

;; "The null string, "", is not a valid value for any component of a logical
;; pathname." 19.3.2.2
(deftest logical-pathname.1
  #-clisp
  (signals-error (logical-pathname ":") 'error)
  #+clisp
  (check-logical-pathname (logical-pathname ":") "" '(:absolute) nil nil nil)
  t)

;; Parse error.
(deftest logical-pathname.2
  (signals-error (logical-pathname "effluvia::foo.bar")
                 #-(or allegro clisp) 'parse-error
                 #+(or allegro clisp) 'type-error)
  t)

;; If the prefix isn't a defined logical host, it's not a logical pathname.
#-(or cmu (and clisp windows))
;; CMUCL parses this as (:ABSOLUTE #<SEARCH-LIST foo>) "bar.baz" "42".
;; CLISP signals a parse error reading #p"foo:bar.baz.42".
(deftest logical.1
  (let ((pathname #p"foo:bar.baz.42"))
    #+allegro
    ;; Except in Allegro.
    (check-logical-pathname pathname "foo" nil "bar" "baz" nil)
    #-allegro
    (check-physical-pathname pathname nil "foo:bar.baz" "42"))
  t)
#+lispworks
(pushnew 'logical.1 *expected-failures*)

#+sbcl
(deftest logical.2
  ;; Even though "effluvia" is defined as a logical host, "bop" is not a valid
  ;; logical pathname version, so this can't be a logical pathname.
  (check-physical-pathname #p"effluvia:bar.baz.bop" nil "effluvia:bar.baz" "bop")
  t)

(deftest logical.3
  #-allegro
  (check-logical-pathname (make-pathname :defaults "effluvia:foo.lisp")
                          "EFFLUVIA" '(:absolute) "FOO" "LISP" nil)
  #+allegro
  (check-logical-pathname (make-pathname :defaults "effluvia:foo.lisp")
                          "effluvia" nil "foo" "lisp" nil)
  t)

#-allegro
(deftest logical.4
  (check-logical-pathname #p"effluvia:bar.baz.42" "EFFLUVIA" '(:absolute) "BAR" "BAZ" 42)
  t)

#-allegro
(deftest logical.5
  (string= (write-to-string #p"effluvia:bar.baz.42" :escape t)
           "#P\"EFFLUVIA:BAR.BAZ.42\"")
  t)

#+allegro
;; Allegro returns NIL for the device and directory and drops the version
;; entirely (even from the namestring).
(deftest logical.6
  (check-logical-pathname #p"effluvia:bar.baz.42" "effluvia" nil "bar" "baz" nil)
  t)

#+allegro
(deftest logical.7
  (string= (write-to-string #p"effluvia:bar.baz" :escape t)
           #+allegro-v6.2 "#p\"effluvia:bar.baz\""
           #+allegro-v7.0 "#P\"effluvia:bar.baz\"")
  t)

(deftest logical.8
  (typep (parse-namestring "**;*.*.*" "effluvia") 'logical-pathname)
  t)

(deftest logical.9
  (check-namestring (parse-namestring "**;*.*.*" "effluvia")
                    #-(or allegro lispworks)
                    "EFFLUVIA:**;*.*.*"
                    #+allegro
                    ;; Allegro preserves case and drops the version component.
                    "effluvia:**;*.*"
                    #+lispworks
                    "effluvia:**;*.*.*")
  t)

#-allegro
;; The version can be a bignum.
(deftest logical.10
  (check-logical-pathname #p"effluvia:bar.baz.2147483648" "EFFLUVIA" '(:absolute) "BAR" "BAZ" 2147483648)
  t)

#-allegro
(deftest logical.11
  (check-namestring #p"effluvia:bar.baz.2147483648" "EFFLUVIA:BAR.BAZ.2147483648")
  t)
#+sbcl
;; SBCL has a bug when the version is a bignum.
(pushnew 'logical.11 *expected-failures*)

(deftest logical.12
  (check-namestring #p"effluvia:foo.bar.newest"
                    #-allegro "EFFLUVIA:FOO.BAR.NEWEST"
                    #+allegro "effluvia:foo.bar")
  t)

(deftest logical.13
  #-allegro
  (check-logical-pathname #p"effluvia:foo.*" "EFFLUVIA" '(:absolute) "FOO" :wild nil)
  #+allegro
  (check-logical-pathname #p"effluvia:foo.*" "effluvia" nil "foo" :wild nil)
  t)

(deftest logical.14
  #-allegro
  (check-logical-pathname #p"effluvia:*.lisp" "EFFLUVIA" '(:absolute) :wild "LISP" nil)
  #+allegro
  (check-logical-pathname #p"effluvia:*.lisp" "effluvia" nil :wild "lisp" nil)
  t)

(deftest logical.15
  #-allegro
  (check-logical-pathname #p"effluvia:bar.baz.newest" "EFFLUVIA" '(:absolute) "BAR" "BAZ" :newest)
  #+allegro
  (check-logical-pathname #p"effluvia:bar.baz.newest" "effluvia" nil "bar" "baz" nil)
  t)

(deftest logical.16
  #-allegro
  (check-logical-pathname #p"EFFLUVIA:BAR.BAZ.NEWEST" "EFFLUVIA" '(:absolute) "BAR" "BAZ" :newest)
  #+allegro
  (check-logical-pathname #p"EFFLUVIA:BAR.BAZ.NEWEST" "EFFLUVIA" nil "BAR" "BAZ" nil)
  t)

;; The directory component.
(deftest logical.17
  (check-logical-pathname #p"effluvia:foo;bar.baz" "EFFLUVIA" '(:absolute "FOO") "BAR" "BAZ" nil)
  t)

(deftest logical.18
  (check-namestring #p"effluvia:foo;bar.baz"
                    #-allegro "EFFLUVIA:FOO;BAR.BAZ"
                    #+allegro "effluvia:foo;bar.baz")
  t)

(deftest logical.19
  #-allegro
  (check-logical-pathname #p"effluvia:;bar.baz" "EFFLUVIA" '(:relative) "BAR" "BAZ" nil)
  #+allegro
  ;; Allegro drops the directory component and removes the semicolon from the
  ;; namestring.
  (check-logical-pathname #p"effluvia:;bar.baz" "EFFLUVIA" nil "BAR" "BAZ" nil)
  t)

(deftest logical.20
  (check-namestring #p"effluvia:;bar.baz"
                    #+allegro "effluvia:bar.baz"
                    #-allegro "EFFLUVIA:;BAR.BAZ")
  t)

;; "If a relative-directory-marker precedes the directories, the directory
;; component parsed is as relative; otherwise, the directory component is
;; parsed as absolute."
(deftest logical.21
  (equal (pathname-directory #p"effluvia:foo.baz")
         #-allegro '(:absolute)
         #+allegro nil)
  t)

(deftest logical.22
  (typep  #p"effluvia:" 'logical-pathname)
  t)

(deftest logical.23
  (equal (pathname-directory #p"effluvia:")
         #-allegro '(:absolute)
         #+allegro nil)
  t)

;; PARSE-NAMESTRING
(deftest parse-namestring.1
  #-allegro
  (check-logical-pathname (parse-namestring "effluvia:foo.bar")
                          "EFFLUVIA" '(:absolute) "FOO" "BAR" nil)
  #+allegro
  (check-logical-pathname (parse-namestring "effluvia:foo.bar")
                          "effluvia" nil "foo" "bar" nil)
  t)

(deftest parse-namestring.2
  (let ((pathname (parse-namestring "foo.bar" "effluvia")))
    #-(or allegro lispworks)
    (check-logical-pathname pathname "EFFLUVIA" '(:absolute) "FOO" "BAR" nil)
    #+allegro
    (check-logical-pathname pathname "effluvia" nil "foo" "bar" nil)
    #+lispworks
    (check-logical-pathname pathname "effluvia" '(:absolute) "FOO" "BAR" nil))
  t)

(deftest parse-namestring.3
  (let ((pathname (parse-namestring "foo;bar;baz.fas.3" "effluvia")))
    #-(or allegro lispworks)
    (check-logical-pathname pathname "EFFLUVIA" '(:absolute "FOO" "BAR") "BAZ" "FAS" 3)
    #+allegro
    (check-logical-pathname pathname "effluvia" '(:absolute "foo" "bar") "baz" "fas" nil)
    #+lispworks
    (check-logical-pathname pathname "effluvia" '(:absolute "FOO" "BAR") "BAZ" "FAS" 3)
    )
  t)

(deftest parse-namestring.4
  #-(or abcl clisp cmu lispworks (and allegro windows))
  (check-physical-pathname (parse-namestring "effluvia:foo.bar" "")
                           nil "effluvia:foo" "bar")
  #+abcl
  ;; Invalid logical host name: ""
  (signals-error (parse-namestring "effluvia:foo.bar" "") 'error)
  #+(or clisp lispworks)
  ;; Host mismatch.
  (signals-error (parse-namestring "effluvia:foo.bar" "") 'error)
  #+cmu
  (signals-error (parse-namestring "effluvia:foo.bar" "") 'error)
  #+(and allegro windows)
  ;; "effluvia" is the device
  (check-physical-pathname (parse-namestring "effluvia:foo.bar" "")
                           nil "foo" "bar")
  t)

;; "If host is nil and thing is a syntactically valid logical pathname
;; namestring containing an explicit host, then it is parsed as a logical
;; pathname namestring."
(deftest parse-namestring.5
  #-allegro
  (check-logical-pathname (parse-namestring "effluvia:foo.bar" nil)
                          "EFFLUVIA" '(:absolute) "FOO" "BAR" nil)
  #+allegro
  (check-logical-pathname (parse-namestring "effluvia:foo.bar" nil)
                          "effluvia" nil "foo" "bar" nil)
  t)

;; "If host is nil, default-pathname is a logical pathname, and thing is a
;; syntactically valid logical pathname namestring without an explicit host,
;; then it is parsed as a logical pathname namestring on the host that is the
;; host component of default-pathname."
(deftest parse-namestring.6
  #-allegro
  (check-logical-pathname (parse-namestring "foo" nil #p"effluvia:bar")
                          "EFFLUVIA" '(:absolute) "FOO" nil nil)
  #+allegro
  (check-logical-pathname (parse-namestring "foo" nil #p"effluvia:bar")
                          "effluvia" nil "foo" nil nil)
  t)

(deftest parse-namestring.7
  (let* ((*default-pathname-defaults* (logical-pathname "EFFLUVIA:"))
         (pathname (parse-namestring "foo.bar")))
    #-allegro
    (check-logical-pathname pathname "EFFLUVIA" '(:absolute) "FOO" "BAR" nil)
    #+allegro
    (check-logical-pathname pathname "effluvia" nil "foo" "bar" nil))
  t)

(deftest parse-namestring.8
  (let* ((*default-pathname-defaults* #p"effluvia:bar")
         (pathname (parse-namestring "foo" nil)))
    #-allegro
    (check-logical-pathname pathname "EFFLUVIA" '(:absolute) "FOO" nil nil)
    #+allegro
    (check-logical-pathname pathname "effluvia" nil "foo" nil nil))
  t)

;; WILD-PATHNAME-P
(deftest wild-pathname-p.1
  (wild-pathname-p #p"effluvia:;*.baz")
  #+(or cmu sbcl) (:wild :wild-inferiors)
  #-(or cmu sbcl) t)

;; PATHNAME-MATCH-P
(deftest pathname-match-p.1
  (pathname-match-p "/foo/bar/baz" "/*/*/baz")
  t)

(deftest pathname-match-p.2
  (pathname-match-p "/foo/bar/baz" "/**/baz")
  t)

(deftest pathname-match-p.3
  (pathname-match-p "/foo/bar/quux/baz" "/**/baz")
  t)

(deftest pathname-match-p.4
  (pathname-match-p "foo.bar" "/**/*.*")
  t)

(deftest pathname-match-p.5
  (pathname-match-p "/usr/local/bin/foo.bar" "/**/foo.bar")
  t)

(deftest pathname-match-p.6
  (pathname-match-p "/usr/local/bin/foo.bar" "**/foo.bar")
  nil)

(deftest pathname-match-p.7
  (pathname-match-p "/foo/bar.txt" "/**/*.*")
  t)

(deftest pathname-match-p.8
  (pathname-match-p "/foo/bar.txt" "**/*.*")
  nil)

(deftest pathname-match-p.9
  (pathname-match-p #p"effluvia:foo.bar" #p"effluvia:**;*.*.*")
  t)

(deftest pathname-match-p.10
  (pathname-match-p "foo" "foo.*")
  t)

;; TRANSLATE-PATHNAME
(deftest translate-pathname.1
  #-clisp
  (equal (translate-pathname "foo" "*" "bar") #p"bar")
  #+clisp
  (signals-error (translate-pathname "foo" "*" "bar") 'error)
  t)

(deftest translate-pathname.2
  (equal (translate-pathname "foo" "*" "*")   #p"foo")
  t)

(deftest translate-pathname.3
  #-abcl
  (string= (pathname-name (translate-pathname "foobar" "*" "foo*"))
           #-allegro-v7.0 "foofoobar"
           #+allegro-v7.0 "foo*")
  #+abcl
  ;; ABCL doesn't implement this translation. Verify that it signals an error.
  (signals-error (translate-pathname "foobar" "*" "foo*") 'error)
  t)

(deftest translate-pathname.4
  #-abcl
  (equal (translate-pathname "foobar" "foo*" "*baz")
         #-allegro-v7.0 #p"barbaz"
         #+allegro-v7.0 #p"*baz")
  #+abcl
  ;; ABCL doesn't implement this translation. Verify that it signals an error.
  (signals-error (translate-pathname "foobar" "foo*" "*baz") 'error)
  t)

(deftest translate-pathname.5
  #-abcl
  (equal (translate-pathname "foobar" "foo*" "")
         #+(or allegro clisp) #p"bar"
         #+(or cmu sbcl lispworks) #p"foobar")
  #+abcl
  ;; ABCL doesn't implement this translation. Verify that it signals an error.
  (signals-error (translate-pathname "foobar" "foo*" "") 'error)
  t)

(deftest translate-pathname.6
  (equal (translate-pathname "foo/bar" "*/bar" "*/baz") #p"foo/baz")
  t)

(deftest translate-pathname.7
  (equal (translate-pathname "bar/foo" "bar/*" "baz/*") #p"baz/foo")
  t)

(deftest translate-pathname.8
  (equal (translate-pathname "foo/bar" "*/bar" "*/baz") #p"foo/baz")
  t)

(deftest translate-pathname.9
  (string= (namestring (translate-pathname "test.txt" "*.txt" "*.text"))
           "test.text")
  t)

(deftest translate-pathname.10
  (equal (translate-pathname "foo" "foo.*" "bar") #p"bar")
  t)

(deftest translate-pathname.11
  (equal (translate-pathname "foo" "foo.*" "bar.*") #p"bar")
  t)

(deftest translate-pathname.12
  (string= (namestring (translate-pathname "foo.bar" "*.*" "/usr/local/*.*"))
           #-windows "/usr/local/foo.bar"
           #+windows "\\usr\\local\\foo.bar")
  t)

(deftest translate-pathname.13
  (equal (translate-pathname "foo.bar" "*.*" "/usr/local/*.*")
         #p"/usr/local/foo.bar")
  t)

(deftest translate-pathname.14
  (check-translate-pathname '("/foo/" "/*/" "/usr/local/*/") "/usr/local/foo/")
  t)

(deftest translate-pathname.15
  (check-translate-pathname '("/foo/baz/bar.txt" "/**/*.*" "/usr/local/**/*.*")
                            "/usr/local/foo/baz/bar.txt")
  t)

(deftest translate-pathname.16
  (equal (translate-pathname "/foo/" "/*/" "/usr/local/*/bar/") #p"/usr/local/foo/bar/")
  t)

(deftest translate-pathname.17
  (equal (translate-pathname "/foo/bar.txt" "/*/*.*" "/usr/local/*/*.*")
         #P"/usr/local/foo/bar.txt")
  t)

;; "TRANSLATE-PATHNAME translates SOURCE (that matches FROM-WILDCARD)..."
(deftest pathname-match-p.11
  (pathname-match-p "/foo/bar.txt" "**/*.*")
  nil)

;; Since (pathname-match-p "/foo/bar.txt" "**/*.*" ) => NIL...
(deftest translate-pathname.18
  #+(or clisp allegro abcl cmu lispworks)
  ;; This seems to be the correct behavior.
  (signals-error (translate-pathname "/foo/bar.txt" "**/*.*" "/usr/local/**/*.*") 'error)
  #+sbcl
  ;; This appears to be a bug, since SOURCE doesn't match FROM-WILDCARD.
  (equal (translate-pathname "/foo/bar.txt" "**/*.*" "/usr/local/**/*.*")
         #p"/usr/local/foo/bar.txt")
  t)

(deftest pathname-match-p.12
  (pathname-match-p "/foo/bar.txt" "/**/*.*")
  t)

(deftest translate-pathname.19
  (equal (translate-pathname "/foo/bar.txt" "/**/*.*" "/usr/local/**/*.*")
         #p"/usr/local/foo/bar.txt")
  t)

#-clisp
(deftest translate-pathname.20
  (equal (translate-pathname "foo.bar" "/**/*.*" "/usr/local/") #p"/usr/local/foo.bar")
  t)

;; TRANSLATE-LOGICAL-PATHNAME

;; "PATHNAME is first coerced to a pathname. If the coerced pathname is a
;; physical pathname, it is returned."
(deftest translate-logical-pathname.1
  (equal (translate-logical-pathname #p"/") #p"/")
  t)

#+(or abcl clisp)
(deftest translate-logical-pathname.2
  (equal (translate-logical-pathname "effluvia:foo.bar") #p"/usr/local/foo.bar")
  t)

#+(or sbcl cmu)
(deftest translate-logical-pathname.3
  ;; Device mismatch.
  (and (eq (pathname-device (translate-logical-pathname "effluvia:foo.bar"))
           :unspecific)
       (eq (pathname-device #p"/usr/local/foo/bar")
           nil))
  t)

(deftest translate-logical-pathname.4
  (check-namestring (translate-logical-pathname "effluvia:foo.bar")
                    "/usr/local/foo.bar")
  t)

(deftest translate-logical-pathname.5
  (check-namestring (translate-logical-pathname "effluvia:foo;bar.txt")
                    "/usr/local/foo/bar.txt")
  t)

(deftest translate-logical-pathname.6
  #-allegro
  (check-logical-pathname #p"effluvia:Foo.Bar" "EFFLUVIA" '(:absolute) "FOO" "BAR" nil)
  #+allegro
  ;; Allegro preserves case.
  (check-logical-pathname #p"effluvia:Foo.Bar" "effluvia" nil "Foo" "Bar" nil)
  t)

;; "TRANSLATE-PATHNAME [and thus also TRANSLATE-LOGICAL-PATHNAME] maps
;; customary case in SOURCE into customary case in the output pathname."
(deftest translate-logical-pathname.7
  #-allegro
  (check-physical-pathname (translate-logical-pathname #p"effluvia:Foo.Bar")
                           '(:absolute "usr" "local") "foo" "bar")
  #+allegro
  ;; Allegro preserves case.
  (check-physical-pathname (translate-logical-pathname #p"effluvia:Foo.Bar")
                           '(:absolute "usr" "local") "Foo" "Bar")
  t)

(deftest merge-pathnames.1
  #-allegro
  (check-logical-pathname (merge-pathnames "effluvia:foo.bar")
                          "EFFLUVIA" '(:absolute) "FOO" "BAR" :newest)
  #+allegro
  ;; Allegro's MERGE-PATHNAMES apparently calls TRANSLATE-LOGICAL-PATHNAME.
  (check-physical-pathname (merge-pathnames "effluvia:foo.bar")
                           '(:absolute "usr" "local") "foo" "bar")
  t)

(deftest merge-pathnames.2
  (equal (merge-pathnames (logical-pathname "effluvia:;foo;bar;")
                          (logical-pathname "effluvia:baz;quux.lisp.3"))
         #-allegro
         (make-pathname :host "EFFLUVIA"
                        :device :unspecific
                        :directory '(:absolute "BAZ" "FOO" "BAR")
                        :name "QUUX"
                        :type "LISP"
                        :version 3)
         #+allegro
         (make-pathname :host "effluvia"
                        :device nil
                        :directory '(:absolute "baz" "foo" "bar")
                        :name "quux"
                        :type "lisp"
                        :version nil)
         )
  t)

(deftest compile-file-pathname.1
  (equal (compile-file-pathname "effluvia:foo.lisp")
         #+abcl
         ;; Is this a bug? (Should version be :NEWEST?)
         #p"EFFLUVIA:FOO.ABCL"
         #+allegro #p"effluvia:foo.fasl"
         #+(or cmu sbcl) #p"EFFLUVIA:FOO.FASL.NEWEST"
         #+clisp
         ;; Is this a bug?
         ;; #p"EFFLUVIA:FOO.fas.NEWEST"
         (make-pathname :host "EFFLUVIA" :directory '(:absolute)
                        :name "FOO" :type "fas" :version :newest)
         #+(and lispworks unix) #p"EFFLUVIA:FOO.UFSL.NEWEST"
         #+(and lispworks windows) #p"EFFLUVIA:FOO.FSL.NEWEST")
  t)

(deftest file-namestring.1
  (equal (file-namestring #p"")
         #+(or abcl allegro cmu)
         nil
         #+(or clisp lispworks sbcl)
         "")
  t)

(deftest file-namestring.2
  (equal (file-namestring #p"foo") "foo")
  t)

(deftest file-namestring.3
  (let ((pathname (make-pathname :type "foo")))
    #+abcl
    (equal (file-namestring pathname) nil)
    #+allegro
    (equal (file-namestring pathname) "NIL.foo") ;; bug
    #+(or clisp lispworks)
    (equal (file-namestring pathname) ".foo")
    #+(or cmu sbcl)
    (signals-error (file-namestring pathname) 'error))
  t)

;; A variant of FILE-NAMESTRING.3 that detects Allegro's bug as a bug.
(deftest file-namestring.4
  (let ((pathname (make-pathname :type "foo")))
    #-(or cmu sbcl)
    (not (equal (file-namestring pathname) "NIL.foo"))
    #+(or cmu sbcl)
    (signals-error (file-namestring pathname) 'error))
  t)
#+allegro
(pushnew 'file-namestring.4 *expected-failures*)

(deftest enough-namestring.1
  (equal (enough-namestring #p"/foo" #p"/") "foo")
  t)
#+sbcl
(pushnew 'enough-namestring.1 *expected-failures*)

(deftest enough-namestring.2
  #-windows
  (equal (enough-namestring #p"foo/bar" #p"foo") "foo/bar")
  #+windows
  (equal (enough-namestring #p"foo\\bar" #p"foo") "foo\\bar")
  t)

(deftest enough-namestring.3
  (equal (enough-namestring #p"foo/bar" #p"foo/") "bar")
  t)
#+sbcl
(pushnew 'enough-namestring.3 *expected-failures*)

;; The following tests are adapted from SBCL's pathnames.impure.lisp.
(setf (logical-pathname-translations "demo0")
      '(("**;*.*.*" "/tmp/")))
(deftest sbcl.1
  (pathname-match-p "demo0:file.lisp" (logical-pathname "demo0:tmp;**;*.*.*"))
  nil)

#-clisp
(deftest sbcl.2
  (check-namestring (translate-logical-pathname "demo0:file.lisp") "/tmp/file.lisp")
  t)

(setf (logical-pathname-translations "demo1")
      '(("**;*.*.*" "/tmp/**/*.*") (";**;*.*.*" "/tmp/rel/**/*.*")))
;; Remove "**" from the resulting pathname when the source directory is NIL.
(deftest sbcl.3
  (equal (namestring (translate-logical-pathname "demo1:foo.lisp"))
         #-windows "/tmp/**/foo.lisp"
         #+windows "\\tmp\\**\\foo.lisp")
  nil)

(deftest sbcl.4
  (check-namestring (translate-logical-pathname "demo1:foo.lisp") "/tmp/foo.lisp")
  t)

;;; Check for absolute/relative path confusion.
#-allegro
(deftest sbcl.5
  (pathname-match-p "demo1:;foo.lisp" "demo1:**;*.*.*")
  nil)

#+(or sbcl cmu)
;; BUG Pathnames should match if the following translation is to work.
(deftest sbcl.6
  (pathname-match-p "demo1:;foo.lisp" "demo1:;**;*.*.*")
  t)

#+clisp
(deftest sbcl.7
  (pathname-match-p "demo1:;foo.lisp" ";**;*.*.*")
  t)

(deftest sbcl.8
  (check-namestring (translate-logical-pathname "demo1:;foo.lisp")
                    #+abcl "/tmp/rel/foo.lisp"
                    #+allegro "/tmp/foo.lisp"
                    #-(or allegro abcl) "/tmp/rel/foo.lisp")
  t)

(setf (logical-pathname-translations "demo2")
      '(("test;**;*.*" "/tmp/demo2/test")))

(deftest sbcl.9
  (equal (enough-namestring "demo2:test;foo.lisp")
         #+sbcl "DEMO2:;TEST;FOO.LISP"
         #+(or abcl cmu lispworks) "DEMO2:TEST;FOO.LISP"
         #+allegro-v7.0 "demo2:test;foo.lisp"
         #+allegro-v6.2 "/test/foo.lisp" ;; BUG
         #+(and clisp unix) "TEST;FOO.LISP"
         #+(and clisp windows) "DEMO2:TEST;FOO.LISP")
  t)

#-(or allegro clisp cmu)
(deftest sbcl.10
  (signals-error (make-pathname :host "EFFLUVIA" :directory "!bla" :name "bar")
                 'error)
  t)
#-(or allegro cmu)
(deftest sbcl.11
  (signals-error (make-pathname :host "EFFLUVIA" :directory "bla" :name "!bar")
                 'error)
  t)
#-(or allegro cmu)
(deftest sbcl.12
  (signals-error (make-pathname :host "EFFLUVIA" :directory "bla" :name "bar" :type "&baz")
                 'error)
  t)

(deftest sbcl.13
  (equal (namestring (parse-namestring "" "EFFLUVIA")) "EFFLUVIA:")
  t)

(deftest sbcl.14
  #-cmu
  (equal (namestring (parse-namestring "" :unspecific)) "")
  #+cmu
  ;; It seems reasonable to signal an error here, since the HOST argument to
  ;; PARSE-NAMESTRING is specified to be "a valid pathname host, a logical host,
  ;; or NIL".
  (signals-error (parse-namestring "" :unspecific) 'type-error)
  t)

(deftest sbcl.15
  (equal (namestring (parse-namestring ""
                                       (pathname-host
                                        (translate-logical-pathname
                                         "EFFLUVIA:"))))
         "")
  t)

;; PARSE-NAMESTRING host mismatch: "If HOST is supplied and not NIL, and THING
;; contains a manifest host name, an error of type ERROR is signaled if the
;; hosts do not match."
(deftest sbcl.16
  (signals-error (parse-namestring "effluvia:foo.bar" "demo2") 'error)
  t)

(setf (logical-pathname-translations "bazooka")
      '(("todemo;*.*.*" "demo0:*.*.*")))

(deftest sbcl.17
  #+allegro ;; BUG
  (check-namestring (translate-logical-pathname "bazooka:todemo;x.y") "/tmp/todemo/x.y")
  #+clisp ;; BUG
  (signals-error (translate-logical-pathname "bazooka:todemo;x.y") 'error)
  #-(or allegro clisp)
  (check-namestring (translate-logical-pathname "bazooka:todemo;x.y") "/tmp/x.y")
  t)

(deftest sbcl.18
  #+clisp ;; BUG
  (signals-error (translate-logical-pathname "demo0:x.y") 'error)
  #-clisp
  (equal (namestring (translate-logical-pathname "demo0:x.y"))
         #-windows "/tmp/x.y"
         #+windows "\\tmp\\x.y")
  t)

#-(or allegro clisp)
(deftest sbcl.19
  (equal (namestring (translate-logical-pathname "bazooka:todemo;x.y"))
         (namestring (translate-logical-pathname "demo0:x.y")))
  t)

;; "If HOST is incorrectly supplied, an error of type TYPE-ERROR is signaled."
(deftest sbcl.20
  (signals-error (logical-pathname-translations "unregistered-host")
                 #+(or clisp lispworks) 'error ;; BUG
                 #+cmu 'file-error ;; BUG
                 #-(or clisp lispworks cmu) 'type-error)
  t)

(deftest sbcl.21
  (string-equal (host-namestring (parse-namestring "OTHER-HOST:ILLEGAL/LPN")) "OTHER-HOST")
  nil)
#+(or lispworks (and clisp windows))
(pushnew 'sbcl.21 *expected-failures*)

(deftest sbcl.22
  (string= (pathname-name (parse-namestring "OTHER-HOST:ILLEGAL/LPN")) "LPN")
  t)
#+(and clisp windows)
(pushnew 'sbcl.22 *expected-failures*)

(setf (logical-pathname-translations "test0")
      '(("**;*.*.*"              "/library/foo/**/")))

(deftest sbcl.23
  (check-namestring (translate-logical-pathname "test0:foo;bar;baz;mum.quux")
                    "/library/foo/foo/bar/baz/mum.quux")
  t)

(setf (logical-pathname-translations "prog")
      '(("CODE;*.*.*"             "/lib/prog/")))

#-allegro
(deftest sbcl.24
  (check-namestring (translate-logical-pathname "prog:code;documentation.lisp")
                    "/lib/prog/documentation.lisp")
  t)

(setf (logical-pathname-translations "prog1")
      '(("CODE;DOCUMENTATION.*.*" "/lib/prog/docum.*")
        ("CODE;*.*.*"             "/lib/prog/")))

#-allegro
(deftest sbcl.25
  (check-namestring (translate-logical-pathname "prog1:code;documentation.lisp")
                    "/lib/prog/docum.lisp")
  t)

;; "ANSI section 19.3.1.1.5 specifies that translation to a filesystem which
;; doesn't have versions should ignore the version slot. CMU CL didn't ignore
;; this as it should, but we [i.e. SBCL] do."
;; "Some file systems do not have versions. Logical pathname translation to
;; such a file system ignores the version." 19.3.1.1.5
#-cmu
;; CMUCL supports emacs-style versions.
(deftest sbcl.26
  (check-namestring (translate-logical-pathname "test0:foo;bar;baz;mum.quux.3")
                    "/library/foo/foo/bar/baz/mum.quux")
  t)
#+lispworks
(pushnew 'sbcl.26 *expected-failures*)

(setf (logical-pathname-translations "scratch")
      '(("**;*.*.*" "/usr/local/doc/**/*")))

;; Trivial merge.
(deftest sbcl.27
  (check-merge-pathnames #p"foo" #p"/usr/local/doc/" #p"/usr/local/doc/foo")
  t)

;; If pathname does not specify a host, device, directory, name, or type, each
;; such component is copied from default-pathname.
;; 1) no name, no type
(deftest sbcl.28
  (check-merge-pathnames #p"/supplied-dir/" #p"/dir/name.type"
                         #p"/supplied-dir/name.type")
  t)

;; 2) no directory, no type
(deftest sbcl.29
  (check-merge-pathnames #p"supplied-name" #p"/dir/name.type"
                         #p"/dir/supplied-name.type")
  t)

;; 3) no name, no dir (must use make-pathname as ".foo" is parsed
;; as a name)
(deftest sbcl.30
  (check-merge-pathnames (make-pathname :type "supplied-type")
                         #p"/dir/name.type"
                         #p"/dir/name.supplied-type")
  t)

;; If (pathname-directory pathname) is a list whose car is
;; :relative, and (pathname-directory default-pathname) is a
;; list, then the merged directory is [...]
(deftest sbcl.31
  (check-merge-pathnames #p"qqq/www" #p"/aaa/bbb/ccc/ddd/eee"
                         #p"/aaa/bbb/ccc/ddd/qqq/www")
  t)

;; except that if the resulting list contains a string or
;; :wild immediately followed by :back, both of them are
;; removed.
(deftest sbcl.32
  (check-merge-pathnames
   ;; "../" in a namestring is parsed as :up not :back, so MAKE-PATHNAME.
   (make-pathname :directory '(:relative :back "blah"))
   #p"/aaa/bbb/ccc/ddd/eee" #P"/aaa/bbb/ccc/blah/eee")
  t)

;; If (pathname-directory default-pathname) is not a list or
;; (pathname-directory pathname) is not a list whose car is
;; :relative, the merged directory is (or (pathname-directory
;; pathname) (pathname-directory default-pathname))
(deftest sbcl.33
  (check-merge-pathnames #p"/absolute/path/name" #p"/dir/default-name.type"
                         #P"/absolute/path/name.type")
  t)

(deftest sbcl.34
  (check-merge-pathnames #p"scratch:;name2" #p"scratch:foo;"
                         #p"SCRATCH:FOO;NAME2")
  t)

(deftest sbcl.35
  (check-merge-pathnames #p"scratch:;foo" #p"/usr/local/doc/"
                         #-(or allegro clisp lispworks) #P"SCRATCH:USR;LOCAL;DOC;FOO"
                         #+(and allegro unix) #p"/usr/local/doc/foo"
                         #+(and allegro windows) #p"scratch:usr;local;doc;foo"
                         #+clisp #p"SCRATCH:;FOO"
                         #+lispworks #p"SCRATCH:FOO")
  t)

(deftest sbcl.36
  (check-merge-pathnames #p"scratch:supplied-dir;" #p"/dir/name.type"
                         #-clisp #p"SCRATCH:SUPPLIED-DIR;NAME.TYPE"
                         #+clisp
                         ;; #p"SCRATCH:SUPPLIED-DIR;name.type.NEWEST"
                         (make-pathname :host "SCRATCH"
                                        :directory '(:absolute "SUPPLIED-DIR")
                                        :name "name"
                                        :type "type"))
  t)

(deftest sbcl.37
  (check-merge-pathnames #p"scratch:;supplied-name" #p"/dir/name.type"
                         #-(or allegro clisp lispworks)
                         #p"SCRATCH:DIR;SUPPLIED-NAME.TYPE"
                         #+(and allegro unix)
                         #p"/usr/local/doc/supplied-name.type"
                         #+(and allegro windows)
                         #P"scratch:dir;supplied-name.type"
                         #+clisp
                         ;; #P"SCRATCH:;SUPPLIED-NAME.type.NEWEST"
                         (make-pathname :host "SCRATCH"
                                        :directory '(:relative)
                                        :name "SUPPLIED-NAME"
                                        :type "type")
                         #+lispworks
                         ;; #P"SCRATCH:SUPPLIED-NAME.TYPE.NEWEST"
                         (make-pathname :host "SCRATCH"
                                        :directory '(:absolute)
                                        :name "SUPPLIED-NAME"
                                        :type "TYPE"))
  t)

(deftest sbcl.38
  (check-merge-pathnames (make-pathname :host "scratch" :type "supplied-type")
                         #p"/dir/name.type"
                         #-(or allegro clisp lispworks)
                         #p"SCRATCH:DIR;NAME.SUPPLIED-TYPE"
                         #+(and allegro unix)
                         #p"/usr/local/doc/name.supplied-type"
                         #+(and allegro windows)
                         #P"scratch:dir;name.supplied-type"
                         #+clisp
                         ;; #P"SCRATCH:dir;name.supplied-type.NEWEST"
                         (make-pathname :host "SCRATCH"
                                        :directory '(:absolute "dir")
                                        :name "name"
                                        :type "supplied-type")
                         #+lispworks
                         ;; #P"SCRATCH:NAME.SUPPLIED-TYPE.NEWEST"
                         (make-pathname :host "SCRATCH"
                                        :directory '(:absolute)
                                        :name "NAME"
                                        :type "SUPPLIED-TYPE"))
  t)

(deftest sbcl.39
  (let ((pathname (make-pathname :host "scratch"
                                        :directory '(:relative "foo")
                                        :name "bar"))
        (default-pathname #p"/aaa/bbb/ccc/ddd/eee"))
    #-allegro
    (check-merge-pathnames pathname default-pathname
                           #-(or clisp lispworks)
                           #p"SCRATCH:AAA;BBB;CCC;DDD;FOO;BAR"
                           #+clisp
                           ;; #P"SCRATCH:;foo;bar"
                           (make-pathname :host "SCRATCH"
                                          :directory '(:relative "foo")
                                          :name "bar")
                           #+lispworks
                           #p"SCRATCH:FOO;BAR")
    #+(and allegro unix)
    (signals-error (merge-pathnames pathname default-pathname) 'error)
    #+(and allegro windows)
    (check-merge-pathnames pathname default-pathname
                           #P"scratch:aaa;bbb;ccc;ddd;foo;bar"))
  t)

#-lispworks
(deftest sbcl.40
  (let ((pathname (make-pathname :host "scratch"
                                 :directory '(:relative :back "foo")
                                 :name "bar"))
        (default-pathname #p"/aaa/bbb/ccc/ddd/eee"))
    #-allegro
    (check-merge-pathnames pathname default-pathname
                           #-clisp #p"SCRATCH:AAA;BBB;CCC;FOO;BAR"
                           #+clisp
                           ;; #P"SCRATCH:;..;foo;bar.NEWEST"
                           (make-pathname :host "SCRATCH"
                                          :directory '(:relative :back "foo")
                                          :name "bar"))
    #+(and allegro unix)
    (signals-error (merge-pathnames pathname default-pathname) 'error)
    #+(and allegro windows)
    (check-merge-pathnames pathname default-pathname
                           #P"scratch:aaa;bbb;ccc;foo;bar"))
  t)

#+lispworks
;; "Illegal logical pathname directory component: :BACK."
(deftest sbcl.40
  (signals-error (make-pathname :host "scratch"
                                :directory '(:relative :back "foo")
                                :name "bar")
                 'error)
  t)

(deftest sbcl.41
  (check-merge-pathnames #p"scratch:absolute;path;name"
                         #p"/dir/default-name.type"
                         #-clisp #p"SCRATCH:ABSOLUTE;PATH;NAME.TYPE"
                         #+clisp
                         ;; #P"SCRATCH:ABSOLUTE;PATH;NAME.type.NEWEST"
                         (make-pathname :host "SCRATCH"
                                        :directory '(:absolute "ABSOLUTE" "PATH")
                                        :name "NAME"
                                        :type "type"))
  t)

(deftest sbcl.42
  (check-namestring (parse-namestring "/foo" (host-namestring #p"/bar")) "/foo")
  t)
#+lispworks
(pushnew 'sbcl.42 *expected-failures*)

(deftest sbcl.43
  (string= (namestring (parse-namestring "FOO" (host-namestring #p"SCRATCH:BAR")))
           "SCRATCH:FOO")
  t)

#-(or allegro clisp cmu lispworks)
(deftest sbcl.44
  ;; "The null string, "", is not a valid value for any component of a logical
  ;; pathname." 19.3.2.2
  (signals-error (setf (logical-pathname-translations "")
                       (list '("**;*.*.*" "/**/*.*")))
                 'error)
  t)

#-clisp
(deftest sbcl.45
  (check-namestring (translate-logical-pathname "/") "/")
  t)

(deftest sbcl.46
  (signals-error (pathname (make-string-input-stream "FOO"))
                 #-(or allegro-v6.2 cmu) 'type-error
                 #+allegro-v6.2     'stream-error
                 #+cmu              'error)
  t)

(deftest sbcl.47
  (signals-error (merge-pathnames (make-string-output-stream))
                 #-allegro-v6.2 'type-error
                 #+allegro-v6.2 'stream-error)
  t)

(deftest sbcl.48
  (check-readable-or-signals-error (make-pathname :name "foo" :type "txt" :version :newest))
  t)
#+lispworks
(pushnew 'sbcl.48 *expected-failures*)

#-allegro
(deftest sbcl.49
  (check-readable-or-signals-error (make-pathname :name "foo" :type "txt" :version 1))
  t)
#+lispworks
(pushnew 'sbcl.49 *expected-failures*)

(deftest sbcl.50
  #-clisp
  (check-readable-or-signals-error (make-pathname :name "foo" :type ".txt"))
  #+clisp
  (signals-error (make-pathname :name "foo" :type ".txt") 'error)
  t)
#+(or allegro cmu lispworks)
(pushnew 'sbcl.50 *expected-failures*)

(deftest sbcl.51
  (check-readable-or-signals-error (make-pathname :name "foo." :type "txt"))
  t)

(deftest sbcl.52
  (check-readable-or-signals-error (parse-namestring "SCRATCH:FOO.TXT.1"))
  t)

(deftest sbcl.53
  (check-readable-or-signals-error (parse-namestring "SCRATCH:FOO.TXT.NEWEST"))
  t)

(deftest sbcl.54
  (check-readable-or-signals-error (parse-namestring "SCRATCH:FOO.TXT"))
  t)

(deftest sbcl.55
  (equal (parse-namestring "foo" nil "/")
         (parse-namestring "foo" nil #p"/"))
  t)

#-allegro
(deftest sbcl.56
  (let ((test "parse-namestring-test.tmp"))
    (unwind-protect
        (with-open-file (f test :direction :output)
          ;; FIXME: This test is a bit flaky, since we only check that
          ;; no error is signalled. The dilemma here is "what is the
          ;; correct result when defaults is a _file_, not a
          ;; directory". Currently (0.8.10.73) we get #P"foo" here (as
          ;; opposed to eg. #P"/path/to/current/foo"), which is
          ;; possibly mildly surprising but probably conformant.
          (equal (parse-namestring "foo" nil f) #p"foo"))
      (when (probe-file test)
        (delete-file test))))
  t)

;;; ENOUGH-NAMESTRING should probably not fail when the namestring in
;;; question has a :RELATIVE pathname.
(deftest sbcl.57
  (equal (enough-namestring #p"foo" #p"./") "foo")
  t)

;;; bug reported by Artem V. Andreev: :WILD not handled in unparsing
;;; directory lists.
(deftest sbcl.58
  (check-namestring #p"/tmp/*/" "/tmp/*/")
  t)

#-allegro
(deftest sbcl.59
  (string= (with-standard-io-syntax (write-to-string #p"/foo"))
           #-windows "#P\"/foo\""
           #+(and windows (not lispworks)) "#P\"\\\\foo\""
           #+(and windows lispworks) "#P\"/foo\"")
  t)

#-allegro
(deftest sbcl.60
  (string= (with-standard-io-syntax (write-to-string #p"/foo" :readably nil))
           #-windows "#P\"/foo\""
           #+(and windows (not lispworks)) "#P\"\\\\foo\""
           #+(and windows lispworks) "#P\"/foo\"")
  t)

#-allegro
(deftest sbcl.61
  (string= (with-standard-io-syntax (write-to-string #p"/foo" :escape nil))
           #-windows "#P\"/foo\""
           #+(and windows (not lispworks)) "#P\"\\\\foo\""
           #+(and windows lispworks) "#P\"/foo\"")
  t)

(deftest sbcl.62
  (string= (with-standard-io-syntax (write-to-string #p"/foo" :readably nil :escape nil))
           #-windows "/foo"
           #+windows "\\foo")
  t)

(do-tests)
