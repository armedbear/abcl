;;; java-tests.lisp
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

#+abcl
(use-package '#:java)

#+allegro
(require :jlinker)
#+allegro
(use-package '#:javatools.jlinker)
#+allegro
(use-package '#:javatools.jlinker '#:cl-user) ;; For convenience only.
#+(and allegro mswindows)
(use-package '#:javatools.jlinker '#:cg-user) ;; For convenience only.
#+allegro
(load (merge-pathnames "jl-config.cl" *load-truename*))
#+allegro
(or (jlinker-query) (jlinker-init))

#+abcl
(defmacro with-registered-exception (exception condition &body body)
  `(unwind-protect
       (progn
         (register-java-exception ,exception ,condition)
         ,@body)
     (unregister-java-exception ,exception)))

#+abcl
(deftest java-object.1
  (class-name (find-class 'java-object nil))
  java-object)

(deftest jclass.1
  (jcall (jmethod "java.lang.Object" "toString") (jclass "java.lang.String"))
  "class java.lang.String")

(deftest jclass.2
  (equal (jcall (jmethod "java.lang.Object" "getClass") "foo")
         (jclass "java.lang.String"))
  #+abcl    t
  #+allegro nil)

(deftest jclass.3
  (equal (jclass '|java.lang.String|) (jclass "java.lang.String"))
  t)

(deftest jclass.4
  (let ((class1 (jcall (jmethod "java.lang.Object" "getClass") "foo"))
        (class2 (jclass "java.lang.String")))
    (jcall (jmethod "java.lang.Object" "equals" "java.lang.Object")
           class1 class2))
  t)

(deftest jclass.5
  (jcall (jmethod "java.lang.Object" "toString") (jclass "int"))
  "int")

(deftest jclass.6
  (equal (jclass '|int|) (jclass "int"))
  t)

;; No such class.
(deftest jclass.error.1
  (signals-error (jclass "foo") 'error)
  t)

;; Silly argument.
(deftest jclass.error.2
  (signals-error (jclass 42) 'error)
  t)

(deftest jclass-of.1
  (jclass-of "foo")
  "java.lang.String"
  "java.lang.String")

(deftest jclass-of.2
  (jclass-of "foo" "java.lang.String")
  t
  "java.lang.String")

(deftest jclass-of.3
  (jclass-of "foo" "bar")
  nil
  "java.lang.String")

(deftest jclass-of.4
  (jclass-of 42)
  nil
  nil)

(deftest jclass-of.5
  (jclass-of 'foo)
  nil
  nil)

(deftest jclass-name.1
  (jclass-name "java.lang.String")
  "java.lang.String")

(deftest jclass-name.2
  (signals-error (jclass-name "foo") 'error)
  t)

(deftest jclass-name.3
  (signals-error (jclass-name 42) 'error)
  t)

(deftest jclass-name.4
  (jclass-name (jclass "java.lang.String"))
  "java.lang.String")

(deftest jclass-name.5
  (jclass-name (jclass "java.lang.String") "java.lang.String")
  t
  "java.lang.String")

(deftest jclass-name.6
  (jclass-name (jclass "java.lang.String") "java.lang.Object")
  nil
  "java.lang.String")

(deftest jclass-name.7
  (jclass-name (jclass "java.lang.String") "foo")
  nil
  "java.lang.String")

(deftest jclass-name.8
  (jclass-name (jclass "int"))
  "int")

(deftest jconstructor.1
  (jclass-of (jconstructor "java.lang.String" "java.lang.String"))
  "java.lang.reflect.Constructor"
  "java.lang.reflect.Constructor")

(deftest jnew.1
  (let ((constructor (jconstructor "java.lang.String" "java.lang.String")))
    (jclass-of (jnew constructor "foo")))
  "java.lang.String"
  "java.lang.String")

(deftest jnew.2
  (jclass-of (jnew (jconstructor "java.awt.Point")))
  "java.awt.Point"
  "java.awt.Point")

#-abcl
(deftest jnew.3
  (jclass-of (jnew "java.awt.Point") "java.awt.Point")
  t
  "java.awt.Point")

(deftest jnew.error.1
  (signals-error (jnew (jconstructor "java.lang.String" "java.lang.String")
                       (make-immediate-object nil :ref))
                 #+abcl    'java-exception
                 #+allegro 'jlinker-error)
  t)

(deftest jcall.1
  (let ((method (jmethod "java.lang.String" "length")))
    (jcall method "test"))
  4)

(deftest jcall.2
  (jcall "length" "test")
  4)

(deftest jcall.3
  (let ((method (jmethod "java.lang.String" "regionMatches" 4)))
    (jcall method "test" 0 "this is a test" 10 4))
  t)

(deftest jcall.4
  (let ((method (jmethod "java.lang.String" "regionMatches" 5)))
    (jcall method "test" (make-immediate-object nil :boolean) 0 "this is a test" 10 4))
  t)

(deftest jfield.1
  (type-of (jfield "java.lang.Integer" "TYPE"))
  #+abcl    java-object
  #+allegro tran-struct)

(deftest jmethod.1
  (jcall (jmethod "java.lang.Object" "toString")
         (jmethod "java.lang.String" "substring" 1))
  "public java.lang.String java.lang.String.substring(int)")

(deftest jmethod.2
  (jcall (jmethod "java.lang.Object" "toString")
         (jmethod "java.lang.String" "substring" 2))
  "public java.lang.String java.lang.String.substring(int,int)")

(deftest jmethod.3
  (signals-error (jmethod "java.lang.String" "substring" 3) 'error)
  t)

#+abcl
(deftest jmethod-return-type.1
  (jclass-name (jmethod-return-type (jmethod "java.lang.String" "length")))
  "int")

#+abcl
(deftest jmethod-return-type.2
  (jclass-name (jmethod-return-type (jmethod "java.lang.String" "substring" 1)))
  "java.lang.String")

#+abcl
(deftest jmethod-return-type.error.1
  (signals-error (jmethod-return-type (jclass "java.lang.String")) 'error)
  t)

#+abcl
(deftest jmethod-return-type.error.2
  (signals-error (jmethod-return-type 42) 'error)
  t)

#+abcl
(deftest define-condition.1
  (progn
    (define-condition throwable (java-exception) ())
    (let ((c (make-condition 'throwable)))
      (signals-error (simple-condition-format-control c) 'unbound-slot)))
  t)

#+abcl
(deftest define-condition.2
  (progn
    (define-condition throwable (java-exception) ())
    (let ((c (make-condition 'throwable)))
      (simple-condition-format-arguments c)))
  nil)

#+abcl
(deftest define-condition.3
  (progn
    (define-condition throwable (java-exception) ())
    (let ((c (make-condition 'throwable
                             :format-control "The bear is armed.")))
      (simple-condition-format-control c)))
  "The bear is armed.")

#+abcl
(deftest define-condition.4
  (progn
    (define-condition throwable (java-exception) ())
    (let ((c (make-condition 'throwable
                             :format-control "The bear is armed.")))
      (simple-condition-format-arguments c)))
  nil)

#+abcl
(deftest java-exception-cause.1
  (progn
    (define-condition throwable (java-exception) ())
    (signals-error (java-exception-cause (make-condition 'throwable))
                   'unbound-slot))
  t)

#+abcl
(deftest java-exception-cause.2
  (progn
    (define-condition throwable (java-exception) ())
    (java-exception-cause (make-condition 'throwable :cause 42)))
  42)

#+abcl
(deftest unregister-java-exception.1
  (progn
    (define-condition throwable (java-exception) ())
    (register-java-exception "java.lang.Throwable" 'throwable)
    (unregister-java-exception "java.lang.Throwable"))
  t)

#+abcl
(deftest unregister-java-exception.2
  (unregister-java-exception "java.lang.Throwable")
  nil)

#+abcl
(deftest register-java-exception.1
  (progn
    (define-condition throwable (java-exception) ())
    (with-registered-exception "java.lang.Throwable" 'throwable
      (signals-error
       (jnew (jconstructor "java.lang.String" "java.lang.String")
             (make-immediate-object nil :ref))
       'throwable)))
  t)

#+abcl
(deftest register-java-exception.1a
  (progn
    (define-condition throwable (java-exception) ())
    (with-registered-exception "java.lang.Throwable" 'throwable
      (handler-case
          (jnew (jconstructor "java.lang.String" "java.lang.String")
                (make-immediate-object nil :ref))
        (condition (c) (values (type-of c) (princ-to-string c))))))
  throwable
  "java.lang.NullPointerException")

#+abcl
(deftest register-java-exception.2
  (progn
    (define-condition throwable (java-exception) ())
    (with-registered-exception "java.lang.Throwable" 'throwable
      (signals-error
       (jnew (jconstructor "java.lang.String" "java.lang.String") 42)
       'throwable)))
  t)

#+abcl
;; Behavior is non-deterministic.
(deftest register-java-exception.2a
  (progn
    (define-condition throwable (java-exception) ())
    (with-registered-exception "java.lang.Throwable" 'throwable
      (handler-case
          (jnew (jconstructor "java.lang.String" "java.lang.String") 42)
        (condition (c) (let* ((s (princ-to-string c)))
                         ;; The actual string returned by Throwable.getMessage()
                         ;; is either "argument type mismatch" or something
                         ;; like "java.lang.ClassCastException@9d0366".
                         (or (string= s "argument type mismatch")
                             (and (> (length s) (length "java.lang.ClassCastException"))
                                  (string= (subseq s 0 (length "java.lang.ClassCastException"))
                                           "java.lang.ClassCastException"))))))))
  t)

#+abcl
(deftest register-java-exception.3
  (progn
    (define-condition throwable (java-exception) ())
    (with-registered-exception "java.lang.Throwable" 'throwable
      (signals-error
       (jstatic (jmethod "java.lang.String" "valueOf" "int") "java.lang.String" "12")
       'throwable)))
  t)

#+abcl
;; Behavior is non-deterministic.
(deftest register-java-exception.3a
  (progn
    (define-condition throwable (java-exception) ())
    (with-registered-exception "java.lang.Throwable" 'throwable
      (handler-case
          (jstatic (jmethod "java.lang.String" "valueOf" "int") "java.lang.String" "12")
        (condition (c) (let ((s (princ-to-string c)))
                         (or (string= s "argument type mismatch")
                             (string= s "java.lang.IllegalArgumentException")))))))
  t)

#+abcl
(deftest register-java-exception.4
  (progn
    (define-condition throwable (java-exception) ())
    (define-condition illegal-argument-exception (java-exception) ())
    (with-registered-exception "java.lang.Throwable" 'throwable
      (with-registered-exception "java.lang.IllegalArgumentException" 'illegal-argument-exception
        (signals-error
         (jstatic (jmethod "java.lang.String" "valueOf" "int") "java.lang.String" "12")
         'throwable))))
  nil)

#+abcl
(deftest register-java-exception.5
  (progn
    (define-condition throwable (java-exception) ())
    (define-condition illegal-argument-exception (java-exception) ())
    (with-registered-exception "java.lang.Throwable" 'throwable
      (with-registered-exception "java.lang.IllegalArgumentException" 'illegal-argument-exception
        (signals-error
         (jstatic (jmethod "java.lang.String" "valueOf" "int") "java.lang.String" "12")
         'illegal-argument-exception))))
  t)


#+abcl
(deftest register-java-exception.6
  (progn 
    (define-condition foo () ())
    (register-java-exception "java.lang.Throwable" 'foo))
  nil)

#+abcl
(deftest register-java-exception.7
  (progn 
    (define-condition throwable (java-exception) ())
    (register-java-exception "java.lang.Throwable" 'throwable))
  t)

#+abcl
(deftest register-java-exception.8
  (progn 
    (define-condition throwable (java-exception) ())
    (with-registered-exception "java.lang.Throwable" 'throwable
      (define-condition throwable () ())
      (signals-error
       (jstatic (jmethod "java.lang.String" "valueOf" "int") "java.lang.String" "12")
       'java-exception)))
  t)
      
#+abcl
(deftest register-java-exception.9
  (progn 
    (define-condition throwable (java-exception) ())
    (define-condition illegal-argument-exception (throwable) ())
    (with-registered-exception "java.lang.IllegalArgumentException" 'illegal-argument-exception
      (signals-error
       (jstatic (jmethod "java.lang.String" "valueOf" "int") "java.lang.String" "12")
       'illegal-argument-exception)))
  t)

(do-tests)

;;#+allegro
;;(jlinker-end)
