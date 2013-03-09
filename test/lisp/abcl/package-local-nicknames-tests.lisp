;;; package-local-nicknames-tests.lisp
;;;
;;; Copyright (C) 2013 Nikodemus Siivola, Rudolf Schlatte
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

;;; Most of these tests are adapted from the SBCL test suite.

(in-package #:abcl.test.lisp)

(defmacro with-tmp-packages (bindings &body body)
  `(let ,(mapcar #'car bindings)
     (unwind-protect
          (progn
            (setf ,@(apply #'append bindings))
            ,@body)
       ,@(mapcar (lambda (p)
                   `(when ,p (delete-package ,p)))
                 (mapcar #'car bindings)))))

(defpackage :package-local-nicknames-test-1
           (:local-nicknames (:l :cl) (:e :ext)))

(defpackage :package-local-nicknames-test-2
           (:export "CONS"))

(deftest pln-introspect
    (let ((alist (ext:package-local-nicknames :package-local-nicknames-test-1)))
      (values
       (equal (cons "L" (find-package "CL")) (assoc "L" alist :test 'string=))
       (equal (cons "E" (find-package "EXT")) (assoc "E" alist :test 'string=))
       (eql 2 (length alist))))
  t
  t
  t)

(deftest pln-usage
    (let ((*package* (find-package :package-local-nicknames-test-1)))
      (let ((cons0 (read-from-string "L:CONS"))
            (exit0 (read-from-string "E:EXIT"))
            (cons1 (find-symbol "CONS" :l))
            (exit1 (find-symbol "EXIT" :e))
            (cl (find-package :l))
            (ext (find-package :e)))
        (values
         (eq 'cons cons0)
         (eq 'cons cons1)
         (equal "L:CONS" (prin1-to-string cons0))
         (eq 'ext:exit exit0)
         (eq 'ext:exit exit1)
         (equal "E:EXIT" (prin1-to-string exit0))
         (eq cl (find-package :common-lisp))
         (eq ext (find-package :ext)))))
  T
  T
  T
  T
  T
  T
  T
  T)

(deftest pln-add-nickname-twice
    (handler-case
        (ext:add-package-local-nickname :l :package-local-nicknames-test-2
                                        :package-local-nicknames-test-1)
      (error ()
        :oopsie))
  :oopsie)

(deftest pln-add-same-nickname
    (progn (ext:add-package-local-nickname :l :cl
                                           :package-local-nicknames-test-1)
           :okay)
  :okay)

(deftest pln-remove-local-nickname
    (progn
      (assert (ext:remove-package-local-nickname :l :package-local-nicknames-test-1))
      (assert (not (ext:remove-package-local-nickname :l :package-local-nicknames-test-1)))
      (let ((*package* (find-package :package-local-nicknames-test-1)))
        (let ((exit0 (read-from-string "E:EXIT"))
              (exit1 (find-symbol "EXIT" :e))
              (e (find-package :e)))
          (assert (eq 'ext:exit exit0))
          (assert (eq 'ext:exit exit1))
          (assert (equal "E:EXIT" (prin1-to-string exit0)))
          (assert (eq e (find-package :ext)))
          (assert (not (find-package :l)))))
      (assert (eq (find-package :package-local-nicknames-test-1)
                  (ext:add-package-local-nickname :l :package-local-nicknames-test-2
                                              :package-local-nicknames-test-1)))
      (let ((*package* (find-package :package-local-nicknames-test-1)))
        (let ((cons0 (read-from-string "L:CONS"))
              (exit0 (read-from-string "E:EXIT"))
              (cons1 (find-symbol "CONS" :l))
              (exit1 (find-symbol "EXIT" :e))
              (cl (find-package :l))
              (e (find-package :e)))
          (assert (eq cons0 cons1))
          (assert (not (eq 'cons cons0)))
          (assert (eq (find-symbol "CONS" :package-local-nicknames-test-2)
                      cons0))
          (assert (equal "L:CONS" (prin1-to-string cons0)))
          (assert (eq 'ext:exit exit0))
          (assert (eq 'ext:exit exit1))
          (assert (equal "E:EXIT" (prin1-to-string exit0)))
          (assert (eq cl (find-package :package-local-nicknames-test-2)))
          (assert (eq e (find-package :ext)))))
      :success)
  :success)

(deftest pln-delete-locally-nicknaming-package
    (with-tmp-packages ((p1 (make-package "LOCALLY-NICKNAMES-OTHERS"))
                        (p2 (make-package "LOCALLY-NICKNAMED-BY-OTHERS")))
      (ext:add-package-local-nickname :foo p2 p1)
      (assert (equal (list p1) (ext:package-locally-nicknamed-by-list p2)))
      (delete-package p1)
      (assert (null (ext:package-locally-nicknamed-by-list p2)))
      :success)
  :success)

(deftest pln-delete-locally-nicknamed-package
    (with-tmp-packages ((p1 (make-package "LOCALLY-NICKNAMES-OTHERS"))
                        (p2 (make-package "LOCALLY-NICKNAMED-BY-OTHERS")))
      (ext:add-package-local-nickname :foo p2 p1)
      (assert (ext:package-local-nicknames p1))
      (delete-package p2)
      (assert (null (ext:package-local-nicknames p1)))
      :success)
  :success)

(deftest pln-own-name-as-local-nickname
    (with-tmp-packages ((p1 (make-package "OWN-NAME-AS-NICKNAME1"))
                        (p2 (make-package "OWN-NAME-AS-NICKNAME2")))
      (assert (eq :oops
                  (handler-case
                      (ext:add-package-local-nickname :own-name-as-nickname1 p2 p1)
                    (error ()
                      :oops))))
      ;; TODO: add continuable errors for this
      ;; (handler-bind ((error #'continue))
      ;;   (ext:add-package-local-nickname :own-name-as-nickname1 p2 p1))
      ;; (assert (eq (intern "FOO" p2)
      ;;             (let ((*package* p1))
      ;;               (intern "FOO" :own-name-as-nickname1))))
      :success)
  :success)



(deftest pln-own-nickname-as-local-nickname
  (with-tmp-packages ((p1 (make-package "OWN-NICKNAME-AS-NICKNAME1"
                                        :nicknames '("OWN-NICKNAME")))
                      (p2 (make-package "OWN-NICKNAME-AS-NICKNAME2")))
    (assert (eq :oops
                (handler-case
                    (add-package-local-nickname :own-nickname p2 p1)
                  (error ()
                    :oops))))
    ;; TODO: make errors continuable
    ;; (handler-bind ((error #'continue))
    ;;   (add-package-local-nickname :own-nickname p2 p1))
    ;; (assert (eq (intern "FOO" p2)
    ;;             (let ((*package* p1))
    ;;               (intern "FOO" :own-nickname))))
    :success)
  :success)
