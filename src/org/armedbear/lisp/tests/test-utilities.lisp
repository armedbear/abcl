;;; test-utilities.lisp
;;;
;;; Copyright (C) 2005-2006 Peter Graves
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

#+(and allegro mswindows)
(pushnew :windows *features*)
#+(and clisp win32)
(pushnew :windows *features*)
#+(and lispworks win32)
(pushnew :windows *features*)

(unless (member "RT" *modules* :test #'string=)
  (load (merge-pathnames "rt-package.lisp" *load-truename*))
  (load #+abcl (compile-file-if-needed (merge-pathnames "rt.lisp" *load-truename*))
        ;; Force compilation to avoid fasl name conflict between SBCL and
        ;; Allegro.
        #-abcl (compile-file (merge-pathnames "rt.lisp" *load-truename*)))
  (provide "RT"))

(unless (find-package '#:test)
  (defpackage #:test (:use #:cl #:regression-test)))

(in-package #:regression-test)

(export '(signals-error))

(defmacro signals-error (form error-name)
  `(locally (declare (optimize safety))
     (handler-case ,form
       (condition (c) (typep c ,error-name))
       (:no-error (&rest ignored) (declare (ignore ignored)) nil))))

(rem-all-tests)

(setf *expected-failures* nil)
