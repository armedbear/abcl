;;; -*- Mode:Lisp -*-

(in-package :cl-user)

(defpackage :named-readtables-test
  (:use :cl :named-readtables)
  (:import-from :named-readtables
     #:dispatch-macro-char-p
     #:do-readtable
     #:ensure-function
     #:ensure-dispatch-macro-character
     #:function=))