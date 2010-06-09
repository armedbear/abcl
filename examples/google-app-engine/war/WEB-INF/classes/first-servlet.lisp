(defpackage #:first-servlet
  (:use :cl)
  (:export #:do-get))

(in-package #:first-servlet)

(defun do-get ()
  (format t "Hello, World!~%"))