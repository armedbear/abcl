;;; Copyright (C) 2023 Alejandro Zamora Fonseca

;;; compiler-backend.lisp

;;; General utilities to distinguish different compiler backends

(in-package #:compiler-backend)

(defparameter *current-backend* :jvm)

(defmacro with-backend (&body body)
  `(case *current-backend*
     ,@body))

(defmacro with-backend-error (&body body)
  `(case *current-backend*
     ,@body
     (t (error "Unknown compiler backend"))))

;; Usage example:
;; (let ((*current-backend* :jvm-stepper))
;;   (with-backend
;;     (:jvm (print "I'm compiling for the JVM"))
;;     (:jvm-stepper (print "I'm compiling for JVM-STEPPER"))))


;; TODO: for the stepper, consider move it to a new package
(defun step-handler ())
(defun after-step-handler-1 ())
(defun after-step-handler-2 ())
(defun after-step-handler-3 ())

(export '(*current-backend* with-backend with-backend-error))

(provide 'compiler-backend)
