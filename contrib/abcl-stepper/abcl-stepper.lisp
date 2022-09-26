;;; This file is part of ABCL contrib
;;;
;;; Copyright (C) 2023 Alejandro Zamora Fonseca <ale2014.zamora@gmail.com>

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
;;;
;;; As a special exception, the copyright holders of this library give you
;;; permission to link this library with independent modules to produce an
;;; executable, regardless of the license terms of these independent
;;; modules, and to copy and distribute the resulting executable under
;;; terms of your choice, provided that you also meet, for each linked
;;; independent module, the terms and conditions of the license of that
;;; module.  An independent module is a module which is not derived from
;;; or based on this library.  If you modify this library, you may extend
;;; this exception to your version of the library, but you are not
;;; obligated to do so.  If you do not wish to do so, delete this
;;; exception statement from your version.

(defpackage #:abcl-stepper
  (:use :cl)
  (:nicknames #:stepper)
  (:shadow #:step)
  (:export #:step
           #:*stepper-stop-packages*
           #:*stepper-stop-symbols*))

(in-package #:abcl-stepper)

(defparameter *stepper-stop-packages* nil
  "List of packages in which the stepper will stop in its external symbols")

(defparameter *stepper-stop-symbols* nil
  "List of symbols in which the stepper will stop")

(defun print-stepper-str (string newline)
  "Prints a line using the java method 'System.out.println'"
  (let* ((system-class (java:jclass "java.lang.System"))
         (out-field (java:jfield system-class "out"))
         (println-method (java:jmethod "java.io.PrintStream"
                                       (if newline "println" "print")
                                       "java.lang.String")))
    (java:jcall println-method out-field string)))

(defun print-stepper-help ()
  (print-stepper-str "Type 'l' to see the values of bindings on the local environment" t)
  (print-stepper-str "Type 'c' to resume the evaluation until the end without the stepper" t)
  (print-stepper-str "Type 'n' to resume the evaluation until the next form previously selected to step in" t)
  (print-stepper-str "Type 's' to step into the form" t)
  (print-stepper-str "Type 'i' to inspect the current value of a variable or symbol" t)
  (print-stepper-str "Type 'q' to quit the evaluation and return NIL" t))

(defun pprint-list-locals (locals)
  (loop :for pair :in locals
        :do (print-stepper-str (format nil "~a=~a" (car pair) (cdr pair)) t)))

(defun stop-at-symbol-p (symbol)
  "Indicates if the stepper need to stop at the current symbol"
  (or (find symbol *stepper-stop-symbols* :test 'eq)
      (some (lambda (package)
                (do-external-symbols (s (find-package package))
                  (if (eq s symbol)
                      (return t))))
            *stepper-stop-packages*)))

(defmacro step (form)
  (let ((stepper-block (gensym)))
    `(let ()
       (cond ((some (lambda (c)
                      (and (find-package c)
                           (symbol-value (find-symbol "*EMACS-CONNECTION*" c))))
                    '(:swank :slynk))
              ;; We are in an Sly/Slime connection
              (format
               t
               "This stepper is not still ready to be used from an Sly/Slime~
 , but it can be used from a plain REPL")
              ,form)
             (t (block ,stepper-block
                  (sys:%set-stepper-on)
                  (multiple-value-prog1 ,form
                    (sys:%set-stepper-off)
                    (sys:%set-delimited-stepping-off))))))))

(provide :abcl-stepper)
