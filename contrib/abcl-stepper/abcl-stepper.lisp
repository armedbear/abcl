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
           #:start
           #:stop
           #:*stepper-stop-packages*
           #:*stepper-stop-symbols*))

(in-package #:abcl-stepper)

(defparameter *stepper-stop-packages* nil
  "List of packages in which the stepper will stop in its external symbols")

(defparameter *stepper-stop-symbols* nil
  "List of symbols in which the stepper will stop")

(defparameter *stepper-watch-symbols* nil
  "List of symbols in which will be printed in every step")

(defmacro without-active-stepping (&body body)
  `(progn (sys:%set-stepper-off)
          ,@body
          (sys:%set-stepper-on)))

(defun print-stepper-str (string newline)
  "Prints a line using the java method 'System.out.println'"
  (let* ((system-class (java:jclass "java.lang.System"))
         (out-field (java:jfield system-class "out"))
         (println-method (java:jmethod "java.io.PrintStream"
                                       (if newline "println" "print")
                                       "java.lang.String")))
    (java:jcall println-method out-field string)))

(defun pprint-stepper-str (string)
  "Pretty prints a line using the java method 'System.out.println'"
  (print-stepper-str (with-output-to-string (s)
                       (pprint string s))
                     t))

(defun pprint-form-to-step (symbol args step-count)
  (print-stepper-str "We are in the stepper mode" t)
  (print-stepper-str (format nil "Evaluating step ~a -->" step-count) nil)
  (print-stepper-str
   (with-output-to-string (s)
     (pprint `(,symbol ,@args) s))
   t))

(defun add-breakpoint ()
  (print-stepper-str "Type the name of the symbol to use as a breakpoint with next (n): " nil)
  (let* ((symbol-str (read-line))
         (symbol (ignore-errors (read-from-string symbol-str))))
    ;; ensure we found the symbol
    (unless symbol
      (print-stepper-str (format nil "Couldn't find the symbol ~a" symbol-str) t))
    (pushnew symbol *stepper-stop-symbols*)))

(defun remove-breakpoint ()
  (print-stepper-str "Type the name of the breakpoint symbol to remove: " nil)
  (let* ((symbol-str (read-line))
         (symbol (ignore-errors (read-from-string symbol-str))))
    ;; ensure we found the symbol
    (unless symbol
      (print-stepper-str (format nil "Couldn't find the symbol ~a" symbol-str) t))
    (setf *stepper-stop-symbols* (remove symbol *stepper-stop-symbols*))))

(defun remove-all-breakpoints ()
  (setf *stepper-stop-symbols* nil)
  (print-stepper-str "Removed all symbol breakpoints" t))

(defun lookup-symbol (symbol env &optional var-description)
  (let* ((lookup-method (java:jmethod "org.armedbear.lisp.Environment" "lookup" "org.armedbear.lisp.LispObject"))
         (symbol-lookup (java:jcall-raw lookup-method env symbol)))
    (cond ((or (not (java:java-object-p symbol-lookup))
               (not (java:jnull-ref-p symbol-lookup)))
           (print-stepper-str
            (if var-description
                (format nil "~a=~a" symbol symbol-lookup)
                (format nil "~a" symbol-lookup))
            t))
          ((boundp symbol)
           (print-stepper-str
            (if var-description
                (format nil "~a=~a" symbol (symbol-value symbol))
                (format nil "~a" (symbol-value symbol)))
            t))
          (t
           (print-stepper-str (format nil "Couldn't find a value for symbol ~a" symbol) t)))))

(defun inspect-variable (env)
  (print-stepper-str "Type the name of the symbol: " nil)
  (let* ((symbol-str (read-line))
         (symbol (ignore-errors (read-from-string symbol-str))))
    ;; ensure we found the symbol
    (unless symbol
      (print-stepper-str (format nil "Couldn't find the symbol ~a" symbol-str) t)
      (return-from inspect-variable))
    ;; let's try to retrieve the value from the symbol
    (lookup-symbol symbol env)))

(defun print-stepper-help ()
  (print-stepper-str "Type ':l' to see the values of bindings on the local environment" t)
  (print-stepper-str "Type ':c' to resume the evaluation until the end without the stepper" t)
  (print-stepper-str "Type ':n' to resume the evaluation until the next form previously selected to step in" t)
  (print-stepper-str "Type ':s' to step into the form" t)
  (print-stepper-str "Type ':i' to inspect the current value of a variable or symbol" t)
  (print-stepper-str "Type ':b' to add a symbol as a breakpoint to use with next (n)" t)
  (print-stepper-str "Type ':r' to remove a symbol used as a breakpoint with next (n)" t)
  (print-stepper-str "Type ':d' to remove all breakpoints used with next (n)" t)
  (print-stepper-str "Type ':w' to print the value of a binding in all the steps (watch)" t)
  (print-stepper-str "Type ':u' to remove a watched binding (unwatch)" t)
  (print-stepper-str "Type ':bt' to show the backtrace" t)
  (print-stepper-str "Type ':q' to quit the evaluation and return NIL" t))

(defun pprint-list-locals (locals)
  (loop :for pair :in locals
        :do (print-stepper-str (format nil "~a=~a" (car pair) (cdr pair)) t)))

(defun insert-watch-symbol ()
  (print-stepper-str "Type the name of the symbol to watch: " nil)
  (let* ((symbol-str (read-line))
         (symbol (ignore-errors (read-from-string symbol-str))))
    ;; ensure we found the symbol
    (unless symbol
      (print-stepper-str (format nil "Couldn't find the symbol ~a" symbol-str) t)
      (return-from insert-watch-symbol))
    (pushnew symbol *stepper-watch-symbols*)))

(defun remove-watch-symbol ()
  (print-stepper-str "Type the name of the symbol to (un)watch : " nil)
  (let* ((symbol-str (read-line))
         (symbol (ignore-errors (read-from-string symbol-str))))
    ;; ensure we found the symbol
    (unless symbol
      (print-stepper-str (format nil "Couldn't find the symbol ~a" symbol-str) t)
      (return-from remove-watch-symbol))
    (setf *stepper-watch-symbols* (remove symbol *stepper-watch-symbols*))))

(defun step-in-symbol-p (fun object delimited-stepping)
  "Decides if the stepper will be applied to the OBJECT being evaluated and manages the internal
states of the stepper"
  (cond
    ((or
      (and (consp object)
           (eq (car object)
               'CL:MULTIPLE-VALUE-PROG1)
           (equal (car (last object))
                  '(system:%set-delimited-stepping-off)))
      (equal fun #'sys:%set-stepper-off))
     ;; we don't step the expansion of 'step' macro
     nil)
    (delimited-stepping
     ;; Analyze next symbols
     (sys:%set-stepper-off)
     (let* ((function-name
              (ignore-errors (nth-value 2 (function-lambda-expression fun))))
            (stop-at-symbol-p-value
              (and function-name (stop-at-symbol-p function-name))))
       (sys:%set-stepper-on)
       (when stop-at-symbol-p-value
         (sys:%set-delimited-stepping-off)
         t)))
    (t t)))

(defun stop-at-symbol-p (symbol)
  "Indicates if the stepper need to stop at the current symbol"
  (or (find symbol *stepper-stop-symbols* :test 'eq)
      (some (lambda (package)
                (do-external-symbols (s (find-package package))
                  (if (eq s symbol)
                      (return t))))
            *stepper-stop-packages*)))

(defun list-locals (env)
  (print-stepper-str "Showing the values of variable bindings." t)
  (print-stepper-str "From inner to outer scopes:" t)
  (pprint-list-locals (sys:environment-all-variables env))
  (print-stepper-str "Showing the values of function bindings." t)
  (print-stepper-str "From inner to outer scopes:" t)
  (pprint-list-locals (sys:environment-all-functions env)))

(defun print-watched-symbols (env)
  (when *stepper-watch-symbols*
    (print-stepper-str "Watched bindings:" t)
    (loop :for watch-symbol :in *stepper-watch-symbols*
           :do (lookup-symbol watch-symbol env t))))

(defun handle-user-interaction (env)
  (let ((leave-prompt nil)
        (unexpected-input-user nil)
        (char-input-user nil))
    (loop :until leave-prompt
          :do (unless unexpected-input-user
                (print-stepper-str "Type ':?' for a list of options" t)
                (without-active-stepping (print-watched-symbols env)))
              (setf char-input-user (read))
              (clear-input)
              (case char-input-user
                ((:? :help)
                 (without-active-stepping (print-stepper-help)))
                ((:l :locals)
                 (without-active-stepping (list-locals env)))
                ((:c :continue)
                 (sys:%set-stepper-off)
                 (setf leave-prompt t))
                ((:n :next)
                 (sys:%set-delimited-stepping-on)
                 (setf leave-prompt t))
                ((:s :step) (setf leave-prompt t))
                ((:q :quit)
                 (sys:%set-stepper-off)
                 (sys:%set-delimited-stepping-off)
                 (sys:%return-from-stepper))
                ((:i :inspect)
                  (without-active-stepping (inspect-variable env)))
                ((:b :br+ :add-breakpoint)
                 (without-active-stepping (add-breakpoint)))
                ((:r :br- :remove-breakpoint)
                 (without-active-stepping (remove-breakpoint)))
                ((:d :br! :delete-breakpoints)
                 (without-active-stepping (remove-all-breakpoints)))
                ((:w :watch)
                 (without-active-stepping (insert-watch-symbol)))
                ((:u :unwatch)
                 (without-active-stepping (remove-watch-symbol)))
                ((:bt :backtrace)
                 (without-active-stepping
                   ;; we avoid the first 2 entries of the backtrace
                   ;; because they are constant and unrelated to the code
                   ;; being stepped
                   (pprint-stepper-str (subseq (sys:backtrace) 2))))
                (otherwise (setf unexpected-input-user t))))))

(defun in-slime-repl-p ()
  "Determines if we are in Slime/Sly connection"
  (some (lambda (c)
          (and (find-package c)
               (symbol-value (find-symbol "*EMACS-CONNECTION*" c))))
        '(:swank :slynk)))

(defun warn-not-yet-ready ()
  (format t "This stepper is not still ready to be used from an Sly/Slime~
 , but it can be used from a plain REPL~%"))

(defun start ()
  "Starts the stepper. To be used similar to cl:break, but allowing step from the point it is called."
  (cond ((in-slime-repl-p)
         ;; We are in an Sly/Slime connection
         (warn-not-yet-ready))
        (t (print-stepper-str "This function activates the stepper." t)
           (print-stepper-str "Remember to deactivate it after the end of the execution using (stepper:stop)." t)
           (print-stepper-str "To clean its internal flags" t)
           (sys:%initialize-step-counter)
           (sys:%initialize-step-block)
           (sys:%set-stepper-on))))

(defun stop ()
  "Stops the stepper"
  (cond ((in-slime-repl-p)
         ;; We are in an Sly/Slime connection
         (warn-not-yet-ready))
        (t
         (sys:%set-stepper-off)
         (sys:%set-delimited-stepping-off))))

(defmacro step (form)
  (let ((stepper-block (gensym)))
    `(let ()
       (cond ((in-slime-repl-p)
              ;; We are in an Sly/Slime connection
              (warn-not-yet-ready)
              ,form)
             (t (block ,stepper-block
                  (sys:%initialize-step-counter)
                  (sys:%initialize-step-block)
                  (sys:%set-stepper-on)
                  (multiple-value-prog1 ,form
                    (sys:%set-stepper-off)
                    (sys:%set-delimited-stepping-off))))))))

(provide :abcl-stepper)
