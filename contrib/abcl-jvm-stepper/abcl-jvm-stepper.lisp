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

(defpackage #:abcl-jvm-stepper
  (:use :cl)
  (:nicknames #:jvm-stepper #:jstepper)
  (:shadow #:step)
  (:export #:step
           #:instrument-compile-form
           #:instrument-compile-call
           #:instrument-compile-function))


(in-package :abcl-jvm-stepper)


(defparameter *global-stop* nil)


(defparameter *delimited-stop* nil)


(defparameter *seen-env-pairs* nil)


(defparameter *step-counter* 0)


(defparameter *step-counter-stacktrace* nil)


(defparameter *stepper-stop-packages* nil
  "List of packages in which the stepper will stop in its external symbols")


(defparameter *stepper-stop-symbols* nil
  "List of symbols in which the stepper will stop")


(defparameter *stepper-watch-symbols* nil
  "List of symbols in which will be printed in every step")


(defparameter *step-next-active* nil)


(defparameter *env* nil)


(defun print-help ()
  (format t "Type ':g' to see the values of symbols of the current package ~%")
  (format t "Type ':c' to resume the evaluation until the end without the stepper~%")
  (format t "Type ':n' to resume the evaluation until the next form previously selected to step in~%")
  (format t "Type ':s' to step into the form~%")
  (format t "Type ':sn' to step to the next form~%")
  (format t "Type ':i' to inspect the current value of a global variable or symbol~%")
  (format t "Type ':b' to add a symbol as a breakpoint to use with next (n)~%")
  (format t "Type ':r' to remove a symbol used as a breakpoint with next (n)~%")
  (format t "Type ':d' to remove all breakpoints used with next (n)~%")
  (format t "Type ':w' to print the value of a global binding in all the steps (watch)~%")
  (format t "Type ':u' to remove a watched binding (unwatch)~%")
  (format t "Type ':bt' to show the backtrace~%")
  (format t "Type ':q' to quit the evaluation and return NIL~%"))


(defun print-global-vars (current-symbol)
  (do-symbols  (symbol (symbol-package current-symbol))
    (unless (find (symbol-package symbol)
                  (list #.(find-package :java)
                        #.(find-package :system)
                        #.(find-package :cl)
                        #.(find-package :ext)))
      (ignore-errors
       (let ((symbol-name (symbol-name symbol))
             (symbol-value (symbol-value symbol)))
         (when symbol
           (format t "~a=~a~%" symbol-name symbol-value)))))))


(defmacro with-user-symbol (prompt &body action)
  `(progn
     (format t ,prompt)
     (finish-output)
     (let* ((symbol-str (read-line))
            (symbol (ignore-errors (read-from-string symbol-str))))
       ;; ensure we found the symbol
       (unless symbol
         (format t "Couldn't find the symbol ~a~%" symbol-str))
       (handler-case
           (when symbol
             ,@action)
         (error (e) (print e) (terpri))))))


(defun step-in-symbol-p (current-symbol)
  (and
   (not *step-next-active*)
   (or (not *delimited-stop*)
       (and *delimited-stop*
           (or (find current-symbol *stepper-stop-symbols*)
               (some (lambda (package)
                       (do-external-symbols (s (find-package package))
                         (if (eq s current-symbol)
                             (return t))))
                     *stepper-stop-packages*))))))


(defun c-backend::step-handler ()
  (when *global-stop*
    (let* ((backtrace (sys:backtrace))
           (current-stack-trace (nth 1 backtrace))
           (curr-stack-trace-list (java:jcall "toLispList" current-stack-trace))
           (current-symbol (car curr-stack-trace-list)))
      (when (step-in-symbol-p current-symbol)
        (setf (gethash current-stack-trace *step-counter-stacktrace*) (list (incf *step-counter*) current-symbol))
        (format t "~%We are in the stepper mode~%")
        (format t "Evaluating step ~a -->~%~a~%" *step-counter* curr-stack-trace-list)
        (loop :with leave-prompt = nil
                :until leave-prompt
                :do (format t "Type ':?' for a list of options~%")
                    (when *stepper-watch-symbols*
                      (format t "Watched bindings:~%")
                      (loop :for watched-symbol :in *stepper-watch-symbols*
                            :do (handler-case
                                    (format t "~a = ~a~%"
                                            (symbol-name watched-symbol)
                                            (symbol-value watched-symbol))
                                  (error (e) (declare (ignore e))
                                    (format t "Couldn't find a value for symbol ~a~%" watched-symbol)))))
                    (case (read)
                      ((:help :?)
                       (print-help))
                      ((:n :next)
                       (setf *delimited-stop* t)
                       (setf leave-prompt t))
                      ((:sn :step-next)
                       (setf *step-next-active* t)
                       (setf *delimited-stop* nil)
                       (setf leave-prompt t))
                      ((:w :watch)
                       (with-user-symbol
                           "Type the name of the symbol to watch: "
                         (pushnew symbol *stepper-watch-symbols*)))
                      ((:u :unwatch)
                       (with-user-symbol
                           "Type the name of the symbol to (un)watch: "
                         (setf *stepper-watch-symbols*
                               (remove symbol *stepper-watch-symbols*))))
                      ((:b :br+ :add-breakpoint)
                       (with-user-symbol
                           "Type the name of the symbol to use as a breakpoint with next (n): "
                         (pushnew symbol *stepper-stop-symbols*)))
                      ((:r :br- :remove-breakpoint)
                       (with-user-symbol
                           "Type the name of the breakpoint symbol to remove: "
                         (setf *stepper-stop-symbols*
                               (remove symbol *stepper-stop-symbols*))))
                      ((:d :br! :delete-breakpoints)
                       (setf *stepper-stop-symbols* nil)
                       (format t "Removed all symbol breakpoints~%"))
                      ((:inspect :i)
                       (with-user-symbol "Enter the symbol to inspect: "
                         (print (eval symbol))
                         (terpri)))
                      ((:continue :c) (setf *global-stop* nil) (setf leave-prompt t))
                      ((:step :s) (setf *delimited-stop* nil) (setf leave-prompt t))
                      ((:globals :g)
                       (when (symbolp current-symbol)
                         ;; it won't work on compiled lambda functions
                         (print-global-vars current-symbol)))
                      ((:backtrace :bt)
                       (pprint (subseq backtrace 1))
                       (terpri))
                      ((:quit :q)
                       (sys:%return-from-environment *env*))
                      (otherwise nil)))))))


(defun c-backend::get-stack-trace-data ()
  (let ((current-stack-trace (nth 1 (sys:backtrace))))
    (and *step-counter-stacktrace*
         (gethash current-stack-trace *step-counter-stacktrace*))))


(defun c-backend::after-step-handler-1 ()
  (format t "step "))


(defun c-backend::after-step-handler-2 ()
  (format t " ==> value: "))


(defun c-backend::after-step-handler-3 ()
  (setf *step-next-active* nil)
  (terpri))


(defun get-current-env ()
  (let* ((env-stack (java:jcall "getEnvStack" (threads:current-thread)))
         (env-array (java:jcall "toArray" env-stack))
         (curr-env (find-if
                    (lambda (env)
                      (let ((env-vars (sys:environment-all-variables env)))
                        (when (and env-vars (first (car env-vars)))
                          (eq (second (car env-vars)) 'jvm-stepper:step))))
                    env-array)))
    curr-env))


(defmacro instrument-compile-form (form)
  `(let ((c-backend:*current-backend* :jvm-stepper))
     (compile nil (lambda () ,form))))


(defmacro instrument-compile-function (function)
  `(let ((c-backend:*current-backend* :jvm-stepper))
     (compile ,function)))


(defmacro instrument-compile-call (form)
  `(let ((c-backend:*current-backend* :jvm-stepper))
     ,form))


(defmacro step (form)
  (let ((stepper-block (gensym)))
    `(let ((*global-stop* t)
           (*delimited-stop* nil)
           (*step-counter* 0)
           (*step-counter-stacktrace* (make-hash-table :test 'eql))
           (*step-next-active* nil)
           (*env* (get-current-env)))
       (block ,stepper-block
         ,form))))
