;;; step.lisp
;;;
;;; Copyright (C) 2004 Peter Graves
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

;;; From SBCL.

(in-package "SYSTEM")

(defparameter *stop-packages* nil
  "List of packages in which the stepper will stop in its external symbols")

(defparameter *stop-symbols* nil
  "List of symbols in which the stepper will stop")

(defun stop-at-symbol-p (symbol)
  "Indicates if the stepper need to stop at the current symbol"
  (or (find symbol *stop-symbols* :test 'eq)
      (some (lambda (package)
                (do-external-symbols (s (find-package package))
                  (if (eq s symbol)
                      (return t))))
            *stop-packages*)))

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
                  (%set-stepper-on)
                  (multiple-value-prog1 ,form
                    (%set-stepper-off)
                    (%set-delimited-stepping-off))))))))
