;;; boot.lisp
;;;
;;; Copyright (C) 2003-2007 Peter Graves <peter@armedbear.org>
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
;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
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

(sys:%in-package "SYSTEM")

(setq *load-verbose*     nil)
(setq *autoload-verbose* nil)

;; Redefined in macros.lisp.
(defmacro in-package (name)
  (list '%in-package (string name)))

(defmacro lambda (lambda-list &rest body)
  (list 'function (list* 'lambda lambda-list body)))

(defmacro named-lambda (name lambda-list &rest body)
  (list 'function (list* 'named-lambda name lambda-list body)))

;; Redefined in macros.lisp.
(defmacro return (&optional result)
  (list 'return-from nil result))

;; Redefined in precompiler.lisp.
(defmacro defun (name lambda-list &rest body)
  (let ((block-name (fdefinition-block-name name)))
    (list '%defun
          (list 'quote name)
          (list 'lambda lambda-list (list* 'block block-name body)))))

;; Redefined in macros.lisp.
(defmacro defconstant (name initial-value &optional docstring)
  (list '%defconstant (list 'quote name) initial-value docstring))

;; Redefined in macros.lisp.
(defmacro defparameter (name initial-value &optional docstring)
  (list '%defparameter (list 'quote name) initial-value docstring))

(defmacro declare (&rest ignored) nil)

(in-package #:extensions)

(export '(%car %cdr %cadr %caddr))

(defmacro %car (x)
  (list 'car (list 'truly-the 'cons x)))

(defmacro %cdr (x)
  (list 'cdr (list 'truly-the 'cons x)))

(defmacro %cadr (x)
  (list '%car (list '%cdr x)))

(defmacro %caddr (x)
  (list '%car (list '%cdr (list '%cdr x))))

(in-package #:system)

;; Redefined in precompiler.lisp.
(defun eval (form)
  (%eval form))

;; Redefined in pprint.lisp.
(defun terpri (&optional output-stream)
  (%terpri output-stream))

;; Redefined in pprint.lisp.
(defun fresh-line (&optional output-stream)
  (%fresh-line output-stream))

;; Redefined in pprint.lisp.
(defun write-char (character &optional output-stream)
  (%write-char character output-stream))

(in-package #:extensions)

;; Redefined in pprint.lisp.
(defun charpos (stream)
  (sys::stream-charpos stream))

;; Redefined in pprint.lisp.
(defun (setf charpos) (new-value stream)
  (sys::stream-%set-charpos stream new-value))

(export 'charpos '#:extensions)

;; Redefined in precompiler.lisp.
(defun precompile (name &optional definition)
  (declare (ignore name definition))
  nil)

(export 'precompile '#:extensions)

(in-package #:system)

(defun simple-format (destination control-string &rest args)
  (apply *simple-format-function* destination control-string args))

(export 'simple-format '#:system)

;; INVOKE-DEBUGGER is redefined in debug.lisp.
(defun invoke-debugger (condition)
  (sys::%format t "~A~%" condition)
  (ext:quit))

;;Redefined in extensible-sequences.lisp
(defun length (sequence)
  (%length sequence))

(defun elt (sequence index)
  (%elt sequence index))

(defun subseq (sequence start &optional end)
  (sys::%subseq sequence start end))

(defun reverse (sequence)
  (sys::%reverse sequence))

(defun nreverse (sequence)
  (sys::%nreverse sequence))

(load-system-file "autoloads")
(load-system-file "early-defuns")
(load-system-file "backquote")
(load-system-file "destructuring-bind")
(load-system-file "defmacro")
(load-system-file "setf")
(load-system-file "fdefinition")
(load-system-file "featurep")
(load-system-file "read-conditional")
(load-system-file "macros")

;; Redefined in package.lisp
(defun make-package (package-name &key nicknames use)
  (%make-package package-name nicknames use))

(load-system-file "read-circle")

(copy-readtable +standard-readtable+ *readtable*)

;; SYS::%COMPILE is redefined in precompiler.lisp.
(defun sys::%compile (name definition)
  (values (if name name definition) nil nil))

(load-system-file "inline")
(load-system-file "proclaim")
(load-system-file "arrays")
(load-system-file "compiler-macro")
(load-system-file "subtypep")
(load-system-file "typep")
(load-system-file "signal")
(load-system-file "list")
(load-system-file "require")
(load-system-file "extensible-sequences-base")
(load-system-file "sequences")
(load-system-file "error")
(load-system-file "defpackage")
(load-system-file "define-modify-macro")
(load-system-file "defstruct")

;; The actual stream and system-stream classes
;; are created in BuiltInClass.java, however, that code does not
;; set up the structure internals correctly: we wouldn't be able
;; to :include the structure classes. Fix that here.
(defstruct (stream (:constructor nil)
                   (:copier nil)
                   (:predicate nil)))  ;; Predicate STREAMP defined elsewhere
(defstruct (system-stream (:include stream)
                          (:constructor nil)
                          (:copier nil)))

(load-system-file "restart")
(load-system-file "late-setf")
(load-system-file "debug")
(load-system-file "print")
(load-system-file "pprint-dispatch")
(load-system-file "defsetf")
(load-system-file "package")

(unless (featurep :j)
  (unless *noinform*
    (%format t "Startup completed in ~A seconds.~%"
             (float (/ (ext:uptime) 1000)))))


