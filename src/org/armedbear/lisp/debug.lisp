;;; debug.lisp
;;;
;;; Copyright (C) 2003-2007 Peter Graves
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

;;; Adapted from SBCL.

(in-package #:extensions)

(export '(*debug-condition* *debug-level* show-restarts))

(defvar *debug-condition* nil)

(defvar *debug-level* 0)

(in-package #:system)

(defun show-restarts (restarts stream)
  (when restarts
    (fresh-line stream)
    (%format stream "Restarts:~%")
    (let ((max-name-len 0))
      (dolist (restart restarts)
        (let ((name (restart-name restart)))
          (when name
            (let ((len (length (princ-to-string name))))
              (when (> len max-name-len)
                (setf max-name-len len))))))
      (let ((count 0))
        (dolist (restart restarts)
          (let ((name (restart-name restart))
                (report-function (restart-report-function restart)))
            (%format stream "  ~D: ~A" count name)
            (when (functionp report-function)
              (dotimes (i (1+ (- max-name-len (length (princ-to-string name)))))
                (write-char #\space stream))
              (funcall report-function stream))
            (terpri stream))
          (incf count))))))

(defun internal-debug ()
  (if (fboundp 'tpl::repl)
      (let* ((current-debug-io
              (if (typep *debug-io* 'synonym-stream)
                  (symbol-value (synonym-stream-symbol *debug-io*))
                  *debug-io*))
             (in (two-way-stream-input-stream current-debug-io))
             (out (two-way-stream-output-stream current-debug-io)))
        (loop
          (tpl::repl in out)))
      (quit)))

(defun debug-loop ()
  (let ((*debug-level* (1+ *debug-level*)))
    (show-restarts (compute-restarts) *debug-io*)
    (internal-debug)))

(defun invoke-debugger-report-condition (condition)
  (when condition
    (fresh-line *debug-io*)
    (with-standard-io-syntax
      (let ((*print-structure* nil)
	    (*print-readably* nil))
        (when (and *load-truename* (streamp *load-stream*))
          (simple-format *debug-io*
                         "Error loading ~A at line ~D (offset ~D)~%"
                         *load-truename*
                         (stream-line-number *load-stream*)
                         (stream-offset *load-stream*)))
        (simple-format *debug-io*
                       (if (fboundp 'tpl::repl)
                           "~S: Debugger invoked on condition of type ~A~%"
                           "~S: Unhandled condition of type ~A:~%")
                       (threads:current-thread)
                       (type-of condition))
        (simple-format *debug-io* "  ~A~%" condition)))))

(declaim (inline run-hook))
(defun run-hook (hook &rest args)
  (let ((hook-function (symbol-value hook)))
    (when hook-function
      (progv (list hook) (list nil)
        (apply hook-function args)))))

(defvar *invoke-debugger-hook* nil
  "Like *DEBUGGER-HOOK* but observed by INVOKE-DEBUGGER even when
called by BREAK. This hook is run before *DEBUGGER-HOOK*.")

;;; We run *INVOKE-DEBUGGER-HOOK* before *DEBUGGER-HOOK* because SBCL
;;; does so, too, and for good reason: This way, you can specify
;;; default debugger behaviour that trumps over whatever the users
;;; wants to do with *DEBUGGER-HOOK*.
(defun invoke-debugger (condition)
  (let ((*saved-backtrace* (sys:backtrace)))
    (run-hook '*invoke-debugger-hook* condition *invoke-debugger-hook*)
    (run-hook '*debugger-hook*        condition *debugger-hook*)
    (invoke-debugger-report-condition condition)
    (unless (fboundp 'tpl::repl)
      (quit))
    (let ((original-package *package*))
      (with-standard-io-syntax
        (let ((*package* original-package)
              (*print-readably* nil)    ; Top-level default.
              (*print-structure* nil)
              (*debug-condition* condition)
              (level *debug-level*))
          (clear-input *debug-io*)
          (if (> level 0)
              (with-simple-restart (abort "Return to debug level ~D." level)
                (debug-loop))
              (debug-loop)))))))

(defun break (&optional (format-control "BREAK called") &rest format-arguments)
  (let ((*debugger-hook* nil)) ; Specifically required by ANSI.
    (with-simple-restart (continue "Return from BREAK.")
      (invoke-debugger
       (%make-condition 'simple-condition
                        (list :format-control format-control
                              :format-arguments format-arguments))))
    nil))

(defun backtrace-as-list (&optional (n 0))
  "Return BACKTRACE with each element converted to a list."
  (mapcar #'sys::frame-to-list (sys:backtrace n)))
