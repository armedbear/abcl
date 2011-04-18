;;; run-program.lisp
;;;
;;; Copyright (C) 2011 Alessio Stalla
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

(in-package "SYSTEM")

(require "JAVA")

;;Vaguely inspired by sb-ext:run-program in SBCL. See <http://www.sbcl.org/manual/Running-external-programs.html>. This implementation uses the JVM facilities for running external processes: <http://download.oracle.com/javase/6/docs/api/java/lang/ProcessBuilder.html>.
(defun run-program (program args &key environment (wait t))
  ;;For documentation, see below.
  (let ((pb (%make-process-builder program args)))
    (when environment
      (let ((env-map (%process-builder-environment pb)))
        (dolist (entry environment)
          (%process-builder-env-put env-map
                                    (princ-to-string (car entry))
                                    (princ-to-string (cdr entry))))))
    (let ((process (make-process (%process-builder-start pb))))
      (when wait (process-wait process))
      process)))

(setf (documentation 'run-program 'function)
      "run-program creates a new process specified by the program argument. args are the standard arguments that can be passed to a program. For no arguments, use nil (which means that just the name of the program is passed as arg 0).

run-program will return a process structure.

Notes about Unix environments (as in the :environment):

    * The ABCL implementation of run-program, like SBCL, Perl and many other programs, copies the Unix environment by default.
    * Running Unix programs from a setuid process, or in any other situation where the Unix environment is under the control of someone else, is a mother lode of security problems. If you are contemplating doing this, read about it first. (The Perl community has a lot of good documentation about this and other security issues in script-like programs.)

The &key arguments have the following meanings:

:environment
    a alist of STRINGs (name . value) describing the new environment. The default is to copy the environment of the current process.
:wait
    If non-NIL (default), wait until the created process finishes. If nil, continue running Lisp until the program finishes.")

;;The process structure.

(defstruct (process (:constructor %make-process (jprocess)))
  jprocess input output error)

(defun make-process (proc)
  (let ((process (%make-process proc)))
    (setf (process-input process) (%make-process-input-stream proc))
    (setf (process-output process) (%make-process-output-stream proc))
    (setf (process-error process) (%make-process-error-stream proc))
    process))

(defun process-alive-p (process)
  "Return t if process is still alive, nil otherwise."
  (%process-alive-p (process-jprocess process)))

(defun process-wait (process)
  "Wait for process to quit running for some reason."
  (%process-wait (process-jprocess process)))

(defun process-exit-code (instance)
  "The exit code of a process."
  (%process-exit-code (process-jprocess instance)))

(defun process-kill (process)
  "Kills the process."
  (%process-kill (process-jprocess process)))

;;Low-level functions. For now they're just a refactoring of the initial implementation with direct
;;jnew & jcall forms in the code. As per Ville's suggestion, these should really be implemented as
;;primitives.

(defun %make-process-builder (program args)
  (java:jnew "java.lang.ProcessBuilder"
             (java:jnew-array-from-list "java.lang.String" (cons program args))))

(defun %process-builder-environment (pb)
  (java:jcall "environment" pb))

(defun %process-builder-env-put (env-map key value)
  (java:jcall "put" env-map key value))

(defun %process-builder-start (pb)
  (java:jcall "start" pb))

(defun %make-process-input-stream (proc)
  (java:jnew "org.armedbear.lisp.Stream" 'system-stream
             (java:jcall "getOutputStream" proc) ;;not a typo!
             'character))

(defun %make-process-output-stream (proc)
  (java:jnew "org.armedbear.lisp.Stream" 'system-stream
             (java:jcall "getInputStream" proc) ;;not a typo|
             'character))

(defun %make-process-error-stream (proc)
  (java:jnew "org.armedbear.lisp.Stream" 'system-stream
             (java:jcall "getErrorStream" proc)
             'character))

(defun %process-alive-p (jprocess)
  (not (ignore-errors (java:jcall "exitValue" jprocess))))

(defun %process-wait (jprocess)
  (java:jcall "waitFor" jprocess))

(defun %process-exit-code (jprocess)
  (ignore-errors (java:jcall "exitValue" jprocess)))

(defun %process-kill (jprocess)
  (java:jcall "destroy" jprocess))