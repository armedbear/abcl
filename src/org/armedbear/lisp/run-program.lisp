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
  (let ((pb (java:jnew "java.lang.ProcessBuilder"
                       (java:jnew-array-from-list "java.lang.String" (cons program args)))))
    (when environment
      (let ((env-map (java:jcall "environment" pb)))
        (dolist (entry environment)
          (java:jcall "put" env-map
                 (princ-to-string (car entry))
                 (princ-to-string (cdr entry))))))
    (let ((process (make-process (java:jcall "start" pb))))
      (when wait (process-wait process))
      process)))

;;The process structure.

(defstruct (process (:constructor %make-process (jprocess)))
  jprocess input output error)

(defun make-process (proc)
  (let ((process (%make-process proc)))
    (setf (process-input process)
          (java:jnew "org.armedbear.lisp.Stream" 'system-stream
                     (java:jcall "getOutputStream" proc)
                     'character)) ;;not a typo!
    (setf (process-output process)
          (java:jnew "org.armedbear.lisp.Stream" 'system-stream
                (java:jcall "getInputStream" proc) ;;not a typo|
                'character))
    (setf (process-error process)
          (java:jnew "org.armedbear.lisp.Stream" 'system-stream
                (java:jcall "getErrorStream" proc)
                'character))
    process))

(defun process-alive-p (process)
  "Return t if process is still alive, nil otherwise."
  (not (ignore-errors (java:jcall "exitValue" (process-jprocess process)))))

(defun process-wait (process)
  "Wait for process to quit running for some reason."
  (java:jcall "waitFor" (process-jprocess process)))

(defun process-exit-code (instance)
  "The exit code of a process."
  (ignore-errors (java:jcall "exitValue" (process-jprocess instance))))

(defun process-kill (process)
  "Kills the process."
  (java:jcall "destroy" (process-jprocess process)))

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
