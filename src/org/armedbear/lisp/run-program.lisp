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
(in-package :system)

(require :java)

(defparameter *implementations*
  '(:java-1.6 :java-1.7 :java-1.8)) ;; UNUSED
(defun not-java-6 ()
  (not (find :java-1.6 *features*)))

(export '(run-program process process-p process-input process-output
          process-error process-alive-p process-wait process-exit-code
          process-kill process-pid))

;;; Vaguely inspired by sb-ext:run-program in SBCL.
;;;
;;; See <http://www.sbcl.org/manual/Running-external-programs.html>.
;;;
;;; This implementation uses the JVM facilities for running external
;;; processes.
;;; <http://download.oracle.com/javase/6/docs/api/java/lang/ProcessBuilder.html>.
(defun run-program (program args &key environment (wait t) clear-environment
                                      (input :stream) (output :stream) (error :stream)
                                      if-input-does-not-exist (if-output-exists :error)
                                      (if-error-exists :error) directory)
  "Run PROGRAM with ARGS in with ENVIRONMENT variables.

Possibly WAIT for subprocess to exit.

Optionally CLEAR-ENVIRONMENT of the subprocess of any non specified values.

Creates a new process running the the PROGRAM.

ARGS are a list of strings to be passed to the program as arguments.

For no arguments, use nil which means that just the name of the
program is passed as arg 0.

Returns a process structure containing the JAVA-OBJECT wrapped Process
object, and the PROCESS-INPUT, PROCESS-OUTPUT, and PROCESS-ERROR streams.

c.f. http://download.oracle.com/javase/6/docs/api/java/lang/Process.html

Notes about Unix environments (as in the :environment):

    * The ABCL implementation of run-program, like SBCL, Perl and many
      other programs, copies the Unix environment by default.

    * Running Unix programs from a setuid process, or in any other
      situation where the Unix environment is under the control of
      someone else, is a mother lode of security problems. If you are
      contemplating doing this, read about it first. (The Perl
      community has a lot of good documentation about this and other
      security issues in script-like programs.

The &key arguments have the following meanings:

:environment
    An alist of STRINGs (name . value) describing new
    environment values that replace existing ones.

:clear-environment
    If non-NIL, the current environment is cleared before the
    values supplied by :environment are inserted.

:wait
    If non-NIL, which is the default, wait until the created process
    finishes. If NIL, continue running Lisp until the program
    finishes.

:input
    If T, I/O is inherited from the Java process. If NIL, /dev/null is used
    (nul on Windows). If a PATHNAME designator other than a stream is
    supplied, input will be read from that file. If set to :STREAM, a stream
    will be available via PROCESS-INPUT to read from. Defaults to :STREAM.

:if-input-does-not-exist
    If :input points to a non-existing file, this may be set to :ERROR in
    order to signal an error, :CREATE to create and read from an empty file,
    or NIL to immediately NIL instead of creating the process.
    Defaults to NIL.

:output
    If T, I/O is inherited from the Java process. If NIL, /dev/null is used
    (nul on Windows). If a PATHNAME designator other than a stream is
    supplied, output will be redirect to that file. If set to :STREAM, a
    stream will be available via PROCESS-OUTPUT to write to.
    Defaults to :STREAM.

:if-output-exists
    If :output points to a non-existing file, this may be set to :ERROR in
    order to signal an error, :SUPERSEDE to supersede the existing file,
    :APPEND to append to it instead, or NIL to immediately NIL instead of
    creating the process. Defaults to :ERROR.

:error
    Same as :output, but can also be :output, in which case the error stream
    is redirected to wherever the standard output stream goes.
    Defaults to :STREAM.

:if-error-exists
    Same as :if-output-exists, but for the :error target.

:directory
    If set will become the working directory for the new process, otherwise
    the working directory will be unchanged from the current Java process.
    Defaults to NIL.
"
  (let* ((program-namestring (namestring (pathname program)))
         (process-builder (%make-process-builder program-namestring args)))
    (let ((env-map (%process-builder-environment process-builder)))
      (when clear-environment
        (%process-builder-env-clear env-map))
      (when environment
        (dolist (entry environment)
          (%process-builder-env-put env-map
                                    (princ-to-string (car entry))
                                    (princ-to-string (cdr entry))))))
    (let ((input-stream-p (eq input :stream))
          (output-stream-p (eq output :stream))
          (error-stream-p (eq error :stream))
          output-redirection
          input-redirection
          error-redirection)
      (unless output-stream-p
        (unless (setf output-redirection
                      (setup-output-redirection process-builder output NIL if-output-exists))
          (return-from run-program)))
      (if (eq error :output)
          (java:jcall "redirectErrorStream" process-builder T)
          (unless error-stream-p
            (unless (setf error-redirection
                          (setup-output-redirection process-builder error T if-error-exists))
              (return-from run-program))))
      (unless input-stream-p
        (unless (setf input-redirection
                      (setup-input-redirection process-builder input if-input-does-not-exist))
          (return-from run-program)))
      (when directory
        (java:jcall "directory" process-builder (java:jnew "java.io.File" (namestring directory))))
      (let ((process 
             (if (not-java-6)
                 (make-process (%process-builder-start process-builder)
                               input-stream-p output-stream-p error-stream-p)
                 (make-process (%process-builder-start process-builder)
                               t t t))))
        (when (find :java-1.6 *features*)
          (when input-redirection
            (let ((input (process-input process)))
              (threads:make-thread (lambda () (from-file input-redirection input)))))
          (when output-redirection
            (let ((output (process-output process))
                  (file (first output-redirection))
                  (appendp (second output-redirection)))
              (threads:make-thread (lambda () (to-file output file :append appendp)))))
          (when error-redirection
            (let ((error (process-error process))
                  (file (first output-redirection))
                  (appendp (second output-redirection)))
              (threads:make-thread (lambda () (to-file error file :append appendp))))))
        (when (or wait
                  (not-java-6)
                  (process-wait process))
          process)))))

(defconstant +inherit+
  (ignore-errors
    (java:jfield "java.lang.ProcessBuilder$Redirect" "INHERIT")))

(defun coerce-to-file (value)
  (java:jnew
   "java.io.File"
   (if value
       (namestring value)
       (cond
         ((ext:os-unix-p)
          "/dev/null")
         ((ext:os-windows-p) 
          "nul")
         (t
          (error "Don't know how to set up null stream on this platform."))))))

(define-condition implementation-not-available (error)
  ((missing :initarg :missing
            :reader missing))
  (:report (lambda (condition stream)
             (format stream "This JVM is missing the ~a implementation." (missing condition)))))

(defun setup-input-redirection (process-builder value if-does-not-exist)
  "Returns boolean truth when input redirections has been successfully set up.

As a second value, returns either nil if input should inherit from the
parent process, or a java.io.File reference to the file to read input from."
  (let ((redirect (if (eq value T)
                      ;; Either inherit stdio or fail
                      (if (not-java-6)
                          +inherit+
                          (signal 'implementation-not-available
                                  :missing "Inheritance for subprocess of standard input"))
                      ;; or read from a file
                      (let ((file (coerce-to-file value)))
                        (when value
                          (if (eq if-does-not-exist :create)
                              (open value :direction :probe :if-does-not-exist :create)
                              (unless (probe-file value)
                                (ecase if-does-not-exist
                                  (:error
                                   (error "Input file ~S does not already exist." value))
                                  ((NIL)
                                   (return-from setup-input-redirection))))))
                        (if (not-java-6)
                            (java:jstatic "from" "java.lang.ProcessBuilder$Redirect" file)
                            file)))))
    (when (not-java-6)
      (java:jcall "redirectInput" process-builder redirect))
    redirect))

#|
value
  t   inherit from
|#
(defun setup-output-redirection (process-builder value errorp if-does-exist)
  (let ((redirect (if (eq value T)
                      (if (not-java-6)
                          +inherit+
                          (if errorp
                              (signal 'implementation-not-available
                                      :missing "Inheritance for subprocess of standard error")
                              (signal 'implementation-not-available
                                      :missing "Inheritance for subprocess of standard output")))
                      (let ((file (coerce-to-file value))
                            appendp)
                        (when (and value (probe-file value))
                          (ecase if-does-exist
                            (:error (error "Output file ~S does already exist." value))
                            (:supersede
                             (with-open-file (f value
                                                :direction :output
                                                :if-exists if-does-exist)))
                            (:append (setf appendp T))
                            ((NIL) (return-from setup-output-redirection))))
			(if (not-java-6)
			  (if appendp
			      (java:jstatic "appendTo" "java.lang.ProcessBuilder$Redirect" file)
			      (java:jstatic "to" "java.lang.ProcessBuilder$Redirect" file))
			  (list file appendp))))))
    (when (not-java-6)
      (if errorp
	  (java:jcall "redirectError" process-builder redirect)
	  (java:jcall "redirectOutput" process-builder redirect)))
    redirect))

;;; The process structure.
(defstruct (process (:constructor %make-process (jprocess)))
  jprocess %input %output %error)

(defun make-process (proc inputp outputp errorp)
  (let ((process (%make-process proc)))
    (when inputp
      (setf (process-%input process) (%make-process-input-stream proc)))
    (when outputp
      (setf (process-%output process) (%make-process-output-stream proc)))
    (when errorp
      (setf (process-%error process) (%make-process-error-stream proc)))
    process))

(defun process-input (process)
  (process-%input process))

(defun process-output (process)
  (process-%output process))

(defun process-error (process)
  (process-%error process))

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

(defun process-pid (process)
  "Return the process ID."
  (%process-pid (process-jprocess process)))

;;; Low-level functions. For now they're just a refactoring of the
;;; initial implementation with direct jnew & jcall forms in the
;;; code. As per Ville's suggestion, these should really be implemented
;;; as primitives.
(defun %make-process-builder (program args)
  (java:jnew "java.lang.ProcessBuilder"
             (java:jnew-array-from-list "java.lang.String" (cons program args))))

(defun %process-builder-environment (pb)
  (java:jcall "environment" pb))

(defun %process-builder-env-put (env-map key value)
  (java:jcall "put" env-map key value))

(defun %process-builder-env-clear (env-map)
  (java:jcall "clear" env-map))

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

(defun %process-pid (jprocess)
  (if (ext:os-unix-p)
      ;; TODO: memoize this 
      (let ((field (java:jcall "getDeclaredField" (java:jclass "java.lang.UNIXProcess") "pid")))
        (java:jcall "setAccessible" field java:+true+)
        (java:jcall "get" field jprocess))
      (error "Can't retrieve PID on this platform.")))

(defun %process-kill (jprocess)
  (java:jcall "destroy" jprocess))

(defun to-file (input java.io.file &key (append nil))
  (declare (ignore append)) ;; FIXME
  (let ((file (java:jcall "toString" java.io.file)))
    (with-open-file (s file
                       :direction :output
                       :element-type (stream-element-type input))
      (let ((buffer (make-array 8192 :element-type (stream-element-type input))))
        (loop
           :for bytes-read = (read-sequence buffer input)
           :while (plusp bytes-read)
           :do (write-sequence buffer s :end bytes-read)))))
  (close input))

(defun from-file (java.io.file output)
  (let ((file (java:jcall "toString" java.io.file)))
    (with-open-file (s file
                       :direction :input
                       :element-type (stream-element-type output))
      (let ((buffer (make-array 8192 :element-type (stream-element-type output))))
        (loop
           :for bytes-read = (read-sequence buffer s)
           :while (plusp bytes-read)
           :do (write-sequence buffer output :end bytes-read))))
    (close output)))
  
#|
tests

(uiop:run-program "uname -a" :output :string)

(uiop:run-program "cat /etc/passwd" :output :string)

|#
