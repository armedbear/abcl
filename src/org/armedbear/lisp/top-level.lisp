;;; top-level.lisp
;;;
;;; Copyright (C) 2003-2006 Peter Graves
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

;;; Adapted from SB-ACLREPL (originally written by Kevin Rosenberg).

(in-package #:top-level)

(require 'inspect)

(defvar *null-cmd* (gensym))
(defvar *handled-cmd* (gensym))

(defvar *command-char* #\:)

(defvar *cmd-number* 1
  "Number of the next command")

(defun prompt-package-name ()
  (let ((result (package-name *package*)))
    (dolist (nickname (package-nicknames *package*))
      (when (< (length nickname) (length result))
        (setf result nickname)))
    result))

(defun repl-prompt-fun (stream)
  (fresh-line stream)
  (when (> *debug-level* 0)
    (sys::%format stream "[~D~A] "
             *debug-level*
             (if sys::*inspect-break* "i" "")))
  (sys::%format stream "~A(~D): " (prompt-package-name) *cmd-number*))

(defparameter *repl-prompt-fun* #'repl-prompt-fun)

(defun peek-char-non-whitespace (stream)
  (loop
    (let ((c (read-char stream nil)))
      (when (null c) ; control d
        (quit))
      (unless (eql c #\space)
        (unread-char c stream)
        (return c)))))

(defun apropos-command (args)
  (when args (apropos args)))

(defun continue-command (args)
  (when args
    (let ((n (read-from-string args)))
      (let ((restarts (compute-restarts)))
        (when (< -1 n (length restarts))
          (invoke-restart-interactively (nth n restarts)))))))

(defun describe-command (args)
  (let ((obj (eval (read-from-string args))))
    (describe obj)))

(defun error-command (ignored)
  (declare (ignore ignored))
  (when *debug-condition*
    (let* ((s (sys::%format nil "~A" *debug-condition*))
           (len (length s)))
      (when (plusp len)
        (setf (schar s 0) (char-upcase (schar s 0)))
        (unless (eql (schar s (1- len)) #\.)
          (setf s (concatenate 'string s "."))))
      (sys::%format *debug-io* "~A~%" s))
    (show-restarts (compute-restarts) *debug-io*)))

(defun print-frame (frame stream &key prefix)
  (when prefix
    (write-string prefix stream))
  (etypecase frame
    (sys::lisp-stack-frame
     (let ((frame (sys:frame-to-list frame)))
       (pprint-logical-block (stream nil :prefix "(" :suffix ")")
         (ignore-errors
           (prin1 (car frame) stream)
           (let ((args (cdr frame)))
             (if (listp args)
                 (format stream "~{ ~_~S~}" args)
                 (format stream " ~S" args)))))))
    (sys::java-stack-frame
     (write-string (sys:frame-to-string frame) stream))))


(defun backtrace-command (args)
  (let ((count (or (and args (ignore-errors (parse-integer args)))
                   8))
        (n 0))
    (with-standard-io-syntax
      (let ((*print-pretty* t)
            (*print-readably* nil)
            (*print-structure* nil)
            (*print-array* nil))
        (dolist (frame *saved-backtrace*)
          (fresh-line *debug-io*)
          (print-frame frame *debug-io* :prefix (format nil "~3D: " n))
          (incf n)
          (when (>= n count)
            (fresh-line *debug-io*)
            (return))))))
  (fresh-line *debug-io*)
  (values))

(defun frame-command (args)
  (let* ((n (or (and args (ignore-errors (parse-integer args)))
                0))
         (frame (nth n *saved-backtrace*)))
    (when frame
      (with-standard-io-syntax
        (let ((*print-pretty* t)
              (*print-readably* nil)
              (*print-structure* nil))
          (fresh-line *debug-io*)
	  (print-frame frame *debug-io*)))
      (setf *** **
            ** *
            * frame)))
  (values))

(defun inspect-command (args)
  (let ((obj (eval (read-from-string args))))
    (inspect obj)))

(defun istep-command (args)
  (sys::istep args))

(defun macroexpand-command (args)
  (let ((s (with-output-to-string (stream)
             (pprint (macroexpand (read-from-string args)) stream))))
    (write-string (string-left-trim '(#\return #\linefeed) s)))
  (values))

(defvar *old-package* nil)

(defun package-command (args)
  (cond ((null args)
         (sys::%format *standard-output* "The ~A package is current.~%"
                  (package-name *package*)))
        ((and *old-package* (string= args "-") (null (find-package "-")))
         (rotatef *old-package* *package*))
        (t
         (when (and (plusp (length args)) (eql (char args 0) #\:))
           (setf args (subseq args 1)))
         (setf args (nstring-upcase args))
         (let ((pkg (find-package args)))
           (if pkg
               (setf *old-package* *package*
                     *package* pkg)
               (sys::%format *standard-output* "Unknown package ~A.~%" args))))))

(defun reset-command (ignored)
  (declare (ignore ignored))
  (invoke-restart 'top-level))

(defun exit-command (ignored)
  (declare (ignore ignored))
  (exit))

(defvar *old-pwd* nil)

(defun cd-command (args)
  (cond ((null args)
         (setf args (if (featurep :windows)
                        "C:\\"
                        (namestring (user-homedir-pathname)))))
        ((string= args "-")
         (if *old-pwd*
             (setf args (namestring *old-pwd*))
             (progn
               (sys::%format t "No previous directory.")
               (return-from cd-command))))
        ((and (> (length args) 1) (string= (subseq args 0 2) "~/")
              (setf args (concatenate 'string
                                      (namestring (user-homedir-pathname))
                                      (subseq args 2))))))
  (let ((dir (probe-directory args)))
    (if dir
        (progn
          (unless (equal dir *default-pathname-defaults*)
            (setf *old-pwd* *default-pathname-defaults*
                  *default-pathname-defaults* dir))
          (sys::%format t "~A" (namestring *default-pathname-defaults*)))
        (sys::%format t "Error: no such directory (~S).~%" args))))

(defun ls-command (args)
  (let ((args (if (stringp args) args ""))
        (ls-program (if (featurep :windows) "dir" "ls")))
    (run-shell-command (concatenate 'string ls-program " " args)
                       :directory *default-pathname-defaults*))
  (values))

(defun tokenize (string)
  (do* ((res nil)
        (string (string-left-trim " " string)
                (string-left-trim " " (subseq string end)))
        (end (position #\space string) (position #\space string)))
       ((zerop (length string)) (nreverse res))
    (unless end
      (setf end (length string)))
    (push (subseq string 0 end) res)))

(defvar *last-files-loaded* nil)

(defun ld-command (args)
  (let ((files (if args (tokenize args) *last-files-loaded*)))
    (setf *last-files-loaded* files)
    (dolist (file files)
      (load file))))

(defun cf-command (args)
  (let ((files (tokenize args)))
    (dolist (file files)
      (compile-file file))))

(defvar *last-files-cloaded* nil)

(defun cload-command (args)
  (let ((files (if args (tokenize args) *last-files-cloaded*)))
    (setf *last-files-cloaded* files)
    (dolist (file files)
      (load (compile-file file)))))

(defun rq-command (args)
  (let ((modules (tokenize (string-upcase args))))
    (dolist (module modules)
      (require module))))

(defun pwd-command (ignored)
  (declare (ignore ignored))
  (sys::%format t "~A~%" (namestring *default-pathname-defaults*)))

(defun trace-command (args)
  (if (null args)
    (sys::%format t "~A~%" (sys::list-traced-functions))
    (dolist (f (tokenize args))
      (sys::trace-1 (read-from-string f)))))

(defun untrace-command (args)
  (if (null args)
    (sys::untrace-all)
    (dolist (f (tokenize args))
      (sys::untrace-1 (read-from-string f)))))

(defconstant spaces (make-string 32 :initial-element #\space))

(defun pad (string width)
  (if (< (length string) width)
      (concatenate 'string string (subseq spaces 0 (- width (length string))))
      string))

(defparameter *command-table*
  '(("apropos" "ap" apropos-command "apropos")
    ("bt" nil backtrace-command "backtrace n stack frames (default 8)")
    ("cd" nil cd-command "change default directory")
    ("cf" nil cf-command "compile file(s)")
    ("cload" "cl" cload-command "compile and load file(s)")
    ("continue" "cont" continue-command "invoke restart n")
    ("describe" "de" describe-command "describe an object")
    ("error" "err" error-command "print the current error message")
    ("exit" "ex" exit-command "exit lisp")
    ("frame" "fr" frame-command "set the value of cl:* to be frame n (default 0)")
    ("help" "he" help-command "print this help")
    ("inspect" "in" inspect-command "inspect an object")
    ("istep" "i" istep-command "navigate within inspection of an object")
    ("ld" nil ld-command "load a file")
    ("ls" nil ls-command "list directory")
    ("macroexpand" "ma" macroexpand-command "macroexpand an expression")
    ("package" "pa" package-command "change *PACKAGE*")
    ("pwd" "pw" pwd-command "print current directory")
    ("reset" "res" reset-command "return to top level")
    ("rq" nil rq-command "require a module")
    ("trace" "tr" trace-command "trace function(s)")
    ("untrace" "untr" untrace-command "untrace function(s)")))

(defun %help-command (prefix)
  (let ((prefix-len (length prefix)))
    (when (and (> prefix-len 0)
               (eql (schar prefix 0) *command-char*))
      (setf prefix (subseq prefix 1))
      (decf prefix-len))
    (sys::%format t "~%  COMMAND     ABBR DESCRIPTION~%")
    (dolist (entry *command-table*)
      (when (or (null prefix)
                (and (<= prefix-len (length (entry-name entry)))
                     (string-equal prefix (subseq (entry-name entry) 0 prefix-len))))
        (sys::%format t "  ~A~A~A~%"
                 (pad (entry-name entry) 12)
                 (pad (entry-abbreviation entry) 5)
                 (entry-help entry))))
    (sys::%format t "~%Commands must be prefixed by the command character, which is '~A'~A.~%~%"
             *command-char* (if (eql *command-char* #\:) " by default" ""))))

(defun help-command (&optional ignored)
  (declare (ignore ignored))
  (%help-command nil))

(defun entry-name (entry)
  (first entry))

(defun entry-abbreviation (entry)
  (second entry))

(defun entry-command (entry)
  (third entry))

(defun entry-help (entry)
  (fourth entry))

(defun find-command (string)
  (let ((len (length string)))
    (when (and (> len 0)
               (eql (schar string 0) *command-char*))
      (setf string (subseq string 1)
            len (1- len)))
    (dolist (entry *command-table*)
      (when (or (string-equal string (entry-abbreviation entry))
                (string-equal string (entry-name entry)))
        (return (entry-command entry))))))

(defun process-cmd (form)
  (when (eq form *null-cmd*)
    (return-from process-cmd t))
  (when (and (stringp form)
             (> (length form) 1)
             (eql (char form 0) *command-char*))
    (let* ((pos (or (position #\space form)
                    (position #\return form)))
           (command-string (subseq form 0 pos))
           (args (if pos (subseq form (1+ pos)) nil)))
      (let ((command (find-command command-string)))
        (cond ((null command)
               (sys::%format t "Unknown top-level command \"~A\".~%" command-string)
               (sys::%format t "Type \"~Ahelp\" for a list of available commands." *command-char*))
              (t
               (when args
                 (setf args (string-trim (list #\space #\return) args))
                 (when (zerop (length args))
                   (setf args nil)))
               (funcall command args)))))
      t))

(defun read-cmd (stream)
  (let ((c (peek-char-non-whitespace stream)))
    (cond ((eql c *command-char*)
           (let* ((input (read-line stream))
		  (name (symbol-name (read-from-string input))))
	     (if (find-command name)
		 (progn (process-cmd input) *handled-cmd*)
	       (read-from-string (concatenate 'string ":" name)))))
          ((eql c #\newline)
           (read-line stream)
           *null-cmd*)
          (t
           (read stream nil *null-cmd*)))))

(defun repl-read-form-fun (in out)
  (loop
    (funcall *repl-prompt-fun* out)
    (finish-output out)
    (let ((form (read-cmd in)))
      (setf (charpos out) 0)
      (unless (eq form *null-cmd*)
        (incf *cmd-number*))
      (cond ((or (eq form *null-cmd*)
		 (eq form *handled-cmd*)))
            ((and (> *debug-level* 0)
                  (fixnump form))
             (let ((n form)
                   (restarts (compute-restarts)))
               (if (< -1 n (length restarts))
                   (invoke-restart-interactively (nth n restarts))
                   (return form))))
            (t
             (return form))))))

(defparameter *repl-read-form-fun* #'repl-read-form-fun)

(defun repl (&optional (in *standard-input*) (out *standard-output*))
    (loop
       (let* ((form (funcall *repl-read-form-fun* in out))
              (results (multiple-value-list (sys:interactive-eval form))))
         (dolist (result results)
           (fresh-line out)
           (prin1 result out)))))

(defun top-level-loop ()
  (fresh-line)
  (unless sys:*noinform*
    (sys::%format t "Type \"~Ahelp\" for a list of available commands.~%" *command-char*))
  (loop
    (setf *inspected-object* nil
          *inspected-object-stack* nil
          *inspect-break* nil)
    (with-simple-restart (top-level
                          "Return to top level.")
      (if (featurep :j)
          (handler-case
              (repl)
            (stream-error (c) (declare (ignore c)) (return-from top-level-loop)))
          (repl)))))
