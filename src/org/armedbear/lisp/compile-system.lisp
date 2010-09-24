;;; compile-system.lisp
;;;
;;; Copyright (C) 2004-2008 Peter Graves
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

(in-package "SYSTEM")

(require "LOOP")
(require "COLLECT")
(require "COMPILE-FILE")

(defun check-lisp-home ()
  (loop
    (cond ((and *lisp-home*
                (probe-directory (pathname *lisp-home*)))
           (return))
          (t
           (cerror "Continue"
                   "*LISP-HOME* is NIL or invalid.~%  Please set *LISP-HOME* to the full pathname of the directory containing the Lisp system files.")))))

(defun grovel-java-definitions-in-file (file out)
  (with-open-file (in file)
    (declare (type stream in))
    (let ((system-package (find-package "SYSTEM"))
          (line-number 1))
      (loop
        (let ((text (read-line in nil)))
          (when (null text)
            (return))
          (let ((position (search "###" text)))
            (when position
                 (let* ((name (string (read-from-string (subseq text (+ position 3)))))
                        (symbol (or (find-symbol name system-package) ; uses CL and EXT
                                    (find-symbol name (find-package "MOP"))
                                    (find-symbol name (find-package "JAVA")))))
                   (when symbol
                     ;; Force the symbol's package prefix to be written out
                     ;; with "::" instead of ":" so there won't be a reader
                     ;; error if a symbol that's external now is no longer
                     ;; external when we read the tags file.
                     (%format out "~A::~A ~S ~S~%"
                              (package-name (symbol-package symbol))
                              name
                              file line-number)))))
          (incf line-number))))))

(defun grovel-java-definitions ()
  (check-lisp-home)
  (time
   (let ((files (directory (merge-pathnames "*.java" *lisp-home*))))
     (with-open-file (stream (merge-pathnames "tags" *lisp-home*)
                             :direction :output :if-exists :supersede)
       (dolist (file files)
         (grovel-java-definitions-in-file file stream))))))

(defun %compile-system (&key output-path)
  (let ((*default-pathname-defaults* (pathname *lisp-home*))
        (*warn-on-redefinition* nil))
    (unless output-path
      (setf output-path *default-pathname-defaults*))
    (flet ((do-compile (file)
             (let ((out (make-pathname :type "abcl"
                                       :defaults (merge-pathnames
                                                  file output-path))))
               (compile-file-if-needed file :output-file out))))
      (load (do-compile "coerce.lisp"))
      (load (do-compile "open.lisp"))
      (load (do-compile "dump-form.lisp"))
      (load (do-compile "compiler-types.lisp"))
      (load (do-compile "compile-file.lisp"))
      (load (do-compile "precompiler.lisp"))
      (load (do-compile "compiler-pass1.lisp"))
      (load (do-compile "compiler-pass2.lisp"))
      (load (do-compile "jvm-class-file.lisp"))
      (load (do-compile "jvm.lisp"))
      (load (do-compile "source-transform.lisp"))
      (load (do-compile "compiler-macro.lisp"))
      (load (do-compile "jvm-instructions.lisp"))
      (load (do-compile "setf.lisp"))
      (load (do-compile "extensible-sequences-base.lisp"))
      (load (do-compile "require.lisp"))
      (load (do-compile "substitute.lisp"))
      (load (do-compile "clos.lisp"))
      ;; Order matters for these files.
      (mapc #'do-compile '("collect.lisp"
                           "macros.lisp"
                           "loop.lisp"))
      (load (do-compile "backquote.lisp"))
      (load (do-compile "early-defuns.lisp"))
      (load (do-compile "typep.lisp"))
      (load (do-compile "subtypep.lisp"))
      (load (do-compile "find.lisp"))
      (load (do-compile "print.lisp"))
      (load (do-compile "pprint-dispatch.lisp"))
      (load (do-compile "pprint.lisp"))
      (load (do-compile "format.lisp"))
      (load (do-compile "delete.lisp"))
      (load (do-compile "concatenate.lisp"))
      (load (do-compile "ldb.lisp"))
      (load (do-compile "destructuring-bind.lisp"))
      (load (do-compile "asdf.lisp"))
      ;; But not for these.
      (mapc #'do-compile '("adjoin.lisp"
                           "and.lisp"
                           "apropos.lisp"
                           "arrays.lisp"
                           "assert.lisp"
                           "assoc.lisp"
                           "autoloads.lisp"
                           "aver.lisp"
                           "bit-array-ops.lisp"
                           "boole.lisp"
                           ;;"boot.lisp"
                           "butlast.lisp"
                           "byte-io.lisp"
                           "case.lisp"
                           "chars.lisp"
                           "check-type.lisp"
                           "compile-file-pathname.lisp"
                           "compile-system.lisp"
                           "compiler-error.lisp"
                           "cond.lisp"
                           "copy-seq.lisp"
                           "copy-symbol.lisp"
                           "count.lisp"
                           "debug.lisp"
                           "define-modify-macro.lisp"
                           "define-symbol-macro.lisp"
                           "defmacro.lisp"
                           "defpackage.lisp"
                           "defsetf.lisp"
                           "defstruct.lisp"
                           "deftype.lisp"
                           "delete-duplicates.lisp"
                           "deposit-field.lisp"
                           "describe.lisp"
                           "describe-compiler-policy.lisp"
                           "directory.lisp"
                           "disassemble.lisp"
                           "do-all-symbols.lisp"
                           "do-external-symbols.lisp"
                           "do-symbols.lisp"
                           "do.lisp"
                           "dolist.lisp"
                           "dotimes.lisp"
                           "dribble.lisp"
                           "dump-class.lisp"
                           "ed.lisp"
                           "enough-namestring.lisp"
                           "ensure-directories-exist.lisp"
                           "error.lisp"
			   "extensible-sequences.lisp"
                           "featurep.lisp"
                           "fdefinition.lisp"
                           "fill.lisp"
                           "find-all-symbols.lisp"
                           "gentemp.lisp"
                           "gray-streams.lisp"
			   "gui.lisp"
                           "inline.lisp"
                           "inspect.lisp"
                           ;;"j.lisp"
                           "java.lisp"
                           "java-collections.lisp"
                           "known-functions.lisp"
                           "known-symbols.lisp"
                           "late-setf.lisp"
                           "lcm.lisp"
                           "ldiff.lisp"
                           "list-length.lisp"
                           "list.lisp"
                           "load.lisp"
                           "make-hash-table.lisp"
                           "make-load-form-saving-slots.lisp"
                           "make-sequence.lisp"
                           "make-string-output-stream.lisp"
                           "make-string.lisp"
                           "map-into.lisp"
                           "map.lisp"
                           "map1.lisp"
                           "mask-field.lisp"
                           "member-if.lisp"
                           "mismatch.lisp"
                           "multiple-value-bind.lisp"
                           "multiple-value-list.lisp"
                           "multiple-value-setq.lisp"
                           "nsubstitute.lisp"
                           "nth-value.lisp"
                           "numbers.lisp"
                           "or.lisp"
                           "parse-integer.lisp"
                           "parse-lambda-list.lisp"
                           "package.lisp"
                           "pathnames.lisp"
                           "print-object.lisp"
                           "print-unreadable-object.lisp"
                           "proclaim.lisp"
                           "profiler.lisp"
                           "prog.lisp"
                           "psetf.lisp"
                           "query.lisp"
                           "read-circle.lisp"
                           "read-conditional.lisp"
                           "read-from-string.lisp"
                           "read-sequence.lisp"
                           "reduce.lisp"
                           "remf.lisp"
                           "remove-duplicates.lisp"
                           "remove.lisp"
                           "replace.lisp"
                           "restart.lisp"
                           "revappend.lisp"
                           "rotatef.lisp"
                           ;;"run-benchmarks.lisp"
                           "run-shell-command.lisp"
                           ;;"runtime-class.lisp"
                           "search.lisp"
                           "sequences.lisp"
                           "sets.lisp"
                           "shiftf.lisp"
                           "signal.lisp"
                           "socket.lisp"
                           "sort.lisp"
                           "step.lisp"
                           "strings.lisp"
                           "sublis.lisp"
                           "subst.lisp"
                           "tailp.lisp"
                           "threads.lisp"
                           "time.lisp"
                           "top-level.lisp"
                           "trace.lisp"
                           "tree-equal.lisp"
                           "upgraded-complex-part-type.lisp"
                           "with-accessors.lisp"
                           "with-hash-table-iterator.lisp"
                           "with-input-from-string.lisp"
                           "with-open-file.lisp"
                           "with-output-to-string.lisp"
                           "with-package-iterator.lisp"
                           "with-slots.lisp"
                           "with-standard-io-syntax.lisp"
                           "write-sequence.lisp")))
    t))

(defun compile-system (&key quit (zip t) output-path)
  (let ((status -1))
    (check-lisp-home)
    (time
     (with-compilation-unit ()
       (let ((*compile-file-zip* zip)
             failure-p)
         (handler-bind (((or warning
                             compiler-error)
                         #'(lambda (c)
                             (declare (ignore c))
                             (setf failure-p t)
                             ;; only register that we had this type of signal
                             ;; defer the actual handling to another handler
                             nil)))
           (%compile-system :output-path output-path))
         (unless failure-p
           (setf status 0)))))
    (create-system-logical-translations output-path)
    (when quit
      (quit :status status))))

(defun create-system-logical-translations (output-path)
  (let* ((dir (directory-namestring (pathname output-path)))
         (system (merge-pathnames "system.lisp" dir))
         (home (pathname *lisp-home*))
         (src (format nil "~A**/*.*" home))
         (java (format nil "~A../../../**/*.*" home)))
    (with-open-file (s system :direction :output 
                       :if-exists :supersede)
      (pprint `(setf (logical-pathname-translations "sys")
                    '(("SYS:SRC;**;*.*" ,src)
                      ("SYS:JAVA;**;*.*" ,java)))
       s))))
      
