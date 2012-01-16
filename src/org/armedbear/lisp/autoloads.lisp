;;; autoloads.lisp
;;;
;;; Copyright (C) 2003-2008 Peter Graves
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


;; This file lists public functions which package users can depend upon.
;;
;; In order to avoid loading the full CL system (of which not all functions
;; may be required by the current program), this file makes sure the symbols
;; are available, but when it tries to execute them, the autoloader causes
;; the actual functions or macros to be loaded.

;; This file lists for each autoloaded symbol which file has to be
;; REQUIRE'd to make it available.
;;
;; Please note: the actual function definition may not be always in the
;;    same file as the one which needs to be REQUIRE'd; an example of
;;    such a case is the compiler: all compiler functions have to be
;;    loaded through loading jvm.lisp.


(in-package "SYSTEM")

(autoload '(char/= char> char>= char-not-equal)
          "chars")
(autoload '(string-upcase string-downcase string-capitalize
            nstring-upcase nstring-downcase nstring-capitalize
            string= string/= string-equal string-not-equal
            string< string>
            string<= string>=
            string-lessp string-greaterp
            string-not-lessp string-not-greaterp
            string-left-trim string-right-trim string-trim)
          "strings")
(autoload 'copy-symbol)
(autoload '(open parse-integer))
(autoload '(sort stable-sort merge) "sort")
(autoload 'tree-equal)
(autoload 'make-hash-table)
(autoload 'list-length)
(autoload 'revappend)
(autoload '(butlast nbutlast) "butlast")
(autoload 'ldiff)
(autoload '(subst subst-if subst-if-not nsubst nsubst-if nsubst-if-not)
          "subst")
(autoload '(sublis nsublis) "sublis")
(autoload '(member-if member-if-not) "member-if")
(autoload 'tailp)
(autoload 'adjoin)
(autoload '(union nunion
            intersection nintersection
            set-difference nset-difference
            set-exclusive-or nset-exclusive-or
            subsetp)
          "sets")
(autoload '(assoc assoc-if assoc-if-not rassoc rassoc-if rassoc-if-not
            acons pairlis copy-alist)
          "assoc")
(autoload-macro 'sequence::seq-dispatch "extensible-sequences-base")
(autoload '(mapcan mapl maplist mapcon) "map1")
(autoload 'make-sequence)
;(autoload 'sequence::fill "extensible-sequences")
(autoload '(copy-seq fill replace))
(autoload '(map map-into))
(autoload 'reduce)
(autoload '(delete delete-if delete-if-not) "delete")
(autoload '(remove remove-if remove-if-not) "remove")
(autoload '(remove-duplicates delete-duplicates))
(autoload '(substitute substitute-if substitute-if-not) "substitute")
(autoload '(nsubstitute nsubstitute-if nsubstitute-if-not) "nsubstitute")
(autoload '(position position-if position-if-not find find-if find-if-not
            list-find* vector-find*)
          "find")
(autoload '(count count-if count-if-not) "count")
(autoload '(mismatch search))
(autoload 'make-string)
(autoload 'directory "directory")
(autoload '(signum round ffloor fceiling fround rationalize gcd isqrt
            float-precision decode-float conjugate phase)
          "numbers")
(autoload 'boole)
(export '%ldb '#:system)
(autoload '(byte byte-size byte-position %ldb ldb ldb-test dpb) "ldb")
(autoload 'lcm)
(autoload '(apropos apropos-list) "apropos")
(autoload '(y-or-n-p yes-or-no-p) "query")
(autoload '(decode-universal-time get-decoded-time encode-universal-time)
          "time")
(autoload 'gentemp)
(autoload '(bit-and bit-ior bit-xor bit-eqv bit-nand bit-nor bit-andc1
            bit-andc2 bit-orc1 bit-orc2 bit-not)
          "bit-array-ops")
(autoload 'deposit-field)
(autoload 'mask-field)
(autoload '(ensure-class ensure-generic-function make-condition
            mop::ensure-method
            define-method-combination
            %defgeneric
            canonicalize-direct-superclasses
	    slot-value slot-makunbound slot-boundp)
          "clos")
(export '(ensure-class subclassp %defgeneric canonicalize-direct-superclasses)
        '#:system)
(autoload '(inspect istep) "inspect")
(autoload 'enough-namestring)
(autoload 'upgraded-complex-part-type)

(autoload '(tpl::top-level-loop) "top-level")

(autoload 'hash-table-iterator-function "with-hash-table-iterator")
(autoload-macro 'with-hash-table-iterator)

(autoload 'package-iterator-function "with-package-iterator")
(autoload-macro 'with-package-iterator)

(autoload-macro 'remf)
(autoload-macro 'check-type)
(autoload-macro 'deftype)
(autoload 'expand-deftype "deftype")
(autoload-macro '(defclass defgeneric defmethod define-condition) "clos")
(autoload-macro 'with-standard-io-syntax)
(autoload 'sys::%with-standard-io-syntax "with-standard-io-syntax")
(autoload-macro 'psetf)
(autoload-macro 'rotatef)
(autoload-macro 'shiftf)

(autoload-macro 'do-all-symbols)
(autoload-macro '(trace untrace) "trace")
(autoload '(sys::list-traced-functions sys::trace-1 sys::untrace-1 sys::untrace-all) "trace")
(autoload 'sys::%define-symbol-macro "define-symbol-macro")
(autoload-macro 'define-symbol-macro)
(autoload-macro 'with-slots)
(autoload-macro 'with-accessors)
(autoload-macro '(sys::%print-unreadable-object print-unreadable-object)
                "print-unreadable-object")
(autoload 'print-object)
(autoload-macro '(prog prog*) "prog")
(export 'concatenate-to-string '#:system)
(autoload '(concatenate-to-string concatenate) "concatenate")
(autoload 'parse-lambda-list)
(autoload-macro 'assert)
(autoload '(sys::assert-error sys::assert-prompt) "assert")
(autoload-macro 'with-input-from-string)
(autoload-macro 'with-output-to-string)
(autoload 'ensure-directories-exist)
(autoload 'coerce)
(autoload 'read-from-string)
(autoload 'read-sequence)
(autoload 'write-sequence)
(autoload 'make-load-form-saving-slots)
(autoload 'compile-file)
(autoload 'compile-file-pathname)

(autoload 'format "format")
(autoload-macro 'formatter "format")

(autoload '(write-byte read-byte) "byte-io")
(autoload-macro 'with-open-file)
(autoload '(pathname-host pathname-device pathname-directory pathname-name
            pathname-type wild-pathname-p pathname-match-p translate-pathname
            logical-pathname-translations translate-logical-pathname
            load-logical-pathname-translations logical-pathname
            parse-namestring)
          "pathnames")
(autoload 'make-string-output-stream)
(autoload 'find-all-symbols)
(autoload 'dribble)
(autoload-macro 'step)
(autoload 'load)
(autoload '(compile with-file-compilation) "jvm")
(autoload-macro 'with-compilation-unit "jvm")

(autoload-macro '(case ccase ecase typecase ctypecase etypecase) "case")
(autoload-macro '(and cond dolist dotimes
                  do-symbols do-external-symbols
                  multiple-value-bind multiple-value-list multiple-value-setq
                  nth-value
                  or))
(autoload-macro '(do do*) "do")

(autoload 'ed)
(autoload 'describe)
(autoload 'disassemble)

(in-package "MOP")
(export '(class-precedence-list class-slots slot-definition-allocation 
	  slot-definition-initargs slot-definition-initform
	  slot-definition-initfunction slot-definition-name
	  compute-applicable-methods
	  compute-applicable-methods-using-classes))
(autoload '(class-precedence-list class-slots) "clos")


;; Java interface.
(in-package "JAVA")
(export 'jregister-handler "JAVA")
(autoload 'jregister-handler "java")
(export 'jinterface-implementation "JAVA")
(autoload 'jinterface-implementation "java")
(export 'jmake-invocation-handler "JAVA")
(autoload 'jmake-invocation-handler "java")
(export 'jmake-proxy "JAVA")
(autoload 'jmake-proxy "java")
(export 'jproperty-value "JAVA")
(autoload 'jproperty-value "java")
(export 'jobject-class "JAVA")
(autoload 'jobject-class "java")
(export 'jclass-superclass "JAVA")
(autoload 'jclass-superclass "java")
(export 'jclass-interfaces "JAVA")
(autoload 'jclass-interfaces "java")
(export 'jclass-interface-p "JAVA")
(autoload 'jclass-interface-p "java")
(export 'jclass-superclass-p "JAVA")
(autoload 'jclass-superclass-p "java")
(export 'jclass-array-p "JAVA")
(autoload 'jclass-array-p "java")
(export 'jarray-component-type "JAVA")
(autoload 'jarray-component-type "java")
(export 'jarray-length "JAVA")
(autoload 'jarray-length "java")
(export 'jnew-array-from-array "JAVA")
(autoload 'jnew-array-from-array "java")
(export 'jnew-array-from-list "JAVA")
(autoload 'jnew-array-from-list "java")
(export 'jclass-constructors "JAVA")
(autoload 'jclass-constructors "java")
(export 'jconstructor-params "JAVA")
(autoload 'jconstructor-params "java")
(export 'jclass-field "JAVA")
(autoload 'jclass-field "java")
(export 'jclass-fields "JAVA")
(autoload 'jclass-fields "java")
(export 'jfield-type "JAVA")
(autoload 'jfield-type "java")
(export 'jfield-name "JAVA")
(autoload 'jfield-name "java")
(export 'jclass-methods "JAVA")
(autoload 'jclass-methods "java")
(export 'jmethod-params "JAVA")
(autoload 'jmethod-params "java")
(export 'jmethod-name "JAVA")
(autoload 'jmethod-name "java")
(export 'jinstance-of-p "JAVA")
(autoload 'jinstance-of-p "java")
(export 'jmember-static-p "JAVA")
(autoload 'jmember-static-p "java")
(export 'jmember-public-p "JAVA")
(autoload 'jmember-public-p "java")
(export 'jmember-protected-p "JAVA")
(autoload 'jmember-protected-p "java")
(export 'jnew-runtime-class "JAVA")
(autoload 'jnew-runtime-class "runtime-class")
(export 'define-java-class "JAVA")
(autoload-macro 'define-java-class "runtime-class")
(export 'ensure-java-class "JAVA")
(autoload 'ensure-java-class "java")
(export 'chain "JAVA")
(autoload-macro 'chain "java")
(export 'jmethod-let "JAVA")
(autoload-macro 'jmethod-let "java")
(export 'jequal "JAVA")
(autoload 'jequal "java")

;; Profiler.
(in-package "PROFILER")
(export '(*granularity* show-call-counts show-hot-counts with-profiling))
(autoload '(show-call-counts show-hot-counts) "profiler")
(autoload-macro 'with-profiling "profiler")

;; Extensions.
(in-package "EXTENSIONS")
(export 'simple-search)
(autoload 'simple-search "search")
(export 'run-shell-command)
(autoload 'run-shell-command)
(export 'run-program)
(autoload 'run-program)
(export 'process) ;; Not a function, but a DEFSTRUCT
(export 'process-p)
(autoload 'process-p "run-program")
(export 'process-input)
(autoload 'process-input "run-program")
(export 'process-output)
(autoload 'process-output "run-program")
(export 'process-error)
(autoload 'process-error "run-program")
(export 'process-alive-p)
(autoload 'process-alive-p "run-program")
(export 'process-wait)
(autoload 'process-wait "run-program")
(export 'process-exit-code)
(autoload 'process-exit-code "run-program")
(export 'process-kill)
(autoload 'process-kill "run-program")

(export 'make-socket)
(autoload 'make-socket "socket")
(export 'make-server-socket)
(autoload 'make-server-socket "socket")
(export 'server-socket-close)
(autoload 'server-socket-close "socket")
(export 'socket-accept)
(autoload 'socket-accept "socket")
(export 'socket-close)
(autoload 'socket-close "socket")
(export 'get-socket-stream)
(autoload 'get-socket-stream "socket")
(export 'socket-peer-port)
(autoload 'socket-peer-port "socket")
(export 'socket-local-port)
(autoload 'socket-local-port "socket")
(export 'socket-local-address)
(autoload 'socket-local-address "socket")
(export 'socket-peer-address)
(autoload 'socket-peer-address "socket")

(in-package "THREADS")

(autoload '(;; MAKE-THREAD helper
            thread-function-wrapper

            ;; Mailbox
            make-mailbox mailbox-send mailbox-empty-p
            mailbox-read mailbox-peek

            ;; Lock
            make-thread-lock 

            ;; Mutex
            make-mutex get-mutex release-mutex)
    "threads")

(autoload-macro '(;; Lock
                  with-thread-lock

                  ;; Mutex
                  with-mutex)
                "threads")

(export '(make-mailbox mailbox-send mailbox-empty-p
          mailbox-read mailbox-peek))
(export '(make-thread-lock with-thread-lock))
(export '(make-mutex get-mutex release-mutex with-mutex))


(in-package "EXTENSIONS")

(export '(grovel-java-definitions compile-system))
(autoload '(grovel-java-definitions compile-system) "compile-system")
(export 'aver)
(autoload-macro 'aver)
(autoload 'sys::%failed-aver "aver")
(export 'collect)
(autoload-macro 'collect)
(export 'compile-file-if-needed)
(autoload 'compile-file-if-needed "compile-file")
(export 'describe-compiler-policy)
(autoload 'describe-compiler-policy)
(export 'macroexpand-all)
(autoload 'macroexpand-all "format")

(export '*gui-backend*)
(export 'init-gui)
(autoload 'init-gui "gui")
(export 'make-dialog-prompt-stream)
(autoload 'make-dialog-prompt-stream "gui")

;; JVM compiler.
(in-package "JVM")
(export '(jvm-compile-package))
(autoload '%with-compilation-unit "jvm")

(in-package "LISP")
(export 'compiler-let)
(autoload 'compiler-let)


(in-package "SYSTEM")

;; #:SYSTEM in PRECOMPILER.LISP


(export '(process-optimization-declarations
          inline-p notinline-p inline-expansion expand-inline
          note-name-defined precompile))
(autoload '(process-optimization-declarations
            inline-p notinline-p inline-expansion expand-inline
            note-name-defined precompile) "precompiler")



;; #:SYSTEM in SOURCE-TRANSFORM.LISP

(export '(source-transform define-source-transform expand-source-transform))
(autoload '(source-transform define-source-transform set-source-transform
            expand-source-transform)
    "source-transform")

(in-package "PRECOMPILER")

(export '(precompile-form precompile))
(autoload '(precompile-form) "precompiler")


;; items in the XP package (pprint.lisp)

(in-package "XP")

(sys::autoload '(xp-structure-p write-string++ output-pretty-object
                 pprint-logical-block+ maybe-initiate-xp-printing
                 check-block-abbreviation start-block end-block
                 pprint-pop-check+) "pprint")

(sys::autoload-macro '(pprint-logical-block+ pprint-pop+) "pprint")

(in-package "COMMON-LISP")

(sys::autoload '(documentation) "clos")

(sys::autoload '(write print prin1 princ pprint write-to-string
            prin1-to-string princ-to-string write-char
            write-string write-line terpri finish-output
            fresh-line force-output clear-output
            pprint-newline pprint-indent pprint-tab pprint-linear
            pprint-fill pprint-tabular) "pprint")

(sys::autoload-macro '(pprint-logical-block) "pprint")

(in-package "SYSTEM")

;; This one must be last, or at least past print-object and clos:
;; we don't want FORMATs executed before we can load those to end us
;; in a debugger. This command replaces the earlier function binding
;; where simple-format calls sys::%format

(autoload 'simple-format "format")