;;; compiler-pass2.lisp
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

(in-package "JVM")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "LOOP")
  (require "FORMAT")
  (require "CLOS")
  (require "PRINT-OBJECT")
  (require "COMPILER-TYPES")
  (require "KNOWN-FUNCTIONS")
  (require "KNOWN-SYMBOLS")
  (require "DUMP-FORM")
  (require "OPCODES")
  (require "JAVA"))


(defun dump-pool ()
  (let ((pool (reverse *pool*))
        entry type)
    (dotimes (index (1- *pool-count*))
      (setq entry (car pool))
      (setq type (case (car entry)
                   (7 'class)
                   (9 'field)
                   (10 'method)
                   (11 'interface)
                   (8 'string)
                   (3 'integer)
                   (4 'float)
                   (5 'long)
                   (6 'double)
                   (12 'name-and-type)
                   (1 'utf8)))
      (format t "~D: ~A ~S~%" (1+ index) type entry)
      (setq pool (cdr pool))))
  t)

(defknown pool-get (t) (integer 1 65535))
(defun pool-get (entry)
  (declare (optimize speed (safety 0)))
  (let* ((ht *pool-entries*)
         (index (gethash1 entry ht)))
    (declare (type hash-table ht))
    (unless index
      (setf index *pool-count*)
      (push entry *pool*)
      (setf (gethash entry ht) index)
      (setf *pool-count* (1+ index)))
    index))

(declaim (ftype (function (string) fixnum) pool-name))
(declaim (inline pool-name))
(defun pool-name (name)
  (declare (optimize speed))
  (pool-get (list 1 (length name) name)))

(declaim (ftype (function (string string) fixnum) pool-name-and-type))
(declaim (inline pool-name-and-type))
(defun pool-name-and-type (name type)
  (declare (optimize speed))
  (pool-get (list 12
                  (pool-name name)
                  (pool-name type))))

;; Assumes CLASS-NAME is already in the correct form ("org/armedbear/lisp/Lisp"
;; as opposed to "org.armedbear.lisp.Lisp").
(declaim (ftype (function (string) fixnum) pool-class))
(declaim (inline pool-class))
(defun pool-class (class-name)
  (declare (optimize speed))
  (pool-get (list 7 (pool-name class-name))))

;; (tag class-index name-and-type-index)
(declaim (ftype (function (string string string) fixnum) pool-field))
(declaim (inline pool-field))
(defun pool-field (class-name field-name type-name)
  (declare (optimize speed))
  (pool-get (list 9
                  (pool-class class-name)
                  (pool-name-and-type field-name type-name))))

;; (tag class-index name-and-type-index)
(declaim (ftype (function (string string string) fixnum) pool-method))
(declaim (inline pool-method))
(defun pool-method (class-name method-name type-name)
  (declare (optimize speed))
  (pool-get (list 10
                  (pool-class class-name)
                  (pool-name-and-type method-name type-name))))

(declaim (ftype (function (string) fixnum) pool-string))
(defun pool-string (string)
  (declare (optimize speed))
  (pool-get (list 8 (pool-name string))))

(defknown pool-int (fixnum) (integer 1 65535))
(defun pool-int (n)
  (declare (optimize speed))
  (pool-get (list 3 n)))

(defknown pool-long (integer) (integer 1 65535))
(defun pool-long (n)
  (declare (optimize speed))
  (declare (type java-long n))
  (let* ((entry (list 5
                      (logand (ash n -32) #xffffffff)
                      (logand n #xffffffff)))
         (ht *pool-entries*)
         (index (gethash1 entry ht)))
    (declare (type hash-table ht))
    (unless index
      (setf index *pool-count*)
      (push entry *pool*)
      (setf (gethash entry ht) index)
      ;; The Java Virtual Machine Specification, Section 4.4.5: "All 8-byte
      ;; constants take up two entries in the constant_pool table of the class
      ;; file. If a CONSTANT_Long_info or CONSTANT_Double_info structure is the
      ;; item in the constant_pool table at index n, then the next usable item in
      ;; the pool is located at index n+2. The constant_pool index n+1 must be
      ;; valid but is considered unusable." So:
      (setf *pool-count* (+ index 2)))
    index))

(defknown u2 (fixnum) cons)
(defun u2 (n)
  (declare (optimize speed))
  (declare (type (unsigned-byte 16) n))
  (list (logand (ash n -8) #xff)
        (logand n #xff)))

(defconstant +java-string+ "Ljava/lang/String;")
(defconstant +lisp-class+ "org/armedbear/lisp/Lisp")
(defconstant +lisp-class-class+ "org/armedbear/lisp/LispClass")
(defconstant +lisp-object-class+ "org/armedbear/lisp/LispObject")
(defconstant +lisp-object+ "Lorg/armedbear/lisp/LispObject;")
(defconstant +lisp-object-array+ "[Lorg/armedbear/lisp/LispObject;")
(defconstant +lisp-symbol-class+ "org/armedbear/lisp/Symbol")
(defconstant +lisp-symbol+ "Lorg/armedbear/lisp/Symbol;")
(defconstant +lisp-structure-object-class+ "org/armedbear/lisp/StructureObject")
(defconstant +lisp-thread-class+ "org/armedbear/lisp/LispThread")
(defconstant +lisp-thread+ "Lorg/armedbear/lisp/LispThread;")
(defconstant +lisp-cons-class+ "org/armedbear/lisp/Cons")
(defconstant +lisp-cons+ "Lorg/armedbear/lisp/Cons;")
(defconstant +lisp-fixnum-class+ "org/armedbear/lisp/Fixnum")
(defconstant +lisp-fixnum+ "Lorg/armedbear/lisp/Fixnum;")
(defconstant +lisp-fixnum-array+ "[Lorg/armedbear/lisp/Fixnum;")
(defconstant +lisp-bignum-class+ "org/armedbear/lisp/Bignum")
(defconstant +lisp-bignum+ "Lorg/armedbear/lisp/Bignum;")
(defconstant +lisp-character-class+ "org/armedbear/lisp/LispCharacter")
(defconstant +lisp-character+ "Lorg/armedbear/lisp/LispCharacter;")
(defconstant +lisp-character-array+ "[Lorg/armedbear/lisp/LispCharacter;")
(defconstant +lisp-abstract-bit-vector-class+ "org/armedbear/lisp/AbstractBitVector")
(defconstant +lisp-abstract-vector-class+ "org/armedbear/lisp/AbstractVector")
(defconstant +lisp-abstract-string-class+ "org/armedbear/lisp/AbstractString")
(defconstant +lisp-simple-vector-class+ "org/armedbear/lisp/SimpleVector")
(defconstant +lisp-simple-string-class+ "org/armedbear/lisp/SimpleString")
(defconstant +lisp-simple-string+ "Lorg/armedbear/lisp/SimpleString;")
(defconstant +lisp-environment+ "Lorg/armedbear/lisp/Environment;")
(defconstant +lisp-special-binding+ "Lorg/armedbear/lisp/SpecialBinding;")
(defconstant +lisp-throw-class+ "org/armedbear/lisp/Throw")
(defconstant +lisp-return-class+ "org/armedbear/lisp/Return")
(defconstant +lisp-go-class+ "org/armedbear/lisp/Go")
(defconstant +lisp-ctf-class+ "org/armedbear/lisp/ClosureTemplateFunction")
(defconstant +lisp-compiled-closure-class+ "org/armedbear/lisp/CompiledClosure")
(defconstant +lisp-compiled-function-class+ "org/armedbear/lisp/CompiledFunction")
(defconstant +lisp-primitive-class+ "org/armedbear/lisp/Primitive")
(defconstant +lisp-hash-table-class+ "org/armedbear/lisp/HashTable")
(defconstant +lisp-eql-hash-table-class+ "org/armedbear/lisp/EqlHashTable")
(defconstant +lisp-package-class+ "org/armedbear/lisp/Package")
(defconstant +lisp-readtable-class+ "org/armedbear/lisp/Readtable")
(defconstant +lisp-stream-class+ "org/armedbear/lisp/Stream")

(defstruct (instruction (:constructor make-instruction (opcode args)))
  (opcode 0 :type (integer 0 255))
  args
  stack
  depth)

(defun print-instruction (instruction)
  (sys::%format nil "~A ~A stack = ~S depth = ~S"
          (opcode-name (instruction-opcode instruction))
          (instruction-args instruction)
          (instruction-stack instruction)
          (instruction-depth instruction)))

(defknown inst * t)
(defun inst (instr &optional args)
  (declare (optimize speed))
  (let ((opcode (if (fixnump instr)
                    instr
                    (opcode-number instr))))
    (unless (listp args)
      (setf args (list args)))
    (make-instruction opcode args)))

(defknown %%emit * t)
(defun %%emit (instr &rest args)
  (declare (optimize speed))
  (let ((instruction (make-instruction instr args)))
    (push instruction *code*)
    instruction))

(defknown %emit * t)
(defun %emit (instr &rest args)
  (declare (optimize speed))
  (let ((instruction (inst instr args)))
    (push instruction *code*)
    instruction))

(defmacro emit (instr &rest args)
  (when (and (consp instr) (eq (car instr) 'QUOTE) (symbolp (cadr instr)))
    (setf instr (opcode-number (cadr instr))))
  (if (fixnump instr)
      `(%%emit ,instr ,@args)
      `(%emit ,instr ,@args)))

(defknown label (symbol) t)
(defun label (symbol)
  (declare (type symbol symbol))
  (declare (optimize speed))
  (emit 'label symbol)
  (setf (symbol-value symbol) nil))

(defknown emit-push-nil () t)
(declaim (inline emit-push-nil))
(defun emit-push-nil ()
  (emit 'getstatic +lisp-class+ "NIL" +lisp-object+))

(defknown emit-push-t () t)
(declaim (inline emit-push-t))
(defun emit-push-t ()
  (emit 'getstatic +lisp-class+ "T" +lisp-symbol+))

(defknown emit-push-false (t) t)
(defun emit-push-false (representation)
  (declare (optimize speed (safety 0)))
  (case representation
    (:boolean
     (emit 'iconst_0))
    (t
     (emit-push-nil))))

(defknown emit-push-true (t) t)
(defun emit-push-true (representation)
  (declare (optimize speed (safety 0)))
  (case representation
    (:boolean
     (emit 'iconst_1))
    (t
     (emit-push-t))))

(defknown emit-push-constant-int (fixnum) t)
(defun emit-push-constant-int (n)
  (case n
    (-1
     (emit 'iconst_m1))
    (0
     (emit 'iconst_0))
    (1
     (emit 'iconst_1))
    (2
     (emit 'iconst_2))
    (3
     (emit 'iconst_3))
    (4
     (emit 'iconst_4))
    (5
     (emit 'iconst_5))
    (t
     (if (<= -128 n 127)
         (emit 'bipush n)
         (if (<= -32768 n 32767)
             (emit 'sipush n)
             (emit 'ldc (pool-int n)))))))

(defknown emit-push-constant-long (integer) t)
(defun emit-push-constant-long (n)
  (emit 'ldc2_w (pool-long n)))

(declaim (ftype (function (t t) cons) make-descriptor-info))
(defun make-descriptor-info (arg-types return-type)
  (let ((descriptor (with-standard-io-syntax
                      (with-output-to-string (s)
                        (princ #\( s)
                        (dolist (type arg-types)
                          (princ type s))
                        (princ #\) s)
                        (princ (or return-type "V") s))))
        (stack-effect (let ((result (cond ((null return-type) 0)
                                          ((equal return-type "J") 2)
                                          (t 1))))
                        (dolist (type arg-types result)
                          (decf result (if (equal type "J") 2 1))))))
    (cons descriptor stack-effect)))

(defparameter *descriptors* (make-hash-table :test #'equal))

;; Just an experiment...
(defmacro defsubst (name lambda-list &rest body)
  (let* ((block-name (fdefinition-block-name name))
         (expansion (generate-inline-expansion block-name lambda-list body)))
;;     (format t "expansion = ~S~%" expansion)
    `(progn
       (%defun ',name (lambda ,lambda-list (block ,block-name ,@body)))
       (precompile ',name)
       (eval-when (:compile-toplevel :load-toplevel :execute)
         (setf (inline-expansion ',name) ',expansion))
       ',name)))

#+nil
(defmacro defsubst (&rest args)
  `(defun ,@args))


(declaim (ftype (function (t t) cons) get-descriptor-info))
(defun get-descriptor-info (arg-types return-type)
  (let* ((key (list arg-types return-type))
         (ht *descriptors*)
         (descriptor-info (gethash1 key ht)))
    (declare (type hash-table ht))
    (or descriptor-info
        (setf (gethash key ht) (make-descriptor-info arg-types return-type)))))

(defsubst get-descriptor (arg-types return-type)
  (car (get-descriptor-info arg-types return-type)))

(declaim (ftype (function * t) emit-invokestatic))
(defun emit-invokestatic (class-name method-name arg-types return-type)
  (let* ((info (get-descriptor-info arg-types return-type))
         (descriptor (car info))
         (stack-effect (cdr info))
         (instruction (emit 'invokestatic class-name method-name descriptor)))
    (setf (instruction-stack instruction) stack-effect)))

(defknown pretty-java-type (t) string)
(defun pretty-java-type (type)
  (let ((arrayp nil)
        (pretty-string nil))
    (when (and (stringp type)
               (> (length type) 0)
               (char= (char type 0) #\[))
      (setf arrayp t
            type (subseq type 1)))
    (setf pretty-string
          (cond ((equal type +lisp-object+)
                 "LispObject")
                ((equal type +lisp-symbol+)
                 "Symbol")
                ((equal type +lisp-thread+)
                 "LispThread")
                ((equal type "C")
                 "char")
                ((equal type "I")
                 "int")
                ((equal type "Z")
                 "boolean")
                ((null type)
                 "void")
                (t
                 type)))
    (when arrayp
      (setf pretty-string (concatenate 'string pretty-string "[]")))
    pretty-string))

(declaim (ftype (function t string) pretty-java-class))
(defun pretty-java-class (class)
  (cond ((equal class +lisp-object-class+)
         "LispObject")
        ((equal class +lisp-symbol+)
         "Symbol")
        ((equal class +lisp-thread-class+)
         "LispThread")
        (t
         class)))

(defknown emit-invokevirtual (t t t t) t)
(defun emit-invokevirtual (class-name method-name arg-types return-type)
  (let* ((info (get-descriptor-info arg-types return-type))
         (descriptor (car info))
         (stack-effect (cdr info))
         (instruction (emit 'invokevirtual class-name method-name descriptor)))
    (declare (type (signed-byte 8) stack-effect))
    (let ((explain *explain*))
      (when (and explain (memq :java-calls explain))
        (unless (string= method-name "execute")
          (format t ";   call to ~A ~A.~A(~{~A~^,~})~%"
                  (pretty-java-type return-type)
                  (pretty-java-class class-name)
                  method-name
                  (mapcar 'pretty-java-type arg-types)))))
    (setf (instruction-stack instruction) (1- stack-effect))))

(defknown emit-invokespecial-init (string list) t)
(defun emit-invokespecial-init (class-name arg-types)
  (let* ((info (get-descriptor-info arg-types nil))
         (descriptor (car info))
         (stack-effect (cdr info))
         (instruction (emit 'invokespecial class-name "<init>" descriptor)))
    (declare (type (signed-byte 8) stack-effect))
    (setf (instruction-stack instruction) (1- stack-effect))))

;; Index of local variable used to hold the current thread.
(defvar *thread* nil)

(defvar *initialize-thread-var* nil)

(defun maybe-initialize-thread-var ()
  (when *initialize-thread-var*
    (emit-invokestatic +lisp-thread-class+ "currentThread" nil +lisp-thread+)
    (emit 'astore *thread*)
    (setf *initialize-thread-var* nil)))

(defknown ensure-thread-var-initialized () t)
(declaim (inline ensure-thread-var-initialized))
(defun ensure-thread-var-initialized ()
  (setf *initialize-thread-var* t))

(defknown emit-push-current-thread () t)
(defun emit-push-current-thread ()
  (declare (optimize speed))
  (ensure-thread-var-initialized)
  (emit 'aload *thread*))

(defun local-variable-p (variable)
  "Return non-NIL if `variable' is a local variable.

Special variables are not considered local."
  (or (variable-register variable) ;; either register or index
      (variable-index variable)))  ;; is non-nil for local variables

(defun emit-load-local-variable (variable)
  "Loads a local variable in the top stack position."
  (aver (local-variable-p variable))
  (if (variable-register variable)
      (emit 'aload (variable-register variable))
      (progn
        (emit 'aload (compiland-argument-register *current-compiland*))
        (emit-push-constant-int (variable-index variable))
        (emit 'aaload))))

(defun emit-push-variable-name (variable)
  (emit 'getstatic *this-class* (declare-symbol (variable-name variable))
        +lisp-symbol+))

(defknown generate-instanceof-type-check-for-variable (t t) t)
(defun generate-instanceof-type-check-for-variable (variable expected-type)
  "Generate a type check for `variable'.

The stack pointer is returned to the position from
before the emitted code: the code is 'stack-neutral'."
  (declare (type symbol expected-type))
  (unless (local-variable-p variable)
    (return-from generate-instanceof-type-check-for-variable))
  (let ((instanceof-class (ecase expected-type
                            (SYMBOL     +lisp-symbol-class+)
                            (CHARACTER  +lisp-character-class+)
                            (CONS       +lisp-cons-class+)
                            (HASH-TABLE +lisp-hash-table-class+)
                            (FIXNUM     +lisp-fixnum-class+)
                            (STREAM     +lisp-stream-class+)
                            (STRING     +lisp-abstract-string-class+)
                            (VECTOR     +lisp-abstract-vector-class+)))
        (expected-type-java-symbol-name (case expected-type
                                          (HASH-TABLE "HASH_TABLE")
                                          (t
                                           (symbol-name expected-type))))
        (LABEL1 (gensym)))
    (emit-load-local-variable variable)
    (emit 'instanceof instanceof-class)
    (emit 'ifne LABEL1)
    (emit-load-local-variable variable)
    (emit 'getstatic +lisp-symbol-class+ expected-type-java-symbol-name
          +lisp-symbol+)
    (emit-invokestatic +lisp-class+ "type_error"
                       (lisp-object-arg-types 2) +lisp-object+)
    (emit 'pop) ; Needed for JVM stack consistency.
    (label LABEL1))
  t)

(defun find-type-for-type-check (declared-type)
  (if (eq declared-type :none) nil
    (or
     (when (fixnum-type-p declared-type) 'FIXNUM)
     (find-if #'(lambda (type) (eq type declared-type))
	      '(SYMBOL CHARACTER CONS HASH-TABLE))
     (find-if #'(lambda (type) (subtypep declared-type type)) 
	      '(STRING VECTOR STREAM)))))


(defknown generate-type-check-for-variable (t) t)
(defun generate-type-check-for-variable (variable)
  (let ((type-to-use 
	 (find-type-for-type-check (variable-declared-type variable))))
    (when type-to-use
      (generate-instanceof-type-check-for-variable variable type-to-use))))

(defknown maybe-generate-type-check (t) t)
(defun maybe-generate-type-check (variable)
  (unless (or (zerop *safety*)
              (variable-special-p variable)
              (eq (variable-representation variable) :int))
    (let ((declared-type (variable-declared-type variable)))
      (unless (eq declared-type :none)
        (unless (subtypep (derive-type (variable-initform variable)) declared-type)
          (generate-type-check-for-variable variable))))))

(defknown generate-type-checks-for-variables (list) t)
(defun generate-type-checks-for-variables (variables)
  (unless (zerop *safety*)
    (dolist (variable variables)
      (unless (variable-special-p variable)
        (generate-type-check-for-variable variable)))
    t))

(defun generate-arg-count-check (arity)
  (aver (fixnump arity))
  (aver (not (minusp arity)))
  (aver (not (null (compiland-argument-register *current-compiland*))))
  (let ((label1 (gensym)))
    (emit 'aload (compiland-argument-register *current-compiland*))
    (emit 'arraylength)
    (emit-push-constant-int arity)
    (emit 'if_icmpeq `,label1)
    (emit 'aload 0) ; this
    (emit-invokevirtual *this-class* "argCountError" nil nil)
    (emit 'label `,label1)))

(defun maybe-generate-interrupt-check ()
  (unless (> *speed* *safety*)
    (let ((label1 (gensym)))
      (emit 'getstatic +lisp-class+ "interrupted" "Z")
      (emit 'ifeq `,label1)
      (emit-invokestatic +lisp-class+ "handleInterrupt" nil nil)
      (emit 'label `,label1))))

(defknown single-valued-p (t) t)
(defun single-valued-p (form)
  (cond ((block-node-p form)
         (if (equal (block-name form) '(TAGBODY))
             (not (unsafe-p (node-form form)))
             (single-valued-p (node-form form))))
        ((var-ref-p form)
         t)
        ((atom form)
         t)
        (t
         (let ((op (%car form))
               result-type
               compiland)
           (cond ((eq op 'IF)
                  (and (single-valued-p (third form))
                       (single-valued-p (fourth form))))
                 ((eq op 'PROGN)
                  (single-valued-p (car (last form))))
                 ((eq op 'BLOCK)
                  (single-valued-p (car (last form))))
                 ((memq op '(LET LET*))
                  (single-valued-p (car (last (cddr form)))))
                 ((memq op '(AND OR))
                  (every #'single-valued-p (cdr form)))
                 ((eq op 'RETURN-FROM)
                  (single-valued-p (third form)))
                 ((memq op '(THE TRULY-THE))
                  (single-valued-p (third form)))
                 ((setf result-type
                        (or (function-result-type op)
                            (and (proclaimed-ftype op)
                                 (ftype-result-type (proclaimed-ftype op)))))
                  (cond ((eq result-type '*)
                         nil)
                        ((atom result-type)
                         t)
                        ((eq (%car result-type) 'VALUES)
                         (= (length result-type) 2))
                        (t
                         t)))
                 ((and (setf compiland *current-compiland*)
                       (eq op (compiland-name compiland)))
                  (compiland-%single-valued-p compiland))
                 (t
                  nil))))))

(defknown emit-clear-values () t)
(defun emit-clear-values ()
  (declare (optimize speed (safety 0)))
  (ensure-thread-var-initialized)
  (emit 'clear-values))

(defknown maybe-emit-clear-values (&rest t) t)
(defun maybe-emit-clear-values (&rest forms)
  (declare (optimize speed))
  (dolist (form forms)
    (unless (single-valued-p form)
;;       (let ((*print-structure* nil))
;;         (format t "Not single-valued: ~S~%" form))
      (ensure-thread-var-initialized)
      (emit 'clear-values)
      (return))))

(defun compile-forms-and-maybe-emit-clear-values (&rest forms-and-compile-args)
  (let ((forms-for-emit-clear
	 (loop for (form arg1 arg2) on forms-and-compile-args by #'cdddr
	    do (compile-form form arg1 arg2)
	    collecting form)))
    (maybe-emit-clear-values forms-for-emit-clear)))

(defknown emit-unbox-fixnum () t)
(defun emit-unbox-fixnum ()
  (declare (optimize speed))
  (cond ((= *safety* 3)
         (emit-invokestatic +lisp-fixnum-class+ "getValue"
                            (lisp-object-arg-types 1) "I"))
        (t
         (emit 'checkcast +lisp-fixnum-class+)
         (emit 'getfield +lisp-fixnum-class+ "value" "I"))))

(defknown emit-unbox-character () t)
(defun emit-unbox-character ()
  (cond ((> *safety* 0)
         (emit-invokestatic +lisp-character-class+ "getValue"
                            (lisp-object-arg-types 1) "C"))
        (t
         (emit 'checkcast +lisp-character-class+)
         (emit 'getfield +lisp-character-class+ "value" "C"))))

(defknown emit-unbox-boolean () t)
(defun emit-unbox-boolean ()
  (let ((LABEL1 (gensym))
        (LABEL2 (gensym)))
    (emit-push-nil)
    (emit 'if_acmpeq LABEL1)
    (emit 'iconst_1)
    (emit 'goto LABEL2)
    (label LABEL1)
    (emit 'iconst_0)
    (label LABEL2)))

(defknown fix-boxing (t t) t)
(defun fix-boxing (required-representation derived-type)
  "Generate code to convert a boxed LispObject on the stack to the specified
representation, based on the derived type of the LispObject."
  (cond ((null required-representation)) ; Nothing to do.
        ((eq required-representation :int)
         (cond ((and (fixnum-type-p derived-type)
                     (< *safety* 3))
                (emit 'checkcast +lisp-fixnum-class+)
                (emit 'getfield +lisp-fixnum-class+ "value" "I"))
               (t
                (emit-invokevirtual +lisp-object-class+ "intValue" nil "I"))))
        ((eq required-representation :char)
         (emit-unbox-character))
        ((eq required-representation :boolean)
         (emit-unbox-boolean))
        ((eq required-representation :long)
         (emit-invokevirtual +lisp-object-class+ "longValue" nil "J"))))

(defknown emit-box-long () t)
(defun emit-box-long ()
  (declare (optimize speed))
  (emit-invokestatic +lisp-class+ "number" '("J") +lisp-object+))

(defknown convert-long (t) t)
(defun convert-long (representation)
  (case representation
    (:int
     (emit 'l2i))
    (:long)
    (t
     (emit-box-long))))

(defknown emit-box-boolean () t)
(defun emit-box-boolean ()
  (let ((LABEL1 (gensym))
        (LABEL2 (gensym)))
    (emit 'ifeq LABEL1)
    (emit-push-t)
    (emit 'goto LABEL2)
    (label LABEL1)
    (emit-push-nil)
    (label LABEL2)))

(defknown emit-move-from-stack (t &optional t) t)
(defun emit-move-from-stack (target &optional representation)
  (declare (optimize speed))
  (cond ((null target)
         (emit 'pop))
        ((eq target 'stack)) ; Nothing to do.
        ((fixnump target)
         ;; A register.
         (emit
          (case representation
            ((:int :boolean :char)
             'istore)
            (:long
             'lstore)
            (t
             'astore))
          target))
        (t
         (aver nil))))

;; Expects value on stack.
(defknown emit-invoke-method (t t t) t)
(defun emit-invoke-method (method-name target representation)
  (emit-invokevirtual +lisp-object-class+ method-name nil +lisp-object+)
  (fix-boxing representation nil)
  (emit-move-from-stack target representation))

(defvar *style-warnings* nil)
(defvar *warnings* nil)
(defvar *errors* nil)

(defvar *last-error-context* nil)

(defun note-error-context ()
  (let ((context *compiler-error-context*))
    (when (and context (neq context *last-error-context*))
      (fresh-line *error-output*)
      (princ "; in " *error-output*)
      (let ((*print-length* 2)
            (*print-level* 2)
            (*print-pretty* nil))
        (prin1 context *error-output*))
      (terpri *error-output*)
      (terpri *error-output*)
      (setf *last-error-context* context))))

(defvar *resignal-compiler-warnings* nil) ; bind this to t inside slime compilation

(defun handle-style-warning (condition)
  (cond (*resignal-compiler-warnings*
         (signal condition))
        (t
         (unless *suppress-compiler-warnings*
           (fresh-line *error-output*)
           (note-error-context)
           (format *error-output* "; Caught ~A:~%;   ~A~2%" (type-of condition) condition))
         (incf *style-warnings*)
         (muffle-warning))))

(defun handle-warning (condition)
  (cond (*resignal-compiler-warnings*
         (signal condition))
        (t
         (unless *suppress-compiler-warnings*
           (fresh-line *error-output*)
           (note-error-context)
           (format *error-output* "; Caught ~A:~%;   ~A~2%" (type-of condition) condition))
         (incf *warnings*)
         (muffle-warning))))

(defun handle-compiler-error (condition)
  (fresh-line *error-output*)
  (note-error-context)
  (format *error-output* "; Caught ERROR:~%;   ~A~2%" condition)
  (incf *errors*)
  (throw 'compile-defun-abort (funcall *compiler-error-bailout*)))

;; "In addition to situations for which the standard specifies that conditions
;; of type WARNING must or might be signaled, warnings might be signaled in
;; situations where the compiler can determine that the consequences are
;; undefined or that a run-time error will be signaled. Examples of this
;; situation are as follows: violating type declarations, altering or assigning
;; the value of a constant defined with DEFCONSTANT, calling built-in Lisp
;; functions with a wrong number of arguments or malformed keyword argument
;; lists, and using unrecognized declaration specifiers." (3.2.5)
(defknown check-arg-count (t fixnum) t)
(defun check-arg-count (form n)
  (declare (type fixnum n))
  (let* ((op (car form))
         (args (cdr form))
         (ok (= (length args) n)))
    (declare (type boolean ok))
    (unless ok
      (funcall (if (eq (symbol-package op) +cl-package+)
                   #'compiler-warn ; See above!
                   #'compiler-style-warn)
               "Wrong number of arguments for ~A (expected ~D, but received ~D)."
               op n (length args)))
    ok))

(declaim (ftype (function (t fixnum) t) check-min-args))
(defun check-min-args (form n)
  (declare (type fixnum n))
  (let* ((op (car form))
         (args (cdr form))
         (ok (>= (length args) n)))
    (unless ok
      (funcall (if (eq (symbol-package op) +cl-package+)
                   #'compiler-warn ; See above!
                   #'compiler-style-warn)
               "Wrong number of arguments for ~A (expected at least ~D, but received ~D)."
               op n (length args)))
    ok))

(defun unsupported-opcode (instruction)
  (error "Unsupported opcode ~D." (instruction-opcode instruction)))

(declaim (type hash-table +resolvers+))
(defconst +resolvers+ (make-hash-table))

(defun initialize-resolvers ()
  (let ((ht +resolvers+))
    (dotimes (n (1+ *last-opcode*))
      (setf (gethash n ht) #'unsupported-opcode))
    ;; The following opcodes resolve to themselves.
    (dolist (n '(0 ; nop
                 1 ; aconst_null
                 2 ; iconst_m1
                 3 ; iconst_0
                 4 ; iconst_1
                 5 ; iconst_2
                 6 ; iconst_3
                 7 ; iconst_4
                 8 ; iconst_5
                 9 ; lconst_0
                 10 ; lconst_1
                 42 ; aload_0
                 43 ; aload_1
                 44 ; aload_2
                 45 ; aload_3
                 50 ; aaload
                 75 ; astore_0
                 76 ; astore_1
                 77 ; astore_2
                 78 ; astore_3
                 83 ; aastore
                 87 ; pop
                 89 ; dup
                 90 ; dup_x1
                 91 ; dup_x2
                 92 ; dup2
                 95 ; swap
                 96 ; iadd
                 97 ; ladd
                 100 ; isub
                 101 ; lsub
                 104 ; imul
                 105 ; lmul
                 116 ; ineg
                 117 ; lneg
                 120 ; ishl
                 121 ; lshl
                 122 ; ishr
                 123 ; lshr
                 126 ; iand
                 127 ; land
                 128 ; ior
                 129 ; lor
                 130 ; ixor
                 131 ; lxor
                 133 ; i2l
                 136 ; l2i
                 148 ; lcmp
                 153 ; ifeq
                 154 ; ifne
                 155 ; ifge
                 156 ; ifgt
                 157 ; ifgt
                 158 ; ifle
                 159 ; if_icmpeq
                 160 ; if_icmpne
                 161 ; if_icmplt
                 162 ; if_icmpge
                 163 ; if_icmpgt
                 164 ; if_icmple
                 165 ; if_acmpeq
                 166 ; if_acmpne
                 167 ; goto
                 168 ; jsr
                 169 ; ret
                 176 ; areturn
                 177 ; return
                 190 ; arraylength
                 191 ; athrow
                 198 ; ifnull
                 202 ; label
                 ))
      (setf (gethash n ht) nil))))

(initialize-resolvers)

(defmacro define-resolver (opcodes args &body body)
  (let ((name (gensym)))
    (if (listp opcodes)
        `(progn
           (defun ,name ,args ,@body)
           (eval-when (:load-toplevel :execute)
             (dolist (op ',opcodes)
               (setf (gethash op +resolvers+) (symbol-function ',name)))))
        `(progn
           (defun ,name ,args ,@body)
           (eval-when (:load-toplevel :execute)
             (setf (gethash ,opcodes +resolvers+) (symbol-function ',name)))))))

;; aload
(define-resolver 25 (instruction)
 (let* ((args (instruction-args instruction))
        (index (car args)))
   (declare (type (unsigned-byte 16) index))
   (cond ((<= 0 index 3)
          (inst (+ index 42)))
         ((<= 0 index 255)
          (inst 25 index))
         (t
          (error "ALOAD unsupported case")))))

;; astore
(define-resolver 58 (instruction)
  (let* ((args (instruction-args instruction))
         (index (car args)))
    (declare (type (unsigned-byte 16) index))
    (cond ((<= 0 index 3)
           (inst (+ index 75)))
          ((<= 0 index 255)
           (inst 58 index))
          (t
           (error "ASTORE unsupported case")))))

;; iload
(define-resolver 21 (instruction)
  (let* ((args (instruction-args instruction))
         (index (car args)))
    (declare (type (unsigned-byte 16) index))
    (cond ((<= 0 index 3)
           (inst (+ index 26)))
          ((<= 0 index 255)
           (inst 21 index))
          (t
           (error "ILOAD unsupported case")))))

;; istore
(define-resolver 54 (instruction)
  (let* ((args (instruction-args instruction))
         (index (car args)))
    (declare (type (unsigned-byte 16) index))
    (cond ((<= 0 index 3)
           (inst (+ index 59)))
          ((<= 0 index 255)
           (inst 54 index))
          (t
           (error "ASTORE unsupported case")))))

;; lload
(define-resolver 22 (instruction)
  (let* ((args (instruction-args instruction))
         (index (car args)))
    (declare (type (unsigned-byte 16) index))
    (cond ((<= 0 index 3)
           (inst (+ index 30)))
          ((<= 0 index 255)
           (inst 22 index))
          (t
           (error "LLOAD unsupported case")))))

;; lstore
(define-resolver 55 (instruction)
  (let* ((args (instruction-args instruction))
         (index (car args)))
    (declare (type (unsigned-byte 16) index))
    (cond ((<= 0 index 3)
           (inst (+ index 63)))
          ((<= 0 index 255)
           (inst 55 index))
          (t
           (error "ASTORE unsupported case")))))

;; getstatic, putstatic
(define-resolver (178 179) (instruction)
  (let* ((args (instruction-args instruction))
         (index (pool-field (first args) (second args) (third args))))
    (inst (instruction-opcode instruction) (u2 index))))

;; bipush, sipush
(define-resolver (16 17) (instruction)
  (let* ((args (instruction-args instruction))
         (n (first args)))
    (declare (type fixnum n))
    (cond ((<= 0 n 5)
           (inst (+ n 3)))
          ((<= -128 n 127)
           (inst 16 (logand n #xff))) ; BIPUSH
          (t ; SIPUSH
           (inst 17 (u2 n))))))

;; invokevirtual, invokespecial, invokestatic class-name method-name descriptor
(define-resolver (182 183 184) (instruction)
  (let* ((args (instruction-args instruction))
         (index (pool-method (first args) (second args) (third args))))
    (setf (instruction-args instruction) (u2 index))
    instruction))

;; ldc
(define-resolver 18 (instruction)
  (let* ((args (instruction-args instruction)))
    (unless (= (length args) 1)
      (error "Wrong number of args for LDC."))
    (if (> (car args) 255)
        (inst 19 (u2 (car args))) ; LDC_W
        (inst 18 args))))

;; ldc2_w
(define-resolver 20 (instruction)
;;   (format t "resolving ldc2_w...~%")
  (let* ((args (instruction-args instruction)))
;;     (format t "args = ~S~%" args)
    (unless (= (length args) 1)
      (error "Wrong number of args for LDC2_W."))
;;     (if (> (car args) 255)
;;         (inst 19 (u2 (car args))) ; LDC_W
;;         (inst 18 args))))
    (inst 20 (u2 (car args)))))

;; getfield, putfield class-name field-name type-name
(define-resolver (180 181) (instruction)
  (let* ((args (instruction-args instruction))
         (index (pool-field (first args) (second args) (third args))))
    (inst (instruction-opcode instruction) (u2 index))))

;; new, anewarray, checkcast, instanceof class-name
(define-resolver (187 189 192 193) (instruction)
  (let* ((args (instruction-args instruction))
         (index (pool-class (first args))))
    (inst (instruction-opcode instruction) (u2 index))))

;; iinc
(define-resolver 132 (instruction)
  (let* ((args (instruction-args instruction))
         (register (first args))
         (n (second args)))
    (inst 132 (list register (logand n #xff)))))

(defknown resolve-instruction (t) t)
(defun resolve-instruction (instruction)
  (declare (optimize speed))
  (let ((resolver (gethash1 (instruction-opcode instruction) +resolvers+)))
    (if resolver
        (funcall resolver instruction)
        instruction)))

(defun resolve-instructions (code)
  (let ((vector (make-array 512 :fill-pointer 0 :adjustable t)))
    (dotimes (index (length code) vector)
      (declare (type (unsigned-byte 16) index))
      (let ((instruction (svref code index)))
        (case (instruction-opcode instruction)
          (205 ; CLEAR-VALUES
           (let ((instructions
                  (list
                   (inst 'aload *thread*)
                   (inst 'aconst_null)
                   (inst 'putfield (list +lisp-thread-class+ "_values"
                                         "[Lorg/armedbear/lisp/LispObject;")))))
             (dolist (instruction instructions)
               (vector-push-extend (resolve-instruction instruction) vector))))
          (t
           (vector-push-extend (resolve-instruction instruction) vector)))))))

;; (defconstant +branch-opcodes+
;;   '(153 ; IFEQ
;;     154 ; IFNE
;;     155 ; IFLT
;;     156 ; IFGE
;;     157 ; IFGT
;;     158 ; IFLE
;;     159 ; IF_ICMPEQ
;;     160 ; IF_ICMPNE
;;     161 ; IF_ICMPLT
;;     162 ; IF_ICMPGE
;;     163 ; IF_ICMPGT
;;     164 ; IF_ICMPLE
;;     165 ; IF_ACMPEQ
;;     166 ; IF_ACMPNE
;;     167 ; GOTO
;;     168 ; JSR
;;     198 ; IFNULL
;;     ))

(declaim (ftype (function (t) t) branch-opcode-p))
(declaim (inline branch-opcode-p))
(defun branch-opcode-p (opcode)
  (declare (optimize speed))
  (declare (type '(integer 0 255) opcode))
  (or (<= 153 opcode 168)
      (= opcode 198)))

(declaim (ftype (function (t t t) t) walk-code))
(defun walk-code (code start-index depth)
  (declare (optimize speed))
  (declare (type fixnum start-index depth))
  (do* ((i start-index (1+ i))
        (limit (length code)))
       ((>= i limit))
    (declare (type fixnum i limit))
    (let* ((instruction (aref code i))
           (instruction-depth (instruction-depth instruction))
           (instruction-stack (instruction-stack instruction)))
      (declare (type fixnum instruction-stack))
      (when instruction-depth
        (unless (= (the fixnum instruction-depth) (the fixnum (+ depth instruction-stack)))
          (format t "~&Stack inconsistency at index ~D: found ~S, expected ~S.~%"
                   i instruction-depth (+ depth instruction-stack)))
        (return-from walk-code))
      (let ((opcode (instruction-opcode instruction)))
        (unless (eql opcode 168) ; JSR
          (setf depth (+ depth instruction-stack)))
        (setf (instruction-depth instruction) depth)
        (if (eql opcode 168) ; JSR
            (let ((label (car (instruction-args instruction))))
              (declare (type symbol label))
              (walk-code code (symbol-value label) (1+ depth)))
            (when (branch-opcode-p opcode)
              (let ((label (car (instruction-args instruction))))
                (declare (type symbol label))
                (walk-code code (symbol-value label) depth))))
        (when (member opcode '(167 169 176 191)) ; GOTO RET ARETURN ATHROW
          ;; Current path ends.
          (return-from walk-code))))))

(declaim (ftype (function () t) analyze-stack))
(defun analyze-stack ()
  (declare (optimize speed))
  (let* ((code *code*)
         (code-length (length code)))
    (declare (type vector code))
    (dotimes (i code-length)
      (declare (type (unsigned-byte 16) i))
      (let* ((instruction (aref code i))
             (opcode (instruction-opcode instruction)))
        (when (eql opcode 202) ; LABEL
          (let ((label (car (instruction-args instruction))))
            (set label i)))
        (if (instruction-stack instruction)
            (when (opcode-stack-effect opcode)
              (unless (eql (instruction-stack instruction) (opcode-stack-effect opcode))
                (sys::%format t "instruction-stack = ~S opcode-stack-effect = ~S~%"
                         (instruction-stack instruction)
                         (opcode-stack-effect opcode))
                (sys::%format t "index = ~D instruction = ~A~%" i (print-instruction instruction))))
            (setf (instruction-stack instruction) (opcode-stack-effect opcode)))
        (unless (instruction-stack instruction)
          (sys::%format t "no stack information for instruction ~D~%" (instruction-opcode instruction))
          (aver nil))))
    (walk-code code 0 0)
    (dolist (handler *handlers*)
      ;; Stack depth is always 1 when handler is called.
      (walk-code code (symbol-value (handler-code handler)) 1))
    (let ((max-stack 0))
      (declare (type fixnum max-stack))
      (dotimes (i code-length)
        (declare (type (unsigned-byte 16) i))
        (let* ((instruction (aref code i))
               (instruction-depth (instruction-depth instruction)))
          (when instruction-depth
            (setf max-stack (max max-stack (the fixnum instruction-depth))))))
;;       (when *compiler-debug*
;;         (sys::%format t "compiland name = ~S~%" (compiland-name *current-compiland*))
;;         (sys::%format t "max-stack = ~D~%" max-stack)
;;         (sys::%format t "----- after stack analysis -----~%")
;;         (print-code))
      max-stack)))

(defun resolve-variables ()
  (let ((code (nreverse *code*)))
    (setf *code* nil)
    (dolist (instruction code)
      (case (instruction-opcode instruction)
        (206 ; VAR-REF
         ;; obsolete
         (aver nil))
        (207 ; VAR-SET
         (let ((variable (car (instruction-args instruction))))
           (aver (variable-p variable))
           (aver (not (variable-special-p variable)))
           (cond ((variable-register variable)
                  (dformat t "register = ~S~%" (variable-register variable))
                  (emit 'astore (variable-register variable)))
                 ((variable-closure-index variable)
                  (dformat t "closure-index = ~S~%" (variable-closure-index variable))
                  (aver (not (null (compiland-closure-register *current-compiland*))))
                  (emit 'aload (compiland-closure-register *current-compiland*))
                  (emit 'swap) ; array value
                  (emit-push-constant-int (variable-closure-index variable))
                  (emit 'swap) ; array index value
                  (emit 'aastore))
                 (t
                  (dformat t "var-set fall-through case~%")
                  (aver (not (null (compiland-argument-register *current-compiland*))))
                  (emit 'aload (compiland-argument-register *current-compiland*)) ; Stack: value array
                  (emit 'swap) ; array value
                  (emit-push-constant-int (variable-index variable)) ; array value index
                  (emit 'swap) ; array index value
                  (emit 'aastore)))))
        (t
         (push instruction *code*))))))

(defun finalize-code ()
  (setf *code* (nreverse (coerce *code* 'vector))))

(defun print-code ()
  (dotimes (i (length *code*))
    (let ((instruction (elt *code* i)))
      (sys::%format t "~D ~A ~S ~S ~S~%"
                    i
                    (opcode-name (instruction-opcode instruction))
                    (instruction-args instruction)
                    (instruction-stack instruction)
                    (instruction-depth instruction)))))

(defun print-code2 (code)
  (dotimes (i (length code))
    (let ((instruction (elt code i)))
      (case (instruction-opcode instruction)
        (202 ; LABEL
         (format t "~A:~%" (car (instruction-args instruction))))
        (t
         (format t "~8D:   ~A ~S~%"
                 i
                 (opcode-name (instruction-opcode instruction))
                 (instruction-args instruction)))))))

(declaim (ftype (function (t) boolean) label-p))
(defun label-p (instruction)
;;   (declare (optimize safety))
;;   (declare (type instruction instruction))
  (and instruction
       (= (the fixnum (instruction-opcode (the instruction instruction))) 202)))

(declaim (ftype (function (t) t) instruction-label))
(defun instruction-label (instruction)
;;   (declare (optimize safety))
  (and instruction
       (= (instruction-opcode (the instruction instruction)) 202)
       (car (instruction-args instruction))))

;; Remove unused labels.
(defun optimize-1 ()
  (let ((code (coerce *code* 'vector))
        (changed nil)
        (marker (gensym)))
    ;; Mark the labels that are actually branched to.
    (dotimes (i (length code))
      (declare (type (unsigned-byte 16) i))
      (let ((instruction (aref code i)))
        (when (branch-opcode-p (instruction-opcode instruction))
          (let ((label (car (instruction-args instruction))))
            (set label marker)))))
    ;; Add labels used for exception handlers.
    (dolist (handler *handlers*)
      (set (handler-from handler) marker)
      (set (handler-to handler) marker)
      (set (handler-code handler) marker))
    ;; Remove labels that are not used as branch targets.
    (dotimes (i (length code))
      (declare (type (unsigned-byte 16) i))
      (let ((instruction (aref code i)))
        (when (= (instruction-opcode instruction) 202) ; LABEL
          (let ((label (car (instruction-args instruction))))
            (declare (type symbol label))
            (unless (eq (symbol-value label) marker)
              (setf (aref code i) nil)
              (setf changed t))))))
    (when changed
      (setf *code* (delete nil code))
      t)))

(defun optimize-2 ()
  (let* ((code (coerce *code* 'vector))
         (length (length code))
         (changed nil))
    (declare (type (unsigned-byte 16) length))
    ;; Since we're looking at this instruction and the next one, we can stop
    ;; one before the end.
    (dotimes (i (1- length))
      (declare (type (unsigned-byte 16) i))
      (let ((instruction (aref code i)))
        (when (and instruction (= (instruction-opcode instruction) 167)) ; GOTO
          (do* ((j (1+ i) (1+ j))
                (next-instruction (aref code j) (aref code j)))
               ((>= j length))
            (declare (type (unsigned-byte 16) j))
            (when next-instruction
              (cond ((= (instruction-opcode next-instruction) 167) ; GOTO
                     (cond ((= j (1+ i))
                            ;; Two GOTOs in a row: the second instruction is
                            ;; unreachable.
                            (setf (aref code j) nil)
                            (setf changed t))
                           (;;(equal next-instruction instruction)
                            (eq (car (instruction-args next-instruction))
                                (car (instruction-args instruction)))
                            ;; We've reached another GOTO to the same destination.
                            ;; We don't need the first GOTO; we can just fall
                            ;; through to the second one.
                            (setf (aref code i) nil)
                            (setf changed t)))
                     (return))
                    ((= (instruction-opcode next-instruction) 202) ; LABEL
                     (when (eq (car (instruction-args instruction))
                               (car (instruction-args next-instruction)))
                       ;; GOTO next instruction; we don't need this one.
                       (setf (aref code i) nil)
                       (setf changed t)
                       (return)))
                    (t
                     ;; Not a GOTO or a label.
                     (return))))))))
    (when changed
      (setf *code* (delete nil code))
      t)))

(declaim (ftype (function (t) hash-table) hash-labels))
(defun hash-labels (code)
  (let ((ht (make-hash-table :test 'eq))
        (code (coerce code 'vector))
        (pending-labels '()))
    (dotimes (i (length code))
      (declare (type (unsigned-byte 16) i))
      (let ((instruction (aref code i)))
        (cond ((label-p instruction)
               (push (instruction-label instruction) pending-labels))
              (t
               ;; Not a label.
               (when pending-labels
                 (dolist (label pending-labels)
                   (setf (gethash label ht) instruction))
                 (setf pending-labels nil))))))
    ht))

(defun optimize-2b ()
  (let* ((code (coerce *code* 'vector))
         (ht (hash-labels code))
         (changed nil))
    (dotimes (i (length code))
      (declare (type (unsigned-byte 16) i))
      (let ((instruction (aref code i)))
        (when (and instruction (= (instruction-opcode instruction) 167)) ; GOTO
          (let* ((target-label (car (instruction-args instruction)))
                 (next-instruction (gethash1 target-label ht)))
            (when next-instruction
              (case (instruction-opcode next-instruction)
                (167 ; GOTO
                 (setf (instruction-args instruction)
                       (instruction-args next-instruction)
                       changed t))
                (176 ; ARETURN
                 (setf (instruction-opcode instruction) 176
                       (instruction-args instruction) nil
                       changed t))))))))
    (when changed
      (setf *code* code)
      t)))

;; CLEAR-VALUES CLEAR-VALUES => CLEAR-VALUES
;; GETSTATIC POP => nothing
(defun optimize-3 ()
  (let* ((code (coerce *code* 'vector))
         (changed nil))
    (dotimes (i (1- (length code)))
      (declare (type (unsigned-byte 16) i))
      (let* ((this-instruction (aref code i))
             (this-opcode (and this-instruction (instruction-opcode this-instruction)))
             (next-instruction (aref code (1+ i)))
             (next-opcode (and next-instruction (instruction-opcode next-instruction))))
        (case this-opcode
          (205 ; CLEAR-VALUES
           (when (eql next-opcode 205) ; CLEAR-VALUES
             (setf (aref code i) nil)
             (setf changed t)))
          (178 ; GETSTATIC
           (when (eql next-opcode 87) ; POP
             (setf (aref code i) nil)
             (setf (aref code (1+ i)) nil)
             (setf changed t))))))
    (when changed
      (setf *code* (delete nil code))
      t)))

(defun delete-unreachable-code ()
  ;; Look for unreachable code after GOTO.
  (let* ((code (coerce *code* 'vector))
         (changed nil)
         (after-goto/areturn nil))
    (dotimes (i (length code))
      (declare (type (unsigned-byte 16) i))
      (let* ((instruction (aref code i))
             (opcode (instruction-opcode instruction)))
        (cond (after-goto/areturn
               (if (= opcode 202) ; LABEL
                   (setf after-goto/areturn nil)
                   ;; Unreachable.
                   (progn
                     (setf (aref code i) nil)
                     (setf changed t))))
              ((= opcode 176) ; ARETURN
               (setf after-goto/areturn t))
              ((= opcode 167) ; GOTO
               (setf after-goto/areturn t)))))
    (when changed
      (setf *code* (delete nil code))
      t)))

(defvar *enable-optimization* t)

(defknown optimize-code () t)
(defun optimize-code ()
  (unless *enable-optimization*
    (format t "optimizations are disabled~%"))
  (when *enable-optimization*
    (when *compiler-debug*
      (format t "----- before optimization -----~%")
      (print-code))
    (loop
      (let ((changed-p nil))
        (setf changed-p (or (optimize-1) changed-p))
        (setf changed-p (or (optimize-2) changed-p))
        (setf changed-p (or (optimize-2b) changed-p))
        (setf changed-p (or (optimize-3) changed-p))
        (setf changed-p (or (delete-unreachable-code) changed-p))
        (unless changed-p
          (return))))
    (unless (vectorp *code*)
      (setf *code* (coerce *code* 'vector)))
    (when *compiler-debug*
      (sys::%format t "----- after optimization -----~%")
      (print-code)))
  t)

(defun code-bytes (code)
  (let ((length 0))
    (declare (type (unsigned-byte 16) length))
    ;; Pass 1: calculate label offsets and overall length.
    (dotimes (i (length code))
      (declare (type (unsigned-byte 16) i))
      (let* ((instruction (aref code i))
             (opcode (instruction-opcode instruction)))
        (if (= opcode 202) ; LABEL
            (let ((label (car (instruction-args instruction))))
              (set label length))
            (incf length (opcode-size opcode)))))
    ;; Pass 2: replace labels with calculated offsets.
    (let ((index 0))
      (declare (type (unsigned-byte 16) index))
      (dotimes (i (length code))
        (declare (type (unsigned-byte 16) i))
        (let ((instruction (aref code i)))
          (when (branch-opcode-p (instruction-opcode instruction))
            (let* ((label (car (instruction-args instruction)))
                   (offset (- (the (unsigned-byte 16) (symbol-value (the symbol label))) index)))
              (setf (instruction-args instruction) (u2 offset))))
          (unless (= (instruction-opcode instruction) 202) ; LABEL
            (incf index (opcode-size (instruction-opcode instruction)))))))
    ;; Expand instructions into bytes, skipping LABEL pseudo-instructions.
    (let ((bytes (make-array length))
          (index 0))
      (declare (type (unsigned-byte 16) index))
      (dotimes (i (length code))
        (declare (type (unsigned-byte 16) i))
        (let ((instruction (aref code i)))
          (unless (= (instruction-opcode instruction) 202) ; LABEL
            (setf (svref bytes index) (instruction-opcode instruction))
            (incf index)
            (dolist (byte (instruction-args instruction))
              (setf (svref bytes index) byte)
              (incf index)))))
      bytes)))

(declaim (inline write-u1))
(defun write-u1 (n stream)
  (declare (optimize speed))
  (declare (type (unsigned-byte 8) n))
  (declare (type stream stream))
  (write-8-bits n stream))

(defknown write-u2 (t t) t)
(defun write-u2 (n stream)
  (declare (optimize speed))
  (declare (type (unsigned-byte 16) n))
  (declare (type stream stream))
  (write-8-bits (ash n -8) stream)
  (write-8-bits (logand n #xFF) stream))

(defknown write-u4 (integer stream) t)
(defun write-u4 (n stream)
  (declare (optimize speed))
  (declare (type (unsigned-byte 32) n))
  (write-u2 (ash n -16) stream)
  (write-u2 (logand n #xFFFF) stream))

(declaim (ftype (function (t t) t) write-s4))
(defun write-s4 (n stream)
  (declare (optimize speed))
  (cond ((minusp n)
         (write-u4 (1+ (logxor (- n) #xFFFFFFFF)) stream))
        (t
         (write-u4 n stream))))

(declaim (ftype (function (t t t) t) write-ascii))
(defun write-ascii (string length stream)
  (declare (type string string))
  (declare (type (unsigned-byte 16) length))
  (declare (type stream stream))
  (write-u2 length stream)
  (dotimes (i length)
    (declare (type (unsigned-byte 16) i))
    (write-8-bits (char-code (char string i)) stream)))

(declaim (ftype (function (t t) t) write-utf8))
(defun write-utf8 (string stream)
  (declare (optimize speed))
  (declare (type string string))
  (declare (type stream stream))
  (let ((length (length string))
        (must-convert nil))
    (declare (type fixnum length))
    (dotimes (i length)
      (declare (type fixnum i))
      (unless (< 0 (char-code (char string i)) #x80)
        (setf must-convert t)
        (return)))
    (if must-convert
        (let ((octets (make-array (* length 2)
                                  :element-type '(unsigned-byte 8)
                                  :adjustable t
                                  :fill-pointer 0)))
          (declare (type (vector (unsigned-byte 8)) octets))
          (dotimes (i length)
            (declare (type fixnum i))
            (let* ((c (char string i))
                   (n (char-code c)))
              (cond ((zerop n)
                     (vector-push-extend #xC0 octets)
                     (vector-push-extend #x80 octets))
                    ((< 0 n #x80)
                     (vector-push-extend n octets))
                    (t
                     (let ((char-octets (char-to-utf8 c)))
                       (dotimes (j (length char-octets))
                         (declare (type fixnum j))
                         (vector-push-extend (svref char-octets j) octets)))))))
          (write-u2 (length octets) stream)
          (dotimes (i (length octets))
            (declare (type fixnum i))
            (write-8-bits (aref octets i) stream)))
        (write-ascii string length stream))))

(defknown write-constant-pool-entry (t t) t)
(defun write-constant-pool-entry (entry stream)
  (declare (optimize speed))
  (declare (type stream stream))
  (let ((tag (first entry)))
    (declare (type (integer 1 12) tag))
    (write-u1 tag stream)
    (case tag
      (1 ; UTF8
       (write-utf8 (third entry) stream))
      (3 ; int
       (write-s4 (second entry) stream))
      ((5 6)
       (write-u4 (second entry) stream)
       (write-u4 (third entry) stream))
      ((9 10 11 12)
       (write-u2 (second entry) stream)
       (write-u2 (third entry) stream))
      ((7 8)
       (write-u2 (second entry) stream))
      (t
       (error "write-constant-pool-entry unhandled tag ~D~%" tag)))))

(defun write-constant-pool (stream)
  (declare (optimize speed))
  (write-u2 *pool-count* stream)
  (dolist (entry (reverse *pool*))
    (write-constant-pool-entry entry stream)))

(defstruct (field (:constructor make-field (name descriptor)))
  access-flags
  name
  descriptor
  name-index
  descriptor-index)

(defstruct (java-method (:conc-name method-) (:constructor make-method))
  access-flags
  name
  descriptor
  name-index
  descriptor-index
  max-stack
  max-locals
  code
  handlers)

(defun emit-constructor-lambda-name (lambda-name)
  (cond ((and lambda-name (symbolp lambda-name) (symbol-package (truly-the symbol lambda-name)))
         (emit 'ldc (pool-string (symbol-name (truly-the symbol lambda-name))))
         (emit 'ldc (pool-string (package-name (symbol-package (truly-the symbol lambda-name)))))
         (emit-invokestatic +lisp-class+ "internInPackage"
                            (list +java-string+ +java-string+) +lisp-symbol+))
        (t
         ;; No name.
         (emit-push-nil))))

(defun emit-constructor-lambda-list (lambda-list)
  (if lambda-list
      (let* ((*print-level* nil)
             (*print-length* nil)
             (s (sys::%format nil "~S" lambda-list)))
        (emit 'ldc (pool-string s))
        (emit-invokestatic +lisp-class+ "readObjectFromString"
                           (list +java-string+) +lisp-object+))
      (emit-push-nil)))

(defun make-constructor (super lambda-name args)
  (let* ((*compiler-debug* nil) ; We don't normally need to see debugging output for constructors.
         (constructor (make-method :name "<init>"
                                   :descriptor "()V"))
         (*code* ())
         (*handlers* nil))
    (setf (method-name-index constructor) (pool-name (method-name constructor)))
    (setf (method-descriptor-index constructor) (pool-name (method-descriptor constructor)))
    (setf (method-max-locals constructor) 1)
    (emit 'aload_0) ;; this
    (cond ((equal super +lisp-compiled-function-class+)
           (emit-constructor-lambda-name lambda-name)
           (emit-constructor-lambda-list args)
           (emit-push-nil) ;; body
           (emit 'aconst_null) ;; environment
           (emit-invokespecial-init super
                                    (list +lisp-object+ +lisp-object+
                                          +lisp-object+ +lisp-environment+)))
          ((equal super +lisp-primitive-class+)
           (emit-constructor-lambda-name lambda-name)
           (emit-constructor-lambda-list args)
           (emit-invokespecial-init super (lisp-object-arg-types 2)))
          ((equal super "org/armedbear/lisp/Primitive0R")
           (emit-constructor-lambda-name lambda-name)
           (push '&REST args)
           (emit-constructor-lambda-list args)
           (emit-invokespecial-init super (lisp-object-arg-types 2)))
          ((equal super "org/armedbear/lisp/Primitive1R")
           (emit-constructor-lambda-name lambda-name)
           (setf args (list (first args) '&REST (second args)))
           (emit-constructor-lambda-list args)
           (emit-invokespecial-init super (lisp-object-arg-types 2)))
          ((equal super "org/armedbear/lisp/Primitive2R")
           (emit-constructor-lambda-name lambda-name)
           (setf args (list (first args) (second args) '&REST (third args)))
           (emit-constructor-lambda-list args)
           (emit-invokespecial-init super (lisp-object-arg-types 2)))
          ((equal super +lisp-ctf-class+)
           (emit-constructor-lambda-list args)
           (emit-invokespecial-init super (lisp-object-arg-types 1)))
          (t
           (aver nil)))
    (setf *code* (append *static-code* *code*))
    (emit 'return)
    (finalize-code)
    ;;(optimize-code)
    (setf *code* (resolve-instructions *code*))
    (setf (method-max-stack constructor) (analyze-stack))
    (setf (method-code constructor) (code-bytes *code*))
    (setf (method-handlers constructor) (nreverse *handlers*))
    constructor))

(defun write-exception-table (method stream)
  (let ((handlers (method-handlers method)))
    (write-u2 (length handlers) stream) ; number of entries
    (dolist (handler handlers)
      (write-u2 (symbol-value (handler-from handler)) stream)
      (write-u2 (symbol-value (handler-to handler)) stream)
      (write-u2 (symbol-value (handler-code handler)) stream)
      (write-u2 (handler-catch-type handler) stream))))

(defun write-source-file-attr (source-file stream)
  (let* ((name-index (pool-name "SourceFile"))
         (source-file-index (pool-name source-file)))
    (write-u2 name-index stream)
    ;; "The value of the attribute_length item of a SourceFile_attribute
    ;; structure must be 2."
    (write-u4 2 stream)
    (write-u2 source-file-index stream)))

(defvar *source-line-number* nil)

(defun write-line-number-table (stream)
  (let* ((name-index (pool-name "LineNumberTable")))
    (write-u2 name-index stream)
    (write-u4 6 stream) ; "the length of the attribute, excluding the initial six bytes"
    (write-u2 1 stream) ; number of entries
    (write-u2 0 stream) ; start_pc
    (write-u2 *source-line-number* stream)))

(defun write-code-attr (method stream)
  (declare (optimize speed))
  (declare (type stream stream))
  (let* ((name-index (pool-name "Code"))
         (code (method-code method))
         (code-length (length code))
         (line-number-available-p (and (fixnump *source-line-number*)
                                       (plusp *source-line-number*)))
         (length (+ code-length 12
                    (* (length (method-handlers method)) 8)
                    (if line-number-available-p 12 0)))
         (max-stack (or (method-max-stack method) 20))
         (max-locals (or (method-max-locals method) 1)))
    (write-u2 name-index stream)
    (write-u4 length stream)
    (write-u2 max-stack stream)
    (write-u2 max-locals stream)
    (write-u4 code-length stream)
    (dotimes (i code-length)
      (declare (type index i))
      (write-u1 (the (unsigned-byte 8) (svref code i)) stream))
    (write-exception-table method stream)
    (cond (line-number-available-p
           ; attributes count
           (write-u2 1 stream)
           (write-line-number-table stream))
          (t
           ; attributes count
           (write-u2 0 stream)))))

(defun write-method (method stream)
  (declare (optimize speed))
  (write-u2 (or (method-access-flags method) #x1) stream) ; access flags
  (write-u2 (method-name-index method) stream)
  (write-u2 (method-descriptor-index method) stream)
  (write-u2 1 stream) ; attributes count
  (write-code-attr method stream))

(defun write-field (field stream)
  (declare (optimize speed))
  (write-u2 (or (field-access-flags field) #x1) stream) ; access flags
  (write-u2 (field-name-index field) stream)
  (write-u2 (field-descriptor-index field) stream)
  (write-u2 0 stream)) ; attributes count

(defknown declare-field (t t) t)
(defun declare-field (name descriptor)
  (let ((field (make-field name descriptor)))
    (setf (field-access-flags field) (logior #x8 #x2)) ; private static
    (setf (field-name-index field) (pool-name (field-name field)))
    (setf (field-descriptor-index field) (pool-name (field-descriptor field)))
    (push field *fields*)))

(defknown sanitize (symbol) string)
(defun sanitize (symbol)
  (declare (type symbol symbol))
  (declare (optimize speed))
  (let* ((input (symbol-name symbol))
         (output (make-array (length input) :fill-pointer 0 :element-type 'character)))
    (dotimes (i (length input))
      (declare (type fixnum i))
      (let ((c (char-upcase (char input i))))
        (cond ((<= #.(char-code #\A) (char-code c) #.(char-code #\Z))
               (vector-push c output))
              ((<= #.(char-code #\0) (char-code c) #.(char-code #\9))
               (vector-push c output))
              ((eql c #\-)
               (vector-push #\_ output)))))
    (when (plusp (length output))
      output)))

(defknown declare-symbol (symbol) string)
(defun declare-symbol (symbol)
  (declare (type symbol symbol))
  (let* ((ht *declared-symbols*)
         (g (gethash1 symbol ht)))
    (declare (type hash-table ht))
    (unless g
      (cond ((null (symbol-package symbol))
             (setf g (if *compile-file-truename*
                         (declare-object-as-string symbol)
                         (declare-object symbol))))
            (t
             (let ((*code* *static-code*)
                   (s (sanitize symbol)))
               (setf g (symbol-name (gensym)))
               (when s
                 (setf g (concatenate 'string g "_" s)))
               (declare-field g +lisp-symbol+)
               (emit 'ldc (pool-string (symbol-name symbol)))
               (emit 'ldc (pool-string (package-name (symbol-package symbol))))
               (emit-invokestatic +lisp-class+ "internInPackage"
                                  (list +java-string+ +java-string+) +lisp-symbol+)
               (emit 'putstatic *this-class* g +lisp-symbol+)
               (setf *static-code* *code*)
               (setf (gethash symbol ht) g)))))
    g))

(defknown declare-keyword (symbol) string)
(defun declare-keyword (symbol)
  (declare (type symbol symbol))
  (let* ((ht *declared-symbols*)
         (g (gethash1 symbol ht)))
    (declare (type hash-table ht))
    (unless g
      (let ((*code* *static-code*))
        (setf g (symbol-name (gensym)))
        (declare-field g +lisp-symbol+)
        (emit 'ldc (pool-string (symbol-name symbol)))
        (emit-invokestatic +lisp-class+ "internKeyword"
                           (list +java-string+) +lisp-symbol+)
        (emit 'putstatic *this-class* g +lisp-symbol+)
        (setf *static-code* *code*)
        (setf (gethash symbol ht) g)))
    g))

(defknown declare-function (symbol) string)
(defun declare-function (symbol)
  (declare (type symbol symbol))
  (let* ((ht *declared-functions*)
         (f (gethash1 symbol ht)))
    (declare (type hash-table ht))
    (unless f
      (setf f (symbol-name (gensym)))
      (let ((s (sanitize symbol)))
        (when s
          (setf f (concatenate 'string f "_" s))))
      (let ((*code* *static-code*)
            (g (gethash1 symbol (the hash-table *declared-symbols*))))
        (cond (g
               (emit 'getstatic *this-class* g +lisp-symbol+))
              (t
               (emit 'ldc (pool-string (symbol-name symbol)))
               (emit 'ldc (pool-string (package-name (symbol-package symbol))))
               (emit-invokestatic +lisp-class+ "internInPackage"
                                  (list +java-string+ +java-string+)
                                  +lisp-symbol+)))
        (declare-field f +lisp-object+)
        (emit-invokevirtual +lisp-symbol-class+ "getSymbolFunctionOrDie"
                            nil +lisp-object+)
        (emit 'putstatic *this-class* f +lisp-object+)
        (setf *static-code* *code*)
        (setf (gethash symbol ht) f)))
    f))

(defknown declare-setf-function (name) string)
(defun declare-setf-function (name)
  (let* ((ht *declared-functions*)
         (f (gethash1 name ht)))
    (declare (type hash-table ht))
    (unless f
      (let ((symbol (cadr name)))
        (declare (type symbol symbol))
        (setf f (symbol-name (gensym)))
        (let ((s (sanitize symbol)))
          (when s
            (setf f (concatenate 'string f "_SETF_" s))))
        (let ((*code* *static-code*)
              (g (gethash1 symbol (the hash-table *declared-symbols*))))
          (cond (g
                 (emit 'getstatic *this-class* g +lisp-symbol+))
                (t
                 (emit 'ldc (pool-string (symbol-name symbol)))
                 (emit 'ldc (pool-string (package-name (symbol-package symbol))))
                 (emit-invokestatic +lisp-class+ "internInPackage"
                                    (list +java-string+ +java-string+)
                                    +lisp-symbol+)))
          (declare-field f +lisp-object+)
          (emit-invokevirtual +lisp-symbol-class+ "getSymbolSetfFunctionOrDie"
                              nil +lisp-object+)
          (emit 'putstatic *this-class* f +lisp-object+)
          (setf *static-code* *code*)
          (setf (gethash name ht) f))))
    f))

(defknown declare-local-function (local-function) string)
(defun declare-local-function (local-function)
  (let* ((ht *declared-functions*)
         (g (gethash1 local-function ht)))
    (declare (type hash-table ht))
    (unless g
      (setf g (symbol-name (gensym)))
      (let* ((pathname (class-file-pathname (local-function-class-file local-function)))
             (*code* *static-code*))
        (declare-field g +lisp-object+)
        (emit 'ldc (pool-string (file-namestring pathname)))
        (emit-invokestatic +lisp-class+ "loadCompiledFunction"
                           (list +java-string+) +lisp-object+)
        (emit 'putstatic *this-class* g +lisp-object+)
        (setf *static-code* *code*)
        (setf (gethash local-function ht) g)))
    g))

(defknown declare-fixnum (fixnum) string)
(defun declare-fixnum (n)
  (declare (type fixnum n))
  (let* ((ht *declared-integers*)
         (g (gethash1 n ht)))
    (declare (type hash-table ht))
    (unless g
      (let ((*code* *static-code*))
        (setf g (format nil "FIXNUM_~A~D"
                        (if (minusp n) "MINUS_" "")
                        (abs n)))
        (declare-field g +lisp-fixnum+)
        (cond ((<= 0 n 255)
               (emit 'getstatic +lisp-fixnum-class+ "constants" +lisp-fixnum-array+)
               (emit-push-constant-int n)
               (emit 'aaload))
              (t
               (emit 'new +lisp-fixnum-class+)
               (emit 'dup)
               (emit-push-constant-int n)
               (emit-invokespecial-init +lisp-fixnum-class+ '("I"))))
        (emit 'putstatic *this-class* g +lisp-fixnum+)
        (setf *static-code* *code*)
        (setf (gethash n ht) g)))
    g))

(defknown declare-bignum (integer) string)
(defun declare-bignum (n)
  (let* ((ht *declared-integers*)
         (g (gethash1 n ht)))
    (declare (type hash-table ht))
    (unless g
      (cond ((<= most-negative-java-long n most-positive-java-long)
             (let ((*code* *static-code*))
               (setf g (format nil "BIGNUM_~A~D"
                               (if (minusp n) "MINUS_" "")
                               (abs n)))
               (declare-field g +lisp-bignum+)
               (emit 'new +lisp-bignum-class+)
               (emit 'dup)
               (emit 'ldc2_w (pool-long n))
               (emit-invokespecial-init +lisp-bignum-class+ '("J"))
               (emit 'putstatic *this-class* g +lisp-bignum+)
               (setf *static-code* *code*)))
            (t
             (let* ((*print-base* 10)
                    (s (with-output-to-string (stream) (dump-form n stream)))
                    (*code* *static-code*))
               (setf g (concatenate 'string "BIGNUM_" (symbol-name (gensym))))
               (declare-field g +lisp-bignum+)
               (emit 'new +lisp-bignum-class+)
               (emit 'dup)
               (emit 'ldc (pool-string s))
               (emit-push-constant-int 10)
               (emit-invokespecial-init +lisp-bignum-class+ (list +java-string+ "I"))
               (emit 'putstatic *this-class* g +lisp-bignum+)
               (setf *static-code* *code*))))
      (setf (gethash n ht) g))
    g))

(defknown declare-character (t) string)
(defun declare-character (c)
  (let ((g (symbol-name (gensym)))
        (n (char-code c))
        (*code* *static-code*))
    (declare-field g +lisp-character+)
    (cond ((<= 0 n 255)
           (emit 'getstatic +lisp-character-class+ "constants" +lisp-character-array+)
           (emit-push-constant-int n)
           (emit 'aaload))
          (t
           (emit 'new +lisp-character-class+)
           (emit 'dup)
           (emit-push-constant-int n)
           (emit-invokespecial-init +lisp-character-class+ '("C"))))
    (emit 'putstatic *this-class* g +lisp-character+)
    (setf *static-code* *code*)
    g))

(defknown declare-object-as-string (t) string)
(defun declare-object-as-string (obj)
  (let* ((g (symbol-name (gensym)))
         (s (with-output-to-string (stream) (dump-form obj stream)))
         (*code* *static-code*))
    (declare-field g +lisp-object+)
    (emit 'ldc (pool-string s))
    (emit-invokestatic +lisp-class+ "readObjectFromString"
                       (list +java-string+) +lisp-object+)
    (emit 'putstatic *this-class* g +lisp-object+)
    (setf *static-code* *code*)
    g))

(defun declare-load-time-value (obj)
  (let* ((g (symbol-name (gensym)))
         (s (with-output-to-string (stream) (dump-form obj stream)))
         (*code* *static-code*))
    (declare-field g +lisp-object+)
    (emit 'ldc (pool-string s))
    (emit-invokestatic +lisp-class+ "readObjectFromString"
                       (list +java-string+) +lisp-object+)
    (emit-invokestatic +lisp-class+ "loadTimeValue"
                       (lisp-object-arg-types 1) +lisp-object+)
    (emit 'putstatic *this-class* g +lisp-object+)
    (setf *static-code* *code*)
    g))

(defknown declare-instance (t) t)
(defun declare-instance (obj)
  (aver (not (null *compile-file-truename*)))
  (aver (or (structure-object-p obj) (standard-object-p obj)
            (java:java-object-p obj)))
  (let* ((g (symbol-name (gensym)))
         (s (with-output-to-string (stream) (dump-form obj stream)))
         (*code* *static-code*))
    (declare-field g +lisp-object+)
    (emit 'ldc (pool-string s))
    (emit-invokestatic +lisp-class+ "readObjectFromString"
                       (list +java-string+) +lisp-object+)
    (emit-invokestatic +lisp-class+ "loadTimeValue"
                       (lisp-object-arg-types 1) +lisp-object+)
    (emit 'putstatic *this-class* g +lisp-object+)
    (setf *static-code* *code*)
    g))

(defun declare-package (obj)
  (let* ((g (symbol-name (gensym)))
         (*print-level* nil)
         (*print-length* nil)
         (s (format nil "#.(FIND-PACKAGE ~S)" (package-name obj)))
         (*code* *static-code*))
    (declare-field g +lisp-object+)
    (emit 'ldc (pool-string s))
    (emit-invokestatic +lisp-class+ "readObjectFromString"
                       (list +java-string+) +lisp-object+)
    (emit 'putstatic *this-class* g +lisp-object+)
    (setf *static-code* *code*)
    g))

(declaim (ftype (function (t) string) declare-object))
(defun declare-object (obj)
  (let ((key (symbol-name (gensym))))
    (remember key obj)
    (let* ((g1 (declare-string key))
           (g2 (symbol-name (gensym)))
           (*code* *static-code*))
      (declare-field g2 +lisp-object+)
      (emit 'getstatic *this-class* g1 +lisp-simple-string+)
      (emit-invokestatic +lisp-class+ "recall"
                         (list +lisp-simple-string+) +lisp-object+)
      (emit 'putstatic *this-class* g2 +lisp-object+)
      (setf *static-code* *code*)
      g2)))

(defun declare-lambda (obj)
  (let* ((g (symbol-name (gensym)))
         (*print-level* nil)
         (*print-length* nil)
         (s (format nil "~S" obj))
         (*code* *static-code*))
    (declare-field g +lisp-object+)
    (emit 'ldc
          (pool-string s))
    (emit-invokestatic +lisp-class+ "readObjectFromString"
                       (list +java-string+) +lisp-object+)
    (emit-invokestatic +lisp-class+ "coerceToFunction"
                       (lisp-object-arg-types 1) +lisp-object+)
    (emit 'putstatic *this-class* g +lisp-object+)
    (setf *static-code* *code*)
    g))

(defun declare-string (string)
  (let* ((ht *declared-strings*)
         (g (gethash1 string ht)))
    (declare (type hash-table ht))
    (unless g
      (let ((*code* *static-code*))
        (setf g (symbol-name (gensym)))
        (declare-field g +lisp-simple-string+)
        (emit 'new +lisp-simple-string-class+)
        (emit 'dup)
        (emit 'ldc (pool-string string))
        (emit-invokespecial-init +lisp-simple-string-class+ (list +java-string+))
        (emit 'putstatic *this-class* g +lisp-simple-string+)
        (setf *static-code* *code*)
        (setf (gethash string ht) g)))
    g))

(defknown compile-constant (t t t) t)
(defun compile-constant (form target representation)
  (unless target
    (return-from compile-constant))
  (case representation
    (:int
     (cond ((fixnump form)
            (emit-push-constant-int form)
            (emit-move-from-stack target representation)
            (return-from compile-constant))
           ((integerp form)
            (emit 'getstatic *this-class* (declare-bignum form) +lisp-bignum+)
            (emit-invokevirtual +lisp-object-class+ "intValue" nil "I")
            (emit-move-from-stack target representation)
            (return-from compile-constant))
           (t
            (assert nil))))
    (:long
     (cond ((fixnump form)
            (case form
              (0
               (emit 'lconst_0))
              (1
               (emit 'lconst_1))
              (t
               (emit-push-constant-int form)
               (emit 'i2l)))
            (emit-move-from-stack target representation)
            (return-from compile-constant))
           ((<= most-negative-java-long form most-positive-java-long)
            (emit 'ldc2_w (pool-long form))
            (return-from compile-constant))
           ((integerp form)
            (emit 'getstatic *this-class* (declare-bignum form) +lisp-bignum+)
            (emit-invokevirtual +lisp-object-class+ "longValue" nil "J")
            (emit-move-from-stack target representation)
            (return-from compile-constant))
           (t
            (assert nil))))
    (:char
     (cond ((characterp form)
            (emit-push-constant-int (char-code form))
            (emit-move-from-stack target representation)
            (return-from compile-constant))
           (t
            (assert nil))))
    (:boolean
     (emit (if form 'iconst_1 'iconst_0))
     (emit-move-from-stack target representation)
     (return-from compile-constant)))
  (cond ((fixnump form)
         (let ((translation (case form
                              (0  "ZERO")
                              (1  "ONE")
                              (2  "TWO")
                              (3  "THREE")
                              (-1 "MINUS_ONE"))))
           (if translation
               (emit 'getstatic +lisp-fixnum-class+ translation +lisp-fixnum+)
               (emit 'getstatic *this-class* (declare-fixnum form) +lisp-fixnum+))))
        ((integerp form)
         ;; A bignum.
         (emit 'getstatic *this-class* (declare-bignum form) +lisp-bignum+))
        ((numberp form)
         ;; A number, but not a fixnum.
         (emit 'getstatic *this-class*
               (declare-object-as-string form) +lisp-object+))
        ((stringp form)
         (if *compile-file-truename*
             (emit 'getstatic *this-class*
                   (declare-string form) +lisp-simple-string+)
             (emit 'getstatic *this-class*
                   (declare-object form) +lisp-object+)))
        ((vectorp form)
         (if *compile-file-truename*
             (emit 'getstatic *this-class*
                   (declare-object-as-string form) +lisp-object+)
             (emit 'getstatic *this-class*
                   (declare-object form) +lisp-object+)))
        ((characterp form)
         (emit 'getstatic *this-class*
               (declare-character form) +lisp-character+))
        ((or (hash-table-p form) (typep form 'generic-function))
         (emit 'getstatic *this-class*
               (declare-object form) +lisp-object+))
        ((pathnamep form)
         (let ((g (if *compile-file-truename*
                      (declare-object-as-string form)
                      (declare-object form))))
           (emit 'getstatic *this-class* g +lisp-object+)))
        ((packagep form)
         (let ((g (if *compile-file-truename*
                      (declare-package form)
                      (declare-object form))))
           (emit 'getstatic *this-class* g +lisp-object+)))
        ((or (structure-object-p form)
             (standard-object-p form)
             (java:java-object-p form))
         (let ((g (if *compile-file-truename*
                      (declare-instance form)
                      (declare-object form))))
           (emit 'getstatic *this-class* g +lisp-object+)))
        (t
         (if *compile-file-truename*
             (error "COMPILE-CONSTANT unhandled case ~S" form)
             (emit 'getstatic *this-class*
                   (declare-object form) +lisp-object+))))
  (emit-move-from-stack target representation))

(defparameter *unary-operators* nil)

(defun initialize-unary-operators ()
  (let ((ht (make-hash-table :test 'eq)))
    (dolist (pair '((ABS             "ABS")
                    (CADDR           "caddr")
                    (CADR            "cadr")
                    (CDDR            "cddr")
                    (CDR             "cdr")
                    (CLASS-OF        "classOf")
                    (COMPLEXP        "COMPLEXP")
                    (DENOMINATOR     "DENOMINATOR")
                    (FIRST           "car")
                    (LENGTH          "LENGTH")
                    (NREVERSE        "nreverse")
                    (NUMERATOR       "NUMERATOR")
                    (REST            "cdr")
                    (REVERSE         "reverse")
                    (SECOND          "cadr")
                    (SIMPLE-STRING-P "SIMPLE_STRING_P")
                    (STRING          "STRING")
                    (THIRD           "caddr")))
      (setf (gethash (%car pair) ht) (%cadr pair)))
    (setf *unary-operators* ht)))

(initialize-unary-operators)

(defknown install-p2-handler * t)
(defun install-p2-handler (symbol &optional handler)
  (declare (type symbol symbol))
  (let ((handler (or handler
                     (find-symbol (concatenate 'string "COMPILE-" (symbol-name symbol)) 'jvm))))
    (unless (and handler (fboundp handler))
      (error "Handler not found: ~S" handler))
    (setf (get symbol 'p2-handler) handler)))

(defparameter *predicates* (make-hash-table :test 'eq))

(defun define-predicate (name boxed-method-name unboxed-method-name)
  (setf (gethash name *predicates*) (cons boxed-method-name unboxed-method-name))
  (install-p2-handler name 'p2-predicate))

(defknown p2-predicate (t t t) t)
(defun p2-predicate (form target representation)
  (unless (= (length form) 2)
    (compile-function-call form target representation)
    (return-from p2-predicate))
  (let* ((op (car form))
         (info (gethash op *predicates*))
         (boxed-method-name (car info))
         (unboxed-method-name (cdr info)))
    (cond ((and boxed-method-name unboxed-method-name)
           (let ((arg (cadr form)))
	     (compile-forms-and-maybe-emit-clear-values arg 'stack nil)
             (case representation
               (:boolean
                (emit-invokevirtual +lisp-object-class+
                                    unboxed-method-name
                                    nil "Z"))
               (t
                (emit-invokevirtual +lisp-object-class+
                                    boxed-method-name
                                    nil +lisp-object+)))
             (emit-move-from-stack target representation)))
          (t
           (compile-function-call form target representation)))))

(define-predicate 'constantp "CONSTANTP" "constantp")
(define-predicate 'endp      "ENDP"      "endp")
(define-predicate 'evenp     "EVENP"     "evenp")
(define-predicate 'floatp    "FLOATP"    "floatp")
(define-predicate 'integerp  "INTEGERP"  "integerp")
(define-predicate 'listp     "LISTP"     "listp")
(define-predicate 'minusp    "MINUSP"    "minusp")
(define-predicate 'numberp   "NUMBERP"   "numberp")
(define-predicate 'oddp      "ODDP"      "oddp")
(define-predicate 'plusp     "PLUSP"     "plusp")
(define-predicate 'rationalp "RATIONALP" "rationalp")
(define-predicate 'realp     "REALP"     "realp")

(declaim (ftype (function (t t t t) t) compile-function-call-1))
(defun compile-function-call-1 (op args target representation)
  (let ((arg (first args)))
    (when (eq op '1+)
      (p2-plus (list '+ arg 1) target representation)
      (return-from compile-function-call-1 t))
    (when (eq op '1-)
      (p2-minus (list '- arg 1) target representation)
      (return-from compile-function-call-1 t))
    (let ((s (gethash1 op (the hash-table *unary-operators*))))
      (cond (s
	     (compile-forms-and-maybe-emit-clear-values arg 'stack nil)
             (emit-invoke-method s target representation)
             t)
            (t
             nil)))))

(defparameter *binary-operators* nil)

(defun initialize-binary-operators ()
  (let ((ht (make-hash-table :test 'eq)))
    (dolist (pair '((EQL          "EQL")
                    (EQUAL        "EQUAL")
                    (+            "add")
                    (-            "subtract")
                    (/            "divideBy")
                    (*            "multiplyBy")
                    (<            "IS_LT")
                    (<=           "IS_LE")
                    (>            "IS_GT")
                    (>=           "IS_GE")
                    ( =           "IS_E")
                    (/=           "IS_NE")
                    (ASH          "ash")
                    (AREF         "AREF")
                    (SIMPLE-TYPEP "typep")
                    (RPLACA       "RPLACA")
                    (RPLACD       "RPLACD")))
      (setf (gethash (%car pair) ht) (%cadr pair)))
    (setf *binary-operators* ht)))

(initialize-binary-operators)

(defun compile-binary-operation (op args target representation)
  (let ((arg1 (car args))
        (arg2 (cadr args)))
    (compile-forms-and-maybe-emit-clear-values arg1 'stack nil
					       arg2 'stack nil)
    (emit-invokevirtual +lisp-object-class+ op
			(lisp-object-arg-types 1) +lisp-object+)
    (fix-boxing representation nil)
    (emit-move-from-stack target representation)))

(declaim (ftype (function (t t t t) t) compile-function-call-2))
(defun compile-function-call-2 (op args target representation)
  (let ((translation (gethash1 op (the hash-table *binary-operators*))))
    (when translation
      (compile-binary-operation translation args target representation))))

(declaim (ftype (function (t) t) fixnum-or-unboxed-variable-p))
(defun fixnum-or-unboxed-variable-p (arg)
  (or (fixnump arg)
      (unboxed-fixnum-variable arg)))

(declaim (ftype (function (t) t) emit-push-int))
(defun emit-push-int (arg)
  (if (fixnump arg)
      (emit-push-constant-int arg)
      (let ((variable (unboxed-fixnum-variable arg)))
        (if variable
            (emit 'iload (variable-register variable))
            (aver nil)))))

(declaim (ftype (function (t) t) emit-push-long))
(defun emit-push-long (arg)
  (cond ((eql arg 0)
         (emit 'lconst_0))
        ((eql arg 1)
         (emit 'lconst_1))
        ((fixnump arg)
         (emit-push-constant-int arg)
         (emit 'i2l))
        (t
         (let ((variable (unboxed-fixnum-variable arg)))
           (aver (not (null variable)))
           (aver (not (null (variable-register variable))))
           (emit 'iload (variable-register variable))
           (emit 'i2l)))))

(defknown p2-eq/neq (t t t) t)
(defun p2-eq/neq (form target representation)
  (aver (or (null representation) (eq representation :boolean)))
  (unless (check-arg-count form 2)
    (compile-function-call form target representation)
    (return-from p2-eq/neq))
  (let* ((op (%car form))
         (args (%cdr form))
         (arg1 (%car args))
         (arg2 (%cadr args)))
    (compile-forms-and-maybe-emit-clear-values arg1 'stack nil
					       arg2 'stack nil)
     (let ((LABEL1 (gensym))
           (LABEL2 (gensym)))
       (emit (if (eq op 'EQ) 'if_acmpne 'if_acmpeq) `,LABEL1)
       (emit-push-true representation)
       (emit 'goto `,LABEL2)
       (label `,LABEL1)
       (emit-push-false representation)
       (label `,LABEL2))
     (emit-move-from-stack target representation))
   t)

(defun emit-ifne-for-eql (representation instruction-type)
  (emit-invokevirtual +lisp-object-class+ "eql" instruction-type "Z")
  (case representation
    (:boolean)
    (t
     (let ((label1 (gensym))
	   (label2 (gensym)))
       (emit 'ifne label1)
       (emit-push-nil)
       (emit 'goto label2)
       (emit 'label label1)
       (emit-push-t)
       (emit 'label label2)))))

(defknown p2-eql (t t t) t)
(defun p2-eql (form target representation)
  (aver (or (null representation) (eq representation :boolean)))
  (unless (check-arg-count form 2)
    (compile-function-call form target representation)
    (return-from p2-eql))
  (let* ((arg1 (%cadr form))
         (arg2 (%caddr form))
         (type1 (derive-compiler-type arg1))
         (type2 (derive-compiler-type arg2)))
    (cond ((and (fixnum-type-p type1)
                (fixnum-type-p type2))
	   (compile-forms-and-maybe-emit-clear-values arg1 'stack :int
						      arg2 'stack :int)
           (let ((label1 (gensym))
                 (label2 (gensym)))
             (emit 'if_icmpeq `,label1)
             (emit-push-false representation)
             (emit 'goto `,label2)
             (emit 'label `,label1)
             (emit-push-true representation)
             (emit 'label `,label2)))
          ((fixnum-type-p type2)
	   (compile-forms-and-maybe-emit-clear-values arg1 'stack nil
						      arg2 'stack :int)
	   (emit-ifne-for-eql representation '("I")))
          ((fixnum-type-p type1)
	   (compile-forms-and-maybe-emit-clear-values arg1 'stack :int
						      arg2 'stack nil)
           (emit 'swap)
	   (emit-ifne-for-eql representation '("I")))
          ((eq type2 'CHARACTER)
	   (compile-forms-and-maybe-emit-clear-values arg1 'stack nil
						      arg2 'stack :char)
	   (emit-ifne-for-eql representation '("C")))
          ((eq type1 'CHARACTER)
	   (compile-forms-and-maybe-emit-clear-values arg1 'stack :char
						      arg2 'stack nil)
           (emit 'swap)
	   (emit-ifne-for-eql representation '("C")))
          (t
	   (compile-forms-and-maybe-emit-clear-values arg1 'stack nil
						      arg2 'stack nil)
           (case representation
             (:boolean
              (emit-invokevirtual +lisp-object-class+ "eql"
                                  (lisp-object-arg-types 1) "Z"))
             (t
              (emit-invokevirtual +lisp-object-class+ "EQL"
                                  (lisp-object-arg-types 1) +lisp-object+)))))
    (emit-move-from-stack target representation)))

(defknown p2-memq (t t t) t)
(defun p2-memq (form target representation)
;;   (format t "p2-memq representation = ~S~%" representation)
  (unless (check-arg-count form 2)
    (compile-function-call form target representation)
    (return-from p2-memq))
  (cond ((eq representation :boolean)
         (let* ((args (cdr form))
                (arg1 (first args))
                (arg2 (second args)))
           (compile-form arg1 'stack nil)
           (compile-form arg2 'stack nil)
           (emit-invokestatic +lisp-class+ "memq"
                              (lisp-object-arg-types 2) "Z")
           (emit-move-from-stack target representation)))
        (t
         (compile-function-call form target representation))))

(defknown p2-memql (t t t) t)
(defun p2-memql (form target representation)
  (unless (check-arg-count form 2)
    (compile-function-call form target representation)
    (return-from p2-memql))
  (cond ((eq representation :boolean)
         (let* ((args (cdr form))
                (arg1 (first args))
                (arg2 (second args))
                (type1 (derive-compiler-type arg1)))
           (compile-form arg1 'stack nil)
           (compile-form arg2 'stack nil)
           (cond ((eq type1 'SYMBOL) ; FIXME
                  (emit-invokestatic +lisp-class+ "memq"
                                     (lisp-object-arg-types 2) "Z"))
                 (t
                  (emit-invokestatic +lisp-class+ "memql"
                                     (lisp-object-arg-types 2) "Z")))
           (emit-move-from-stack target representation)))
        (t
         (compile-function-call form target representation))))

(defun p2-gensym (form target representation)
  (cond ((and (null representation) (null (cdr form)))
         (emit-push-current-thread)
         (emit-invokestatic +lisp-class+ "gensym"
                            (list +lisp-thread+) +lisp-symbol+)
         (emit-move-from-stack target))
        (t
         (compile-function-call form target representation))))

;; get symbol indicator &optional default => value
(defun p2-get (form target representation)
  (let* ((args (cdr form))
         (arg1 (first args))
         (arg2 (second args))
         (arg3 (third args)))
    (case (length args)
      ((2 3)
       (compile-form arg1 'stack nil)
       (compile-form arg2 'stack nil)
       (cond ((null arg3)
              (maybe-emit-clear-values arg1 arg2))
             (t
              (compile-form arg3 'stack nil)
              (maybe-emit-clear-values arg1 arg2 arg3)))
       (emit-invokestatic +lisp-class+ "get"
                          (lisp-object-arg-types (if arg3 3 2))
                          +lisp-object+)
       (fix-boxing representation nil)
       (emit-move-from-stack target representation))
      (t
       (compiler-warn "Wrong number of arguments for ~A (expected 2 or 3, but received ~D)."
                    'GET (length args))
       (compile-function-call form target representation)))))

;; getf plist indicator &optional default => value
(defun p2-getf (form target representation)
  (let* ((args (cdr form))
         (arg-count (length args)))
    (case arg-count
      ((2 3)
       (let ((arg1 (first args))
             (arg2 (second args))
             (arg3 (third args)))
	 (compile-forms-and-maybe-emit-clear-values arg1 'stack nil
						    arg2 'stack nil
						    arg3 'stack nil)
         (emit-invokestatic +lisp-class+ "getf"
                            (lisp-object-arg-types 3) +lisp-object+)
         (fix-boxing representation nil)
         (emit-move-from-stack target representation)))
      (t
       (compile-function-call form target representation)))))

;; gethash key hash-table &optional default => value, present-p
(defun p2-gethash (form target representation)
  (cond ((and (eq (car form) 'GETHASH1)
              (= (length form) 3)
              (eq (derive-type (%caddr form)) 'HASH-TABLE))
         (let ((key-form (%cadr form))
               (ht-form (%caddr form)))
           (compile-form ht-form 'stack nil)
           (emit 'checkcast +lisp-hash-table-class+)
           (compile-form key-form 'stack nil)
           (maybe-emit-clear-values ht-form key-form)
           (emit-invokevirtual +lisp-hash-table-class+ "gethash1"
                               (lisp-object-arg-types 1) +lisp-object+)
           (fix-boxing representation nil)
           (emit-move-from-stack target representation)))
        (t
         (compile-function-call form target representation))))

;; puthash key hash-table new-value &optional default => value
(defun p2-puthash (form target representation)
  (cond ((and (= (length form) 4)
              (eq (derive-type (%caddr form)) 'HASH-TABLE))
         (let ((key-form (%cadr form))
               (ht-form (%caddr form))
               (value-form (fourth form)))
           (compile-form ht-form 'stack nil)
           (emit 'checkcast +lisp-hash-table-class+)
           (compile-form key-form 'stack nil)
           (compile-form value-form 'stack nil)
           (maybe-emit-clear-values ht-form key-form value-form)
           (cond (target
                  (emit-invokevirtual +lisp-hash-table-class+ "puthash"
                                      (lisp-object-arg-types 2) +lisp-object+)
                  (fix-boxing representation nil)
                  (emit-move-from-stack target representation))
                 (t
                  (emit-invokevirtual +lisp-hash-table-class+ "put"
                                      (lisp-object-arg-types 2) nil)))))
        (t
         (compile-function-call form target representation))))

(defvar *functions-defined-in-current-file* nil)

(defun inline-ok (name)
  (declare (optimize speed))
  (cond ((notinline-p name)
         nil)
        ((built-in-function-p name)
         t)
        ((memq name *functions-defined-in-current-file*)
         t)
        (t
         nil)))

(defknown process-args (t) t)
(defun process-args (args)
  "Compiles forms specified as function call arguments.

The results are either accumulated on the stack or in an array
in order to call the relevant `execute' form. The function call
itself is *not* compiled by this function."
  (when args
    (let ((numargs (length args)))
      (let ((must-clear-values nil))
        (declare (type boolean must-clear-values))
        (cond ((<= numargs call-registers-limit)
               (dolist (arg args)
                 (compile-form arg 'stack nil)
                 (unless must-clear-values
                   (unless (single-valued-p arg)
                     (setf must-clear-values t)))))
              (t
               (emit-push-constant-int numargs)
               (emit 'anewarray +lisp-object-class+)
               (let ((i 0))
                 (dolist (arg args)
                   (emit 'dup)
                   (emit-push-constant-int i)
                   (compile-form arg 'stack nil)
                   (emit 'aastore) ; store value in array
                   (unless must-clear-values
                     (unless (single-valued-p arg)
                       (setf must-clear-values t)))
                   (incf i)))))
        (when must-clear-values
          (emit-clear-values)))))
  t)

(defknown lisp-object-arg-types (fixnum) list)
(let ((table (make-array 10)))
  (dotimes (i 10)
    (declare (type fixnum i))
    (setf (aref table i) (make-list i :initial-element +lisp-object+)))
  (defun lisp-object-arg-types (n)
    (declare (type fixnum n))
    (declare (optimize speed (safety 0)))
    (if (< n 10)
        (aref table n)
        (make-list n :initial-element +lisp-object+))))

(declaim (ftype (function (t) t) emit-call-execute))
(defun emit-call-execute (numargs)
  (let ((arg-types (if (<= numargs call-registers-limit)
                       (lisp-object-arg-types numargs)
                       (list +lisp-object-array+)))
        (return-type +lisp-object+))
    (emit-invokevirtual +lisp-object-class+ "execute" arg-types return-type)))

(declaim (ftype (function (t) t) emit-call-thread-execute))
(defun emit-call-thread-execute (numargs)
  (let ((arg-types (if (<= numargs call-registers-limit)
                       (lisp-object-arg-types (1+ numargs))
                       (list +lisp-object+ +lisp-object-array+)))
        (return-type +lisp-object+))
    (emit-invokevirtual +lisp-thread-class+ "execute" arg-types return-type)))

(defknown compile-function-call (t t t) t)
(defun compile-function-call (form target representation)
  (let ((op (car form))
        (args (cdr form)))
    (declare (type symbol op))
    (when (find-local-function op)
      (return-from compile-function-call
                   (compile-local-function-call form target representation)))
    (when (and (boundp '*defined-functions*) (boundp '*undefined-functions*))
      (unless (or (fboundp op)
                  (eq op (compiland-name *current-compiland*))
                  (memq op *defined-functions*)
                  (proclaimed-ftype op))
        (pushnew op *undefined-functions*)))
    (let ((numargs (length args)))
      (case numargs
        (1
         (when (compile-function-call-1 op args target representation)
           (return-from compile-function-call)))
        (2
         (when (compile-function-call-2 op args target representation)
           (return-from compile-function-call))))
      (let ((explain *explain*))
        (when (and explain (memq :calls explain))
          (let ((package (symbol-package op)))
            (when (or (eq package +cl-package+) (eq package (find-package "SYSTEM")))
              (format t ";   full call to ~S~%" op)))))
      (when (or (<= *speed* *debug*) *require-stack-frame*)
        (emit-push-current-thread))
      (cond ((eq op (compiland-name *current-compiland*)) ; recursive call
             (if (notinline-p op)
                 (emit 'getstatic *this-class* (declare-symbol op) +lisp-symbol+)
                 (emit 'aload 0)))
            ((null (symbol-package op))
             (let ((g (if *compile-file-truename*
                          (declare-object-as-string op)
                          (declare-object op))))
               (emit 'getstatic *this-class* g +lisp-object+)))
            (t
             (let ((name (lookup-known-symbol op)))
               (if name
                   (emit 'getstatic +lisp-symbol-class+ name +lisp-symbol+)
                   (emit 'getstatic *this-class* (declare-symbol op) +lisp-symbol+)))))
      (process-args args)
      (if (or (<= *speed* *debug*) *require-stack-frame*)
          (emit-call-thread-execute numargs)
          (emit-call-execute numargs))
      (fix-boxing representation (derive-compiler-type form))
      (emit-move-from-stack target representation))))

(defun compile-call (args)
  "Compiles a function call.

Depending on the `*speed*' and `*debug*' settings, a stack frame
is registered (or not)."
  (let ((numargs (length args)))
    (cond ((> *speed* *debug*)
           (process-args args)
           (emit-call-execute numargs))
          (t
           (emit-push-current-thread)
           (emit 'swap) ; Stack: thread function
           (process-args args)
           (emit-call-thread-execute numargs)))))

(define-source-transform funcall (&whole form fun &rest args)
  (cond ((> *debug* *speed*)
         form)
        ((and (consp fun)
              (eq (%car fun) 'FUNCTION)
              (symbolp (cadr fun)))
         `(,(cadr fun) ,@args))
        ((and (consp fun)
              (eq (%car fun) 'QUOTE))
         (let ((sym (cadr fun)))
           (if (and (symbolp sym)
                    (eq (symbol-package (truly-the symbol sym)) +cl-package+)
                    (not (special-operator-p sym))
                    (not (macro-function sym)))
               `(,(cadr fun) ,@args)
               form)))
        (t
         form)))

(define-source-transform mapcar (&whole form function &rest lists)
  (cond ((or (> *debug* *speed*)
             (> *space* *speed*))
         form)
        ((= (length lists) 1)
         (let ((list (gensym))
               (result (gensym))
               (temp (gensym)))
           `(let* ((,list ,(car lists))
                   (,result (list nil))
                   (,temp ,result))
              (loop
                (when (null ,list)
                  (return (cdr ,result)))
                (rplacd ,temp (setf ,temp (list (funcall ,function (car ,list)))))
                (setf ,list (cdr ,list))))))
        (t
         form)))

(define-source-transform mapc (&whole form function &rest lists)
  (cond ((or (> *debug* *speed*)
             (> *space* *speed*))
         form)
        ((= (length lists) 1)
         (let ((list (gensym))
               (result (gensym)))
           `(let* ((,list ,(car lists))
                   (,result ,list))
              (loop
                (when (null ,list)
                  (return ,result))
                (funcall ,function (car ,list))
                (setf ,list (%cdr ,list))))))
        (t
         form)))

;; (define-source-transform min (&whole form &rest args)
;;   (cond ((= (length args) 2)
;;          (let* ((arg1 (%car args))
;;                 (arg2 (%cadr args))
;;                 (sym1 (gensym))
;;                 (sym2 (gensym)))
;;            `(let ((,sym1 ,arg1)
;;                   (,sym2 ,arg2))
;;               (if (<= ,sym1 ,sym2) ,sym1 ,sym2))))
;;         (t
;;          form)))

;; (define-source-transform max (&whole form &rest args)
;;   (cond ((= (length args) 2)
;;          (let* ((arg1 (%car args))
;;                 (arg2 (%cadr args))
;;                 (sym1 (gensym))
;;                 (sym2 (gensym)))
;;            `(let ((,sym1 ,arg1)
;;                   (,sym2 ,arg2))
;;               (if (>= ,sym1 ,sym2) ,sym1 ,sym2))))
;;         (t
;;          form)))

(defknown p2-funcall (t t t) t)
(defun p2-funcall (form target representation)
  (unless (> (length form) 1)
    (compiler-warn "Wrong number of arguments for ~A." (car form))
    (compile-function-call form target representation)
    (return-from p2-funcall))
  (when (> *debug* *speed*)
    (return-from p2-funcall (compile-function-call form target representation)))
  (compile-forms-and-maybe-emit-clear-values (cadr form) 'stack nil)
  (compile-call (cddr form))
;;   (case representation
;;     (:int (emit-unbox-fixnum))
;;     (:char (emit-unbox-character)))
  (fix-boxing representation nil)
  (emit-move-from-stack target))

(defun save-variables (variables)
  (let ((saved-vars '()))
    (dolist (variable variables)
      (when (variable-closure-index variable)
        (let ((register (allocate-register)))
          (emit 'aload (compiland-closure-register *current-compiland*))
          (emit-push-constant-int (variable-closure-index variable))
          (emit 'aaload)
          (emit 'astore register)
          (push (cons variable register) saved-vars))))
    saved-vars))

(defun restore-variables (saved-vars)
  (dolist (saved-var saved-vars)
    (let ((variable (car saved-var))
          (register (cdr saved-var)))
      (emit 'aload (compiland-closure-register *current-compiland*))
      (emit-push-constant-int (variable-closure-index variable))
      (emit 'aload register)
      (emit 'aastore))))

(defknown compile-local-function-call (t t t) t)
(defun compile-local-function-call (form target representation)
  "Compiles a call to a function marked as `*child-p*'; a local function.

Functions this applies to can be FLET, LABELS, LAMBDA or NAMED-LAMBDA.
Note: DEFUN implies a named lambda."
  (let* ((compiland *current-compiland*)
         (op (car form))
         (args (cdr form))
         (local-function (find-local-function op))
         (*register* *register*)
         (saved-vars '()))
    (cond ((local-function-variable local-function)
           ;; LABELS
           (dformat t "compile-local-function-call LABELS case variable = ~S~%"
                   (variable-name (local-function-variable local-function)))
           (unless (null (compiland-parent compiland))
             (setf saved-vars
                   (save-variables (intersection
                                    (compiland-arg-vars (local-function-compiland local-function))
                                    *visible-variables*))))
;;            (emit 'var-ref (local-function-variable local-function) 'stack)
           (compile-var-ref (make-var-ref (local-function-variable local-function)) 'stack nil))
          (t
           (dformat t "compile-local-function-call default case~%")
           (let* ((g (if *compile-file-truename*
                         (declare-local-function local-function)
                         (declare-object (local-function-function local-function)))))
             (emit 'getstatic *this-class* g +lisp-object+) ; Stack: template-function
             (when *closure-variables*
               (emit 'checkcast +lisp-ctf-class+)
               (emit 'aload (compiland-closure-register compiland))
               (emit-invokestatic +lisp-class+ "makeCompiledClosure"
                                  (list +lisp-object+ +lisp-object-array+)
                                  +lisp-object+)))))
    (let ((must-clear-values nil))
      (declare (type boolean must-clear-values))
      (cond ((> (length args) call-registers-limit)
             (emit-push-constant-int (length args))
             (emit 'anewarray +lisp-object-class+)
             (let ((i 0))
               (dolist (arg args)
                 (emit 'dup)
                 (emit-push-constant-int i)
                 (compile-form arg 'stack nil)
                 (emit 'aastore) ; store value in array
                 (unless must-clear-values
                   (unless (single-valued-p arg)
                     (setf must-clear-values t)))
                 (incf i)))) ; array left on stack here
            (t
             (dolist (arg args)
               (compile-form arg 'stack nil)
               (unless must-clear-values
                 (unless (single-valued-p arg)
                   (setf must-clear-values t)))))) ; args left on stack here
      (when must-clear-values
        (emit-clear-values)))
    (let* ((arg-count (length args))
           (arg-types (if (<= arg-count call-registers-limit)
                          (lisp-object-arg-types arg-count)
                          (list +lisp-object-array+))) ;; FIXME
           (result-type +lisp-object+))
      (emit-invokevirtual +lisp-object-class+ "execute" arg-types result-type))
    (fix-boxing representation nil)
    (emit-move-from-stack target representation)
    (when saved-vars
      (restore-variables saved-vars)))
  t)

;; Note that /= is not transitive, so we don't handle it here.
(defknown p2-numeric-comparison (t t t) t)
(defun p2-numeric-comparison (form target representation)
  (aver (or (null representation) (eq representation :boolean)))
  (let ((op (car form))
        (args (%cdr form)))
    (case (length args)
      (2
       (let* ((arg1 (%car args))
              (arg2 (%cadr args))
              (type1 (derive-compiler-type arg1))
              (type2 (derive-compiler-type arg2)))
         (cond ((and (integerp arg1) (integerp arg2))
                (let ((result (funcall op arg1 arg2)))
                  (if result
                      (emit-push-true representation)
                      (emit-push-false representation)))
                (emit-move-from-stack target representation)
                (return-from p2-numeric-comparison))
               ((and (fixnum-type-p type1)
                     (fixnum-type-p type2))
                (let ((LABEL1 (gensym))
                      (LABEL2 (gensym)))
		  (compile-forms-and-maybe-emit-clear-values arg1 'stack :int
							     arg2 'stack :int)
                  (emit (case op
                          (<  'if_icmpge)
                          (<= 'if_icmpgt)
                          (>  'if_icmple)
                          (>= 'if_icmplt)
                          (=  'if_icmpne))
                        LABEL1)
                  (emit-push-true representation)
                  (emit 'goto LABEL2)
                  (label LABEL1)
                  (emit-push-false representation)
                  (label LABEL2))
                (emit-move-from-stack target representation)
                (return-from p2-numeric-comparison))
               ((and (java-long-type-p type1)
                     (java-long-type-p type2))
                (let ((LABEL1 (gensym))
                      (LABEL2 (gensym)))
		  (compile-forms-and-maybe-emit-clear-values arg1 'stack :long
							     arg2 'stack :long)
                  (emit 'lcmp)
                  (emit (case op
                          (<  'ifge)
                          (<= 'ifgt)
                          (>  'ifle)
                          (>= 'iflt)
                          (=  'ifne))
                        LABEL1)
                  (emit-push-true representation)
                  (emit 'goto LABEL2)
                  (label LABEL1)
                  (emit-push-false representation)
                  (label LABEL2))
                (emit-move-from-stack target representation)
                (return-from p2-numeric-comparison))
               ((fixnump arg2)
		(compile-forms-and-maybe-emit-clear-values arg1 'stack nil)
                (emit-push-constant-int arg2)
                (emit-invokevirtual +lisp-object-class+
                                    (case op
                                      (<  "isLessThan")
                                      (<= "isLessThanOrEqualTo")
                                      (>  "isGreaterThan")
                                      (>= "isGreaterThanOrEqualTo")
                                      (=  "isEqualTo"))
                                    '("I")
                                    "Z")
                ;; Java boolean on stack here
                (case representation
                  (:boolean)
                  (t
                   (let ((LABEL1 (gensym))
                         (LABEL2 (gensym)))
                     (emit 'ifeq LABEL1)
                     (emit-push-t)
                     (emit 'goto LABEL2)
                     (label LABEL1)
                     (emit-push-nil)
                     (label LABEL2))))
                (emit-move-from-stack target representation)
                (return-from p2-numeric-comparison)))))
      (3
       (when (dolist (arg args t)
               (unless (fixnum-type-p (derive-compiler-type arg))
                 (return nil)))
         (let* ((arg1 (%car args))
                (arg2 (%cadr args))
                (arg3 (%caddr args))
                (test (case op
                        (<  'if_icmpge)
                        (<= 'if_icmpgt)
                        (>  'if_icmple)
                        (>= 'if_icmplt)
                        (=  'if_icmpne)))
                (LABEL1 (gensym))
                (LABEL2 (gensym))
                ;; If we do both tests, we need to use the arg2 value twice,
                ;; so we store that value in a temporary register.
                (*register* *register*)
                (arg2-register
                 (unless (and (or (node-constant-p arg2)
                                  (var-ref-p arg2))
                              (node-constant-p arg3))
                   (allocate-register)))
                (arg3-register
                 (unless (node-constant-p arg3) (allocate-register))))
           (compile-form arg1 'stack :int)
           (compile-form arg2 'stack :int)
           (when arg2-register
             (emit 'dup)
             (emit 'istore arg2-register))
           (cond (arg3-register
                  (compile-form arg3 'stack :int)
                  (emit 'istore arg3-register)
                  (maybe-emit-clear-values arg1 arg2 arg3))
                 (t
                  (maybe-emit-clear-values arg1 arg2)))
           ;; First test.
           (emit test LABEL1)
           ;; Second test.
           (cond (arg2-register
                  (emit 'iload arg2-register))
                 (t
                  (compile-form arg2 'stack :int)))
           (cond (arg3-register
                  (emit 'iload arg3-register))
                 (t
                  (compile-form arg3 'stack :int)))
           (emit test LABEL1)
           (emit-push-true representation)
           (emit 'goto LABEL2)
           (label LABEL1)
           (emit-push-false representation)
           (label LABEL2)
           (emit-move-from-stack target representation)
           (return-from p2-numeric-comparison))))))
  ;; Still here?
  (compile-function-call form target representation))

(defparameter *p2-test-handlers* nil)

(defun p2-test-handler (op)
  (gethash1 op (the hash-table *p2-test-handlers*)))

(defun initialize-p2-test-handlers ()
  (let ((ht (make-hash-table :test 'eq)))
    (dolist (pair '(
;;                     (CHAR= p2-test-char=)
                    (/=                 p2-test-/=)
                    (<                  p2-test-numeric-comparison)
                    (<=                 p2-test-numeric-comparison)
                    (=                  p2-test-numeric-comparison)
                    (>                  p2-test-numeric-comparison)
                    (>=                 p2-test-numeric-comparison)
                    (AND                p2-test-and)
                    (ATOM               p2-test-atom)
                    (BIT-VECTOR-P       p2-test-bit-vector-p)
                    (CHAR=              p2-test-char=)
                    (CHARACTERP         p2-test-characterp)
                    (CLASSP             p2-test-classp)
                    (CONSP              p2-test-consp)
                    (CONSTANTP          p2-test-constantp)
                    (ENDP               p2-test-endp)
                    (EQ                 p2-test-eq)
                    (NEQ                p2-test-neq)
                    (EQL                p2-test-eql)
                    (EQUAL              p2-test-equality)
                    (EQUALP             p2-test-equality)
                    (EVENP              p2-test-evenp)
                    (FIXNUMP            p2-test-fixnump)
                    (FLOATP             p2-test-floatp)
                    (INTEGERP           p2-test-integerp)
                    (LISTP              p2-test-listp)
                    (MEMQ               p2-test-memq)
                    (MEMQL              p2-test-memql)
                    (MINUSP             p2-test-minusp)
                    (NOT                p2-test-not/null)
                    (NULL               p2-test-not/null)
                    (NUMBERP            p2-test-numberp)
                    (PACKAGEP           p2-test-packagep)
                    (ODDP               p2-test-oddp)
                    (PLUSP              p2-test-plusp)
                    (RATIONALP          p2-test-rationalp)
                    (REALP              p2-test-realp)
                    (SIMPLE-TYPEP       p2-test-simple-typep)
                    (SIMPLE-VECTOR-P    p2-test-simple-vector-p)
                    (SPECIAL-OPERATOR-P p2-test-special-operator-p)
                    (SPECIAL-VARIABLE-P p2-test-special-variable-p)
                    (STRINGP            p2-test-stringp)
                    (SYMBOLP            p2-test-symbolp)
                    (VECTORP            p2-test-vectorp)
                    (ZEROP              p2-test-zerop)
                    ))
      (setf (gethash (%car pair) ht) (%cadr pair)))
    (setf *p2-test-handlers* ht)))

(initialize-p2-test-handlers)

(defknown p2-test-predicate (t t) t)
(defun p2-test-predicate (form java-predicate)
  (when (check-arg-count form 1)
    (let ((arg (%cadr form)))
      (compile-forms-and-maybe-emit-clear-values arg 'stack nil)
      (emit-invokevirtual +lisp-object-class+ java-predicate nil "Z")
      'ifeq)))

(declaim (ftype (function (t t) t) p2-test-instanceof-predicate))
(defun p2-test-instanceof-predicate (form java-class)
  (when (check-arg-count form 1)
    (let ((arg (%cadr form)))
      (compile-forms-and-maybe-emit-clear-values arg 'stack nil)
      (emit 'instanceof java-class)
      'ifeq)))

(defun p2-test-bit-vector-p (form)
  (p2-test-instanceof-predicate form +lisp-abstract-bit-vector-class+))

(defun p2-test-characterp (form)
  (p2-test-instanceof-predicate form +lisp-character-class+))

;; constantp form &optional environment => generalized-boolean
(defun p2-test-constantp (form)
  (when (= (length form) 2)
    (let ((arg (%cadr form)))
      (compile-forms-and-maybe-emit-clear-values arg 'stack nil)
      (emit-invokevirtual +lisp-object-class+ "constantp" nil "Z")
      'ifeq)))

(defun p2-test-endp (form)
  (p2-test-predicate form "endp"))

(defun p2-test-evenp (form)
  (when (check-arg-count form 1)
    (let ((arg (%cadr form)))
      (cond ((fixnum-type-p (derive-compiler-type arg))
	     (compile-forms-and-maybe-emit-clear-values arg 'stack :int)
             (emit-push-constant-int 1)
             (emit 'iand)
             'ifne)
            (t
             (p2-test-predicate form "evenp"))))))

(defun p2-test-oddp (form)
  (when (check-arg-count form 1)
    (let ((arg (%cadr form)))
      (cond ((fixnum-type-p (derive-compiler-type arg))
	     (compile-forms-and-maybe-emit-clear-values arg 'stack :int)
             (emit-push-constant-int 1)
             (emit 'iand)
             'ifeq)
            (t
             (p2-test-predicate form "oddp"))))))

(defun p2-test-floatp (form)
  (p2-test-predicate form "floatp"))

(defun p2-test-integerp (form)
  (p2-test-predicate form "integerp"))

(defun p2-test-listp (form)
  (when (check-arg-count form 1)
    (let* ((arg (%cadr form))
           (arg-type (derive-compiler-type arg)))
      (cond ((memq arg-type '(CONS LIST NULL))
	     (compile-forms-and-maybe-emit-clear-values arg nil nil)
             :consequent)
            ((neq arg-type t)
	     (compile-forms-and-maybe-emit-clear-values arg nil nil)
             :alternate)
            (t
             (p2-test-predicate form "listp"))))))

(defun p2-test-minusp (form)
  (when (check-arg-count form 1)
    (let ((arg (%cadr form)))
      (cond ((fixnum-type-p (derive-compiler-type arg))
	     (compile-forms-and-maybe-emit-clear-values arg 'stack :int)
             'ifge)
            (t
             (p2-test-predicate form "minusp"))))))

(defun p2-test-plusp (form)
  (when (check-arg-count form 1)
    (let ((arg (%cadr form)))
      (cond ((fixnum-type-p (derive-compiler-type arg))
	   (compile-forms-and-maybe-emit-clear-values arg 'stack :int)
             'ifle)
            (t
             (p2-test-predicate form "plusp"))))))

(defun p2-test-zerop (form)
  (when (check-arg-count form 1)
    (let ((arg (%cadr form)))
      (cond ((fixnum-type-p (derive-compiler-type arg))
	   (compile-forms-and-maybe-emit-clear-values arg 'stack :int)
             'ifne)
            (t
             (p2-test-predicate form "zerop"))))))

(defun p2-test-numberp (form)
  (p2-test-predicate form "numberp"))

(defun p2-test-packagep (form)
  (p2-test-instanceof-predicate form +lisp-package-class+))

(defun p2-test-rationalp (form)
  (p2-test-predicate form "rationalp"))

(defun p2-test-realp (form)
  (p2-test-predicate form "realp"))

(defun p2-test-special-operator-p (form)
  (p2-test-predicate form "isSpecialOperator"))

(defun p2-test-special-variable-p (form)
  (p2-test-predicate form "isSpecialVariable"))

(defun p2-test-classp (form)
  (p2-test-instanceof-predicate form +lisp-class-class+))

(defun p2-test-symbolp (form)
  (p2-test-instanceof-predicate form +lisp-symbol-class+))

(defun p2-test-consp (form)
  (p2-test-instanceof-predicate form +lisp-cons-class+))

(defun p2-test-atom (form)
  (p2-test-instanceof-predicate form +lisp-cons-class+)
  'ifne)

(defun p2-test-fixnump (form)
  (p2-test-instanceof-predicate form +lisp-fixnum-class+))

(defun p2-test-stringp (form)
  (p2-test-instanceof-predicate form +lisp-abstract-string-class+))

(defun p2-test-vectorp (form)
  (p2-test-instanceof-predicate form +lisp-abstract-vector-class+))

(defun p2-test-simple-vector-p (form)
  (p2-test-instanceof-predicate form +lisp-simple-vector-class+))

(defknown compile-test-form (t) t)
(defun compile-test-form (test-form)
  (when (consp test-form)
    (let* ((op (%car test-form))
           (handler (p2-test-handler op))
           (result (and handler (funcall handler test-form))))
      (when result
        (return-from compile-test-form result))))
  (cond ((eq test-form t)
         :consequent)
        ((null test-form)
         :alternate)
        ((eq (derive-compiler-type test-form) 'BOOLEAN)
	 (compile-forms-and-maybe-emit-clear-values test-form 'stack :boolean)
         'ifeq)
        (t
	 (compile-forms-and-maybe-emit-clear-values test-form 'stack nil)
         (emit-push-nil)
         'if_acmpeq)))

(defun p2-test-not/null (form)
  (when (check-arg-count form 1)
    (let* ((arg (%cadr form))
           (result (compile-test-form arg)))
      (ecase result
        ('if_acmpeq  'if_acmpne)
        ('if_acmpne  'if_acmpeq)
        ('ifeq       'ifne)
        ('ifne       'ifeq)
        ('iflt       'ifge)
        ('ifge       'iflt)
        ('ifgt       'ifle)
        ('ifle       'ifgt)
        ('if_icmpeq  'if_icmpne)
        ('if_icmpne  'if_icmpeq)
        ('if_icmplt  'if_icmpge)
        ('if_icmpge  'if_icmplt)
        ('if_icmpgt  'if_icmple)
        ('if_icmple  'if_icmpgt)
        (:alternate  :consequent)
        (:consequent :alternate)))))

(defun p2-test-char= (form)
  (when (check-arg-count form 2)
    (let* ((arg1 (%cadr form))
           (arg2 (%caddr form)))
      (compile-forms-and-maybe-emit-clear-values arg1 'stack :char
						 arg2 'stack :char)
      'if_icmpne)))

(defun p2-test-eq (form)
  (when (check-arg-count form 2)
    (let ((arg1 (%cadr form))
          (arg2 (%caddr form)))
      (compile-forms-and-maybe-emit-clear-values arg1 'stack nil
						 arg2 'stack nil)
     'if_acmpne)))

(defun p2-test-and (form)
  (let ((args (cdr form)))
    (case (length args)
      (0
       :consequent)
      (1
       (compile-test-form (%car args)))
      (2
       (compile-form form 'stack :boolean)
       'ifeq)
      (t
       (compile-forms-and-maybe-emit-clear-values form 'stack nil)
       (emit-push-nil)
       'if_acmpeq))))

(defun p2-test-neq (form)
  (p2-test-eq form)
  'if_acmpeq)

(defun p2-test-eql (form)
  (when (check-arg-count form 2)
    (let* ((arg1 (%cadr form))
           (arg2 (%caddr form))
           (type1 (derive-compiler-type arg1))
           (type2 (derive-compiler-type arg2)))
      (cond ((and (fixnum-type-p type1) (fixnum-type-p type2))
	     (compile-forms-and-maybe-emit-clear-values arg1 'stack :int
							arg2 'stack :int)
             'if_icmpne)
            ((and (eq type1 'CHARACTER) (eq type2 'CHARACTER))
	     (compile-forms-and-maybe-emit-clear-values arg1 'stack :char
							arg2 'stack :char)
             'if_icmpne)
            ((eq type2 'CHARACTER)
	     (compile-forms-and-maybe-emit-clear-values arg1 'stack nil
							arg2 'stack :char)
             (emit-invokevirtual +lisp-object-class+ "eql" '("C") "Z")
             'ifeq)
            ((eq type1 'CHARACTER)
	     (compile-forms-and-maybe-emit-clear-values arg1 'stack :char
							arg2 'stack nil)
             (emit 'swap)
             (emit-invokevirtual +lisp-object-class+ "eql" '("C") "Z")
             'ifeq)
            ((fixnum-type-p type2)
	     (compile-forms-and-maybe-emit-clear-values arg1 'stack nil
							arg2 'stack :int)
             (emit-invokevirtual +lisp-object-class+ "eql" '("I") "Z")
             'ifeq)
            ((fixnum-type-p type1)
	     (compile-forms-and-maybe-emit-clear-values arg1 'stack :int
							arg2 'stack nil)
             (emit 'swap)
             (emit-invokevirtual +lisp-object-class+ "eql" '("I") "Z")
             'ifeq)
            (t
	     (compile-forms-and-maybe-emit-clear-values arg1 'stack nil
							arg2 'stack nil)
             (emit-invokevirtual +lisp-object-class+ "eql"
                                 (lisp-object-arg-types 1) "Z")
             'ifeq)))))

(defun p2-test-equality (form)
;;   (format t "p2-test-equality ~S~%" (%car form))
  (when (check-arg-count form 2)
    (let* ((op (%car form))
           (translated-op (ecase op
;;                             (EQL    "eql")
                            (EQUAL  "equal")
                            (EQUALP "equalp")))
           (arg1 (%cadr form))
           (arg2 (%caddr form)))
      (cond ((fixnum-type-p (derive-compiler-type arg2))
	     (compile-forms-and-maybe-emit-clear-values arg1 'stack nil
							arg2 'stack :int)
             (emit-invokevirtual +lisp-object-class+
                                 translated-op
                                 '("I") "Z"))
            (t
	     (compile-forms-and-maybe-emit-clear-values arg1 'stack nil
							arg2 'stack nil)
             (emit-invokevirtual +lisp-object-class+
                                 translated-op
                                 (lisp-object-arg-types 1) "Z")))
      'ifeq)))

(defun p2-test-simple-typep (form)
  (when (check-arg-count form 2)
    (let ((arg1 (%cadr form))
          (arg2 (%caddr form)))
      (compile-forms-and-maybe-emit-clear-values arg1 'stack nil
						 arg2 'stack nil)
      (emit-invokevirtual +lisp-object-class+ "typep"
                          (lisp-object-arg-types 1) +lisp-object+)
      (emit-push-nil)
      'if_acmpeq)))

(defun p2-test-memq (form)
  (when (check-arg-count form 2)
    (let ((arg1 (%cadr form))
          (arg2 (%caddr form)))
      (compile-forms-and-maybe-emit-clear-values arg1 'stack nil
						 arg2 'stack nil)
      (emit-invokestatic +lisp-class+ "memq"
                         (lisp-object-arg-types 2) "Z")
      'ifeq)))

(defun p2-test-memql (form)
  (when (check-arg-count form 2)
    (let ((arg1 (%cadr form))
          (arg2 (%caddr form)))
      (compile-forms-and-maybe-emit-clear-values arg1 'stack nil
						 arg2 'stack nil)
      (emit-invokestatic +lisp-class+ "memql"
                         (lisp-object-arg-types 2) "Z")
      'ifeq)))

(defun p2-test-/= (form)
  (when (= (length form) 3)
    (let* ((arg1 (%cadr form))
           (arg2 (%caddr form))
           (type1 (derive-compiler-type arg1))
           (type2 (derive-compiler-type arg2)))
      (cond ((and (numberp arg1) (numberp arg2))
             (if (/= arg1 arg2) :consequent :alternate))
            ((and (fixnum-type-p type1)
                  (fixnum-type-p type2))
	     (compile-forms-and-maybe-emit-clear-values arg1 'stack :int
							arg2 'stack :int)
             'if_icmpeq)
            ((fixnum-type-p type2)
	     (compile-forms-and-maybe-emit-clear-values arg1 'stack nil
							arg2 'stack :int)
             (emit-invokevirtual +lisp-object-class+ "isNotEqualTo" '("I") "Z")
             'ifeq)
            ((fixnum-type-p type1)
             ;; FIXME Compile the args in reverse order and avoid the swap if
             ;; either arg is a fixnum or a lexical variable.
	     (compile-forms-and-maybe-emit-clear-values arg1 'stack :int
							arg2 'stack nil)
             (emit 'swap)
             (emit-invokevirtual +lisp-object-class+ "isNotEqualTo" '("I") "Z")
             'ifeq)
            (t
	     (compile-forms-and-maybe-emit-clear-values arg1 'stack nil
							arg2 'stack nil)
             (emit-invokevirtual +lisp-object-class+ "isNotEqualTo"
                                 (lisp-object-arg-types 1) "Z")
             'ifeq)))))

(defun p2-test-numeric-comparison (form)
  (when (check-min-args form 1)
    (when (= (length form) 3)
      (let* ((op (%car form))
             (args (%cdr form))
             (arg1 (%car args))
             (arg2 (%cadr args))
             (type1 (derive-compiler-type arg1))
             (type2 (derive-compiler-type arg2)))
        (cond ((and (fixnump arg1) (fixnump arg2))
               (if (funcall op arg1 arg2) :consequent :alternate))
              ((and (fixnum-type-p type1) (fixnum-type-p type2))
	       (compile-forms-and-maybe-emit-clear-values arg1 'stack :int
							  arg2 'stack :int)
               (ecase op
                 (<  'if_icmpge)
                 (<= 'if_icmpgt)
                 (>  'if_icmple)
                 (>= 'if_icmplt)
                 (=  'if_icmpne)))
              ((and (java-long-type-p type1) (java-long-type-p type2))
	       (compile-forms-and-maybe-emit-clear-values arg1 'stack :long
							  arg2 'stack :long)
               (emit 'lcmp)
               (ecase op
                 (<  'ifge)
                 (<= 'ifgt)
                 (>  'ifle)
                 (>= 'iflt)
                 (=  'ifne)))
              ((fixnum-type-p type2)
	       (compile-forms-and-maybe-emit-clear-values arg1 'stack nil
							  arg2 'stack :int)
               (emit-invokevirtual +lisp-object-class+
                                   (ecase op
                                     (<  "isLessThan")
                                     (<= "isLessThanOrEqualTo")
                                     (>  "isGreaterThan")
                                     (>= "isGreaterThanOrEqualTo")
                                     (=  "isEqualTo"))
                                   '("I") "Z")
               'ifeq)
              ((fixnum-type-p type1)
               ;; FIXME We can compile the args in reverse order and avoid
               ;; the swap if either arg is a fixnum or a lexical variable.
	       (compile-forms-and-maybe-emit-clear-values arg1 'stack :int
							  arg2 'stack nil)
               (emit 'swap)
               (emit-invokevirtual +lisp-object-class+
                                   (ecase op
                                     (<  "isGreaterThan")
                                     (<= "isGreaterThanOrEqualTo")
                                     (>  "isLessThan")
                                     (>= "isLessThanOrEqualTo")
                                     (=  "isEqualTo"))
                                   '("I") "Z")
               'ifeq)
              (t
	       (compile-forms-and-maybe-emit-clear-values arg1 'stack nil
							  arg2 'stack nil)
               (emit-invokevirtual +lisp-object-class+
                                   (ecase op
                                     (<  "isLessThan")
                                     (<= "isLessThanOrEqualTo")
                                     (>  "isGreaterThan")
                                     (>= "isGreaterThanOrEqualTo")
                                     (=  "isEqualTo"))
                                   (lisp-object-arg-types 1) "Z")
               'ifeq))))))

(defknown p2-if-or (t t t) t)
(defun p2-if-or (form target representation)
  (let* ((test (second form))
         (consequent (third form))
         (alternate (fourth form))
         (LABEL1 (gensym))
         (LABEL2 (gensym)))
    (aver (and (consp test) (eq (car test) 'OR)))
    (let* ((args (cdr test)))
      (case (length args)
        (0
         (compile-form alternate target representation))
        (1
         (p2-if (list 'IF (%car args) consequent alternate) target representation))
        (t
         (dolist (arg args)
           (cond ((and (consp arg) (eq (first arg) 'EQ))
                  ;; ERROR CHECKING HERE!
                  (let ((arg1 (second arg))
                        (arg2 (third arg)))
		    (compile-forms-and-maybe-emit-clear-values arg1 'stack nil
							       arg2 'stack nil)
                    (emit 'if_acmpeq LABEL1)))
                 ((eq (derive-compiler-type arg) 'BOOLEAN)
		  (compile-forms-and-maybe-emit-clear-values arg 'stack :boolean)
                  (emit 'ifne LABEL1))
                 (t
		  (compile-forms-and-maybe-emit-clear-values arg 'stack nil)
                  (emit-push-nil)
                  (emit 'if_acmpne LABEL1))))
         (compile-form alternate target representation)
         (emit 'goto LABEL2)
         (label LABEL1)
         (compile-form consequent target representation)
         (label LABEL2))))))

(defknown p2-if-and (t t t) t)
(defun p2-if-and (form target representation)
  (let* ((test (second form))
         (consequent (third form))
         (alternate (fourth form))
         (LABEL1 (gensym))
         (LABEL2 (gensym)))
    (aver (and (consp test) (eq (car test) 'AND)))
    (let* ((args (cdr test)))
      (case (length args)
        (0
         (compile-form consequent target representation))
        (1
         (p2-if (list 'IF (%car args) consequent alternate) target representation))
        (t
         (dolist (arg args)
;;            (let ((type (derive-compiler-type arg)))
;;              (cond
;;               ((eq type 'BOOLEAN)
	   (compile-forms-and-maybe-emit-clear-values arg 'stack :boolean)
	   (emit 'ifeq LABEL1)
;;                )
;;                    (t
;;                     (compile-form arg 'stack nil)
;;                     (maybe-emit-clear-values arg)
;;                     (emit-push-nil)
;;                     (emit 'if_acmpeq LABEL1))
;;                    )
;;              )
           )
         (compile-form consequent target representation)
         (emit 'goto LABEL2)
         (label LABEL1)
         (compile-form alternate target representation)
         (label LABEL2))))))

(defknown p2-if-not-and (t t t) t)
(defun p2-if-not-and (form target representation)
;;   (format t "p2-if-not-and~%")
;;   (aver (eq (first form) 'IF))
;;   (aver (consp (second form)))
;;   (aver (memq (first (second form)) '(NOT NULL)))
;;   (aver (eq (first (second (second form))) 'AND))
  (let* ((inverted-test (second (second form)))
         (consequent (third form))
         (alternate (fourth form))
         (LABEL1 (gensym))
         (LABEL2 (gensym)))
;;     (aver (and (consp inverted-test) (eq (car inverted-test) 'AND)))
    (let* ((args (cdr inverted-test)))
      (case (length args)
        (0
         (compile-form alternate target representation))
        (1
         (p2-if (list 'IF (%car args) alternate consequent) target representation))
        (t
         (dolist (arg args)
           (let ((type (derive-compiler-type arg)))
             (cond ((eq type 'BOOLEAN)
		    (compile-forms-and-maybe-emit-clear-values arg 'stack :boolean)
                    (emit 'ifeq LABEL1))
                   (t
		    (compile-forms-and-maybe-emit-clear-values arg 'stack nil)
                    (emit-push-nil)
                    (emit 'if_acmpeq LABEL1)))))
         (compile-form alternate target representation)
         (emit 'goto LABEL2)
         (label LABEL1)
         (compile-form consequent target representation)
         (label LABEL2))))))

(defknown p2-if (t t t) t)
(defun p2-if (form target representation)
  (let* ((test (second form))
         (consequent (third form))
         (alternate (fourth form))
         (LABEL1 (gensym))
         (LABEL2 (gensym)))
    (cond ((eq test t)
           (compile-form consequent target representation))
          ((null test)
           (compile-form alternate target representation))
          ((numberp test)
           (compile-form consequent target representation))
          ((equal (derive-compiler-type test) +true-type+)
	   (compile-forms-and-maybe-emit-clear-values test nil nil)
           (compile-form consequent target representation))
          ((and (consp test) (eq (car test) 'OR))
           (p2-if-or form target representation))
          ((and (consp test) (eq (car test) 'AND))
           (p2-if-and form target representation))
          ((and (consp test)
                (memq (first test) '(NOT NULL))
                (consp (second test))
                (eq (first (second test)) 'AND))
           (p2-if-not-and form target representation))
          (t
           (let ((result (compile-test-form test)))
             (case result
               (:consequent
                (compile-form consequent target representation))
               (:alternate
                (compile-form alternate target representation))
               (t
                (emit result LABEL1)
                (compile-form consequent target representation)
                (emit 'goto LABEL2)
                (label LABEL1)
                (compile-form alternate target representation)
                (label LABEL2))))))))

(defun compile-multiple-value-list (form target representation)
  (emit-clear-values)
  (compile-form (second form) 'stack nil)
  (emit-invokestatic +lisp-class+ "multipleValueList"
                     (lisp-object-arg-types 1) +lisp-object+)
  (fix-boxing representation nil)
  (emit-move-from-stack target))

(defun compile-multiple-value-prog1 (form target representation)
  (let ((first-subform (cadr form))
        (subforms (cddr form))
        (result-register (allocate-register))
        (values-register (allocate-register)))
    ;; Make sure there are no leftover values from previous calls.
    (emit-clear-values)
    (compile-form first-subform result-register nil)
    ;; Save multiple values returned by first subform.
    (emit-push-current-thread)
    (emit 'getfield +lisp-thread-class+ "_values" "[Lorg/armedbear/lisp/LispObject;")
    (emit 'astore values-register)
    (dolist (subform subforms)
      (compile-form subform nil nil))
    ;; Restore multiple values returned by first subform.
    (emit-push-current-thread)
    (emit 'aload values-register)
    (emit 'putfield +lisp-thread-class+ "_values" "[Lorg/armedbear/lisp/LispObject;")
    ;; Result.
    (emit 'aload result-register)
    (fix-boxing representation nil)
    (emit-move-from-stack target)))

(defun compile-multiple-value-call (form target representation)
  ;; FIXME What if we're called with a non-NIL representation?
  (aver (null representation))
  (case (length form)
    (1
     (error "Wrong number of arguments for MULTIPLE-VALUE-CALL."))
    (2
     (compile-form (second form) 'stack nil)
     (emit-invokestatic +lisp-class+ "coerceToFunction"
                        (lisp-object-arg-types 1) +lisp-object+)
     (emit-invokevirtual +lisp-object-class+ "execute" nil +lisp-object+))
    (3
     (let* ((*register* *register*)
            (function-register (allocate-register)))
       (compile-form (second form) function-register nil)
       (compile-form (third form) 'stack nil)
       (emit 'aload function-register)
       (emit-push-current-thread)
       (emit-invokestatic +lisp-class+ "multipleValueCall1"
                          (list +lisp-object+ +lisp-object+ +lisp-thread+)
                          +lisp-object+)))
    (t
     ;; The general case.
     (let* ((*register* *register*)
            (function-register (allocate-register))
            (values-register (allocate-register)))
       (compile-form (second form) 'stack nil)
       (emit-invokestatic +lisp-class+ "coerceToFunction"
                          (lisp-object-arg-types 1) +lisp-object+)
       (emit-move-from-stack function-register)
       (emit 'aconst_null)
       (emit 'astore values-register)
       (dolist (values-form (cddr form))
         (compile-form values-form 'stack nil)
         (emit-push-current-thread)
         (emit 'swap)
         (emit 'aload values-register)
         (emit-invokevirtual +lisp-thread-class+ "accumulateValues"
                             (list +lisp-object+ +lisp-object-array+)
                             +lisp-object-array+)
         (emit 'astore values-register)
         (maybe-emit-clear-values values-form))
       (emit 'aload function-register)
       (emit 'aload values-register)
       (emit-invokevirtual +lisp-object-class+ "dispatch"
                           (list +lisp-object-array+) +lisp-object+))))
  (fix-boxing representation nil)
  (emit-move-from-stack target))

(defknown unused-variable (t) t)
(defun unused-variable (variable)
  (unless (or (variable-ignore-p variable)
              (variable-ignorable-p variable))
    (compiler-style-warn "The variable ~S is defined but never used."
                         (variable-name variable))))

(defknown check-for-unused-variables (list) t)
(defun check-for-unused-variables (list)
  (dolist (variable list)
    (when (and (not (variable-special-p variable))
               (zerop (variable-reads variable))
               (zerop (variable-writes variable)))
      (unused-variable variable))))

;; Generates code to bind variable to value at top of runtime stack.
(declaim (ftype (function (t) t) compile-binding))
(defun compile-binding (variable)
  (cond ((variable-register variable)
         (emit 'astore (variable-register variable)))
        ((variable-special-p variable)
         (emit-push-current-thread)
         (emit 'swap)
         (emit-push-variable-name variable)
         (emit 'swap)
         (emit-invokevirtual +lisp-thread-class+ "bindSpecial"
                             (list +lisp-symbol+ +lisp-object+) nil))
        ((variable-closure-index variable)
         (emit 'aload (compiland-closure-register *current-compiland*))
         (emit 'swap) ; array value
         (emit-push-constant-int (variable-closure-index variable))
         (emit 'swap) ; array index value
         (emit 'aastore))
        (t
         (aver nil))))

(defknown compile-progn-body (t t &optional t) t)
(defun compile-progn-body (body target &optional representation)
  (cond ((null body)
         (when target
           (emit-push-nil)
           (emit-move-from-stack target)))
        (t
         (let ((clear-values nil)
               (tail body))
           (loop
             (let ((form (car tail)))
               (cond ((null (cdr tail))
                      ;; Last form.
                      (when clear-values
                        (emit-clear-values))
                      (compile-form form target representation)
                      (return))
                     (t
                      ;; Not the last form.
                      (compile-form form nil nil)
                      (unless clear-values
                        (unless (single-valued-p form)
                          (setq clear-values t)))))
               (setq tail (cdr tail)))))))
  t)

(defun p2-m-v-b-node (block target)
  (let* ((*blocks* (cons block *blocks*))
         (*register* *register*)
         (form (block-form block))
         (*visible-variables* *visible-variables*)
         (vars (second form))
         (bind-special-p nil)
         (variables (block-vars block)))
    (dolist (variable variables)
      (let ((special-p (variable-special-p variable)))
        (cond (special-p
               (setf bind-special-p t))
              (t
               (unless (variable-closure-index variable)
                 (setf (variable-register variable) (allocate-register)))))))
    ;; If we're going to bind any special variables...
    (when bind-special-p
      (dformat t "p2-m-v-b-node lastSpecialBinding~%")
      ;; Save current dynamic environment.
      (setf (block-environment-register block) (allocate-register))
      (emit-push-current-thread)
      (emit 'getfield +lisp-thread-class+ "lastSpecialBinding" +lisp-special-binding+)
      (emit 'astore (block-environment-register block)))
    ;; Make sure there are no leftover values from previous calls.
    (emit-clear-values)
    ;; Bind the variables.
    (aver (= (length vars) (length variables)))
    (cond ((= (length vars) 1)
	   (compile-forms-and-maybe-emit-clear-values (third form) 'stack nil)
           (compile-binding (car variables)))
          (t
           (let* ((*register* *register*)
                  (result-register (allocate-register))
                  (values-register (allocate-register))
                  (LABEL1 (gensym))
                  (LABEL2 (gensym)))
             ;; Store primary value from values form in result register.
             (compile-form (third form) result-register nil)
             ;; Store values from values form in values register.
             (emit-push-current-thread)
             (emit 'getfield +lisp-thread-class+ "_values" "[Lorg/armedbear/lisp/LispObject;")
             (emit-move-from-stack values-register)
             ;; Did we get just one value?
             (emit 'aload values-register)
             (emit 'ifnull LABEL1)
             ;; Reaching here, we have multiple values (or no values at all). We need
             ;; the slow path if we have more variables than values.
             (emit 'aload values-register)
             (emit 'arraylength)
             (emit-push-constant-int (length vars))
             (emit 'if_icmplt LABEL1)
             ;; Reaching here, we have enough values for all the variables. We can use
             ;; the values we have. This is the fast path.
             (emit 'aload values-register)
             (emit 'goto LABEL2)
             (label LABEL1)
             (emit-push-current-thread)
             (emit 'aload result-register)
             (emit-push-constant-int (length vars))
             (emit-invokevirtual +lisp-thread-class+ "getValues"
                                 (list +lisp-object+ "I") +lisp-object-array+)
             ;; Values array is now on the stack at runtime.
             (label LABEL2)
             (let ((index 0))
               (dolist (variable variables)
                 (when (< index (1- (length vars)))
                   (emit 'dup))
                 (emit-push-constant-int index)
                 (incf index)
                 (emit 'aaload)
                 ;; Value is on the runtime stack at this point.
                 (compile-binding variable)))
             (maybe-emit-clear-values (third form)))))
    ;; Make the variables visible for the body forms.
    (dolist (variable variables)
      (push variable *visible-variables*))
    ;; Body.
    (compile-progn-body (cdddr form) target)
    (when bind-special-p
      ;; Restore dynamic environment.
      (emit 'aload *thread*)
      (emit 'aload (block-environment-register block))
      (emit 'putfield +lisp-thread-class+ "lastSpecialBinding" +lisp-special-binding+))))

(defun propagate-vars (block)
  (let ((removed '()))
    (dolist (variable (block-vars block))
      (unless (or (variable-special-p variable)
                  (variable-closure-index variable))
        (when (eql (variable-writes variable) 0)
          ;; There are no writes to the variable.
          (let ((initform (variable-initform variable)))
            (cond ((var-ref-p initform)
                   (let ((source-var (var-ref-variable initform)))
                     (cond ((null source-var)
                            (aver (var-ref-constant-p initform))
                            (let ((value (var-ref-constant-value initform)))
                              (dolist (ref (variable-references variable))
                                (aver (eq (var-ref-variable ref) variable))
                                (setf (var-ref-variable ref) nil
                                      (var-ref-constant-p ref) t
                                      (var-ref-constant-value ref) value))))
                           (t
                            (unless (or (variable-special-p source-var)
                                        (variable-used-non-locally-p source-var))
                              (when (eql (variable-writes source-var) 0)
                                ;; We can eliminate the variable.
                                ;; FIXME This may no longer be true when we start tracking writes!
                                (aver (= (variable-reads variable) (length (variable-references variable))))
                                (dolist (ref (variable-references variable))
                                  (aver (eq (var-ref-variable ref) variable))
                                  (setf (var-ref-variable ref) source-var))
                                ;; Check for DOTIMES limit variable.
                                (when (get (variable-name variable) 'sys::dotimes-limit-variable-p)
                                  (let* ((symbol (get (variable-name variable) 'sys::dotimes-index-variable-name))
                                         (index-variable (find-variable symbol (block-vars block))))
                                    (when index-variable
                                      (setf (get (variable-name index-variable) 'sys::dotimes-limit-variable-name)
                                            (variable-name source-var)))))
                                (push variable removed)))))))
                  ((fixnump initform)
                   (dolist (ref (variable-references variable))
                     (aver (eq (var-ref-variable ref) variable))
                     (setf (var-ref-variable ref) nil
                           (var-ref-constant-p ref) t
                           (var-ref-constant-value ref) initform))
                   (push variable removed)))))))
    (when removed
      (dolist (variable removed)
        (setf (block-vars block) (remove variable (block-vars block)))))))

(defknown p2-let-bindings (t) t)
(defun p2-let-bindings (block)
  (dolist (variable (block-vars block))
    (unless (or (variable-special-p variable)
                (variable-closure-index variable)
                (zerop (variable-reads variable)))
      (aver (null (variable-register variable)))
      (setf (variable-register variable) t)))
  (let ((must-clear-values nil))
    (declare (type boolean must-clear-values))
    ;; Evaluate each initform. If the variable being bound is special, allocate
    ;; a temporary register for the result; LET bindings must be done in
    ;; parallel, so we can't modify any specials until all the initforms have
    ;; been evaluated. Note that we can't just push the values on the stack
    ;; because we'll lose JVM stack consistency if there is a non-local
    ;; transfer of control from one of the initforms.
    (dolist (variable (block-vars block))
      (let* ((initform (variable-initform variable))
             (unused-p (and (not (variable-special-p variable))
                            ;; If it's never read, we don't care about writes.
                            (zerop (variable-reads variable)))))
        (cond (unused-p
               (compile-form initform nil nil)) ; for effect
              (t
               (cond (initform
                      (when (eq (variable-register variable) t)
                        (let ((declared-type (variable-declared-type variable)))
                          (cond ((neq declared-type :none)
                                 (cond ((fixnum-type-p declared-type)
                                        (setf (variable-representation variable) :int))
                                       ((java-long-type-p declared-type)
                                        (setf (variable-representation variable) :long))))
                                ((zerop (variable-writes variable))
                                 (let ((derived-type (derive-compiler-type initform)))
                                   (setf (variable-derived-type variable) derived-type)
                                   (cond ((fixnum-type-p derived-type)
                                          (setf (variable-representation variable) :int))
                                         ((java-long-type-p derived-type)
                                          (setf (variable-representation variable) :long)))))
                                ((get (variable-name variable) 'sys::dotimes-index-variable-p)
                                 ;; DOTIMES index variable.
                                 (let* ((name (get (variable-name variable) 'sys::dotimes-limit-variable-name))
                                        (limit-variable (and name
                                                             (or (find-variable name (block-vars block))
                                                                 (find-visible-variable name)))))
                                   (when limit-variable
                                     (let ((type (variable-derived-type limit-variable)))
                                       (when (eq type :none)
                                         (setf type (variable-declared-type limit-variable)))
                                       (cond ((fixnum-type-p type)
                                              (setf (variable-representation variable) :int
;;                                                     (variable-derived-type variable) 'FIXNUM
                                                    (variable-derived-type variable) type
                                                    ))
                                             ((java-long-type-p type)
                                              (setf (variable-representation variable) :long
;;                                                     (variable-derived-type variable) 'JAVA-LONG
                                                    (variable-derived-type variable) type
                                                    ))))))))))
                      (compile-form initform 'stack (variable-representation variable))
                      (unless must-clear-values
                        (unless (single-valued-p initform)
                          (setf must-clear-values t))))
                     (t
                      ;; No initform.
                      (emit-push-nil)))
               (when (eq (variable-register variable) t)
                 ;; Now allocate the register.
                 (setf (variable-register variable)
                       (case (variable-representation variable)
                         (:long
                          ;; We need two registers for a long.
                          (allocate-register-pair))
                         (t
                          (allocate-register)))))
               (cond ((variable-special-p variable)
                      (emit-move-from-stack (setf (variable-temp-register variable) (allocate-register))))
                     ((eq (variable-representation variable) :int)
                      (emit 'istore (variable-register variable)))
                     ((eq (variable-representation variable) :long)
                      (emit 'lstore (variable-register variable)))
                     (t
                      (compile-binding variable)))))))
    (when must-clear-values
      (emit-clear-values))
    ;; Now that all the initforms have been evaluated, move the results from
    ;; the temporary registers (if any) to their proper destinations.
    (dolist (variable (block-vars block))
      (when (variable-temp-register variable)
        (aver (variable-special-p variable))
        (emit 'aload (variable-temp-register variable))
        (compile-binding variable))))
  ;; Now make the variables visible.
  (dolist (variable (block-vars block))
    (push variable *visible-variables*))
  t)

(defknown p2-let*-bindings (t) t)
(defun p2-let*-bindings (block)
  (let ((must-clear-values nil))
    (declare (type boolean must-clear-values))
    ;; Generate code to evaluate initforms and bind variables.
    (dolist (variable (block-vars block))
      (let* ((initform (variable-initform variable))
             (unused-p (and (not (variable-special-p variable))
                            (zerop (variable-reads variable))
                            (zerop (variable-writes variable))))
             (boundp nil))
        (declare (type boolean unused-p boundp))
        (macrolet ((update-must-clear-values ()
                     `(unless must-clear-values
                        (unless (single-valued-p initform)
                          (setf must-clear-values t)))))
          (cond ((and (variable-special-p variable)
                      (eq initform (variable-name variable)))
                 ;; The special case of binding a special to its current value.
                 (emit-push-current-thread)
                 (emit-push-variable-name variable)
                 (emit-invokevirtual +lisp-thread-class+
                                     "bindSpecialToCurrentValue"
                                     (list +lisp-symbol+)
                                     nil)
                 (setf boundp t))
                ((and (not (variable-special-p variable))
                      (zerop (variable-reads variable)))
                 ;; We don't have to bind it if we never read it.
                 (compile-form initform nil nil) ; for effect
                 (update-must-clear-values)
                 (setf boundp t))
                ((null initform)
                 (cond ((and (null (variable-closure-index variable))
                             (not (variable-special-p variable))
                             (eq (variable-declared-type variable) 'BOOLEAN))
                        (setf (variable-representation variable) :boolean)
                        (setf (variable-register variable) (allocate-register))
                        (emit 'iconst_0)
                        (emit 'istore (variable-register variable))
                        (setf boundp t))
                       (t
                        (emit-push-nil))))
                (t
                  (cond (unused-p
                         (compile-form initform nil nil) ; for effect
                         (update-must-clear-values)
                         (setf boundp t))
                        ((and (null (variable-closure-index variable))
                              (not (variable-special-p variable)))
                         (let ((declared-type (variable-declared-type variable)))
                           (cond ((and (neq declared-type :none)
                                       (fixnum-type-p declared-type))
                                  (setf (variable-representation variable) :int)
                                  (compile-form initform 'stack :int)
                                  (update-must-clear-values)
                                  (setf (variable-register variable) (allocate-register))
                                  (emit 'istore (variable-register variable))
                                  (setf boundp t))
                                 ((and (neq declared-type :none)
                                       (java-long-type-p declared-type))
                                  (setf (variable-representation variable) :long)
                                  (compile-form initform 'stack :long)
                                  (update-must-clear-values)
                                  (setf (variable-register variable)
                                        ;; We need two registers for a long.
                                        (allocate-register-pair))
                                  (emit 'lstore (variable-register variable))
                                  (setf boundp t))
                                 ((and (neq declared-type :none)
                                       (eq declared-type 'BOOLEAN))
                                  (setf (variable-representation variable) :boolean)
                                  (compile-form initform 'stack :boolean)
                                  (update-must-clear-values)
                                  (setf (variable-register variable) (allocate-register))
                                  (emit 'istore (variable-register variable))
                                  (setf boundp t))
                                 ((eql (variable-writes variable) 0)
                                  (let ((type (derive-compiler-type initform)))
                                    (setf (variable-derived-type variable) type)
                                    (cond ((fixnum-type-p type)
                                           (setf (variable-representation variable) :int)
                                           (setf (variable-register variable) (allocate-register))
                                           (compile-form initform 'stack :int)
                                           (update-must-clear-values)
                                           (emit 'istore (variable-register variable))
                                           (setf boundp t))
                                          ((java-long-type-p type)
                                           (setf (variable-representation variable) :long)
                                           (setf (variable-register variable)
                                                 ;; We need two registers for a long.
                                                 (allocate-register-pair))
                                           (compile-form initform 'stack :long)
                                           (update-must-clear-values)
                                           (emit 'lstore (variable-register variable))
                                           (setf boundp t))
                                          ((eq type 'CHARACTER)
                                           (setf (variable-representation variable) :char)
                                           (setf (variable-register variable) (allocate-register))
                                           (compile-form initform 'stack :char)
                                           (update-must-clear-values)
                                           (emit 'istore (variable-register variable))
                                           (setf boundp t))
                                          (t
                                           (compile-form initform 'stack nil)
                                           (update-must-clear-values)))))
                                 (t
                                  (compile-form initform 'stack nil)
                                  (update-must-clear-values)))))
                        (t
                         (compile-form initform 'stack nil)
                         (update-must-clear-values))))))
        (unless (or boundp (variable-special-p variable))
          (unless (or (variable-closure-index variable) (variable-register variable))
            (setf (variable-register variable) (allocate-register))))
        (push variable *visible-variables*)
        (unless boundp
          (compile-binding variable))
        (maybe-generate-type-check variable)))
    (when must-clear-values
      (emit-clear-values)))
  t)

(defun p2-let/let*-node (block target representation)
  (let* ((*blocks* (cons block *blocks*))
         (*register* *register*)
         (form (block-form block))
         (*visible-variables* *visible-variables*)
         (specialp nil))
    ;; Walk the variable list looking for special bindings and unused lexicals.
    (dolist (variable (block-vars block))
      (cond ((variable-special-p variable)
             (setf specialp t))
            ((zerop (variable-reads variable))
             (unused-variable variable))))
    ;; If there are any special bindings...
    (when specialp
      ;; We need to save current dynamic environment.
      (setf (block-environment-register block) (allocate-register))
      (emit-push-current-thread)
      (emit 'getfield +lisp-thread-class+ "lastSpecialBinding" +lisp-special-binding+)
      (emit 'astore (block-environment-register block)))
    (propagate-vars block)
    (ecase (car form)
      (LET
       (p2-let-bindings block))
      (LET*
       (p2-let*-bindings block)))
    ;; Make declarations of free specials visible.
    (dolist (variable (block-free-specials block))
      (push variable *visible-variables*))
    ;; Body of LET/LET*.
    (with-saved-compiler-policy
      (process-optimization-declarations (cddr form))
      (compile-progn-body (cddr form) target representation))
    (when specialp
      ;; Restore dynamic environment.
      (emit 'aload *thread*)
      (emit 'aload (block-environment-register block))
      (emit 'putfield +lisp-thread-class+ "lastSpecialBinding" +lisp-special-binding+))))

(defun p2-locally (form target representation)
  (with-saved-compiler-policy
    (let ((body (cdr form)))
      (process-optimization-declarations body)
      (compile-progn-body body target representation))))

(defknown find-tag (t) t)
(defun find-tag (name)
  (dolist (tag *visible-tags*)
    (when (eql name (tag-name tag))
      (return tag))))

(defknown p2-tagbody-node (t t) t)
(defun p2-tagbody-node (block target)
  (let* ((*blocks* (cons block *blocks*))
         (*visible-tags* *visible-tags*)
         (*register* *register*)
         (form (block-form block))
         (body (cdr form))
         (local-tags ())
         (BEGIN-BLOCK (gensym))
         (END-BLOCK (gensym))
         (EXIT (gensym))
         environment-register
         (must-clear-values nil))
    ;; Scan for tags.
    (dolist (subform body)
      (when (or (symbolp subform) (integerp subform))
        (let* ((tag (make-tag :name subform :label (gensym) :block block)))
          (push tag local-tags)
          (push tag *visible-tags*))))
    (when (block-non-local-go-p block)
      (dformat t "p2-tagbody-node lastSpecialBinding~%")
      (setf environment-register (allocate-register))
      (emit-push-current-thread)
      (emit 'getfield +lisp-thread-class+ "lastSpecialBinding" +lisp-special-binding+)
      (emit 'astore environment-register))
    (label BEGIN-BLOCK)
    (do* ((rest body (cdr rest))
          (subform (car rest) (car rest)))
         ((null rest))
      (cond ((or (symbolp subform) (integerp subform))
             (let ((tag (find-tag subform)))
               (unless tag
                 (error "COMPILE-TAGBODY: tag not found: ~S~%" subform))
               (label (tag-label tag))))
            (t
             (compile-form subform nil nil)
             (unless must-clear-values
               (unless (single-valued-p subform)
;;                  (let ((*print-structure* nil))
;;                    (format t "not single-valued: ~S~%" subform))
                 (setf must-clear-values t))))))
    (label END-BLOCK)
    (emit 'goto EXIT)
    (when (block-non-local-go-p block)
      ; We need a handler to catch non-local GOs.
      (let* ((HANDLER (gensym))
             (*register* *register*)
             (go-register (allocate-register))
             (tag-register (allocate-register)))
        (label HANDLER)
        ;; The Go object is on the runtime stack. Stack depth is 1.
        (emit 'dup)
        (emit 'astore go-register)
        ;; Get the tag.
        (emit 'checkcast +lisp-go-class+)
        (emit 'getfield +lisp-go-class+ "tag" +lisp-object+) ; Stack depth is still 1.
        (emit 'astore tag-register)
        (dolist (tag local-tags)
          (let ((NEXT (gensym)))
            (emit 'aload tag-register)
            (emit 'getstatic *this-class*
                  (if *compile-file-truename*
                      (declare-object-as-string (tag-label tag))
                      (declare-object (tag-label tag)))
                  +lisp-object+)
            (emit 'if_acmpne NEXT) ;; Jump if not EQ.
            ;; Restore dynamic environment.
            (emit-push-current-thread)
            (aver (fixnump environment-register))
            (emit 'aload environment-register)
            (emit 'putfield +lisp-thread-class+ "lastSpecialBinding" +lisp-special-binding+)
            (emit 'goto (tag-label tag))
            (label NEXT)))
        ;; Not found. Re-throw Go.
        (emit 'aload go-register)
        (emit 'athrow)
        ;; Finally...
        (push (make-handler :from BEGIN-BLOCK
                            :to END-BLOCK
                            :code HANDLER
                            :catch-type (pool-class +lisp-go-class+))
              *handlers*)))
    (label EXIT)
    (when must-clear-values
      (emit-clear-values))
    ;; TAGBODY returns NIL.
    (when target
      (emit-push-nil)
      (emit-move-from-stack target))))

(defknown p2-go (t t t) t)
(defun p2-go (form target representation)
  ;; FIXME What if we're called with a non-NIL representation?
  (declare (ignore representation))
  (let* ((name (cadr form))
         (tag (find-tag name)))
    (unless tag
      (error "p2-go: tag not found: ~S" name))
    (when (eq (tag-compiland tag) *current-compiland*)
      ;; Local case.
      (let* ((tag-block (tag-block tag))
             (register nil)
             (protected
              ;; Does the GO leave an enclosing CATCH or UNWIND-PROTECT?
              (dolist (enclosing-block *blocks*)
                (when (eq enclosing-block tag-block)
                  (return nil))
                (let ((block-name (block-name enclosing-block)))
                  (when (or (equal block-name '(CATCH))
                            (equal block-name '(UNWIND-PROTECT)))
                    (return t))))))
        (unless protected
          (dolist (block *blocks*)
            (if (eq block tag-block)
                (return)
                (setf register (or (block-environment-register block) register))))
          (when register
            ;; Restore dynamic environment.
            (emit 'aload *thread*)
            (emit 'aload register)
            (emit 'putfield +lisp-thread-class+ "lastSpecialBinding" +lisp-special-binding+))
          (maybe-generate-interrupt-check)
          (emit 'goto (tag-label tag))
          (return-from p2-go))))
    ;; Non-local GO.
    (emit 'new +lisp-go-class+)
    (emit 'dup)
    (compile-form `',(tag-label tag) 'stack nil) ; Tag.
    (emit-invokespecial-init +lisp-go-class+ (lisp-object-arg-types 1))
    (emit 'athrow)
    ;; Following code will not be reached, but is needed for JVM stack
    ;; consistency.
    (when target
      (emit-push-nil)
      (emit-move-from-stack target))))

(defknown p2-atom (t t t) t)
(defun p2-atom (form target representation)
  (aver (or (null representation) (eq representation :boolean)))
  (unless (check-arg-count form 1)
    (compile-function-call form target representation)
    (return-from p2-atom))
  (compile-forms-and-maybe-emit-clear-values (cadr form) 'stack nil)
  (emit 'instanceof +lisp-cons-class+)
  (let ((LABEL1 (gensym))
        (LABEL2 (gensym)))
    (emit 'ifeq LABEL1)
    (case representation
      (:boolean
       (emit 'iconst_0))
      (t
       (emit-push-nil)))
    (emit 'goto LABEL2)
    (label LABEL1)
    (case representation
      (:boolean
       (emit 'iconst_1))
      (t
       (emit-push-t)))
    (label LABEL2)
    (emit-move-from-stack target representation)))

(defknown p2-instanceof-predicate (t t t t) t)
(defun p2-instanceof-predicate (form target representation java-class)
  (unless (check-arg-count form 1)
    (compile-function-call form target representation)
    (return-from p2-instanceof-predicate))
  (let ((arg (%cadr form)))
    (cond ((null target)
	   (compile-forms-and-maybe-emit-clear-values arg nil nil))
          (t
	   (compile-forms-and-maybe-emit-clear-values arg 'stack nil)
           (emit 'instanceof java-class)
           (case representation
             (:boolean)
             (t
              (let ((LABEL1 (gensym))
                    (LABEL2 (gensym)))
                (emit 'ifeq LABEL1)
                (emit-push-t)
                (emit 'goto LABEL2)
                (label LABEL1)
                (emit-push-nil)
                (label LABEL2)
                (emit-move-from-stack target representation))))))))

(defun p2-bit-vector-p (form target representation)
  (p2-instanceof-predicate form target representation +lisp-abstract-bit-vector-class+))

(defun p2-characterp (form target representation)
  (p2-instanceof-predicate form target representation +lisp-character-class+))

(defun p2-classp (form target representation)
  (p2-instanceof-predicate form target representation +lisp-class-class+))

(defun p2-consp (form target representation)
  (p2-instanceof-predicate form target representation +lisp-cons-class+))

(defun p2-fixnump (form target representation)
  (p2-instanceof-predicate form target representation +lisp-fixnum-class+))

(defun p2-packagep (form target representation)
  (p2-instanceof-predicate form target representation +lisp-package-class+))

(defun p2-readtablep (form target representation)
  (p2-instanceof-predicate form target representation +lisp-readtable-class+))

(defun p2-simple-vector-p (form target representation)
  (p2-instanceof-predicate form target representation +lisp-simple-vector-class+))

(defun p2-stringp (form target representation)
  (p2-instanceof-predicate form target representation +lisp-abstract-string-class+))

(defun p2-symbolp (form target representation)
  (p2-instanceof-predicate form target representation +lisp-symbol-class+))

(defun p2-vectorp (form target representation)
  (p2-instanceof-predicate form target representation +lisp-abstract-vector-class+))

(defun p2-coerce-to-function (form target representation)
  (unless (check-arg-count form 1)
    (compile-function-call form target representation)
    (return-from p2-coerce-to-function))
  (compile-forms-and-maybe-emit-clear-values (%cadr form) 'stack nil)
  (emit-invokestatic +lisp-class+ "coerceToFunction"
                     (lisp-object-arg-types 1) +lisp-object+)
  (emit-move-from-stack target))

(defun p2-block-node (block target representation)
  (unless (block-node-p block)
    (sys::%format t "type-of block = ~S~%" (type-of block))
    (aver (block-node-p block)))
  (let* ((*blocks* (cons block *blocks*))
         (*register* *register*))
    (cond ((block-return-p block)
           (setf (block-target block) target)
           (dformat t "p2-block-node lastSpecialBinding~%")
           (dformat t "*all-variables* = ~S~%" (mapcar #'variable-name *all-variables*))
           (cond ((some #'variable-special-p *all-variables*)
                  ;; Save the current dynamic environment.
                  (setf (block-environment-register block) (allocate-register))
                  (emit-push-current-thread)
                  (emit 'getfield +lisp-thread-class+ "lastSpecialBinding" +lisp-special-binding+)
                  (emit 'astore (block-environment-register block)))
                 (t
                  (dformat t "no specials~%")))
           (setf (block-catch-tag block) (gensym))
           (let* ((*register* *register*)
                  (BEGIN-BLOCK (gensym))
                  (END-BLOCK (gensym))
                  (BLOCK-EXIT (block-exit block)))
             (label BEGIN-BLOCK) ; Start of protected range.
             ;; Implicit PROGN.
             (compile-progn-body (cddr (block-form block)) target)
             (label END-BLOCK) ; End of protected range.
             (emit 'goto BLOCK-EXIT) ; Jump over handler (if any).
             (when (block-non-local-return-p block)
               ; We need a handler to catch non-local RETURNs.
               (let ((HANDLER (gensym))
                     (RETHROW (gensym)))
                 (label HANDLER)
                 ;; The Return object is on the runtime stack. Stack depth is 1.
                 (emit 'dup) ; Stack depth is 2.
                 (emit 'getfield +lisp-return-class+ "tag" +lisp-object+) ; Still 2.
                 (compile-form `',(block-catch-tag block) 'stack nil) ; Tag. Stack depth is 3.
                 ;; If it's not the tag we're looking for...
                 (emit 'if_acmpne RETHROW) ; Stack depth is 1.
                 (emit 'getfield +lisp-return-class+ "result" +lisp-object+)
                 (emit-move-from-stack target) ; Stack depth is 0.
                 (emit 'goto BLOCK-EXIT)
                 (label RETHROW)
                 ;; Not the tag we're looking for.
                 (emit 'athrow)
                 ;; Finally...
                 (push (make-handler :from BEGIN-BLOCK
                                     :to END-BLOCK
                                     :code HANDLER
                                     :catch-type (pool-class +lisp-return-class+))
                       *handlers*)))
             (label BLOCK-EXIT))
           (when (block-environment-register block)
             ;; We saved the dynamic environment above. Restore it now.
             (emit 'aload *thread*)
             (emit 'aload (block-environment-register block))
             (emit 'putfield +lisp-thread-class+ "lastSpecialBinding" +lisp-special-binding+))
           (fix-boxing representation nil)
           )
          (t
           ;; No explicit returns.
           (compile-progn-body (cddr (block-form block)) target representation)))))

(defknown p2-return-from (t t t) t)
(defun p2-return-from (form target representation)
  ;; FIXME What if we're called with a non-NIL representation?
  (declare (ignore representation))
  (let* ((name (second form))
         (result-form (third form))
         (block (find-block name)))
    (when (null block)
      (error "No block named ~S is currently visible." name))
    (let ((compiland *current-compiland*))
      (when (eq (block-compiland block) compiland)
        ;; Local case. Is the RETURN nested inside an UNWIND-PROTECT which is
        ;; inside the block we're returning from?
        (let ((protected
               (dolist (enclosing-block *blocks*)
                 (when (eq enclosing-block block)
                   (return nil))
                 (when (equal (block-name enclosing-block) '(UNWIND-PROTECT))
                   (return t)))))
          (unless protected
            (unless (compiland-single-valued-p *current-compiland*)
;;               (format t "compiland not single-valued: ~S~%"
;;                       (compiland-name *current-compiland*))
              (emit-clear-values))
            (compile-form result-form (block-target block) nil)
            (emit 'goto (block-exit block))
            (return-from p2-return-from)))))
    ;; Non-local RETURN.
    (aver (block-non-local-return-p block))
    (cond ((node-constant-p result-form)
           (emit 'new +lisp-return-class+)
           (emit 'dup)
           (compile-form `',(block-catch-tag block) 'stack nil) ; Tag.
           (emit-clear-values)
           (compile-form result-form 'stack nil)) ; Result.
          (t
           (let* ((*register* *register*)
                  (temp-register (allocate-register)))
             (emit-clear-values)
             (compile-form result-form temp-register nil) ; Result.
             (emit 'new +lisp-return-class+)
             (emit 'dup)
             (compile-form `',(block-catch-tag block) 'stack nil) ; Tag.
             (emit 'aload temp-register))))
    (emit-invokespecial-init +lisp-return-class+ (lisp-object-arg-types 2))
    (emit 'athrow)
    ;; Following code will not be reached, but is needed for JVM stack
    ;; consistency.
    (when target
      (emit-push-nil)
      (emit-move-from-stack target))))

(defun emit-cast/getfield-for-car/cdr (arg target representation field)
  (compile-form arg 'stack nil)
  (emit 'checkcast +lisp-cons-class+)
  (emit 'getfield +lisp-cons-class+ field +lisp-object+)
  (fix-boxing representation nil)
  (emit-move-from-stack target representation))

(defun p2-car (form target representation)
  (unless (check-arg-count form 1)
    (compile-function-call form target representation)
    (return-from p2-car))
  (let ((arg (%cadr form)))
    (cond ((and (null target) (< *safety* 3))
           (compile-form arg target nil))
          ((and (consp arg) (eq (%car arg) 'cdr) (= (length arg) 2))
	   (compile-forms-and-maybe-emit-clear-values (second arg) 'stack nil)
           (emit-invoke-method "cadr" target representation))
          ((eq (derive-type arg) 'CONS)
	   (emit-cast/getfield-for-car/cdr arg target representation "car"))
          (t
	   (compile-forms-and-maybe-emit-clear-values arg 'stack nil)
           (emit-invoke-method "car" target representation)))))

(defun p2-cdr (form target representation)
  (unless (check-arg-count form 1)
    (compile-function-call form target representation)
    (return-from p2-cdr))
  (let ((arg (%cadr form)))
    (cond ((eq (derive-type arg) 'CONS)
	   (emit-cast/getfield-for-car/cdr arg target representation "cdr"))
          (t
	   (compile-forms-and-maybe-emit-clear-values arg 'stack nil)
           (emit-invoke-method "cdr" target representation)))))

(defun p2-cons (form target representation)
  (unless (check-arg-count form 2)
    (compile-function-call form target representation)
    (return-from p2-cons))
  (emit 'new +lisp-cons-class+)
  (emit 'dup)
  (let* ((args (%cdr form))
         (arg1 (%car args))
         (arg2 (%cadr args)))
    (compile-forms-and-maybe-emit-clear-values arg1 'stack nil
					       arg2 'stack nil))
  (emit-invokespecial-init +lisp-cons-class+ (lisp-object-arg-types 2))
  (emit-move-from-stack target))

(defun compile-progn (form target representation)
  (compile-progn-body (cdr form) target)
  (fix-boxing representation nil))

(defun p2-eval-when (form target representation)
  (cond ((or (memq :execute (cadr form))
             (memq 'eval (cadr form)))
         (compile-progn-body (cddr form) target)
         (fix-boxing representation nil))
        (t
         (emit-push-nil)
         (emit-move-from-stack target))))

(defun p2-load-time-value (form target representation)
  (cond (*compile-file-truename*
         (emit 'getstatic *this-class*
               (declare-load-time-value (second form)) +lisp-object+)
         (fix-boxing representation nil)
         (emit-move-from-stack target representation))
        (t
         (compile-constant (eval (second form)) target representation))))

(defun p2-progv (form target representation)
  (let* ((symbols-form (cadr form))
         (values-form (caddr form))
         (*register* *register*)
         (environment-register (allocate-register)))
    (compile-form symbols-form 'stack nil)
    (compile-form values-form 'stack nil)
    (unless (and (single-valued-p symbols-form)
                 (single-valued-p values-form))
      (emit-clear-values))
    (emit-push-current-thread)
    (emit 'getfield +lisp-thread-class+ "lastSpecialBinding" +lisp-special-binding+)
    (emit 'astore environment-register)
    ;; Compile call to Lisp.progvBindVars().
    (emit 'aload *thread*)
    (emit-invokestatic +lisp-class+ "progvBindVars"
                       (list +lisp-object+ +lisp-object+ +lisp-thread+) nil)
    ;; Implicit PROGN.
    (compile-progn-body (cdddr form) target)
    ;; Restore dynamic environment.
    (emit 'aload *thread*)
    (emit 'aload environment-register)
    (emit 'putfield +lisp-thread-class+ "lastSpecialBinding" +lisp-special-binding+)
    (fix-boxing representation nil)))

(defun p2-quote (form target representation)
  (aver (or (null representation) (eq representation :boolean)))
  (let ((obj (second form)))
    (cond ((null obj)
           (when target
             (emit-push-false representation)
             (emit-move-from-stack target representation)))
          ((eq representation :boolean)
           (emit 'iconst_1)
           (emit-move-from-stack target representation))
          ((keywordp obj)
           (let ((name (lookup-known-keyword obj)))
              (if name
                  (emit 'getstatic "org/armedbear/lisp/Keyword" name +lisp-symbol+)
                  (emit 'getstatic *this-class* (declare-keyword obj) +lisp-symbol+)))
            (emit-move-from-stack target representation))
          ((symbolp obj)
           (let ((name (lookup-known-symbol obj)))
             (cond (name
                    (emit 'getstatic +lisp-symbol-class+ name +lisp-symbol+))
                   ((symbol-package (truly-the symbol obj))
                    (emit 'getstatic *this-class* (declare-symbol obj) +lisp-symbol+))
                   (t
                    ;; An uninterned symbol.
                    (let ((g (if *compile-file-truename*
                                 (declare-object-as-string obj)
                                 (declare-object obj))))
                      (emit 'getstatic *this-class* g +lisp-object+))))
             (emit-move-from-stack target representation)))
          ((listp obj)
           (let ((g (if *compile-file-truename*
                        (declare-object-as-string obj)
                        (declare-object obj))))
             (emit 'getstatic *this-class* g +lisp-object+)
             (emit-move-from-stack target representation)))
          ((constantp obj)
           (compile-constant obj target representation))
          (t
           (compiler-unsupported "COMPILE-QUOTE: unsupported case: ~S" form)))))

(defun p2-rplacd (form target representation)
  (unless (check-arg-count form 2)
    (compile-function-call form target representation)
    (return-from p2-rplacd))
  (let ((args (cdr form)))
    (compile-form (first args) 'stack nil)
    (when target
      (emit 'dup))
    (compile-form (second args) 'stack nil)
    (emit-invokevirtual +lisp-object-class+
                        "setCdr"
                        (lisp-object-arg-types 1)
                        nil)
    (when target
      (fix-boxing representation nil)
      (emit-move-from-stack target representation))))

(defun p2-set-car/cdr (form target representation)
  (unless (check-arg-count form 2)
    (compile-function-call form target representation)
    (return-from p2-set-car/cdr))
  (let ((op (%car form))
        (args (%cdr form)))
    (compile-form (%car args) 'stack nil)
    (compile-form (%cadr args) 'stack nil)
    (when target
      (emit 'dup_x1))
    (emit-invokevirtual +lisp-object-class+
                        (if (eq op 'sys:set-car) "setCar" "setCdr")
                        (lisp-object-arg-types 1)
                        nil)
    (when target
      (fix-boxing representation nil)
      (emit-move-from-stack target representation))))

(defun compile-declare (form target representation)
  (declare (ignore form representation))
  (when target
    (emit-push-nil)
    (emit-move-from-stack target)))

(defun compile-and-write-to-file (class-file compiland)
  (with-class-file class-file
    (let ((*current-compiland* compiland))
      (with-saved-compiler-policy
	  (p2-compiland compiland)
	(write-class-file (compiland-class-file compiland))))))

(defun set-compiland-and-write-class-file (class-file compiland)
  (setf (compiland-class-file compiland) class-file)
  (compile-and-write-to-file class-file compiland))

(defknown p2-flet-process-compiland (t) t)
(defun p2-flet-process-compiland (local-function)
  (let* ((compiland (local-function-compiland local-function))
         (lambda-list (cadr (compiland-lambda-expression compiland))))
    (cond (*compile-file-truename*
           (let* ((pathname (sys::next-classfile-name))
                  (class-file (make-class-file :pathname pathname
                                               :lambda-list lambda-list)))
	     (set-compiland-and-write-class-file class-file compiland)
             ;; Verify that the class file is loadable.
             (let ((*load-truename* (pathname pathname)))
               (unless (ignore-errors (load-compiled-function pathname))
                 (error "Unable to load ~S." pathname)))
             (setf (local-function-class-file local-function) class-file))

           (when (local-function-variable local-function)
             (let ((g (declare-local-function local-function)))
               (emit 'getstatic *this-class* g +lisp-object+)

               (let ((parent (compiland-parent compiland)))
                 (when (compiland-closure-register parent)
                   (dformat t "(compiland-closure-register parent) = ~S~%"
                            (compiland-closure-register parent))
                   (emit 'checkcast +lisp-ctf-class+)
                   (emit 'aload (compiland-closure-register parent))
                   (emit-invokestatic +lisp-class+ "makeCompiledClosure"
                                      (list +lisp-object+ +lisp-object-array+)
                                      +lisp-object+)))

               (dformat t "p2-flet-process-compiland var-set ~S~%" (variable-name (local-function-variable local-function)))
               (emit 'var-set (local-function-variable local-function)))))
          (t
           (let* ((pathname (make-temp-file))
                  (class-file (make-class-file :pathname pathname
                                               :lambda-list lambda-list)))
             (unwind-protect
                 (progn
		   (set-compiland-and-write-class-file class-file compiland)
                   (setf (local-function-class-file local-function) class-file)
                   (setf (local-function-function local-function) (load-compiled-function pathname))

                   (when (local-function-variable local-function)
                     (let ((g (declare-object (load-compiled-function pathname))))
                       (emit 'getstatic *this-class* g +lisp-object+)

                       (let ((parent (compiland-parent compiland)))
                         (when (compiland-closure-register parent)
                           (dformat t "(compiland-closure-register parent) = ~S~%"
                                    (compiland-closure-register parent))
                           (emit 'checkcast +lisp-ctf-class+)
                           (emit 'aload (compiland-closure-register parent))
                           (emit-invokestatic +lisp-class+ "makeCompiledClosure"
                                              (list +lisp-object+ +lisp-object-array+)
                                              +lisp-object+)))

                       (emit 'var-set (local-function-variable local-function)))))
               (delete-file pathname)))))))

(defknown p2-labels-process-compiland (t) t)
(defun p2-labels-process-compiland (local-function)
  (let* ((compiland (local-function-compiland local-function))
         (lambda-list (cadr (compiland-lambda-expression compiland))))
    (cond (*compile-file-truename*
           (let* ((pathname (sys::next-classfile-name))
                  (class-file (make-class-file :pathname pathname
                                               :lambda-list lambda-list)))
	     (set-compiland-and-write-class-file class-file compiland)
             ;; Verify that the class file is loadable.
             (let ((*load-truename* (pathname pathname)))
               (unless (ignore-errors (load-compiled-function pathname))
                 (error "Unable to load ~S." pathname)))
             (setf (local-function-class-file local-function) class-file)
             (let ((g (declare-local-function local-function)))
               (emit 'getstatic *this-class* g +lisp-object+)

               (let ((parent (compiland-parent compiland)))
                 (when (compiland-closure-register parent)
                   (dformat t "(compiland-closure-register parent) = ~S~%"
                            (compiland-closure-register parent))
                   (emit 'checkcast +lisp-ctf-class+)
                   (emit 'aload (compiland-closure-register parent))
                   (emit-invokestatic +lisp-class+ "makeCompiledClosure"
                                      (list +lisp-object+ +lisp-object-array+)
                                      +lisp-object+)))


               (emit 'var-set (local-function-variable local-function)))))
          (t
           (let* ((pathname (make-temp-file))
                  (class-file (make-class-file :pathname pathname
                                               :lambda-list lambda-list)))
             (unwind-protect
                 (progn
		   (set-compiland-and-write-class-file class-file compiland)
                   (setf (local-function-class-file local-function) class-file)
                   (let ((g (declare-object (load-compiled-function pathname))))
                     (emit 'getstatic *this-class* g +lisp-object+)

                     (let ((parent (compiland-parent compiland)))
                       (when (compiland-closure-register parent)
                         (dformat t "(compiland-closure-register parent) = ~S~%"
                                  (compiland-closure-register parent))
                         (emit 'checkcast +lisp-ctf-class+)
                         (emit 'aload (compiland-closure-register parent))
                         (emit-invokestatic +lisp-class+ "makeCompiledClosure"
                                            (list +lisp-object+ +lisp-object-array+)
                                            +lisp-object+)))

                     (emit 'var-set (local-function-variable local-function))))
               (delete-file pathname)))))))

(defknown p2-flet (t t t) t)
(defun p2-flet (form target representation)
  ;; FIXME What if we're called with a non-NIL representation?
  (declare (ignore representation))
  (let ((*local-functions* *local-functions*)
        (*visible-variables* *visible-variables*)
        (local-functions (cadr form))
        (body (cddr form)))
    (dolist (local-function local-functions)
      (let ((variable (local-function-variable local-function)))
        (when variable
          (aver (null (variable-register variable)))
          (unless (variable-closure-index variable)
            (setf (variable-register variable) (allocate-register))))))
    (dolist (local-function local-functions)
      (p2-flet-process-compiland local-function))
    (dolist (local-function local-functions)
      (push local-function *local-functions*)
      (let ((variable (local-function-variable local-function)))
        (when variable
          (push variable *visible-variables*))))
    (do ((forms body (cdr forms)))
        ((null forms))
      (compile-form (car forms) (if (cdr forms) nil target) nil))))

(defknown p2-labels (t t t) t)
(defun p2-labels (form target representation)
  (let ((*local-functions* *local-functions*)
        (*visible-variables* *visible-variables*)
        (local-functions (cadr form))
        (body (cddr form)))
    (dolist (local-function local-functions)
      (push local-function *local-functions*)
      (push (local-function-variable local-function) *visible-variables*))
    (dolist (local-function local-functions)
      (let ((variable (local-function-variable local-function)))
        (aver (null (variable-register variable)))
        (unless (variable-closure-index variable)
          (setf (variable-register variable) (allocate-register)))))
    (dolist (local-function local-functions)
      (p2-labels-process-compiland local-function))
    (do ((forms body (cdr forms)))
        ((null forms))
      (compile-form (car forms) (if (cdr forms) nil 'stack) nil))
    (fix-boxing representation nil)
    (emit-move-from-stack target representation)))

(defun p2-lambda (compiland target)
  (let* ((lambda-list (cadr (compiland-lambda-expression compiland))))
    (aver (null (compiland-class-file compiland)))
    (cond (*compile-file-truename*
           (setf (compiland-class-file compiland)
                 (make-class-file :pathname (sys::next-classfile-name)
                                  :lambda-list lambda-list))
           (let ((class-file (compiland-class-file compiland)))
	     (compile-and-write-to-file class-file compiland)
             (emit 'getstatic *this-class*
                   (declare-local-function (make-local-function :class-file class-file))
                   +lisp-object+)))
          (t
           (let ((pathname (make-temp-file)))
             (setf (compiland-class-file compiland)
                   (make-class-file :pathname pathname
                                    :lambda-list lambda-list))
             (unwind-protect
                 (progn
		   (compile-and-write-to-file (compiland-class-file compiland) compiland)
                   (emit 'getstatic *this-class*
                         (declare-object (load-compiled-function pathname))
                         +lisp-object+))
               (delete-file pathname)))))
    (cond ((null *closure-variables*)) ; Nothing to do.
          ((compiland-closure-register *current-compiland*)
           (emit 'aload (compiland-closure-register *current-compiland*))
           (emit-invokestatic +lisp-class+ "makeCompiledClosure"
                              (list +lisp-object+ +lisp-object-array+)
                              +lisp-object+)
           (emit 'checkcast +lisp-compiled-closure-class+)) ; Stack: compiled-closure
          (t
           (aver nil))) ;; Shouldn't happen.
    (emit-move-from-stack target)))

(defknown p2-function (t t t) t)
(defun p2-function (form target representation)
  ;; FIXME What if we're called with a non-NIL representation?
  (declare (ignore representation))
  (let ((name (second form))
        local-function)
    (cond ((symbolp name)
           (dformat t "p2-function case 1~%")
           (cond ((setf local-function (find-local-function name))
                  (dformat t "p2-function 1~%")
                  (cond ((local-function-variable local-function)
                         (dformat t "p2-function 2 emitting var-ref~%")
;;                          (emit 'var-ref (local-function-variable local-function) 'stack)
                         (compile-var-ref (make-var-ref (local-function-variable local-function)) 'stack nil)
                         )
                        (t
                         (let ((g (if *compile-file-truename*
                                      (declare-local-function local-function)
                                      (declare-object (local-function-function local-function)))))
                           (emit 'getstatic *this-class* g +lisp-object+) ; Stack: template-function

                           (when (compiland-closure-register *current-compiland*)
                             (emit 'checkcast +lisp-ctf-class+)
                             (emit 'aload (compiland-closure-register *current-compiland*))
                             (emit-invokestatic +lisp-class+ "makeCompiledClosure"
                                                (list +lisp-object+ +lisp-object-array+)
                                                +lisp-object+)))))
                  (emit-move-from-stack target))
                 ((inline-ok name)
                  (emit 'getstatic *this-class*
                        (declare-function name) +lisp-object+)
                  (emit-move-from-stack target))
                 (t
                  (emit 'getstatic *this-class*
                        (declare-symbol name) +lisp-symbol+)
                  (emit-invokevirtual +lisp-object-class+ "getSymbolFunctionOrDie"
                                      nil +lisp-object+)
                  (emit-move-from-stack target))))
          ((and (consp name) (eq (%car name) 'SETF))
           (dformat t "p2-function case 2~%")
           ; FIXME Need to check for NOTINLINE declaration!
           (cond ((setf local-function (find-local-function name))
                  (dformat t "p2-function 1~%")
                  (when (eq (local-function-compiland local-function) *current-compiland*)
                    (emit 'aload 0) ; this
                    (emit-move-from-stack target)
                    (return-from p2-function))
                  (cond ((local-function-variable local-function)
                         (dformat t "p2-function 2~%")
;;                          (emit 'var-ref (local-function-variable local-function) 'stack)
                         (compile-var-ref (make-var-ref (local-function-variable local-function)) 'stack nil)
                         )
                        (t
                         (let ((g (if *compile-file-truename*
                                      (declare-local-function local-function)
                                      (declare-object (local-function-function local-function)))))
                           (emit 'getstatic *this-class*
                                 g +lisp-object+))))) ; Stack: template-function
                 ((member name *functions-defined-in-current-file* :test #'equal)
                  (emit 'getstatic *this-class*
                        (declare-setf-function name) +lisp-object+)
                  (emit-move-from-stack target))
                 ((and (null *compile-file-truename*)
                       (fboundp name)
                       (fdefinition name))
                  (emit 'getstatic *this-class*
                        (declare-object (fdefinition name)) +lisp-object+)
                  (emit-move-from-stack target))
                 (t
                  (emit 'getstatic *this-class*
                        (declare-symbol (cadr name)) +lisp-symbol+)
                  (emit-invokevirtual +lisp-symbol-class+
                                      "getSymbolSetfFunctionOrDie"
                                      nil +lisp-object+)
                  (emit-move-from-stack target))))
          ((compiland-p name)
           (dformat t "p2-function case 3~%")
           (p2-lambda name target))
          (t
           (compiler-unsupported "p2-function: unsupported case: ~S" form)))))

(defknown p2-ash (t t t) t)
(defun p2-ash (form target representation)
  (unless (check-arg-count form 2)
    (compile-function-call form target representation)
    (return-from p2-ash))
  (let* ((args (%cdr form))
         (arg1 (%car args))
         (arg2 (%cadr args))
         (type1 (derive-compiler-type arg1))
         (type2 (derive-compiler-type arg2))
         (low2 (and (fixnum-type-p type2) (integer-type-low type2)))
         (high2 (and (fixnum-type-p type2) (integer-type-high type2)))
         (constant-shift (fixnum-constant-value type2))
         (result-type (derive-compiler-type form)))
;;     (format t "~&p2-ash type1 = ~S~%" type1)
;;     (format t "p2-ash type2 = ~S~%" type2)
;;     (format t "p2-ash result-type = ~S~%" result-type)
;;     (format t "p2-ash representation = ~S~%" representation)
    (cond ((and (integerp arg1) (integerp arg2))
           (compile-constant (ash arg1 arg2) target representation))
          ((and constant-shift
                ;; ishl/ishr only use the low five bits of the mask.
                (<= -31 constant-shift 31)
                (fixnum-type-p type1)
                (fixnum-type-p result-type))
           (when (null representation)
             (emit 'new +lisp-fixnum-class+)
             (emit 'dup))
           (compile-form arg1 'stack :int)
           (cond ((plusp constant-shift)
                  (compile-form arg2 'stack :int)
                  (maybe-emit-clear-values arg1 arg2)
                  (emit 'ishl))
                 ((minusp constant-shift)
                  (cond ((fixnump arg2)
                         (emit-push-constant-int (- arg2)))
                        (t
                         (compile-form arg2 'stack :int)
                         (emit 'ineg)))
                  (maybe-emit-clear-values arg1 arg2)
                  (emit 'ishr))
                 ((zerop constant-shift)
                  (compile-form arg2 nil nil))) ; for effect
           (case representation
             (:int)
             (:long
              (emit 'i2l))
             (t
              (emit-invokespecial-init +lisp-fixnum-class+ '("I"))))
           (emit-move-from-stack target representation))
          ((and constant-shift
                ;; lshl/lshr only use the low six bits of the mask.
                (<= -63 constant-shift 63)
                (java-long-type-p type1)
                (java-long-type-p result-type))
           (compile-form arg1 'stack :long)
           (cond ((plusp constant-shift)
                  (compile-form arg2 'stack :int)
                  (maybe-emit-clear-values arg1 arg2)
                  (emit 'lshl))
                 ((minusp constant-shift)
                  (cond ((fixnump arg2)
                         (emit-push-constant-int (- arg2)))
                        (t
                         (compile-form arg2 'stack :int)
                         (emit 'ineg)))
                  (maybe-emit-clear-values arg1 arg2)
                  (emit 'lshr))
                 ((zerop constant-shift)
                  (compile-form arg2 nil nil))) ; for effect
           (convert-long representation)
           (emit-move-from-stack target representation))
          ((and (fixnum-type-p type1)
                low2 high2 (<= -31 low2 high2 0)) ; Negative shift.
           (when (null representation)
             (emit 'new +lisp-fixnum-class+)
             (emit 'dup))
	   (compile-forms-and-maybe-emit-clear-values arg1 'stack :int
						      arg2 'stack :int)
           (emit 'ineg)
           (emit 'ishr)
           (case representation
             (:int)
             (:long
              (emit 'i2l))
             (t
              (emit-invokespecial-init +lisp-fixnum-class+ '("I"))))
           (emit-move-from-stack target representation))
          ((fixnum-type-p type2)
           (cond ((and low2 high2 (<= 0 low2 high2 63) ; Non-negative shift.
                       (java-long-type-p type1)
                       (java-long-type-p result-type))
		  (compile-forms-and-maybe-emit-clear-values arg1 'stack :long
							     arg2 'stack :int)
                  (emit 'lshl)
                  (convert-long representation))
                 ((and low2 high2 (<= -63 low2 high2 0) ; Negative shift.
                       (java-long-type-p type1)
                       (java-long-type-p result-type))
		  (compile-forms-and-maybe-emit-clear-values arg1 'stack :long
							     arg2 'stack :int)
                  (emit 'ineg)
                  (emit 'lshr)
                  (convert-long representation))
                 (t
;;                   (format t "p2-ash call to LispObject.ash(int)~%")
;;                   (format t "p2-ash type1 = ~S type2 = ~S~%" type1 type2)
;;                   (format t "p2-ash result-type = ~S~%" result-type)
		  (compile-forms-and-maybe-emit-clear-values arg1 'stack nil
							     arg2 'stack :int)
                  (emit-invokevirtual +lisp-object-class+ "ash" '("I") +lisp-object+)
                  (fix-boxing representation result-type)))
           (emit-move-from-stack target representation))
          (t
;;            (format t "p2-ash full call~%")
           (compile-function-call form target representation)))))

(defknown p2-logand (t t t) t)
(defun p2-logand (form target representation)
  (let* ((args (cdr form))
;;          (len (length args))
         )
;;     (cond ((= len 2)
    (case (length args)
      (2
       (let* ((arg1 (%car args))
              (arg2 (%cadr args))
              (type1 (derive-compiler-type arg1))
              (type2 (derive-compiler-type arg2))
              (result-type (derive-compiler-type form)))
         ;;              (let ((*print-structure* nil))
         ;;                (format t "~&p2-logand arg1 = ~S~%" arg1)
         ;;                (format t "p2-logand arg2 = ~S~%" arg2))
         ;;              (format t "~&p2-logand type1 = ~S~%" type1)
         ;;              (format t "p2-logand type2 = ~S~%" type2)
         ;;              (format t "p2-logand result-type = ~S~%" result-type)
         ;;              (format t "p2-logand representation = ~S~%" representation)
         (cond ((and (integerp arg1) (integerp arg2))
                (compile-constant (logand arg1 arg2) target representation))
               ((and (integer-type-p type1) (eql arg2 0))
		(compile-forms-and-maybe-emit-clear-values arg1 nil nil)
                (compile-constant 0 target representation))
               ((eql (fixnum-constant-value type1) -1)
		(compile-forms-and-maybe-emit-clear-values arg1 nil nil
							   arg2 target representation))
               ((eql (fixnum-constant-value type2) -1)
		(compile-forms-and-maybe-emit-clear-values arg1 target representation
							   arg2 nil nil))
               ((and (fixnum-type-p type1) (fixnum-type-p type2))
                ;;                     (format t "p2-logand fixnum case~%")
                ;; Both arguments are fixnums.
                (when (null representation)
                  (emit 'new +lisp-fixnum-class+)
                  (emit 'dup))
		(compile-forms-and-maybe-emit-clear-values arg1 'stack :int
							   arg2 'stack :int)
                (emit 'iand)
                (case representation
                  (:int)
                  (:long
                   (emit 'i2l))
                  (t
                   (emit-invokespecial-init +lisp-fixnum-class+ '("I"))))
                (emit-move-from-stack target representation))
               ((or (and (fixnum-type-p type1)
                         (compiler-subtypep type1 'unsigned-byte))
                    (and (fixnum-type-p type2)
                         (compiler-subtypep type2 'unsigned-byte)))
                ;; One of the arguments is a positive fixnum.
                (when (null representation)
                  (emit 'new +lisp-fixnum-class+)
                  (emit 'dup))
		(compile-forms-and-maybe-emit-clear-values arg1 'stack :int
							   arg2 'stack :int)
                (emit 'iand)
                (case representation
                  (:int)
                  (:long
                   (emit 'i2l))
                  (t
                   (emit-invokespecial-init +lisp-fixnum-class+ '("I"))))
                (emit-move-from-stack target representation))
               ((and (java-long-type-p type1) (java-long-type-p type2))
                ;; Both arguments are longs.
		(compile-forms-and-maybe-emit-clear-values arg1 'stack :long
							   arg2 'stack :long)
                (emit 'land)
                (case representation
                  (:int
                   (emit 'l2i))
                  (:long)
                  (t
                   (emit-box-long)))
                (emit-move-from-stack target representation))
               ((or (and (java-long-type-p type1)
                         (compiler-subtypep type1 'unsigned-byte))
                    (and (java-long-type-p type2)
                         (compiler-subtypep type2 'unsigned-byte)))
                ;; One of the arguments is a positive long.
		(compile-forms-and-maybe-emit-clear-values arg1 'stack :long
							   arg2 'stack :long)
                (emit 'land)
                (case representation
                  (:int
                   (emit 'l2i))
                  (:long)
                  (t
                   (emit-box-long)))
                (emit-move-from-stack target representation))
               ((fixnum-type-p type2)
                ;;                     (format t "p2-logand LispObject.LOGAND(int) 1~%")
		(compile-forms-and-maybe-emit-clear-values arg1 'stack nil
							   arg2 'stack :int)
                (emit-invokevirtual +lisp-object-class+ "LOGAND" '("I") +lisp-object+)
                (fix-boxing representation result-type)
                (emit-move-from-stack target representation))
               ((fixnum-type-p type1)
                ;;                     (format t "p2-logand LispObject.LOGAND(int) 2~%")
                ;; arg1 is a fixnum, but arg2 is not
		(compile-forms-and-maybe-emit-clear-values arg1 'stack :int
							   arg2 'stack nil)
                ;; swap args
                (emit 'swap)
                (emit-invokevirtual +lisp-object-class+ "LOGAND" '("I") +lisp-object+)
                (fix-boxing representation result-type)
                (emit-move-from-stack target representation))
               (t
                ;;                     (format t "p2-logand LispObject.LOGAND(LispObject)~%")
		(compile-forms-and-maybe-emit-clear-values arg1 'stack nil
							   arg2 'stack nil)
                (emit-invokevirtual +lisp-object-class+ "LOGAND"
                                    (lisp-object-arg-types 1) +lisp-object+)
                (fix-boxing representation result-type)
                (emit-move-from-stack target representation)))))
      (t
       (compile-function-call form target representation)))))

(defknown p2-logior (t t t) t)
(defun p2-logior (form target representation)
  (let ((args (cdr form)))
    (case (length args)
      (0
       (compile-constant 0 target representation))
      (1
       (let ((arg (%car args)))
	 (compile-forms-and-maybe-emit-clear-values arg target representation)))
      (2
       (let* ((arg1 (%car args))
              (arg2 (%cadr args))
              type1 type2 result-type)
         (when (and (integerp arg1) (integerp arg2))
           (compile-constant (logior arg1 arg2) target representation)
           (return-from p2-logior t))
         (when (integerp arg1)
           (setf arg1 (%cadr args)
                 arg2 (%car args)))
         (setf type1 (derive-compiler-type arg1)
               type2 (derive-compiler-type arg2)
               result-type (derive-compiler-type form))
         (cond ((and (fixnum-constant-value type1) (fixnum-constant-value type2))
		(compile-forms-and-maybe-emit-clear-values arg1 nil nil
							   arg2 nil nil)
                (compile-constant (logior (fixnum-constant-value type1)
                                          (fixnum-constant-value type2))
                                  target representation))
               ((and (fixnum-type-p type1) (fixnum-type-p type2))
                (when (null representation)
                  (emit 'new +lisp-fixnum-class+)
                  (emit 'dup))
		(compile-forms-and-maybe-emit-clear-values arg1 'stack :int
							   arg2 'stack :int)
                (emit 'ior)
                (case representation
                  (:int)
                  (:long
                   (emit 'i2l))
                  (t
                   (emit-invokespecial-init +lisp-fixnum-class+ '("I"))))
                (emit-move-from-stack target representation))
               ((and (eql (fixnum-constant-value type1) 0) (< *safety* 3))
		(compile-forms-and-maybe-emit-clear-values arg1 nil nil
							   arg2 target representation))
               ((and (eql (fixnum-constant-value type2) 0) (< *safety* 3))
		(compile-forms-and-maybe-emit-clear-values arg1 target representation
							   arg2 nil nil))
               ((or (eq representation :long)
                    (and (java-long-type-p type1) (java-long-type-p type2)))
		(compile-forms-and-maybe-emit-clear-values arg1 'stack :long
							   arg2 'stack :long)
                (emit 'lor)
                (convert-long representation)
                (emit-move-from-stack target representation))
               ((fixnum-type-p type2)
		(compile-forms-and-maybe-emit-clear-values arg1 'stack nil
							   arg2 'stack :int)
                (emit-invokevirtual +lisp-object-class+ "LOGIOR" '("I") +lisp-object+)
                (fix-boxing representation result-type)
                (emit-move-from-stack target representation))
               ((fixnum-type-p type1)
                ;; arg1 is of fixnum type, but arg2 is not
		(compile-forms-and-maybe-emit-clear-values arg1 'stack :int
							   arg2 'stack nil)
                ;; swap args
                (emit 'swap)
                (emit-invokevirtual +lisp-object-class+ "LOGIOR" '("I") +lisp-object+)
                (fix-boxing representation result-type)
                (emit-move-from-stack target representation))
               (t
		(compile-forms-and-maybe-emit-clear-values arg1 'stack nil
							   arg2 'stack nil)
                (emit-invokevirtual +lisp-object-class+ "LOGIOR"
                                    (lisp-object-arg-types 1) +lisp-object+)
                (fix-boxing representation result-type)
                (emit-move-from-stack target representation)))))
      (t
       ;; (logior a b c d ...) => (logior a (logior b c d ...))
       (let ((new-form `(logior ,(car args) (logior ,@(cdr args)))))
         (p2-logior new-form target representation))))))

(defknown p2-logxor (t t t) t)
(defun p2-logxor (form target representation)
  (let* ((args (cdr form))
         (len (length args)))
    (case len
      (0
       (compile-constant 0 target representation))
      (1
       (let ((arg (%car args)))
	 (compile-forms-and-maybe-emit-clear-values arg target representation)))
      (2
       (let* ((arg1 (%car args))
              (arg2 (%cadr args))
              type1 type2 result-type)
         (when (and (integerp arg1) (integerp arg2))
           (compile-constant (logxor arg1 arg2) target representation)
           (return-from p2-logxor))
         (when (integerp arg1)
           (setf arg1 (%cadr args)
                 arg2 (%car args)))
         (setf type1       (derive-compiler-type arg1)
               type2       (derive-compiler-type arg2)
               result-type (derive-compiler-type form))
         (cond ((eq representation :int)
		(compile-forms-and-maybe-emit-clear-values arg1 'stack :int
							   arg2 'stack :int)
                (emit 'ixor))
               ((and (fixnum-type-p type1) (fixnum-type-p type2))
;;                 (format t "p2-logxor case 2~%")
                (when (null representation)
                  (emit 'new +lisp-fixnum-class+)
                  (emit 'dup))
		(compile-forms-and-maybe-emit-clear-values arg1 'stack :int
							   arg2 'stack :int)
                (emit 'ixor)
                (case representation
                  (:int)
                  (:long
                   (emit 'i2l))
                  (t
                   (emit-invokespecial-init +lisp-fixnum-class+ '("I")))))
               ((and (java-long-type-p type1) (java-long-type-p type2))
		(compile-forms-and-maybe-emit-clear-values arg1 'stack :long
							   arg2 'stack :long)
                (emit 'lxor)
                (convert-long representation))
               ((fixnum-type-p type2)
		(compile-forms-and-maybe-emit-clear-values arg1 'stack nil
							   arg2 'stack :int)
                (emit-invokevirtual +lisp-object-class+ "LOGXOR" '("I") +lisp-object+)
                (fix-boxing representation result-type))
               (t
		(compile-forms-and-maybe-emit-clear-values arg1 'stack nil
							   arg2 'stack nil)
                (emit-invokevirtual +lisp-object-class+ "LOGXOR"
                                    (lisp-object-arg-types 1) +lisp-object+)
                (fix-boxing representation result-type)))
         (emit-move-from-stack target representation)))
      (t
       ;; (logxor a b c d ...) => (logxor a (logxor b c d ...))
       (let ((new-form `(logxor ,(car args) (logxor ,@(cdr args)))))
         (p2-logxor new-form target representation))))))

(defknown p2-lognot (t t t) t)
(defun p2-lognot (form target representation)
  (unless (check-arg-count form 1)
    (compile-function-call form target representation)
    (return-from p2-lognot))
  (cond ((and (fixnum-type-p (derive-compiler-type form)))
         (let ((arg (%cadr form)))
           (when (null representation)
             (emit 'new +lisp-fixnum-class+)
             (emit 'dup))
	   (compile-forms-and-maybe-emit-clear-values arg 'stack :int)
           (emit 'iconst_m1)
           (emit 'ixor)
           (case representation
             (:int)
             (:long
              (emit 'i2l))
             (t
              (emit-invokespecial-init +lisp-fixnum-class+ '("I"))))
           (emit-move-from-stack target representation)))
        (t
         (let ((arg (%cadr form)))
	   (compile-forms-and-maybe-emit-clear-values arg 'stack nil))
         (emit-invokevirtual +lisp-object-class+ "LOGNOT" nil +lisp-object+)
         (fix-boxing representation nil)
         (emit-move-from-stack target representation))))

;; %ldb size position integer => byte
(defknown p2-%ldb (t t t) t)
(defun p2-%ldb (form target representation)
;;   (format t "~&p2-%ldb~%")
  (unless (check-arg-count form 3)
    (compile-function-call form target representation)
    (return-from p2-%ldb))
  (let* ((args (cdr form))
         (size-arg (%car args))
         (position-arg (%cadr args))
         (arg3 (%caddr args))
         (size-type (derive-compiler-type size-arg))
         (position-type (derive-compiler-type position-arg))
         (size (fixnum-constant-value size-type))
         (position (fixnum-constant-value position-type)))
    ;; FIXME Inline the case where all args are of fixnum type.
    ;; FIXME Add LispObject.ldb(), returning a Java int, for the case where we
    ;; need an unboxed fixnum result.
    (cond ((eql size 0)
	   (compile-forms-and-maybe-emit-clear-values size-arg nil nil
						      position-arg nil nil
						      arg3 nil nil)
           (compile-constant 0 target representation))
          ((and size position)
           (cond ((<= (+ position size) 31)
                  (when (null representation)
                    (emit 'new +lisp-fixnum-class+)
                    (emit 'dup))
		  (compile-forms-and-maybe-emit-clear-values size-arg nil nil
							     position-arg nil nil
							     arg3 'stack :int)
                  (unless (zerop position)
                    (emit-push-constant-int position)
                    (emit 'ishr))
                  (emit-push-constant-int (1- (expt 2 size))) ; mask
                  (emit 'iand)
                  (case representation
                    (:int)
                    (:long
                     (emit 'i2l))
                    (t
                     (emit-invokespecial-init +lisp-fixnum-class+ '("I"))))
                  (emit-move-from-stack target representation))
                 ((<= (+ position size) 63)
                  (when (and (null representation) (<= size 31))
                    ;; Result is a fixnum.
                    (emit 'new +lisp-fixnum-class+)
                    (emit 'dup))
		  (compile-forms-and-maybe-emit-clear-values size-arg nil nil
							     position-arg nil nil
							     arg3 'stack :long)
                  (unless (zerop position)
                    (emit-push-constant-int position)
                    (emit 'lshr))
                  (cond ((<= size 31)
                         (emit 'l2i)
                         (emit-push-constant-int (1- (expt 2 size)))
                         (emit 'iand)
                         (case representation
                           (:int)
                           (:long
                            (emit 'i2l))
                           (t
                            (emit-invokespecial-init +lisp-fixnum-class+ '("I")))))
                        (t
                         (emit-push-constant-long (1- (expt 2 size))) ; mask
                         (emit 'land)
                         (convert-long representation)))
                  (emit-move-from-stack target representation))
                 (t
		  (compile-forms-and-maybe-emit-clear-values arg3 'stack nil)
                  (emit-push-constant-int size)
                  (emit-push-constant-int position)
                  (emit-invokevirtual +lisp-object-class+ "LDB" '("I" "I") +lisp-object+)
                  (fix-boxing representation nil)
                  (emit-move-from-stack target representation))))
          ((and (fixnum-type-p size-type)
                (fixnum-type-p position-type))
	   (compile-forms-and-maybe-emit-clear-values size-arg 'stack :int
						      position-arg 'stack :int
						      arg3 'stack nil)
           (emit 'dup_x2)
           (emit 'pop)
           (emit-invokevirtual +lisp-object-class+ "LDB" '("I" "I") +lisp-object+)
           (fix-boxing representation nil)
           (emit-move-from-stack target representation))
          (t
           (compile-function-call form target representation)))))

(defknown p2-mod (t t t) t)
(defun p2-mod (form target representation)
  (unless (check-arg-count form 2)
    (compile-function-call form target representation)
    (return-from p2-mod))
  (let* ((args (cdr form))
         (arg1 (%car args))
         (arg2 (%cadr args))
         (type1 (derive-compiler-type arg1))
         (type2 (derive-compiler-type arg2)))
    (cond ((and (eq representation :int)
                (fixnum-type-p type1)
                (fixnum-type-p type2))
	   (compile-forms-and-maybe-emit-clear-values arg1 'stack :int
						      arg2 'stack :int)
           (emit-invokestatic +lisp-class+ "mod" '("I" "I") "I")
           (emit-move-from-stack target representation))
          ((fixnum-type-p type2)
	   (compile-forms-and-maybe-emit-clear-values arg1 'stack nil
						      arg2 'stack :int)
           (emit-invokevirtual +lisp-object-class+ "MOD" '("I") +lisp-object+)
           (fix-boxing representation nil) ; FIXME use derived result type
           (emit-move-from-stack target representation))
          (t
	   (compile-forms-and-maybe-emit-clear-values arg1 'stack nil
						      arg2 'stack nil)
           (emit-invokevirtual +lisp-object-class+ "MOD"
                               (lisp-object-arg-types 1) +lisp-object+)
           (fix-boxing representation nil) ; FIXME use derived result type
           (emit-move-from-stack target representation)))))

;; (defknown p2-integerp (t t t) t)
;; (defun p2-integerp (form target representation)
;;   (unless (check-arg-count form 1)
;;     (compile-function-call form target representation)
;;     (return-from p2-integerp))
;;   (let ((arg (cadr form)))
;;     (compile-form arg 'stack nil)
;;     (maybe-emit-clear-values arg)
;;     (case representation
;;       (:boolean
;;        (emit-invokevirtual +lisp-object-class+ "integerp" nil "Z"))
;;       (t
;;        (emit-invokevirtual +lisp-object-class+ "INTEGERP" nil +lisp-object+)))
;;     (emit-move-from-stack target representation)))

;; (defknown p2-listp (t t t) t)
;; (defun p2-listp (form target representation)
;;   (unless (check-arg-count form 1)
;;     (compile-function-call form target representation)
;;     (return-from p2-listp))
;;   (let ((arg (cadr form)))
;;     (compile-form arg 'stack nil)
;;     (maybe-emit-clear-values arg)
;;     (case representation
;;       (:boolean
;;        (emit-invokevirtual +lisp-object-class+ "listp" nil "Z"))
;;       (t
;;        (emit-invokevirtual +lisp-object-class+ "LISTP" nil +lisp-object+)))
;;     (emit-move-from-stack target representation)))

(defknown p2-zerop (t t t) t)
(defun p2-zerop (form target representation)
  (aver (or (null representation) (eq representation :boolean)))
  (unless (check-arg-count form 1)
    (compile-function-call form target representation)
    (return-from p2-zerop))
  (let* ((arg (cadr form))
         (type (derive-compiler-type arg)))
    (cond ((fixnum-type-p type)
	   (compile-forms-and-maybe-emit-clear-values arg 'stack :int)
           (let ((LABEL1 (gensym))
                 (LABEL2 (gensym)))
             (emit 'ifne LABEL1)
             (case representation
               (:boolean
                (emit 'iconst_1))
               (t
                (emit-push-t)))
             (emit 'goto LABEL2)
             (label LABEL1)
             (case representation
               (:boolean
                (emit 'iconst_0))
               (t
                (emit-push-nil)))
             (label LABEL2)
             (emit-move-from-stack target representation)))
          ((java-long-type-p type)
	   (compile-forms-and-maybe-emit-clear-values arg 'stack :long)
           (emit 'lconst_0)
           (emit 'lcmp)
           (let ((LABEL1 (gensym))
                 (LABEL2 (gensym)))
             (emit 'ifne LABEL1)
             (emit-push-true representation)
             (emit 'goto LABEL2)
             (label LABEL1)
             (emit-push-false representation)
             (label LABEL2)
             (emit-move-from-stack target representation)))
          (t
	   (compile-forms-and-maybe-emit-clear-values arg 'stack nil)
           (emit-invoke-method "ZEROP" target representation)))))

;; find-class symbol &optional errorp environment => class
(defknown p2-find-class (t t t) t)
(defun p2-find-class (form target representation)
  (let* ((args (cdr form))
         (arg-count (length args))
         (arg1 (first args))
         class)
    (when (and (<= 1 arg-count 2) ; no environment arg
               (consp arg1)
               (= (length arg1) 2)
               (eq (first arg1) 'QUOTE)
               (symbolp (second arg1))
               (eq (symbol-package (second arg1)) (find-package "CL"))
               (setf class (find-class (second arg1) nil)))
      (compile-constant class target representation)
      (return-from p2-find-class))
    (case arg-count
      (1
       ;; errorp is true
       (compile-forms-and-maybe-emit-clear-values arg1 'stack nil)
       (emit-push-constant-int 1) ; errorp
       (emit-invokestatic +lisp-class-class+ "findClass"
                          (list +lisp-object+ "Z") +lisp-object+)
       (fix-boxing representation nil)
       (emit-move-from-stack target representation))
      (2
       (let ((arg2 (second args)))
	 (compile-forms-and-maybe-emit-clear-values arg1 'stack nil
						    arg2 'stack :boolean)
         (emit-invokestatic +lisp-class-class+ "findClass"
                            (list +lisp-object+ "Z") +lisp-object+)
         (fix-boxing representation nil)
         (emit-move-from-stack target representation)))
      (t
       (compile-function-call form target representation)))))

;; vector-push-extend new-element vector &optional extension => new-index
(defun p2-vector-push-extend (form target representation)
  (let* ((args (cdr form))
         (arg-count (length args))
         (arg1 (first args))
         (arg2 (second args)))
    (case arg-count
      (2
       (compile-forms-and-maybe-emit-clear-values arg1 'stack nil
						  arg2 'stack nil)
       (emit 'swap)
       (cond (target
              (emit-invokevirtual +lisp-object-class+ "VECTOR_PUSH_EXTEND"
                                  (lisp-object-arg-types 1) +lisp-object+)
              (fix-boxing representation nil)
              (emit-move-from-stack target representation))
             (t
              (emit-invokevirtual +lisp-object-class+ "vectorPushExtend"
                                  (lisp-object-arg-types 1) nil))))
      (t
       (compile-function-call form target representation)))))

(defknown p2-std-slot-value (t t t) t)
(defun p2-std-slot-value (form target representation)
  (unless (check-arg-count form 2)
    (compile-function-call form target representation)
    (return-from p2-std-slot-value))
  (let* ((args (cdr form))
         (arg1 (first args))
         (arg2 (second args)))
    (compile-forms-and-maybe-emit-clear-values arg1 'stack nil
					       arg2 'stack nil)
    (emit-invokevirtual +lisp-object-class+ "SLOT_VALUE"
                        (lisp-object-arg-types 1) +lisp-object+)
    (fix-boxing representation nil)
    (emit-move-from-stack target representation)))

;; set-std-slot-value instance slot-name new-value => new-value
(defknown p2-set-std-slot-value (t t t) t)
(defun p2-set-std-slot-value (form target representation)
  (unless (check-arg-count form 3)
    (compile-function-call form target representation)
    (return-from p2-set-std-slot-value))
  (let* ((args (cdr form))
         (arg1 (first args))
         (arg2 (second args))
         (arg3 (third args))
         (*register* *register*)
         (value-register (when target (allocate-register))))
    (compile-forms-and-maybe-emit-clear-values arg1 'stack nil
					       arg2 'stack nil
					       arg3 'stack nil)
    (when value-register
      (emit 'dup)
      (emit 'astore value-register))
    (emit-invokevirtual +lisp-object-class+ "setSlotValue"
                        (lisp-object-arg-types 2) nil)
    (when value-register
      (emit 'aload value-register)
      (fix-boxing representation nil)
      (emit-move-from-stack target representation))))

(defun p2-make-array (form target representation)
  ;; In safe code, we want to make sure the requested length does not exceed
  ;; ARRAY-DIMENSION-LIMIT.
  (cond ((and (< *safety* 3)
              (= (length form) 2)
              (fixnum-type-p (derive-compiler-type (second form)))
              (null representation))
         (let ((arg (second form)))
           (emit 'new +lisp-simple-vector-class+)
           (emit 'dup)
	   (compile-forms-and-maybe-emit-clear-values arg 'stack :int)
           (emit-invokespecial-init +lisp-simple-vector-class+ '("I"))
           (emit-move-from-stack target representation)))
        (t
         (compile-function-call form target representation))))

;; make-sequence result-type size &key initial-element => sequence
(defun p2-make-sequence (form target representation)
  ;; In safe code, we want to make sure the requested length does not exceed
  ;; ARRAY-DIMENSION-LIMIT.
  (unless (and (< *safety* 3)
               (= (length form) 3)
               (null representation))
    (compile-function-call form target representation)
    (return-from p2-make-sequence))
  (let* ((args (cdr form))
         (arg1 (first args))
         (arg2 (second args)))
    (when (and (consp arg1)
               (= (length arg1) 2)
               (eq (first arg1) 'QUOTE))
      (let* ((result-type (second arg1))
             (class
              (case result-type
                ((STRING SIMPLE-STRING)
                 (setf class +lisp-simple-string-class+))
                ((VECTOR SIMPLE-VECTOR)
                 (setf class +lisp-simple-vector-class+)))))
        (when class
          (emit 'new class)
          (emit 'dup)
	  (compile-forms-and-maybe-emit-clear-values arg2 'stack :int)
          (emit-invokespecial-init class '("I"))
          (emit-move-from-stack target representation)
          (return-from p2-make-sequence)))))
  (compile-function-call form target representation))

(defun p2-make-string (form target representation)
  ;; In safe code, we want to make sure the requested length does not exceed
  ;; ARRAY-DIMENSION-LIMIT.
  (cond ((and (< *safety* 3)
              (= (length form) 2)
              (null representation))
         (let ((arg (second form)))
           (emit 'new +lisp-simple-string-class+)
           (emit 'dup)
	   (compile-forms-and-maybe-emit-clear-values arg 'stack :int)
           (emit-invokespecial-init +lisp-simple-string-class+ '("I"))
           (emit-move-from-stack target representation)))
        (t
         (compile-function-call form target representation))))

(defun p2-%make-structure (form target representation)
  (cond ((and (check-arg-count form 2)
              (eq (derive-type (%cadr form)) 'SYMBOL))
         (emit 'new +lisp-structure-object-class+)
         (emit 'dup)
         (compile-form (%cadr form) 'stack nil)
         (emit 'checkcast +lisp-symbol-class+)
         (compile-form (%caddr form) 'stack nil)
         (maybe-emit-clear-values (%cadr form) (%caddr form))
         (emit-invokevirtual +lisp-object-class+ "copyToArray"
                             nil +lisp-object-array+)
         (emit-invokespecial-init +lisp-structure-object-class+
                                  (list +lisp-symbol+ +lisp-object-array+))
         (emit-move-from-stack target representation))
        (t
         (compile-function-call form target representation))))

(defun p2-make-structure (form target representation)
  (let* ((args (cdr form))
         (slot-forms (cdr args))
         (slot-count (length slot-forms)))
    (cond ((and (<= 1 slot-count 6)
                (eq (derive-type (%car args)) 'SYMBOL))
           (emit 'new +lisp-structure-object-class+)
           (emit 'dup)
           (compile-form (%car args) 'stack nil)
           (emit 'checkcast +lisp-symbol-class+)
           (dolist (slot-form slot-forms)
             (compile-form slot-form 'stack nil))
           (apply 'maybe-emit-clear-values args)
           (emit-invokespecial-init +lisp-structure-object-class+
                                    (append (list +lisp-symbol+)
                                            (make-list slot-count :initial-element +lisp-object+)))
           (emit-move-from-stack target representation))
          (t
           (compile-function-call form target representation)))))

(defun p2-make-hash-table (form target representation)
  (cond ((= (length form) 1) ; no args
         (emit 'new +lisp-eql-hash-table-class+)
         (emit 'dup)
         (emit-invokespecial-init +lisp-eql-hash-table-class+ nil)
         (fix-boxing representation nil)
         (emit-move-from-stack target representation))
        (t
         (compile-function-call form target representation))))

(defknown p2-stream-element-type (t t t) t)
(defun p2-stream-element-type (form target representation)
  (unless (check-arg-count form 1)
    (compile-function-call form target representation)
    (return-from p2-stream-element-type))
  (let ((arg (%cadr form)))
    (cond ((eq (derive-compiler-type arg) 'STREAM)
	   (compile-forms-and-maybe-emit-clear-values arg 'stack nil)
           (emit 'checkcast +lisp-stream-class+)
           (emit-invokevirtual +lisp-stream-class+ "getElementType"
                               nil +lisp-object+)
           (emit-move-from-stack target representation))
          (t
           (compile-function-call form target representation)))))

;; write-8-bits byte stream => nil
(defknown p2-write-8-bits (t t t) t)
(defun p2-write-8-bits (form target representation)
  (unless (check-arg-count form 2)
    (compile-function-call form target representation)
    (return-from p2-write-8-bits))
  (let* ((arg1 (%cadr form))
         (arg2 (%caddr form))
         (type1 (derive-compiler-type arg1))
         (type2 (derive-compiler-type arg2)))
    (cond ((and (compiler-subtypep type1 '(UNSIGNED-BYTE 8))
                (eq type2 'STREAM))
           (compile-form arg1 'stack :int)
           (compile-form arg2 'stack nil)
           (emit 'checkcast +lisp-stream-class+)
           (maybe-emit-clear-values arg1 arg2)
           (emit 'swap)
           (emit-invokevirtual +lisp-stream-class+ "_writeByte" '("I") nil)
           (when target
             (emit-push-nil)
             (emit-move-from-stack target)))
          ((fixnum-type-p type1)
           (compile-form arg1 'stack :int)
           (compile-form arg2 'stack nil)
           (maybe-emit-clear-values arg1 arg2)
           (emit-invokestatic +lisp-class+ "writeByte"
                              (list "I" +lisp-object+) nil)
           (when target
             (emit-push-nil)
             (emit-move-from-stack target)))
          (t
           (compile-function-call form target representation)))))

(defun p2-read-line (form target representation)
;;   (format t "p2-read-line~%")
  (let* ((args (cdr form))
         (len (length args)))
    (case len
      (1
       (let* ((arg1 (%car args))
              (type1 (derive-compiler-type arg1)))
         (cond ((compiler-subtypep type1 'stream)
;;                 (format t "p2-read-line optimized case 1~%")
		(compile-forms-and-maybe-emit-clear-values arg1 'stack nil)
                (emit 'checkcast +lisp-stream-class+)
                (emit-push-constant-int 1)
                (emit-push-nil)
                (emit-invokevirtual +lisp-stream-class+ "readLine"
                                    (list "Z" +lisp-object+) +lisp-object+)
                (when target
                  (emit-move-from-stack target)))
               (t
                (compile-function-call form target representation)))))
      (2
       (let* ((arg1 (%car args))
              (type1 (derive-compiler-type arg1))
              (arg2 (%cadr args)))
         (cond ((and (compiler-subtypep type1 'stream) (null arg2))
;;                 (format t "p2-read-line optimized case 2~%")
		(compile-forms-and-maybe-emit-clear-values arg1 'stack nil)
                (emit 'checkcast +lisp-stream-class+)
                (emit-push-constant-int 0)
                (emit-push-nil)
                (emit-invokevirtual +lisp-stream-class+ "readLine"
                                    (list "Z" +lisp-object+) +lisp-object+)
                (when target
                  (emit-move-from-stack target))
                )
               (t
                (compile-function-call form target representation)))))
      (t
       (compile-function-call form target representation)))))

(defmacro define-derive-type-handler (op lambda-list &body body)
  (let ((name (intern (concatenate 'string "DERIVE-TYPE-" (symbol-name op)))))
    `(progn
       (defknown ,name (t) t)
       (defun ,name ,lambda-list ,@body)
       (setf (get ',op 'derive-type-handler) ',name))))

(define-derive-type-handler aref (form)
  (let* ((args (cdr form))
         (array-arg (car args))
         (array-type (normalize-type (derive-type array-arg)))
         (result-type t))
    (cond ((and (consp array-type)
                (memq (%car array-type) '(ARRAY SIMPLE-ARRAY VECTOR)))
           (let ((element-type (second array-type)))
             (unless (eq element-type '*)
               (setf result-type element-type))))
          ((and (consp array-type)
                (memq (%car array-type) '(STRING SIMPLE-STRING)))
           (setf result-type 'CHARACTER)))
    result-type))

(define-derive-type-handler fixnump (form)
   (if (fixnum-type-p (derive-compiler-type (cadr form)))
       +true-type+
       'BOOLEAN))

(define-derive-type-handler setq (form)
  (if (= (length form) 3)
      (derive-compiler-type (third form))
      t))

(defknown derive-type-logior/logxor (t) t)
(defun derive-type-logior/logxor (form)
  (let ((op (car form))
        (args (cdr form))
        (result-type +integer-type+))
    (case (length args)
      (0
       (setf result-type (make-integer-type '(INTEGER 0 0))))
      (1
       (setf result-type (derive-compiler-type (car args))))
      (2
       (let ((type1 (derive-compiler-type (%car args)))
             (type2 (derive-compiler-type (%cadr args))))
         (cond ((and (compiler-subtypep type1 'unsigned-byte)
                     (compiler-subtypep type2 'unsigned-byte))
                (let ((high1 (integer-type-high type1))
                      (high2 (integer-type-high type2)))
                  (cond ((and high1 high2)
                         (let ((length (integer-length (max high1 high2))))
                           (setf result-type
                                 (make-compiler-type (list 'INTEGER 0
                                                           (1- (expt 2 length)))))))
                        (t
                         (setf result-type (make-compiler-type 'unsigned-byte))))))
               ((and (fixnum-type-p type1)
                     (fixnum-type-p type2))
                (setf result-type (make-compiler-type 'fixnum))))))
      (t
       (setf result-type (derive-type-logior/logxor
                          `(,op ,(car args) (,op ,@(cdr args)))))))
    result-type))

(defknown derive-type-logand (t) t)
(defun derive-type-logand (form)
  (let ((args (cdr form)))
    (case (length args)
      (0
       (make-integer-type '(INTEGER -1 -1)))
      (1
       (let ((type (derive-compiler-type (%car args))))
         (if (integer-type-p type)
             type
             (make-integer-type 'INTEGER))))
      (2
       (dformat t "derive-type-logand 2-arg case~%")
       (let* ((type1 (derive-compiler-type (%car args)))
              (type2 (derive-compiler-type (%cadr args)))
              low1 high1 low2 high2 result-low result-high result-type)
         (when (integer-type-p type1)
           (setf low1 (integer-type-low type1)
                 high1 (integer-type-high type1)))
         (when (integer-type-p type2)
           (setf low2 (integer-type-low type2)
                 high2 (integer-type-high type2)))
         (cond ((and low1 low2 (>= low1 0) (>= low2 0))
                ;; Both arguments are non-negative.
                (dformat t "both args are non-negative~%")
                (setf result-low 0)
                (setf result-high (if (and high1 high2)
                                      (min high1 high2)
                                      (or high1 high2)))
;;                 (setf result-type (make-integer-type (list 'INTEGER result-low result-high)))
                )
               ((and low1 (>= low1 0))
                ;; arg1 is non-negative
                (dformat t "arg1 is non-negative~%")
                (setf result-low 0)
                (setf result-high high1)
;;                 (setf result-type (make-integer-type (list 'INTEGER 0 high1)))
                )
               ((and low2 (>= low2 0))
                ;; arg2 is non-negative
                (dformat t "arg2 is non-negative~%")
                (setf result-low 0)
                (setf result-high high2)
;;                 (setf result-type (make-integer-type (list 'INTEGER 0 high2)))
                ))
         (dformat t "result-low = ~S~%" result-low)
         (dformat t "result-high = ~S~%" result-high)
         (setf result-type (make-integer-type (list 'INTEGER result-low result-high)))
         (dformat t "result-type = ~S~%" result-type)
         result-type))
      (t
       (make-integer-type 'INTEGER)))))

(declaim (ftype (function (t) t) derive-type-lognot))
(defun derive-type-lognot (form)
  (let (arg-type)
    (if (and (= (length form) 2)
             (fixnum-type-p (setf arg-type (derive-compiler-type (%cadr form)))))
        (let* ((arg-low (integer-type-low arg-type))
               (arg-high (integer-type-high arg-type))
               (result-low (if arg-high (lognot arg-high) nil))
               (result-high (if arg-low (lognot arg-low) nil)))
          (make-integer-type (list 'INTEGER result-low result-high)))
        +integer-type+)))

;; mod number divisor
(declaim (ftype (function (t) t) derive-type-mod))
(defun derive-type-mod (form)
  (if (= (length form) 3)
      (let* ((arg1 (%cadr form))
             (arg2 (%caddr form))
             (type1 (derive-compiler-type arg1))
             (type2 (derive-compiler-type arg2)))
        (cond ((and (integer-type-p type1) (fixnum-type-p type2))
               'FIXNUM)
              (t
               t)))
      t))

(defknown derive-type-coerce (t) t)
(defun derive-type-coerce (form)
  (if (= (length form) 3)
      (let ((type-form (%caddr form)))
        (if (and (consp type-form) (eq (%car type-form) 'QUOTE) (= (length type-form) 2))
            (%cadr type-form)
            t))
      t))

(defknown derive-type-copy-seq (t) t)
(defun derive-type-copy-seq (form)
  (if (= (length form) 2)
      (let ((type (derive-compiler-type (second form))))
        (case type
          ((STRING SIMPLE-STRING)
           (make-compiler-type type))
          (t
           t)))
      t))

(defknown derive-type-integer-length (t) t)
(defun derive-type-integer-length (form)
  (when (= (length form) 2)
    (let ((type (make-integer-type (derive-type (%cadr form)))))
      (when type
        (let ((low (integer-type-low type))
              (high (integer-type-high type)))
          (when (and (integerp low) (integerp high))
            (return-from derive-type-integer-length
                         (list 'INTEGER 0
                               (max (integer-length low) (integer-length high)))))))))
  (list 'INTEGER 0 '*))

(defknown derive-type-%ldb (t) t)
(defun derive-type-%ldb (form)
  (when (= (length form) 4)
    (let* ((args (cdr form))
           (size-arg (first args)))
      (when (fixnump size-arg)
        (return-from derive-type-%ldb (list 'INTEGER 0 (1- (expt 2 size-arg)))))))
  (list 'INTEGER 0 '*))

(defknown derive-type-minus (t) t)
(defun derive-type-minus (form)
  (let ((args (cdr form))
        (result-type t))
    (case (length args)
      (1
       (let ((type1 (derive-compiler-type (%car args))))
         (when (integer-type-p type1)
           (let* ((low1 (integer-type-low type1))
                  (high1 (integer-type-high type1))
                  (low (and high1 (- high1)))
                  (high (and low1 (- low1))))
             (setf result-type (%make-integer-type low high))))))
      (2
       (let ((type1 (derive-compiler-type (%car args))))
         (when (integer-type-p type1)
           (let ((type2 (derive-compiler-type (%cadr args))))
             (when (integer-type-p type2)
               ;; Both integer types.
               (let* ((low1 (integer-type-low type1))
                      (high1 (integer-type-high type1))
                      (low2 (integer-type-low type2))
                      (high2 (integer-type-high type2))
                      (low (and low1 high2 (- low1 high2)))
                      (high (and high1 low2 (- high1 low2))))
                 (setf result-type (%make-integer-type low high)))))))))
    result-type))

(defknown derive-type-plus (t) t)
(defun derive-type-plus (form)
  (let ((args (cdr form))
        (result-type t))
    (when (= (length args) 2)
      (let ((type1 (derive-compiler-type (%car args))))
        (when (integer-type-p type1)
          (let ((type2 (derive-compiler-type (%cadr args))))
            (when (integer-type-p type2)
              ;; Both integer types.
              (let* ((low1 (integer-type-low type1))
                     (high1 (integer-type-high type1))
                     (low2 (integer-type-low type2))
                     (high2 (integer-type-high type2))
                     (low (and low1 low2 (+ low1 low2)))
                     (high (and high1 high2 (+ high1 high2))))
                (setf result-type (%make-integer-type low high))))))))
    result-type))

(defun derive-type-times (form)
  (let ((args (cdr form))
        (result-type t))
    (when (= (length args) 2)
      (let ((arg1 (%car args))
            (arg2 (%cadr args)))
        (when (and (integerp arg1) (integerp arg2))
          (let ((n (* arg1 arg2)))
            (return-from derive-type-times (%make-integer-type n n))))
      (let ((type1 (derive-compiler-type arg1)))
        (when (integer-type-p type1)
          (let ((type2 (derive-compiler-type arg2)))
            (when (integer-type-p type2)
              ;; Both integer types.
              (let ((low1 (integer-type-low type1))
                    (high1 (integer-type-high type1))
                    (low2 (integer-type-low type2))
                    (high2 (integer-type-high type2))
                    (low nil)
                    (high nil))
                (cond ((not (and low1 low2))
                       ;; Nothing to do.
                       )
                      ((or (minusp low1) (minusp low2))
                       (when (and high1 high2)
                         (let ((max (* (max (abs low1) (abs high1))
                                       (max (abs low2) (abs high2)))))
                           (setf low (- max)
                                 high max))))
                      (t
                       (setf low (* low1 low2))
                       (when (and high1 high2)
                         (setf high (* high1 high2)))))
                (setf result-type (%make-integer-type low high)))))))))
    result-type))

(declaim (ftype (function (t) t) derive-type-max))
(defun derive-type-max (form)
  (dolist (arg (cdr form) (make-compiler-type 'FIXNUM))
    (unless (fixnum-type-p (derive-compiler-type arg))
      (return t))))

(defknown derive-type-min (t) t)
(defun derive-type-min (form)
  (let ((args (cdr form))
        (result-type t))
    (when (= (length form) 3)
      (let* ((type1 (derive-compiler-type (%car args))))
        (when (integer-type-p type1)
          (let ((type2 (derive-compiler-type (%cadr args))))
            (when (integer-type-p type2)
              ;; Both integer types.
              (let ((low1 (integer-type-low type1))
                    (high1 (integer-type-high type1))
                    (low2 (integer-type-low type2))
                    (high2 (integer-type-high type2))
                    low high)
                (setf low (if (and low1 low2)
                              (min low1 low2)
                              nil)
                      high (if (and high1 high2)
                               (min high1 high2)
                               nil))
                (setf result-type (%make-integer-type low high))))))))
    result-type))

;; read-char &optional input-stream eof-error-p eof-value recursive-p => char
(declaim (ftype (function (t) t) derive-type-read-char))
(defun derive-type-read-char (form)
  (if (< (length form) 3) ; no EOF-ERROR-P arg
      'CHARACTER
      t))

;; ash integer count => shifted-integer
(defknown derive-type-ash (t) t)
(defun derive-type-ash (form)
  (let* ((args (cdr form))
         (arg1 (first args))
         (arg2 (second args))
         (type1 (derive-compiler-type arg1))
         (type2 (derive-compiler-type arg2))
         (result-type 'INTEGER))
    (when (and (integer-type-p type1) (integer-type-p type2))
      (let ((low1 (integer-type-low type1))
            (high1 (integer-type-high type1))
            (low2 (integer-type-low type2))
            (high2 (integer-type-high type2)))
        (when (and low1 high1 low2 high2)
          (cond ((fixnum-constant-value type2)
                 (setf arg2 (fixnum-constant-value type2))
                 (cond ((<= -64 arg2 64)
                        (setf result-type
                              (list 'INTEGER (ash low1 arg2) (ash high1 arg2))))
                       ((minusp arg2)
                        (setf result-type
                              (list 'INTEGER
                                    (if (minusp low1) -1 0)
                                    (if (minusp high1) -1 0))))))
                ((and (>= low1 0) (>= high1 0) (>= low2 0) (>= high2 0))
                 ;; Everything is non-negative.
                 (setf result-type (list 'INTEGER
                                         (ash low1 low2)
                                         (ash high1 high2))))
                ((and (>= low1 0) (>= high1 0) (<= low2 0) (<= high2 0))
                 ;; Negative (or zero) second argument.
                 (setf result-type (list 'INTEGER
                                         (ash low1 low2)
                                         (ash high1 high2))))))))
    (make-compiler-type result-type)))

(defknown derive-type (t) t)
(defun derive-type (form)
  (cond ((consp form)
         (let* ((op (%car form))
                (handler (and (symbolp op) (get op 'derive-type-handler))))
           (if handler
               (funcall handler form)
               (case op
                 (ASH
                  (derive-type-ash form))
                 ((CHAR SCHAR)
                  'CHARACTER)
                 (COERCE
                  (derive-type-coerce form))
                 (COPY-SEQ
                  (derive-type-copy-seq form))
                 (INTEGER-LENGTH
                  (derive-type-integer-length form))
                 (%LDB
                  (derive-type-%ldb form))
                 (LENGTH
                  '(INTEGER 0 #.(1- most-positive-fixnum)))
                 (LOGAND
                  (derive-type-logand form))
                 (LOGNOT
                  (derive-type-lognot form))
                 ((LOGIOR LOGXOR)
                  (derive-type-logior/logxor form))
                 (MOD
                  (derive-type-mod form))
                 (-
                  (derive-type-minus form))
                 (1-
                  (derive-type-minus (list '- (cadr form) 1)))
                 (+
                  (derive-type-plus form))
                 (1+
                  (derive-type-plus (list '+ (cadr form) 1)))
                 (*
                  (derive-type-times form))
                 (MAX
                  (derive-type-max form))
                 (MIN
                  (derive-type-min form))
                 (READ-CHAR
                  (derive-type-read-char form))
;;                  (SETQ
;;                   (if (= (length form) 3)
;;                       (derive-type (third form))
;;                       t))
                 ((THE TRULY-THE)
                  (second form))
                 (t
                  (let ((type (or (function-result-type op)
                                  (ftype-result-type (proclaimed-ftype op)))))
                    (if (eq type '*)
                        t
                        type)))))))
        ((null form)
         'NULL)
        ((integerp form)
         (list 'INTEGER form form))
        ((characterp form)
         'CHARACTER)
        ((stringp form)
         'STRING)
        ((arrayp form)
         (type-of form))
        ((variable-p form)
         (cond ((neq (variable-declared-type form) :none)
                (variable-declared-type form))
               ((neq (variable-derived-type form) :none)
                (variable-derived-type form))
               (t
                t)))
        ((var-ref-p form)
         (cond ((var-ref-constant-p form)
                (derive-type (var-ref-constant-value form)))
               (t
                (let ((variable (var-ref-variable form)))
                  (cond ((variable-special-p variable)
                         (or (proclaimed-type (variable-name variable))
                             t))
                        ((neq (variable-declared-type variable) :none)
                         (variable-declared-type variable))
                        ((neq (variable-derived-type variable) :none)
                         (variable-derived-type variable))
                        (t
                         t))))))
        ((symbolp form)
         (cond ((keywordp form)
                'SYMBOL)
               ((eq form t)
                t)
               ((and (special-variable-p form)
                     (constantp form))
                (derive-type (symbol-value form)))
               (t
                (let ((variable (find-visible-variable form)))
                  (if variable
                      (derive-type variable)
                      t)))))
        ((block-node-p form)
         (let ((result t))
           (cond ((equal (block-name form) '(LET))
                  ;;              (format t "derive-type LET/LET* node case~%")
                  (let* ((forms (cddr (block-form form)))
                         (last-form (car (last forms)))
                         (derived-type (derive-compiler-type last-form)))
                    ;;                (unless (eq derived-type t)
                    ;;                  (let ((*print-structure* nil))
                    ;;                    (format t "last-form = ~S~%" last-form))
                    ;;                  (format t "derived-type = ~S~%" derived-type)
                    ;;                  )
                    (setf result derived-type)))
                 ((symbolp (block-name form))
                  (unless (block-return-p form)
                    (let* ((forms (cddr (block-form form)))
                           (last-form (car (last forms)))
                           (derived-type (derive-compiler-type last-form)))
;;                       (unless (eq derived-type t)
;;                         (let ((*print-structure* nil))
;;                           (format t "last-form = ~S~%" last-form))
;;                         (format t "derived-type = ~S~%" derived-type)
;;                         )
                      (setf result derived-type)))))
           result))
        (t
         t)))

(defun derive-compiler-type (form)
  (make-compiler-type (derive-type form)))

;; delete item sequence &key from-end test test-not start end count key
(defknown p2-delete (t t t) t)
(defun p2-delete (form target representation)
  (unless (notinline-p 'delete)
    (when (= (length form) 3)
      ;; No keyword arguments.
      (let* ((args (cdr form))
             (arg1 (%car args))
             (arg2 (%cadr args))
             (type1 (derive-type arg1))
             (type2 (derive-type arg2))
             (test (if (memq type1 '(SYMBOL NULL)) 'eq 'eql)))
        (cond ((subtypep type2 'VECTOR)
               (compile-form arg1 'stack nil)
               (compile-form arg2 'stack nil)
               (emit 'checkcast +lisp-abstract-vector-class+)
               (maybe-emit-clear-values arg1 arg2)
               (emit 'swap)
               (emit-invokevirtual +lisp-abstract-vector-class+
                                   (if (eq test 'eq) "deleteEq" "deleteEql")
                                   (lisp-object-arg-types 1) +lisp-object+)
               (emit-move-from-stack target)
               (return-from p2-delete t))
              (t
               (setf (car form) (if (eq test 'eq) 'delete-eq 'delete-eql)))))))
  (compile-function-call form target representation))

(defun p2-length (form target representation)
  (unless (check-arg-count form 1)
    (compile-function-call form target representation)
    (return-from p2-length))
  (let ((arg (cadr form)))
    (compile-forms-and-maybe-emit-clear-values arg 'stack nil)
    (case representation
      (:int
       (emit-invokevirtual +lisp-object-class+ "length" nil "I"))
      (:long
       (emit-invokevirtual +lisp-object-class+ "length" nil "I")
       (emit 'i2l))
      (:boolean
       ;; FIXME We could optimize this all away in unsafe calls.
       (emit-invokevirtual +lisp-object-class+ "length" nil "I")
       (emit 'pop)
       (emit 'iconst_1))
      (:char
       (aver nil))
      (t
       (emit-invokevirtual +lisp-object-class+ "LENGTH" nil +lisp-object+)))
    (emit-move-from-stack target representation)))

(defun p2-list (form target representation)
  (let* ((args (cdr form))
         (len (length args)))
    (cond ((> len 9) ; list1() through list9() are defined in Lisp.java.
           (compile-function-call form target representation))
          (t
           (cond ((zerop len)
                  (emit-push-nil))
                 ((= len 1)
                  (emit 'new +lisp-cons-class+)
                  (emit 'dup)
                  (compile-form (first args) 'stack nil)
                  (emit-invokespecial-init +lisp-cons-class+ (lisp-object-arg-types 1)))
                 ((and (>= *speed* *space*)
                       (< len 4))
                  (emit 'new +lisp-cons-class+)
                  (emit 'dup)
                  (compile-form (first args) 'stack nil)
                  (emit 'new +lisp-cons-class+)
                  (emit 'dup)
                  (compile-form (second args) 'stack nil)
                  (when (= len 3)
                    (emit 'new +lisp-cons-class+)
                    (emit 'dup)
                    (compile-form (third args) 'stack nil))
                  (emit-invokespecial-init +lisp-cons-class+ (lisp-object-arg-types 1))
                  (emit-invokespecial-init +lisp-cons-class+ (lisp-object-arg-types 2))
                  (when (= len 3)
                    (emit-invokespecial-init +lisp-cons-class+ (lisp-object-arg-types 2))))
                 (t
                  (dolist (arg args)
                    (compile-form arg 'stack nil))
                  (let ((s (copy-seq "list ")))
                    (setf (schar s 4) (code-char (+ (char-code #\0) len)))
                    (emit-invokestatic +lisp-class+ s
                                       (make-list len :initial-element +lisp-object+)
                                       +lisp-cons+))))
           (unless (every 'single-valued-p args)
             (emit-clear-values))
           (emit-move-from-stack target)))))

(defun p2-list* (form target representation)
  (let* ((args (cdr form))
         (length (length args)))
    (cond ((= length 1)
	   (compile-forms-and-maybe-emit-clear-values (first args) 'stack nil)
           (emit-move-from-stack target representation))
          ((= length 2)
           (let ((arg1 (first args))
                 (arg2 (second args)))
             (emit 'new +lisp-cons-class+)
             (emit 'dup)
             (compile-form arg1 'stack nil)
             (compile-form arg2 'stack nil)
             (emit-invokespecial-init +lisp-cons-class+ (lisp-object-arg-types 2))
             (maybe-emit-clear-values arg1 arg2)
             (emit-move-from-stack target representation)))
          ((= length 3)
           (let ((arg1 (first args))
                 (arg2 (second args))
                 (arg3 (third args)))
             (emit 'new +lisp-cons-class+)
             (emit 'dup)
             (compile-form arg1 'stack nil)
             (emit 'new +lisp-cons-class+)
             (emit 'dup)
             (compile-form arg2 'stack nil)
             (compile-form arg3 'stack nil)
             (emit-invokespecial-init +lisp-cons-class+ (lisp-object-arg-types 2))
             (emit-invokespecial-init +lisp-cons-class+ (lisp-object-arg-types 2))
             (maybe-emit-clear-values arg1 arg2 arg3)
             (emit-move-from-stack target representation)))
          ((= length 4)
           (let ((arg1 (first args))
                 (arg2 (second args))
                 (arg3 (third args))
                 (arg4 (fourth args)))
             (emit 'new +lisp-cons-class+)
             (emit 'dup)
             (compile-form arg1 'stack nil)
             (emit 'new +lisp-cons-class+)
             (emit 'dup)
             (compile-form arg2 'stack nil)
             (emit 'new +lisp-cons-class+)
             (emit 'dup)
             (compile-form arg3 'stack nil)
             (compile-form arg4 'stack nil)
             (emit-invokespecial-init +lisp-cons-class+ (lisp-object-arg-types 2))
             (emit-invokespecial-init +lisp-cons-class+ (lisp-object-arg-types 2))
             (emit-invokespecial-init +lisp-cons-class+ (lisp-object-arg-types 2))
             (maybe-emit-clear-values arg1 arg2 arg3 arg4)
             (emit-move-from-stack target representation)))
          (t
           (compile-function-call form target representation)))))

(defun compile-nth (form target representation)
  (unless (check-arg-count form 2)
    (compile-function-call form target representation)
    (return-from compile-nth))
  (let ((index-form (second form))
        (list-form (third form)))
    (compile-forms-and-maybe-emit-clear-values index-form 'stack :int
					       list-form 'stack nil)
    (emit 'swap)
    (emit-invokevirtual +lisp-object-class+ "NTH" '("I") +lisp-object+)
    (fix-boxing representation nil) ; FIXME use derived result type
    (emit-move-from-stack target representation)))

(defun p2-times (form target representation)
  (case (length form)
    (3
     (let* ((args (cdr form))
            (arg1 (%car args))
            (arg2 (%cadr args))
            type1 type2 result-type value)
       (when (fixnump arg1)
         (rotatef arg1 arg2))
       (setf type1 (make-integer-type (derive-type arg1))
             type2 (make-integer-type (derive-type arg2))
             result-type (make-integer-type (derive-type form)))
       (cond ((and (numberp arg1) (numberp arg2))
              (dformat t "p2-times case 1~%")
              (compile-constant (* arg1 arg2) target representation))
             ((setf value (fixnum-constant-value result-type))
              (dformat t "p2-times case 1a~%")
              (compile-constant value target representation))
             ((and (fixnum-type-p type1)
                   (fixnum-type-p type2))
              (cond ((fixnum-type-p result-type)
                     (unless (eq representation :int)
                       (emit 'new +lisp-fixnum-class+)
                       (emit 'dup))
		     (compile-forms-and-maybe-emit-clear-values arg1 'stack :int
								arg2 'stack :int)
                     (emit 'imul)
                     (unless (eq representation :int)
                       (emit-invokespecial-init +lisp-fixnum-class+ '("I"))
                       (fix-boxing representation 'fixnum))
                     (emit-move-from-stack target representation))
                    (t
                     (compile-form arg1 'stack :int)
                     (emit 'i2l)
                     (compile-form arg2 'stack :int)
                     (emit 'i2l)
                     (maybe-emit-clear-values arg1 arg2)
                     (emit 'lmul)
                     (convert-long representation)
                     (emit-move-from-stack target representation))))
             ((and (java-long-type-p type1)
                   (java-long-type-p type2)
                   (java-long-type-p result-type))
	      (compile-forms-and-maybe-emit-clear-values arg1 'stack :long
							 arg2 'stack :long)
              (emit 'lmul)
              (convert-long representation)
              (emit-move-from-stack target representation))
             ((fixnump arg2)
;;               (format t "p2-times case 3~%")
	      (compile-forms-and-maybe-emit-clear-values arg1 'stack nil)
              (emit-push-int arg2)
              (emit-invokevirtual +lisp-object-class+ "multiplyBy" '("I") +lisp-object+)
;;               (when (eq representation :int)
;;                 (emit-unbox-fixnum))
              (fix-boxing representation result-type)
              (emit-move-from-stack target representation))
             (t
              (dformat t "p2-times case 4~%")
              (compile-binary-operation "multiplyBy" args target representation)))))
    (t
     (dformat t "p2-times case 5~%")
     (compile-function-call form target representation))))

(defknown p2-min/max (t t t) t)
(defun p2-min/max (form target representation)
  (cond ((= (length form) 3)
         (let* ((op (%car form))
                (args (%cdr form))
                (arg1 (%car args))
                (arg2 (%cadr args)))
           (when (null target)
	     (compile-forms-and-maybe-emit-clear-values arg1 nil nil
							arg2 nil nil)
             (return-from p2-min/max))
           (when (notinline-p op)
             (compile-function-call form target representation)
             (return-from p2-min/max))
           (let ((type1 (derive-compiler-type arg1))
                 (type2 (derive-compiler-type arg2)))
             (cond ((and (fixnum-type-p type1) (fixnum-type-p type2))
                    (let* ((*register* *register*)
                           (reg1 (allocate-register))
                           (reg2 (allocate-register)))
                      (when (null representation)
                        (emit 'new +lisp-fixnum-class+)
                        (emit 'dup))
                      (compile-form arg1 'stack :int)
                      (emit 'dup)
                      (emit 'istore reg1)
                      (compile-form arg2 'stack :int)
                      (emit 'dup)
                      (emit 'istore reg2)
                      (let ((LABEL1 (gensym))
                            (LABEL2 (gensym)))
                        (emit (if (eq op 'min) 'if_icmpge 'if_icmple) LABEL1)
                        (emit 'iload reg1)
                        (emit 'goto LABEL2)
                        (label LABEL1)
                        (emit 'iload reg2)
                        (label LABEL2)))
                    (case representation
                      (:int)
                      (:long
                       (emit 'i2l))
                      (t
                       (emit-invokespecial-init +lisp-fixnum-class+ '("I"))))
                    (emit-move-from-stack target representation))
                   ((and (java-long-type-p type1) (java-long-type-p type2))
                    (let* ((*register* *register*)
                           (reg1 (allocate-register-pair))
                           (reg2 (allocate-register-pair)))
                      (compile-form arg1 'stack :long)
                      (emit 'dup2)
                      (emit 'lstore reg1)
                      (compile-form arg2 'stack :long)
                      (emit 'dup2)
                      (emit 'lstore reg2)
                      (emit 'lcmp)
                      (let ((LABEL1 (gensym))
                            (LABEL2 (gensym)))
                        (emit (if (eq op 'min) 'ifge 'ifle) LABEL1)
                        (emit 'lload reg1)
                        (emit 'goto LABEL2)
                        (label LABEL1)
                        (emit 'lload reg2)
                        (label LABEL2)))
                    (convert-long representation)
                    (emit-move-from-stack target representation))
                   (t
                    (let* ((*register* *register*)
                           (reg1 (allocate-register))
                           (reg2 (allocate-register)))
                      (compile-form arg1 'stack nil)
                      (emit 'dup)
                      (emit 'astore reg1)
                      (compile-form arg2 'stack nil)
                      (emit 'dup)
                      (emit 'astore reg2)
                      (emit-invokevirtual +lisp-object-class+
                                          (if (eq op 'min)
                                              "isLessThanOrEqualTo"
                                              "isGreaterThanOrEqualTo")
                                          (lisp-object-arg-types 1) "Z")
                      (let ((LABEL1 (gensym))
                            (LABEL2 (gensym)))
                        (emit 'ifeq LABEL1)
                        (emit 'aload reg1)
                        (emit 'goto LABEL2)
                        (label LABEL1)
                        (emit 'aload reg2)
                        (label LABEL2)))
                    (fix-boxing representation nil)
                    (emit-move-from-stack target representation))))))
         (t
          (compile-function-call form target representation))))

(defun p2-plus (form target representation)
  (case (length form)
    (3
     (let* ((args (%cdr form))
            (arg1 (%car args))
            (arg2 (%cadr args))
            (type1 (derive-compiler-type arg1))
            (type2 (derive-compiler-type arg2))
            (result-type (derive-compiler-type form)))
;;        (let ((*print-structure* nil))
;;          (format t "~&p2-plus arg1 = ~S~%" arg1)
;;          (format t "p2-plus arg2 = ~S~%" arg2))
;;        (format t "~&p2-plus type1 = ~S~%" type1)
;;        (format t "p2-plus type2 = ~S~%" type2)
;;        (format t "p2-plus result-type = ~S~%" result-type)
;;        (format t "p2-plus representation = ~S~%" representation)
       (cond ((and (numberp arg1) (numberp arg2))
              (compile-constant (+ arg1 arg2) target representation))
             ((and (numberp arg1) (eql arg1 0))
	      (compile-forms-and-maybe-emit-clear-values arg1 nil nil
							 arg2 'stack representation)
              (emit-move-from-stack target representation))
             ((and (numberp arg2) (eql arg2 0))
	      (compile-forms-and-maybe-emit-clear-values arg1 'stack representation
							 arg2 nil nil)
              (emit-move-from-stack target representation))
             ((and (fixnum-type-p type1) (fixnum-type-p type2))
              (cond ((or (eq representation :int)
                         (fixnum-type-p result-type))
                     (when (null representation)
                       (emit 'new +lisp-fixnum-class+)
                       (emit 'dup))
		     (compile-forms-and-maybe-emit-clear-values arg1 'stack :int
								arg2 'stack :int)
                     (emit 'iadd)
                     (case representation
                       (:int)
                       (:long
                        (emit 'i2l))
                       (t
                        (emit-invokespecial-init +lisp-fixnum-class+ '("I")))))
                    (t
                     (compile-form arg1 'stack :int)
                     (emit 'i2l)
                     (compile-form arg2 'stack :int)
                     (emit 'i2l)
                     (maybe-emit-clear-values arg1 arg2)
                     (emit 'ladd)
                     (convert-long representation)))
              (emit-move-from-stack target representation))
             ((and (java-long-type-p type1)
                   (java-long-type-p type2)
                   (java-long-type-p result-type))
              (cond ((fixnum-type-p type1)
                     (compile-form arg1 'stack :int)
                     (emit 'i2l))
                    (t
                     (compile-form arg1 'stack :long)))
              (cond ((fixnum-type-p type2)
                     (compile-form arg2 'stack :int)
                     (emit 'i2l))
                    (t
                     (compile-form arg2 'stack :long)))
              (maybe-emit-clear-values arg1 arg2)
              (emit 'ladd)
              (convert-long representation)
              (emit-move-from-stack target representation))
             ((eql arg2 1)
	      (compile-forms-and-maybe-emit-clear-values arg1 'stack nil)
              (emit-invoke-method "incr" target representation))
             ((eql arg1 1)
	      (compile-forms-and-maybe-emit-clear-values arg2 'stack nil)
              (emit-invoke-method "incr" target representation))
             ((fixnum-type-p type1)
	      (compile-forms-and-maybe-emit-clear-values arg1 'stack :int
							 arg2 'stack nil)
              (emit 'swap)
              (emit-invokevirtual +lisp-object-class+ "add" '("I") +lisp-object+)
              (fix-boxing representation result-type)
              (emit-move-from-stack target representation))
             ((fixnum-type-p type2)
              (compile-form arg1 'stack nil)
              (maybe-emit-clear-values arg1 arg2)
              (compile-form arg2 'stack :int)
              (emit-invokevirtual +lisp-object-class+ "add" '("I") +lisp-object+)
              (fix-boxing representation result-type)
              (emit-move-from-stack target representation))
             (t
              (compile-binary-operation "add" args target representation)))))
    (4
     ;; (+ a b c) => (+ (+ a b) c)
     (let ((new-form `(+ (+ ,(second form) ,(third form)) ,(fourth form))))
       (p2-plus new-form target representation)))
    (t
     (compile-function-call form target representation))))

(defun p2-minus (form target representation)
  (case (length form)
    (2
     (let* ((arg (%cadr form))
            (type (derive-compiler-type arg)))
       (cond ((eql (fixnum-constant-value type) 0)
              (case representation
                (:int
                 (emit 'iconst_0))
                (:long
                 (emit 'lconst_0))
                (t
                 (emit 'getstatic +lisp-fixnum-class+ "ZERO" +lisp-fixnum+)))
              (emit-move-from-stack target representation))
             ((and (fixnum-type-p type)
                   (integer-type-low type)
                   (> (integer-type-low type) most-negative-fixnum))
              (when (null representation)
                (emit 'new +lisp-fixnum-class+)
                (emit 'dup))
              (compile-form arg 'stack :int)
              (emit 'ineg)
              (case representation
                (:int)
                (:long
                 (emit 'i2l))
                (t
                 (emit-invokespecial-init +lisp-fixnum-class+ '("I"))))
              (emit-move-from-stack target representation))
             ((and (java-long-type-p type)
                   (integer-type-low type)
                   (> (integer-type-low type) most-negative-java-long))
              (compile-form arg 'stack :long)
              (emit 'lneg)
              (case representation
                (:int
                 (emit 'l2i))
                (:long)
                (t
                 (emit-box-long)))
              (emit-move-from-stack target representation))
             (t
	      (compile-forms-and-maybe-emit-clear-values arg 'stack nil)
              (emit-invokevirtual +lisp-object-class+ "negate"
                                  nil +lisp-object+)
              (fix-boxing representation nil)
              (emit-move-from-stack target representation)))))
    (3
     (let* ((args (cdr form))
            (arg1 (first args))
            (arg2 (second args))
            (type1 (derive-compiler-type arg1))
            (type2 (derive-compiler-type arg2))
            (result-type (derive-compiler-type form)))
       (cond ((and (numberp arg1) (numberp arg2))
              (compile-constant (- arg1 arg2) target representation))
             ((and (fixnum-type-p type1) (fixnum-type-p type2))
              (cond ((or (eq representation :int)
                         (fixnum-type-p result-type))
                     (when (null representation)
                       (emit 'new +lisp-fixnum-class+)
                       (emit 'dup))
		     (compile-forms-and-maybe-emit-clear-values arg1 'stack :int
								arg2 'stack :int)
                     (emit 'isub)
                     (case representation
                       (:int)
                       (:long
                        (emit 'i2l))
                       (t
                        (emit-invokespecial-init +lisp-fixnum-class+ '("I")))))
                    (t
                     (compile-form arg1 'stack :int)
                     (emit 'i2l)
                     (compile-form arg2 'stack :int)
                     (emit 'i2l)
                     (maybe-emit-clear-values arg1 arg2)
                     (emit 'lsub)
                     (convert-long representation)))
              (emit-move-from-stack target representation))
             ((and (java-long-type-p type1) (java-long-type-p type2)
                   (java-long-type-p result-type))
	      (compile-forms-and-maybe-emit-clear-values arg1 'stack :long
							 arg2 'stack :long)
              (emit 'lsub)
              (convert-long representation)
              (emit-move-from-stack target representation))
             ((fixnum-type-p type2)
	      (compile-forms-and-maybe-emit-clear-values arg1 'stack nil
							 arg2 'stack :int)
              (emit-invokevirtual +lisp-object-class+ "subtract" '("I") +lisp-object+)
              (fix-boxing representation result-type)
              (emit-move-from-stack target representation))
             (t
              (compile-binary-operation "subtract" args target representation)))))
    (4
     ;; (- a b c) => (- (- a b) c)
     (let ((new-form `(- (- ,(second form) ,(third form)) ,(fourth form))))
       (p2-minus new-form target representation)))
    (t
     (compile-function-call form target representation))))

;; char/schar string index => character
(defknown p2-char/schar (t t t) t)
(defun p2-char/schar (form target representation)
  (unless (check-arg-count form 2)
    (compile-function-call form target representation)
    (return-from p2-char/schar))
  (let* ((op (%car form))
         (args (%cdr form))
         (arg1 (%car args))
         (arg2 (%cadr args))
         (type1 (derive-compiler-type arg1))
         (type2 (derive-compiler-type arg2)))
    (cond ((and (eq representation :char)
                (zerop *safety*))
           (compile-form arg1 'stack nil)
           (emit 'checkcast +lisp-abstract-string-class+)
           (compile-form arg2 'stack :int)
           (maybe-emit-clear-values arg1 arg2)
           (emit-invokevirtual +lisp-abstract-string-class+ "charAt"
                               '("I") "C")
           (emit-move-from-stack target representation))
          ((and (eq representation :char)
                (or (eq op 'CHAR) (< *safety* 3))
                (compiler-subtypep type1 'STRING)
                (fixnum-type-p type2))
           (compile-form arg1 'stack nil)
           (emit 'checkcast +lisp-abstract-string-class+)
           (compile-form arg2 'stack :int)
           (maybe-emit-clear-values arg1 arg2)
           (emit-invokevirtual +lisp-abstract-string-class+ "charAt"
                               '("I") "C")
           (emit-move-from-stack target representation))
          ((fixnum-type-p type2)
	   (compile-forms-and-maybe-emit-clear-values arg1 'stack nil
						      arg2 'stack :int)
           (emit-invokevirtual +lisp-object-class+
                               (symbol-name op) ;; "CHAR" or "SCHAR"
                               '("I") +lisp-object+)
           (when (eq representation :char)
             (emit-unbox-character))
           (emit-move-from-stack target representation))
          (t
           (compile-function-call form target representation)))))

;; set-char/schar string index character => character
(defknown p2-set-char/schar (t t t) t)
(defun p2-set-char/schar (form target representation)
;;   (format t "p2-set-char/schar~%")
  (unless (check-arg-count form 3)
    (compile-function-call form target representation)
    (return-from p2-set-char/schar))
  (let* ((op (%car form))
         (args (%cdr form))
         (arg1 (first args))
         (arg2 (second args))
         (arg3 (third args))
         (type1 (derive-compiler-type arg1))
         (type2 (derive-compiler-type arg2))
         (type3 (derive-compiler-type arg3)))
;;     (format t "p2-set-char/schar type1 = ~S~%" type1)
;;     (format t "p2-set-char/schar type2 = ~S~%" type2)
;;     (format t "p2-set-char/schar type3 = ~S~%" type3)
    (cond ((and (< *safety* 3)
                (or (null representation) (eq representation :char))
                (compiler-subtypep type1 'STRING)
                (fixnum-type-p type2)
                (compiler-subtypep type3 'CHARACTER))
           (let* ((*register* *register*)
                  (value-register (when target (allocate-register)))
                  (class (if (eq op 'SCHAR)
                             +lisp-simple-string-class+
                             +lisp-abstract-string-class+)))
             (compile-form arg1 'stack nil)
             (emit 'checkcast class)
             (compile-form arg2 'stack :int)
             (compile-form arg3 'stack :char)
             (when target
               (emit 'dup)
               (emit-move-from-stack value-register :char))
             (maybe-emit-clear-values arg1 arg2 arg3)
             (emit-invokevirtual class "setCharAt" '("I" "C") nil)
             (when target
               (when (null representation)
                 (emit 'new +lisp-fixnum-class+)
                 (emit 'dup))
               (emit 'iload value-register)
               (case representation
                 (:char)
                 (t
                  (emit-invokespecial-init +lisp-fixnum-class+ '("I"))))
               (emit-move-from-stack target representation))))
          (t
;;            (format t "p2-set-char/schar not optimized~%")
           (compile-function-call form target representation)))))


(defun p2-svref (form target representation)
  (cond ((and (check-arg-count form 2)
              (neq representation :char)) ; FIXME
         (let ((arg1 (%cadr form))
               (arg2 (%caddr form)))
	   (compile-forms-and-maybe-emit-clear-values arg1 'stack nil
						      arg2 'stack :int)
           (emit-invokevirtual +lisp-object-class+ "SVREF" '("I") +lisp-object+)
           (fix-boxing representation nil)
           (emit-move-from-stack target representation)))
        (t
         (compile-function-call form target representation))))

(defun p2-svset (form target representation)
  (cond ((check-arg-count form 3)
         (let* ((arg1 (%cadr form))
                (arg2 (%caddr form))
                (arg3 (fourth form))
                (*register* *register*)
                (value-register (when target (allocate-register))))
           (compile-form arg1 'stack nil) ;; vector
           (compile-form arg2 'stack :int) ;; index
           (compile-form arg3 'stack nil) ;; new value
           (when value-register
             (emit 'dup)
             (emit-move-from-stack value-register nil))
           (maybe-emit-clear-values arg1 arg2 arg3)
           (emit-invokevirtual +lisp-object-class+ "svset" (list "I" +lisp-object+) nil)
           (when value-register
             (emit 'aload value-register)
             (emit-move-from-stack target nil))))
        (t
         (compile-function-call form target representation))))

(defun p2-truncate (form target representation)
  (let ((args (cdr form))
        arg1
        arg2)
    (case (length args)
      (1
       (setf arg1 (%car args)
             arg2 1))
      (2
       (setf arg1 (%car args)
             arg2 (%cadr args)))
      (t
       (compiler-warn "Wrong number of arguments for ~A (expected 1 or 2, but received ~D)."
                      'truncate (length args))
       (compile-function-call form target representation)
       (return-from p2-truncate)))
    (compile-form arg1 'stack nil)
    (compile-form arg2 'stack nil)
    (emit-invokevirtual +lisp-object-class+ "truncate" (lisp-object-arg-types 1) +lisp-object+)
;;     (when (eq representation :int)
;;       (emit-unbox-fixnum))
    (fix-boxing representation nil) ; FIXME use derived result type
    (emit-move-from-stack target representation)))

(defun p2-elt (form target representation)
  (cond ((and (check-arg-count form 2)
              (fixnum-type-p (derive-compiler-type (third form)))
              (neq representation :char)) ; FIXME
         (compile-form (second form) 'stack nil)
         (compile-form (third form) 'stack :int)
         (emit-invokevirtual +lisp-object-class+ "elt" '("I") +lisp-object+)
;;          (when (eq representation :int)
;;            (emit-unbox-fixnum))
         (fix-boxing representation nil) ; FIXME use derived result type
         (emit-move-from-stack target representation))
        (t
         (compile-function-call form target representation))))

(defun p2-aref (form target representation)
  ;; We only optimize the 2-arg case.
  (case (length form)
    (3
     (let* ((arg1 (%cadr form))
            (arg2 (%caddr form))
            (type1 (derive-compiler-type arg1)))
       (case representation
         (:int
	  (compile-forms-and-maybe-emit-clear-values arg1 'stack nil
						     arg2 'stack :int)
          (emit-invokevirtual +lisp-object-class+ "aref" '("I") "I"))
         (:long
	  (compile-forms-and-maybe-emit-clear-values arg1 'stack nil
						     arg2 'stack :int)
          (emit-invokevirtual +lisp-object-class+ "aref_long" '("I") "J"))
         (:char
          (cond ((compiler-subtypep type1 'string)
                 (compile-form arg1 'stack nil) ; array
                 (emit 'checkcast +lisp-abstract-string-class+)
                 (compile-form arg2 'stack :int) ; index
                 (maybe-emit-clear-values arg1 arg2)
                 (emit-invokevirtual +lisp-abstract-string-class+
                                     "charAt" '("I") "C"))
                (t
		 (compile-forms-and-maybe-emit-clear-values arg1 'stack nil
							    arg2 'stack :int)
                 (emit-invokevirtual +lisp-object-class+ "AREF" '("I") +lisp-object+)
                 (emit-unbox-character))))
         (t
	  (compile-forms-and-maybe-emit-clear-values arg1 'stack nil
						     arg2 'stack :int)
          (emit-invokevirtual +lisp-object-class+ "AREF" '("I") +lisp-object+)
          (fix-boxing representation nil)))
       (emit-move-from-stack target representation)))
    (t
     (compile-function-call form target representation))))

(defun p2-aset (form target representation)
  ;; We only optimize the 3-arg case.
  (cond ((= (length form) 4)
         (let* ((args (cdr form))
                (arg1 (first args))
                (arg2 (second args))
                (arg3 (third args))
                (type3 (derive-compiler-type arg3))
                (*register* *register*)
                (value-register (unless (null target) (allocate-register)))
;;                 (array-derived-type t)
                )

;;            (format t "p2-aset type3 = ~S~%" type3)

;;            (when (symbolp arg1)
;;              (let ((variable (find-visible-variable (second form))))
;;                (when variable
;;                  (setf array-derived-type (derive-type variable)))))
           ;; array
           (compile-form arg1 'stack nil)
           ;; index
           (compile-form arg2 'stack :int)
           ;; value
;;            (cond ((subtypep array-derived-type '(array (unsigned-byte 8)))
;;                   (compile-form (fourth form) 'stack :int)
;;                   (when value-register
;;                     (emit 'dup)
;;                     (emit-move-from-stack value-register :int)))
;;                  (t
;;                   (compile-form (fourth form) 'stack nil)
;;                   (when value-register
;;                     (emit 'dup)
;;                     (emit-move-from-stack value-register nil))))
           (cond ((fixnum-type-p type3)
                  (compile-form arg3 'stack :int)
                  (when value-register
                    (emit 'dup)
                    (emit-move-from-stack value-register :int)))
                 (t
                  (compile-form arg3 'stack nil)
                  (when value-register
                    (emit 'dup)
                    (emit-move-from-stack value-register nil))))

;;            (unless (and (single-valued-p (second form))
;;                         (single-valued-p (third form))
;;                         (single-valued-p (fourth form)))
;;              (emit-clear-values))
           (maybe-emit-clear-values arg1 arg2 arg3)

           (cond (;;(subtypep array-derived-type '(array (unsigned-byte 8)))
                  (fixnum-type-p type3)
                  (emit-invokevirtual +lisp-object-class+ "aset" '("I" "I") nil))
                 (t
                  (emit-invokevirtual +lisp-object-class+ "aset" (list "I" +lisp-object+) nil)))
           (when value-register
             (cond ((fixnum-type-p type3)
                    (when (null representation)
                      (emit 'new +lisp-fixnum-class+)
                      (emit 'dup))
                    (emit 'iload value-register)
                    (case representation
                      (:int)
                      (:long
                       (emit 'i2l))
                      (t
                       (emit-invokespecial-init +lisp-fixnum-class+ '("I")))))
                   (t
                    (emit 'aload value-register)
                    (fix-boxing representation type3)))
             (emit-move-from-stack target representation))))
        (t
         (compile-function-call form target representation))))

(defknown p2-structure-ref (t t t) t)
(defun p2-structure-ref (form target representation)
  (unless (check-arg-count form 2)
    (compile-function-call form target representation)
    (return-from p2-structure-ref))
  (let* ((args (cdr form))
         (arg1 (first args))
         (arg2 (second args)))
    (cond ((and (fixnump arg2)
                (null representation))
	   (compile-forms-and-maybe-emit-clear-values arg1 'stack nil)
           (case arg2
             (0
              (emit-invokevirtual +lisp-object-class+ "getSlotValue_0"
                                  nil +lisp-object+))
             (1
              (emit-invokevirtual +lisp-object-class+ "getSlotValue_1"
                                  nil +lisp-object+))
             (2
              (emit-invokevirtual +lisp-object-class+ "getSlotValue_2"
                                  nil +lisp-object+))
             (3
              (emit-invokevirtual +lisp-object-class+ "getSlotValue_3"
                                  nil +lisp-object+))
             (t
              (emit-push-constant-int arg2)
              (emit-invokevirtual +lisp-object-class+ "getSlotValue"
                                  '("I") +lisp-object+)))
           (emit-move-from-stack target representation))
          ((fixnump arg2)
	   (compile-forms-and-maybe-emit-clear-values arg1 'stack nil)
           (emit-push-constant-int arg2)
           (case representation
             (:int
              (emit-invokevirtual +lisp-object-class+ "getFixnumSlotValue"
                                  '("I") "I"))
             (:long
              (emit-invokevirtual +lisp-object-class+ "getSlotValue"
                                  '("I") +lisp-object+)
              (emit-invokevirtual +lisp-object-class+ "longValue"
                                  nil "J"))
             (:char
              (emit-invokevirtual +lisp-object-class+ "getSlotValue"
                                  '("I") +lisp-object+)
              (emit-unbox-character))
             (:boolean
              (emit-invokevirtual +lisp-object-class+ "getSlotValueAsBoolean"
                                  '("I") "Z"))
             (t
              (emit-invokevirtual +lisp-object-class+ "getSlotValue"
                                  '("I") +lisp-object+)))
           (emit-move-from-stack target representation))
          (t
           (compile-function-call form target representation)))))

(defknown p2-structure-set (t t t) t)
(defun p2-structure-set (form target representation)
  (unless (check-arg-count form 3)
    (compile-function-call form target representation)
    (return-from p2-structure-set))
  (let* ((args (cdr form))
         (arg1 (first args))
         (arg2 (second args))
         (arg3 (third args)))
   (cond ((and (fixnump arg2)
               (<= 0 arg2 3))
          (let* ((*register* *register*)
                 (value-register (when target (allocate-register))))
	   (compile-forms-and-maybe-emit-clear-values arg1 'stack nil
						      arg3 'stack nil)
            (when value-register
              (emit 'dup)
              (emit 'astore value-register))
            (emit-invokevirtual +lisp-object-class+
                                (format nil "setSlotValue_~D" arg2)
                                (lisp-object-arg-types 1) nil)
            (when value-register
              (emit 'aload value-register)
              (fix-boxing representation nil)
              (emit-move-from-stack target representation))))
         ((fixnump arg2)
          (let* ((*register* *register*)
                 (value-register (when target (allocate-register))))
            (compile-form arg1 'stack nil)
            (emit-push-constant-int arg2)
            (compile-form arg3 'stack nil)
            (maybe-emit-clear-values arg1 arg3)
            (when value-register
              (emit 'dup)
              (emit 'astore value-register))
            (emit-invokevirtual +lisp-object-class+ "setSlotValue"
                                (list "I" +lisp-object+) nil)
            (when value-register
              (emit 'aload value-register)
              (fix-boxing representation nil)
              (emit-move-from-stack target representation))))
         (t
          (compile-function-call form target representation)))))


(defun p2-not/null (form target representation)
  (aver (or (null representation) (eq representation :boolean)))
  (unless (check-arg-count form 1)
    (compile-function-call form target representation)
    (return-from p2-not/null))
  (let ((arg (second form)))
    (cond ((null arg)
           (emit-push-true representation))
          ((node-constant-p arg)
           (emit-push-false representation))
          ((and (consp arg)
                (memq (%car arg) '(NOT NULL)))
	   (compile-forms-and-maybe-emit-clear-values (second arg) 'stack nil)
           (emit-push-nil)
           (let ((LABEL1 (gensym))
                 (LABEL2 (gensym)))
             (emit 'if_acmpeq LABEL1)
             (emit-push-true representation)
             (emit 'goto LABEL2)
             (emit 'label LABEL1)
             (emit-push-false representation)
             (emit 'label LABEL2)))
          ((eq representation :boolean)
	   (compile-forms-and-maybe-emit-clear-values arg 'stack :boolean)
           (emit 'iconst_1)
           (emit 'ixor))
          ((eq (derive-compiler-type arg) 'BOOLEAN)
	   (compile-forms-and-maybe-emit-clear-values arg 'stack :boolean)
           (let ((LABEL1 (gensym))
                 (LABEL2 (gensym)))
             (emit 'ifeq LABEL1)
             (emit-push-nil)
             (emit 'goto LABEL2)
             (emit 'label LABEL1)
             (emit-push-t)
             (emit 'label LABEL2)))
          (t
	   (compile-forms-and-maybe-emit-clear-values arg 'stack nil)
           (let ((LABEL1 (gensym))
                 (LABEL2 (gensym)))
             (emit-push-nil)
             (emit 'if_acmpeq LABEL1)
             (emit-push-nil)
             (emit 'goto LABEL2)
             (emit 'label LABEL1)
             (emit-push-t)
             (emit 'label LABEL2)))))
  (emit-move-from-stack target representation))

(defun p2-nthcdr (form target representation)
  (unless (check-arg-count form 2)
    (compile-function-call form target representation)
    (return-from p2-nthcdr))
  (let* ((args (%cdr form))
         (arg1 (%car args))
         (arg2 (%cadr args)))
    (cond ((fixnum-type-p (derive-compiler-type arg1))
	   (compile-forms-and-maybe-emit-clear-values arg1 'stack :int
						      arg2 'stack nil)
           (emit 'swap)
           (emit-invokevirtual +lisp-object-class+ "nthcdr" '("I") +lisp-object+)
           (fix-boxing representation nil)
           (emit-move-from-stack target representation))
          (t
           (compile-function-call form target representation)))))

(defun p2-and (form target representation)
  (aver (or (null representation) (eq representation :boolean)))
  (let ((args (cdr form)))
    (case (length args)
      (0
       (emit-push-true representation)
       (emit-move-from-stack target representation))
      (1
       (compile-form (%car args) target representation))
      (2
       (let ((arg1 (%car args))
             (arg2 (%cadr args))
             (FAIL (gensym))
             (DONE (gensym)))
	 (compile-forms-and-maybe-emit-clear-values arg1 'stack :boolean)
         (emit 'ifeq FAIL)
         (case representation
           (:boolean
	    (compile-forms-and-maybe-emit-clear-values arg2 'stack :boolean)
            (emit 'goto DONE)
            (label FAIL)
            (emit 'iconst_0))
           (t
            (compile-form arg2 'stack nil)
            (emit 'goto DONE)
            (label FAIL)
            (emit-push-nil)))
         (label DONE)
         (emit-move-from-stack target representation)))
      (t
       ;; (and a b c d e f) => (and a (and b c d e f))
       (let ((new-form `(and ,(%car args) (and ,@(%cdr args)))))
         (p2-and new-form target representation))))))

(defknown p2-or (t t t) t)
(defun p2-or (form target representation)
  (let ((args (cdr form)))
    (case (length args)
      (0
       (emit-push-false representation)
       (emit-move-from-stack target representation))
      (1
       (compile-form (%car args) target representation))
      (2
       (let ((arg1 (%car args))
             (arg2 (%cadr args))
             (LABEL1 (gensym))
             (LABEL2 (gensym)))
	 (compile-forms-and-maybe-emit-clear-values arg1 'stack nil)
         (emit 'dup)
         (emit-push-nil)
         (emit 'if_acmpne LABEL1)
         (emit 'pop)
         (compile-form arg2 'stack representation)
         (emit 'goto LABEL2)
         (emit 'label LABEL1)
         (fix-boxing representation nil) ; FIXME use derived result type
         (emit 'label LABEL2)
         (emit-move-from-stack target representation)))
      (t
       ;; (or a b c d e f) => (or a (or b c d e f))
       (let ((new-form `(or ,(%car args) (or ,@(%cdr args)))))
         (p2-or new-form target representation))))))

(defun p2-values (form target representation)
  (let* ((args (cdr form))
         (len (length args)))
    (case len
      (0
       (emit-push-current-thread)
       (emit-invokevirtual +lisp-thread-class+ "setValues" nil +lisp-object+)
       (emit-move-from-stack target))
      (1
       (let ((arg (%car args)))
	 (compile-forms-and-maybe-emit-clear-values arg target representation)))
      (2
       (emit-push-current-thread)
       (let ((arg1 (%car args))
             (arg2 (%cadr args)))
         (cond ((and (eq arg1 t)
                     (eq arg2 t))
                (emit-push-t)
                (emit 'dup))
               ((and (eq arg1 nil)
                     (eq arg2 nil))
                (emit-push-nil)
                (emit 'dup))
               (t
                (compile-form arg1 'stack nil)
                (compile-form arg2 'stack nil))))
       (emit-invokevirtual +lisp-thread-class+
                           "setValues"
                           (lisp-object-arg-types len)
                           +lisp-object+)
       (fix-boxing representation nil)
       (emit-move-from-stack target))
      ((3 4)
       (emit-push-current-thread)
       (dolist (arg args)
         (compile-form arg 'stack nil))
       (emit-invokevirtual +lisp-thread-class+
                           "setValues"
                           (lisp-object-arg-types len)
                           +lisp-object+)
       (fix-boxing representation nil)
       (emit-move-from-stack target))
      (t
       (compile-function-call form target representation)))))

(defun compile-special-reference (name target representation)
  (when (constantp name)
    (let ((value (symbol-value name)))
      (when (or (null *compile-file-truename*)
                (stringp value)
                (numberp value)
                (packagep value))
        (compile-constant value target representation)
        (return-from compile-special-reference))))
  (emit 'getstatic *this-class* (declare-symbol name) +lisp-symbol+)
  (cond ((constantp name)
         ;; "... a reference to a symbol declared with DEFCONSTANT always
         ;; refers to its global value."
         (emit-invokevirtual +lisp-symbol-class+ "getSymbolValue"
                             nil +lisp-object+))
        (t
         (emit-push-current-thread)
         (emit-invokevirtual +lisp-symbol-class+ "symbolValue"
                             (list +lisp-thread+) +lisp-object+)))
;;   (when (eq representation :int)
;;     (emit-unbox-fixnum))
  (fix-boxing representation nil)
  (emit-move-from-stack target representation))

(defknown compile-var-ref (t t t) t)
(defun compile-var-ref (ref target representation)
  (when target
    (if (var-ref-constant-p ref)
        (compile-constant (var-ref-constant-value ref) target representation)
        (let ((variable (var-ref-variable ref)))
          (cond ((variable-special-p variable)
                 (compile-special-reference (variable-name variable) target representation))
                ((eq (variable-representation variable) :int)
                 (aver (variable-register variable))
                 (case representation
                   (:int
                    (emit 'iload (variable-register variable)))
                   (:char
                    (aver nil))
                   (:long
                    (emit 'iload (variable-register variable))
                    (emit 'i2l))
                   (:boolean
                    (emit 'iconst_1))
                   (t
                    (emit 'new +lisp-fixnum-class+)
                    (emit 'dup)
                    (emit 'iload (variable-register variable))
                    (emit-invokespecial-init +lisp-fixnum-class+ '("I"))))
                 (emit-move-from-stack target representation))
                ((eq (variable-representation variable) :char)
                 (case representation
                   (:char
                    (aver (variable-register variable))
                    (emit 'iload (variable-register variable)))
                   (:boolean
                    (emit 'iconst_1))
                   (t
                    (emit 'new +lisp-character-class+)
                    (emit 'dup)
                    (aver (variable-register variable))
                    (emit 'iload (variable-register variable))
                    (emit-invokespecial-init +lisp-character-class+ '("C"))))
                 (emit-move-from-stack target representation))
                ((eq (variable-representation variable) :long)
                 (aver (variable-register variable))
                 (case representation
                   (:int
                    (emit 'lload (variable-register variable))
                    (emit 'l2i))
                   (:char
                    (aver nil))
                   (:long
                    (emit 'lload (variable-register variable)))
                   (:boolean
                    (emit 'iconst_1))
                   (t
                    (emit 'lload (variable-register variable))
                    (emit-box-long)))
                 (emit-move-from-stack target representation))
                ((eq (variable-representation variable) :boolean)
                 (aver (variable-register variable))
                 (aver (or (null representation) (eq representation :boolean)))
                 (emit 'iload (variable-register variable))
                 (case representation
                   (:boolean)
                   (t
                    (emit-box-boolean)))
                 (emit-move-from-stack target representation))
                ((variable-register variable)
                 (emit 'aload (variable-register variable))
                 (fix-boxing representation (variable-derived-type variable))
                 (emit-move-from-stack target representation))
                ((variable-closure-index variable)
                 (aver (not (null (compiland-closure-register *current-compiland*))))
                 (emit 'aload (compiland-closure-register *current-compiland*))
                 (emit-push-constant-int (variable-closure-index variable))
                 (emit 'aaload)
                 (fix-boxing representation (derive-type ref))
                 (emit-move-from-stack target representation))
                ((variable-index variable)
                 (aver (not (null (compiland-argument-register *current-compiland*))))
                 (emit 'aload (compiland-argument-register *current-compiland*))
                 (emit-push-constant-int (variable-index variable))
                 (emit 'aaload)
                 (fix-boxing representation (variable-derived-type variable))
                 (emit-move-from-stack target representation))
                (t
                 (aver nil)))))))

(defun p2-set (form target representation)
  (cond ((and (check-arg-count form 2)
              (eq (derive-type (%cadr form)) 'SYMBOL))
         (emit-push-current-thread)
         (compile-form (%cadr form) 'stack nil)
         (emit 'checkcast +lisp-symbol-class+)
         (compile-form (%caddr form) 'stack nil)
         (maybe-emit-clear-values (%cadr form) (%caddr form))
         (emit-invokevirtual +lisp-thread-class+ "setSpecialVariable"
                             (list +lisp-symbol+ +lisp-object+) +lisp-object+)
         (fix-boxing representation nil)
         (emit-move-from-stack target representation))
        (t
         (compile-function-call form target representation))))

(declaim (ftype (function (t) t) rewrite-setq))
(defun rewrite-setq (form)
  (let ((expr (%caddr form)))
    (if (unsafe-p expr)
        (let ((sym (gensym)))
          (list 'LET (list (list sym expr)) (list 'SETQ (%cadr form) sym)))
        form)))

(defknown p2-setq (t t t) t)
(defun p2-setq (form target representation)
  (unless (= (length form) 3)
    (return-from p2-setq (compile-form (precompiler::precompile-setq form)
                                       target representation)))
  (let ((expansion (macroexpand (%cadr form) *compile-file-environment*)))
    (unless (eq expansion (%cadr form))
      (compile-form (list 'SETF expansion (%caddr form)) target representation)
      (return-from p2-setq)))
  (let* ((name (%cadr form))
         (variable (find-visible-variable name))
         (value-form (%caddr form)))
    (when (or (null variable)
              (variable-special-p variable))
      (let ((new-form (rewrite-setq form)))
        (when (neq new-form form)
          (return-from p2-setq (compile-form (p1 new-form) target representation))))
      ;; We're setting a special variable.
      (emit-push-current-thread)
      (emit 'getstatic *this-class* (declare-symbol name) +lisp-symbol+)
;;       (let ((*print-structure* nil))
;;         (format t "p2-setq name = ~S value-form = ~S~%" name value-form))
      (cond ((and (consp value-form)
                  (eq (first value-form) 'CONS)
                  (= (length value-form) 3)
                  (var-ref-p (third value-form))
                  (eq (variable-name (var-ref-variable (third value-form))) name))
             ;; (push thing *special*) => (setq *special* (cons thing *special*))
;;              (format t "compiling pushSpecial~%")
	     (compile-forms-and-maybe-emit-clear-values (second value-form) 'stack nil)
             (emit-invokevirtual +lisp-thread-class+ "pushSpecial"
                                 (list +lisp-symbol+ +lisp-object+) +lisp-object+))
            (t
	     (compile-forms-and-maybe-emit-clear-values value-form 'stack nil)
             (emit-invokevirtual +lisp-thread-class+ "setSpecialVariable"
                                 (list +lisp-symbol+ +lisp-object+) +lisp-object+)))
      (fix-boxing representation nil)
      (emit-move-from-stack target representation)
      (return-from p2-setq))

    (when (zerop (variable-reads variable))
      ;; If we never read the variable, we don't have to set it.
      (cond (target
	     (compile-forms-and-maybe-emit-clear-values value-form 'stack nil)
             (fix-boxing representation nil)
             (emit-move-from-stack target representation))
            (t
             (compile-form value-form nil nil)))
      (return-from p2-setq))

    ;; Optimize the (INCF X) case.
    (let ((incf-p nil))
      (when (and (eq (variable-representation variable) :int)
                 (consp value-form))
        (let ((op (car value-form))
              (len (length value-form)))
          (case op
            (1+
             (when (= len 2)
               (let ((arg (cadr value-form)))
                 (when (and (var-ref-p arg) (eq (var-ref-variable arg) variable))
                   (setf incf-p t)))))
            (+
             (when (= len 3)
               (let ((arg1 (second value-form))
                     (arg2 (third value-form)))
                 (when (eql arg1 1)
                   (setf arg1 arg2 arg2 1)) ;; (+ 1 X) => (+ X 1)
                 (when (eql arg2 1)
                   (when (and (var-ref-p arg1) (eq (var-ref-variable arg1) variable))
                     (setf incf-p t)))))))))
      (when incf-p
        (aver (variable-register variable))
        (emit 'iinc (variable-register variable) 1)
        (when target
          (case representation
            (:int
             (emit 'iload (variable-register variable)))
            (:long
             (emit 'iload (variable-register variable))
             (emit 'i2l))
            (t
             (emit 'new +lisp-fixnum-class+)
             (emit 'dup)
             (aver (variable-register variable))
             (emit 'iload (variable-register variable))
             (emit-invokespecial-init +lisp-fixnum-class+ '("I"))
             (fix-boxing representation nil)))
          (emit-move-from-stack target representation))
        (return-from p2-setq)))

    (cond ((and (eq (variable-representation variable) :int)
                (or (equal value-form (list '1+ (variable-name variable)))
                    (equal value-form (list '+ (variable-name variable) 1))
                    (equal value-form (list '+ 1 (variable-name variable)))))
           ;; FIXME This is the old (INCF X) case. We should be able to remove
           ;; this case once the new code is stable.
           (emit 'iinc (variable-register variable) 1)
           (when target
             (cond ((eq representation :int)
                    (emit 'iload (variable-register variable)))
                   (t
                    (dformat t "p2-setq constructing boxed fixnum for ~S~%"
                             (variable-name variable))
                    (emit 'new +lisp-fixnum-class+)
                    (emit 'dup)
                    (aver (variable-register variable))
                    (emit 'iload (variable-register variable))
                    (emit-invokespecial-init +lisp-fixnum-class+ '("I"))))
             (emit-move-from-stack target representation)))
          ((and (eq (variable-representation variable) :int)
                (or (equal value-form (list '1- (variable-name variable)))
                    (equal value-form (list '- (variable-name variable) 1))))
           (dformat t "p2-setq decf :int case~%")
           (emit 'iinc (variable-register variable) -1)
           (when target
             (cond ((eq representation :int)
                    (emit 'iload (variable-register variable)))
                   (t
                    (dformat t "p2-setq constructing boxed fixnum for ~S~%"
                             (variable-name variable))
                    (emit 'new +lisp-fixnum-class+)
                    (emit 'dup)
                    (aver (variable-register variable))
                    (emit 'iload (variable-register variable))
                    (emit-invokespecial-init +lisp-fixnum-class+ '("I"))))
             (emit-move-from-stack target representation)))
          ((eq (variable-representation variable) :int)
           (dformat t "p2-setq :int case value-form = ~S~%"
                    value-form)
	   (compile-forms-and-maybe-emit-clear-values value-form 'stack :int)
           (when target
             (emit 'dup))
           (emit 'istore (variable-register variable))
           (when target
             ;; int on stack here
             (case representation
               (:int)
               (:long
                (emit 'i2l))
               (t
                ;; need to box int
                (emit 'new +lisp-fixnum-class+) ; stack: int new-fixnum
                (emit 'dup_x1)                  ; stack: new-fixnum int new-fixnum
                (emit 'swap)                    ; stack: new-fixnum new-fixnum int
                (emit-invokespecial-init +lisp-fixnum-class+ '("I")))) ; stack: fixnum
             (emit-move-from-stack target representation)))
          ((eq (variable-representation variable) :char)
           (dformat t "p2-setq :char case~%")
	   (compile-forms-and-maybe-emit-clear-values value-form 'stack :char)
           (when target
             (emit 'dup))
           (emit 'istore (variable-register variable))
           (when target
             ;; char on stack here
             (when (null representation)
               ;; need to box char
               (emit 'new +lisp-character-class+) ; stack: char new-character
               (emit 'dup_x1)                  ; stack: new-character char new-character
               (emit 'swap)                    ; stack: new-character new-character char
               (emit-invokespecial-init +lisp-character-class+ '("C")) ; stack: character
               (emit-move-from-stack target representation))))
          ((eq (variable-representation variable) :long)
	   (compile-forms-and-maybe-emit-clear-values value-form 'stack :long)
           (when target
             (emit 'dup2))
           (emit 'lstore (variable-register variable))
           (when target
             ;; long on stack here
             (case representation
               (:int
                (emit 'l2i))
               (:long)
               (t
                (emit-box-long)))
             (emit-move-from-stack target representation)))
          ((eq (variable-representation variable) :boolean)
	   (compile-forms-and-maybe-emit-clear-values value-form 'stack :boolean)
           (when target
             (emit 'dup))
           (emit 'istore (variable-register variable))
           (when target
             ;; int on stack here
             (case representation
               (:boolean)
               (t
                (emit-box-boolean)))
             (emit-move-from-stack target representation)))
          (t
	   (compile-forms-and-maybe-emit-clear-values value-form 'stack nil)
           (when target
             (emit 'dup))
           (emit 'var-set variable)
           (when target
             (fix-boxing representation nil)
             (emit-move-from-stack target representation))))))

(defun p2-sxhash (form target representation)
  (cond ((check-arg-count form 1)
         (let ((arg (%cadr form)))
           (unless (eq representation :int)
             (emit 'new +lisp-fixnum-class+)
             (emit 'dup))
	   (compile-forms-and-maybe-emit-clear-values arg 'stack nil)
           (emit-invokevirtual +lisp-object-class+ "sxhash" nil "I")
           (unless (eq representation :int)
             (emit-invokespecial-init +lisp-fixnum-class+ '("I"))
             (fix-boxing representation 'fixnum))
           (emit-move-from-stack target representation)))
        (t
         (compile-function-call form target representation))))

(defknown p2-symbol-name (t t t) t)
(defun p2-symbol-name (form target representation)
  (unless (check-arg-count form 1)
    (compile-function-call form target representation)
    (return-from p2-symbol-name))
  (let ((arg (%cadr form)))
    (cond ((and (eq (derive-compiler-type arg) 'SYMBOL) (< *safety* 3))
	   (compile-forms-and-maybe-emit-clear-values arg 'stack nil)
           (emit 'checkcast +lisp-symbol-class+)
           (emit 'getfield  +lisp-symbol-class+ "name" +lisp-simple-string+)
           (emit-move-from-stack target representation))
          (t
           (compile-function-call form target representation)))))

(defknown p2-symbol-package (t t t) t)
(defun p2-symbol-package (form target representation)
  (unless (check-arg-count form 1)
    (compile-function-call form target representation)
    (return-from p2-symbol-package))
  (let ((arg (%cadr form)))
    (cond ((and (eq (derive-compiler-type arg) 'SYMBOL) (< *safety* 3))
	   (compile-forms-and-maybe-emit-clear-values arg 'stack nil)
           (emit 'checkcast +lisp-symbol-class+)
           (emit-invokevirtual +lisp-symbol-class+ "getPackage"
                               nil +lisp-object+)
           (fix-boxing representation nil)
           (emit-move-from-stack target representation))
          (t
           (compile-function-call form target representation)))))

(defknown p2-symbol-value (t t t) t)
(defun p2-symbol-value (form target representation)
  (when (check-arg-count form 1)
    (let ((arg (%cadr form)))
      (when (eq (derive-compiler-type arg) 'SYMBOL)
	(compile-forms-and-maybe-emit-clear-values arg 'stack nil)
        (emit 'checkcast +lisp-symbol-class+)
        (emit-push-current-thread)
        (emit-invokevirtual +lisp-symbol-class+ "symbolValue"
                            (list +lisp-thread+) +lisp-object+)
;;         (when (eq representation :int)
;;           (emit-unbox-fixnum))
        (fix-boxing representation nil)
        (emit-move-from-stack target representation)
        (return-from p2-symbol-value))))
  ;; Otherwise...
  (compile-function-call form target representation))

(defknown generate-instanceof-type-check-for-value (t) t)
(defun generate-instanceof-type-check-for-value (expected-type)
  ;; The value to be checked is on the stack.
  (declare (type symbol expected-type))
  (let ((instanceof-class (ecase expected-type
                            (SYMBOL     +lisp-symbol-class+)
                            (CHARACTER  +lisp-character-class+)
                            (CONS       +lisp-cons-class+)
                            (HASH-TABLE +lisp-hash-table-class+)
                            (FIXNUM     +lisp-fixnum-class+)
			    (STREAM     +lisp-stream-class+)
                            (STRING     +lisp-abstract-string-class+)
                            (VECTOR     +lisp-abstract-vector-class+)))
        (expected-type-java-symbol-name (case expected-type
                                          (HASH-TABLE "HASH_TABLE")
                                          (t
                                           (symbol-name expected-type))))
        (LABEL1 (gensym)))
    (emit 'dup)
    (emit 'instanceof instanceof-class)
    (emit 'ifne LABEL1)
    (emit 'getstatic +lisp-symbol-class+ expected-type-java-symbol-name +lisp-symbol+)
    (emit-invokestatic +lisp-class+ "type_error"
                       (lisp-object-arg-types 2) +lisp-object+)
    (label LABEL1))
  t)

(declaim (ftype (function (t) t) generate-type-check-for-value))
(defun generate-type-check-for-value (declared-type)
  (let ((type-to-use (find-type-for-type-check declared-type)))
    (when type-to-use
      (generate-instanceof-type-check-for-value type-to-use))))

(defun p2-the (form target representation)
  (let ((type-form (second form))
        (value-form (third form)))
;;     (let ((*print-structure* nil))
;;       (format t "p2-the type-form = ~S value-form = ~S~%" type-form value-form))
    (cond ((and (subtypep type-form 'FIXNUM)
                (consp value-form)
                (eq (car value-form) 'structure-ref))
           ;; Special case for structure slot references: getFixnumSlotValue()
           ;; signals an error if the slot's value is not a fixnum.
           (compile-form value-form target representation))
          ((and (> *safety* 0)
                (not (subtypep (derive-type value-form) type-form)))
           (compile-form value-form 'stack nil)
           (generate-type-check-for-value type-form)
           ;; The value is left on the stack here if the type check succeeded.
           (fix-boxing representation nil)
           (emit-move-from-stack target representation))
          (t
           (compile-form value-form target representation)))))

(defun p2-truly-the (form target representation)
  (compile-form (third form) target representation))

(defknown p2-char-code (t t t) t)
(defun p2-char-code (form target representation)
  (unless (check-arg-count form 1)
    (compile-function-call form target representation)
    (return-from p2-char-code))
  (let ((arg (second form)))
    (cond ((characterp arg)
           (compile-constant (char-code arg) target representation))
          ((and (< *safety* 3)
                (eq (derive-compiler-type arg) 'character))
           (when (null representation)
             (emit 'new +lisp-fixnum-class+)
             (emit 'dup))
           (compile-form arg 'stack :char)
           (case representation
             (:int)
             (:long
              (emit 'i2l))
             (t
              (emit-invokespecial-init +lisp-fixnum-class+ '("I"))))
           (emit-move-from-stack target representation))
          (t
           (compile-function-call form target representation)))))

(defknown p2-java-jclass (t t t) t)
(defun p2-java-jclass (form target representation)
  (unless (and (= 2 (length form))
               (stringp (cadr form)))
    (compile-function-call form target representation)
    (return-from p2-java-jclass))
  (let ((c (ignore-errors (java:jclass (cadr form)))))
    (if c (compile-constant c target representation)
      ;; delay resolving the method to run-time; it's unavailable now
      (compile-function-call form target representation))))

(defknown p2-java-jconstructor (t t t) t)
(defun p2-java-jconstructor (form target representation)
  (unless (and (< 1 (length form))
               (every #'stringp (cdr form)))
    (compile-function-call form target representation)
    (return-from p2-java-jconstructor))
  (let ((c (ignore-errors (apply #'java:jconstructor (cdr form)))))
    (if c (compile-constant c target representation)
      ;; delay resolving the method to run-time; it's unavailable now
      (compile-function-call form target representation))))

(defknown p2-java-jmethod (t t t) t)
(defun p2-java-jmethod (form target representation)
  (unless (and (< 1 (length form))
               (every #'stringp (cdr form)))
    (compile-function-call form target representation)
    (return-from p2-java-jmethod))
  (let ((m (ignore-errors (apply #'java:jmethod (cdr form)))))
    (if m (compile-constant m target representation)
      ;; delay resolving the method to run-time; it's unavailable now
      (compile-function-call form target representation))))


(defknown p2-char= (t t t) t)
(defun p2-char= (form target representation)
  (let* ((args (cdr form))
         (numargs (length args)))
    (when (= numargs 0)
      (compiler-warn "Wrong number of arguments for ~A." (car form))
      (compile-function-call form target representation)
      (return-from p2-char=))
    (unless (= numargs 2)
      (compile-function-call form target representation)
      (return-from p2-char=))
    (let ((arg1 (%car args))
          (arg2 (%cadr args)))
      (when (and (characterp arg1) (characterp arg2))
        (cond ((eql arg1 arg2)
               (emit-push-true representation))
              (t
               (emit-push-false representation)))
        (emit-move-from-stack target representation)
        (return-from p2-char=))
      (cond ((characterp arg1)
             (emit-push-constant-int (char-code arg1))
	     (compile-forms-and-maybe-emit-clear-values arg2 'stack :char))
            ((characterp arg2)
	     (compile-forms-and-maybe-emit-clear-values arg1 'stack :char)
             (emit-push-constant-int (char-code arg2)))
            (t
	     (compile-forms-and-maybe-emit-clear-values arg1 'stack :char
							arg2 'stack :char)))
      (let ((LABEL1 (gensym))
            (LABEL2 (gensym)))
        (emit 'if_icmpeq LABEL1)
        (emit-push-false representation)
        (emit 'goto LABEL2)
        (label LABEL1)
        (emit-push-true representation)
        (label LABEL2)
        (emit-move-from-stack target representation)))))

(defknown p2-catch-node (t t) t)
(defun p2-catch-node (block target)
  (let ((form (block-form block)))
    (when (= (length form) 2) ; (catch 'foo)
      (when target
        (emit-push-nil)
        (emit-move-from-stack target))
      (return-from p2-catch-node))
    (let* ((*register* *register*)
           (tag-register (allocate-register))
           (BEGIN-PROTECTED-RANGE (gensym))
           (END-PROTECTED-RANGE (gensym))
           (THROW-HANDLER (gensym))
           (DEFAULT-HANDLER (gensym))
           (EXIT (gensym)))
      (compile-form (second form) tag-register nil) ; Tag.
      (emit-push-current-thread)
      (emit 'aload tag-register)
      (emit-invokevirtual +lisp-thread-class+ "pushCatchTag"
                          (lisp-object-arg-types 1) nil)
      ; Stack depth is 0.
      (label BEGIN-PROTECTED-RANGE) ; Start of protected range.
      (compile-progn-body (cddr form) target) ; Implicit PROGN.
      (label END-PROTECTED-RANGE) ; End of protected range.
      (emit 'goto EXIT) ; Jump over handlers.
      (label THROW-HANDLER) ; Start of handler for THROW.
      ;; The Throw object is on the runtime stack. Stack depth is 1.
      (emit 'dup) ; Stack depth is 2.
      (emit 'getfield +lisp-throw-class+ "tag" +lisp-object+) ; Still 2.
      (emit 'aload tag-register) ; Stack depth is 3.
      ;; If it's not the tag we're looking for, we branch to the start of the
      ;; catch-all handler, which will do a re-throw.
      (emit 'if_acmpne DEFAULT-HANDLER) ; Stack depth is 1.
      (emit 'aload *thread*)
      (emit-invokevirtual +lisp-throw-class+ "getResult"
                          (list +lisp-thread+) +lisp-object+)
      (emit-move-from-stack target) ; Stack depth is 0.
      (emit 'goto EXIT)
      (label DEFAULT-HANDLER) ; Start of handler for all other Throwables.
      ;; A Throwable object is on the runtime stack here. Stack depth is 1.
      (emit 'aload *thread*)
      (emit-invokevirtual +lisp-thread-class+ "popCatchTag" nil nil)
      (emit 'athrow) ; Re-throw.
      (label EXIT)
      ;; Finally...
      (emit 'aload *thread*)
      (emit-invokevirtual +lisp-thread-class+ "popCatchTag" nil nil)
      (let ((handler1 (make-handler :from BEGIN-PROTECTED-RANGE
                                    :to END-PROTECTED-RANGE
                                    :code THROW-HANDLER
                                    :catch-type (pool-class +lisp-throw-class+)))
            (handler2 (make-handler :from BEGIN-PROTECTED-RANGE
                                    :to END-PROTECTED-RANGE
                                    :code DEFAULT-HANDLER
                                    :catch-type 0)))
        (push handler1 *handlers*)
        (push handler2 *handlers*))))
  t)

(defun p2-throw (form target representation)
  ;; FIXME What if we're called with a non-NIL representation?
  (declare (ignore representation))
  (emit-push-current-thread)
  (compile-form (second form) 'stack nil) ; Tag.
  (emit-clear-values) ; Do this unconditionally! (MISC.503)
  (compile-form (third form) 'stack nil) ; Result.
  (emit-invokevirtual +lisp-thread-class+ "throwToTag"
                      (lisp-object-arg-types 2) nil)
  ;; Following code will not be reached.
  (when target
    (emit-push-nil)
    (emit-move-from-stack target)))

(defun p2-unwind-protect-node (block target)
  (let ((form (block-form block)))
    (when (= (length form) 2) ; No cleanup form.
      (compile-form (second form) target nil)
      (return-from p2-unwind-protect-node))
    (let* ((protected-form (cadr form))
           (cleanup-forms (cddr form))
           (*register* *register*)
           (exception-register (allocate-register))
           (result-register (allocate-register))
           (values-register (allocate-register))
           (return-address-register (allocate-register))
           (BEGIN-PROTECTED-RANGE (gensym))
           (END-PROTECTED-RANGE (gensym))
           (HANDLER (gensym))
           (EXIT (gensym))
           (CLEANUP (gensym)))
      ;; Make sure there are no leftover multiple return values from previous calls.
      (emit-clear-values)

      (let* ((*blocks* (cons block *blocks*)))
        (label BEGIN-PROTECTED-RANGE)
        (compile-form protected-form result-register nil)
        (emit-push-current-thread)
        (emit 'getfield +lisp-thread-class+ "_values" "[Lorg/armedbear/lisp/LispObject;")
        (emit 'astore values-register)
        (label END-PROTECTED-RANGE))
      (emit 'jsr CLEANUP)
      (emit 'goto EXIT) ; Jump over handler.
      (label HANDLER) ; Start of exception handler.
      ;; The Throwable object is on the runtime stack. Stack depth is 1.
      (emit 'astore exception-register)
      (emit 'jsr CLEANUP) ; Call cleanup forms.
      (emit-clear-values)
      (emit 'aload exception-register)
      (emit 'athrow) ; Re-throw exception.
      (label CLEANUP) ; Cleanup forms.
      ;; Return address is on stack here.
      (emit 'astore return-address-register)
      (dolist (subform cleanup-forms)
        (compile-form subform nil nil))
      (emit 'ret return-address-register)
      (label EXIT)
      ;; Restore multiple values returned by protected form.
      (emit-push-current-thread)
      (emit 'aload values-register)
      (emit 'putfield +lisp-thread-class+ "_values" "[Lorg/armedbear/lisp/LispObject;")
      ;; Result.
      (emit 'aload result-register)
      (emit-move-from-stack target)
      (let ((handler (make-handler :from BEGIN-PROTECTED-RANGE
                                   :to END-PROTECTED-RANGE
                                   :code HANDLER
                                   :catch-type 0)))
        (push handler *handlers*)))))

(defknown compile-form (t t t) t)
(defun compile-form (form target representation)
  (cond ((consp form)
         (let* ((op (%car form))
                (handler (and (symbolp op) (get op 'p2-handler))))
           (cond (handler
                  (funcall handler form target representation))
                 ((symbolp op)
                  (cond ((macro-function op *compile-file-environment*)
                         (compile-form (macroexpand form *compile-file-environment*)
                                       target representation))
                        ((special-operator-p op)
                         (dformat t "form = ~S~%" form)
                         (compiler-unsupported
                          "COMPILE-FORM: unsupported special operator ~S" op))
                        (t
                         (compile-function-call form target representation))))
                 ((and (consp op) (eq (%car op) 'LAMBDA))
                  (aver (progn 'unexpected-lambda nil))
                  (let ((new-form (list* 'FUNCALL form)))
                    (compile-form new-form target representation)))
                 (t
                  (compiler-unsupported "COMPILE-FORM unhandled case ~S" form)))))
        ((symbolp form)
         (cond ((null form)
                (emit-push-false representation)
                (emit-move-from-stack target representation))
               ((eq form t)
                (emit-push-true representation)
                (emit-move-from-stack target representation))
               ((keywordp form)
                (case representation
                  (:boolean
                   (emit 'iconst_1))
                  (t
                   (let ((name (lookup-known-keyword form)))
                     (if name
                         (emit 'getstatic "org/armedbear/lisp/Keyword" name +lisp-symbol+)
                         (emit 'getstatic *this-class* (declare-keyword form) +lisp-symbol+)))))
                (emit-move-from-stack target representation))
               (t
                ;; Shouldn't happen.
                (aver nil))))
        ((var-ref-p form)
         (compile-var-ref form target representation))
        ((block-node-p form)
         (cond ((equal (block-name form) '(TAGBODY))
                (p2-tagbody-node form target)
                (fix-boxing representation nil)
                )
               ((equal (block-name form) '(LET))
                (p2-let/let*-node form target representation)
;;                 (fix-boxing representation nil)
                )
               ((equal (block-name form) '(MULTIPLE-VALUE-BIND))
                (p2-m-v-b-node form target)
                (fix-boxing representation nil)
                )
               ((equal (block-name form) '(UNWIND-PROTECT))
                (p2-unwind-protect-node form target)
                (fix-boxing representation nil)
                )
               ((equal (block-name form) '(CATCH))
                (p2-catch-node form target)
                (fix-boxing representation nil)
                )
               (t
                (p2-block-node form target representation)
;;                 (fix-boxing representation nil)
                ))
;;          (fix-boxing representation nil)
         )
        ((constantp form)
         (compile-constant form target representation))
        (t
         (compiler-unsupported "COMPILE-FORM unhandled case ~S" form)))
  t)



;; Returns descriptor.
(defun analyze-args (compiland)
  (let* ((args (cadr (compiland-p1-result compiland)))
         (arg-count (length args)))
    (dformat t "analyze-args args = ~S~%" args)
    (aver (not (memq '&AUX args)))

    (when *child-p*
      (when (or (memq '&KEY args)
                (memq '&OPTIONAL args)
                (memq '&REST args))
        (setf *using-arg-array* t)
        (setf *hairy-arglist-p* t)
        (return-from analyze-args
                     (if *closure-variables*
                         (get-descriptor (list +lisp-object-array+ +lisp-object-array+)
                                          +lisp-object+)
                         (get-descriptor (list +lisp-object-array+)
                                          +lisp-object+))))
      (cond (*closure-variables*
             (return-from analyze-args
                          (cond ((<= arg-count call-registers-limit)
                                 (get-descriptor (list* +lisp-object-array+
                                                        (lisp-object-arg-types arg-count))
                                                 +lisp-object+))
                                (t (setf *using-arg-array* t)
                                   (setf (compiland-arity compiland) arg-count)
                                   (get-descriptor (list +lisp-object-array+ +lisp-object-array+) ;; FIXME
                                                   +lisp-object+)))))
            (t
             (return-from analyze-args
                          (cond ((<= arg-count call-registers-limit)
                                 (get-descriptor (lisp-object-arg-types arg-count)
                                                 +lisp-object+))
                                (t (setf *using-arg-array* t)
                                   (setf (compiland-arity compiland) arg-count)
                                   (get-descriptor (list +lisp-object-array+)
                                                   +lisp-object+))))))) ;; FIXME
    (when (or (memq '&KEY args)
              (memq '&OPTIONAL args)
              (memq '&REST args))
      (setf *using-arg-array* t)
      (setf *hairy-arglist-p* t)
      (return-from analyze-args
                   (get-descriptor (list +lisp-object-array+) +lisp-object+)))
    (cond ((<= arg-count call-registers-limit)
           (get-descriptor (lisp-object-arg-types (length args))
                            +lisp-object+))
          (t
           (setf *using-arg-array* t)
           (setf (compiland-arity compiland) arg-count)
           (get-descriptor (list +lisp-object-array+) +lisp-object+)))))

(defun write-class-file (class-file)
  (let* ((super (class-file-superclass class-file))
         (this-index (pool-class (class-file-class class-file)))
         (super-index (pool-class super))
         (constructor (make-constructor super
                                        (class-file-lambda-name class-file)
                                        (class-file-lambda-list class-file))))
    (pool-name "Code") ; Must be in pool!

    (when *compile-file-truename*
      (pool-name "SourceFile") ; Must be in pool!
      (pool-name (file-namestring *compile-file-truename*)))
    (when (and (boundp '*source-line-number*)
               (fixnump *source-line-number*))
      (pool-name "LineNumberTable")) ; Must be in pool!

    ;; Write out the class file.
    (with-open-file (stream (class-file-pathname class-file)
                            :direction :output
                            :element-type '(unsigned-byte 8)
                            :if-exists :supersede)
      (write-u4 #xCAFEBABE stream)
      (write-u2 3 stream)
      (write-u2 45 stream)
      (write-constant-pool stream)
      ;; access flags
      (write-u2 #x21 stream)
      (write-u2 this-index stream)
      (write-u2 super-index stream)
      ;; interfaces count
      (write-u2 0 stream)
      ;; fields count
      (write-u2 (length *fields*) stream)
      ;; fields
      (dolist (field *fields*)
        (write-field field stream))
      ;; methods count
      (write-u2 (1+ (length (class-file-methods class-file))) stream)
      ;; methods
      (dolist (method (class-file-methods class-file))
        (write-method method stream))
      (write-method constructor stream)
      ;; attributes count
      (cond (*compile-file-truename*
             ;; attributes count
             (write-u2 1 stream)
             ;; attributes table
             (write-source-file-attr (file-namestring *compile-file-truename*)
                                     stream))
            (t
             ;; attributes count
             (write-u2 0 stream))))))

(defun compile-xep (xep)
  (declare (type compiland xep))
  (let ((*all-variables* ())
        (*closure-variables* ())
        (*current-compiland* xep)
        (*speed* 3)
        (*safety* 0)
        (*debug* 0))

    (aver (not (null (compiland-class-file xep))))

    ;; Pass 1.
    (p1-compiland xep)
;;     (dformat t "*all-variables* = ~S~%" (mapcar #'variable-name *all-variables*))
    (setf *closure-variables*
          (remove-if-not #'variable-used-non-locally-p *all-variables*))
    (setf *closure-variables*
          (remove-if #'variable-special-p *closure-variables*))
;;     (dformat t "*closure-variables* = ~S~%" (mapcar #'variable-name *closure-variables*))

    (when *closure-variables*
      (let ((i 0))
        (dolist (var (reverse *closure-variables*))
          (setf (variable-closure-index var) i)
          (dformat t "var = ~S closure index = ~S~%" (variable-name var)
                   (variable-closure-index var))
          (incf i))))

    ;; Pass 2.
    (with-class-file (compiland-class-file xep)
      (p2-compiland xep))))


(defun p2-%call-internal (form target representation)
  (dformat t "p2-%call-internal~%")
  (emit 'aload_0) ; this
  (let ((args (cdr form))
        (must-clear-values nil))
    (dformat t "args = ~S~%" args)
    (dolist (arg args)
      (compile-form arg 'stack nil)
      (unless must-clear-values
        (unless (single-valued-p arg)
          (setf must-clear-values t))))
    (let ((arg-types (lisp-object-arg-types (length args)))
          (return-type +lisp-object+))
      (emit-invokevirtual *this-class* "_execute" arg-types return-type))
    (emit-move-from-stack target representation)))

(defknown p2-compiland-process-type-declarations (list) t)
(defun p2-compiland-process-type-declarations (body)
  (flet ((process-declaration (name type)
           (let ((variable (find-visible-variable name)))
             (when variable
               (setf (variable-declared-type variable) type)))))
    (dolist (subform body)
      (unless (and (consp subform) (eq (%car subform) 'DECLARE))
        (return))
      (let ((decls (%cdr subform)))
        (dolist (decl decls)
          (case (car decl)
            (TYPE
             (let ((type (make-compiler-type (cadr decl))))
               (dolist (name (cddr decl))
                 (process-declaration name type))))
            ((IGNORE IGNORABLE)
             (process-ignore/ignorable (%car decl) (%cdr decl) *visible-variables*))
            ((DYNAMIC-EXTENT FTYPE INLINE NOTINLINE OPTIMIZE SPECIAL)
             ;; Nothing to do here.
             )
            (t
             (let ((type (make-compiler-type (car decl))))
               (dolist (name (cdr decl))
                 (process-declaration name type)))))))))
  t)

(defknown p2-compiland-unbox-variable (variable) t)
(defun p2-compiland-unbox-variable (variable)
  (let ((register (variable-register variable)))
    (when (and register
               (not (variable-special-p variable))
               (not (variable-used-non-locally-p variable))
               (zerop (compiland-children *current-compiland*)))
      (let ((type (variable-declared-type variable)))
        (cond ((fixnum-type-p type)
               (emit 'aload register)
               (emit-unbox-fixnum)
               (emit 'istore register)
               (setf (variable-representation variable) :int))
              ((java-long-type-p type)
               (let ((new-register (allocate-register-pair)))
                 (emit 'aload register)
                 (emit-invokevirtual +lisp-object-class+ "longValue" nil "J")
                 (emit 'lstore new-register)
                 (setf (variable-register variable) new-register)
                 (setf (variable-representation variable) :long)))
              ((eq type 'CHARACTER)
               (emit 'aload register)
               (emit-unbox-character)
               (emit 'istore register)
               (setf (variable-representation variable) :char))))))
  t)

(defknown p2-compiland (t) t)
(defun p2-compiland (compiland)
;;   (format t "p2-compiland name = ~S~%" (compiland-name compiland))
  (let* ((p1-result (compiland-p1-result compiland))
         (class-file (compiland-class-file compiland))
         (*this-class* (class-file-class class-file))
         (args (cadr p1-result))
         (body (cddr p1-result))
         (*using-arg-array* nil)
         (*hairy-arglist-p* nil)

         (*child-p* (not (null (compiland-parent compiland))))

         (descriptor (analyze-args compiland))
         (execute-method-name (if (eq (compiland-kind compiland) :external)
                                  "execute" "_execute"))
         (execute-method (make-method :name execute-method-name
                                      :descriptor descriptor))
         (*code* ())
         (*register* 0)
         (*registers-allocated* 0)
         (*handlers* ())
         (*visible-variables* *visible-variables*)

         (parameters ())

         (*thread* nil)
         (*initialize-thread-var* nil)
         (super nil))

    (unless *child-p*
      (when (memq '&REST args)
        (unless (or (memq '&OPTIONAL args) (memq '&KEY args))
          (let ((arg-count (length args)))
            (cond ((and (= arg-count 2) (eq (%car args) '&REST))
                   (setf *using-arg-array* nil)
                   (setf *hairy-arglist-p* nil)
                   (setf descriptor (get-descriptor (lisp-object-arg-types 1)
                                                    +lisp-object+))
                   (setf (compiland-kind compiland) :internal)
                   (setf super "org/armedbear/lisp/Primitive0R")
                   (setf args (cdr args))
                   (setf execute-method-name "_execute")
                   (setf execute-method (make-method :name execute-method-name
                                                     :descriptor descriptor)))
                  ((and (= arg-count 3) (eq (%cadr args) '&REST))
                   (setf *using-arg-array* nil)
                   (setf *hairy-arglist-p* nil)
                   (setf descriptor (get-descriptor (lisp-object-arg-types 2)
                                                    +lisp-object+))
                   (setf (compiland-kind compiland) :internal)
                   (setf super "org/armedbear/lisp/Primitive1R")
                   (setf args (list (first args) (third args)))
                   (setf execute-method-name "_execute")
                   (setf execute-method (make-method :name execute-method-name
                                                     :descriptor descriptor)))
                  ((and (= arg-count 4) (eq (%caddr args) '&REST))
                   (setf *using-arg-array* nil)
                   (setf *hairy-arglist-p* nil)
                   (setf descriptor (get-descriptor (list +lisp-object+ +lisp-object+ +lisp-object+)
                                                    +lisp-object+))
                   (setf (compiland-kind compiland) :internal)
                   (setf super "org/armedbear/lisp/Primitive2R")
                   (setf args (list (first args) (second args) (fourth args)))
                   (setf execute-method-name "_execute")
                   (setf execute-method (make-method :name execute-method-name
                                                     :descriptor descriptor)))
                  )))))

    (dolist (var (compiland-arg-vars compiland))
      (push var *visible-variables*))

    (setf (method-name-index execute-method)
          (pool-name (method-name execute-method)))
    (setf (method-descriptor-index execute-method)
          (pool-name (method-descriptor execute-method)))
    (cond (*hairy-arglist-p*
           (let* ((closure (make-closure p1-result nil))
                  (parameter-names (sys::varlist closure))
                  (index 0))
             (dolist (name parameter-names)
               (let ((variable (find-visible-variable name)))
                 (unless variable
                   (format t "1: unable to find variable ~S~%" name)
                   (aver nil))
                 (aver (null (variable-register variable)))
                 (aver (null (variable-index variable)))
                 (setf (variable-index variable) index)
                 (push variable parameters)
                 (incf index)))))
          (t
           (let ((register (if (and *closure-variables* *child-p*)
                               2 ; Reg 1 is reserved for closure variables array.
                               1))
                 (index 0))
             (dolist (arg args)
               (let ((variable (find-visible-variable arg)))
                 (when (null variable)
                   (format t "2: unable to find variable ~S~%" arg)
                   (aver nil))
                 (aver (null (variable-register variable)))
                 (setf (variable-register variable) (if *using-arg-array* nil register))
                 (aver (null (variable-index variable)))
                 (if *using-arg-array*
                     (setf (variable-index variable) index))
                 (push variable parameters)
                 (incf register)
                 (incf index))))))

    (let ((specials (process-special-declarations body)))
      (dolist (name specials)
        (dformat t "recognizing ~S as special~%" name)
        (let ((variable (find-visible-variable name)))
          (cond ((null variable)
                 (setf variable (make-variable :name name
                                               :special-p t))
                 (push variable *visible-variables*))
                (t
                 (setf (variable-special-p variable) t))))))

    (p2-compiland-process-type-declarations body)

    (allocate-register) ;; register 0: "this" pointer
    (when (and *closure-variables* *child-p*)
      (setf (compiland-closure-register compiland) (allocate-register)) ;; register 1
      (dformat t "p2-compiland 1 closure register = ~S~%" (compiland-closure-register compiland)))
    (cond (*using-arg-array*
           ;; One slot for arg array.
           (setf (compiland-argument-register compiland) (allocate-register))

           (unless (or *closure-variables* *child-p*)
             ;; Reserve a register for each parameter.
             (dolist (variable (reverse parameters))
               (aver (null (variable-register variable)))
               (aver (null (variable-reserved-register variable)))
               (unless (variable-special-p variable)
                 (setf (variable-reserved-register variable) (allocate-register))))))
          (t
           ;; Otherwise, one register for each argument.
           (dolist (arg args)
             (declare (ignore arg))
             (allocate-register))))
    (when (and *closure-variables* (not *child-p*))
      (setf (compiland-closure-register compiland) (allocate-register))
       (dformat t "p2-compiland 2 closure register = ~S~%" (compiland-closure-register compiland)))
    ;; Reserve the next available slot for the thread register.
    (setf *thread* (allocate-register))

    ;; Move args from their original registers to the closure variables array,
    ;; if applicable.
    (when *closure-variables*
      (dformat t "~S moving arguments to closure array (if applicable)~%"
               (compiland-name compiland))
      (cond (*child-p*
             (aver (eql (compiland-closure-register compiland) 1))
             (when (some #'variable-closure-index parameters)
               (emit 'aload (compiland-closure-register compiland))))
            (t
             (emit-push-constant-int (length *closure-variables*))
             (dformat t "p2-compiland ~S anewarray 1~%" (compiland-name compiland))
             (emit 'anewarray "org/armedbear/lisp/LispObject")))
      (dolist (variable parameters)
        (dformat t "considering ~S ...~%" (variable-name variable))
        (when (variable-closure-index variable)
          (dformat t "moving variable ~S~%" (variable-name variable))
          (cond ((variable-register variable)
                 (when (eql (variable-register variable)
                            (compiland-closure-register compiland))
                   (error "ERROR! compiland closure register = ~S var ~S register = ~S~%"
                          (compiland-closure-register compiland)
                          (variable-name variable)
                          (variable-register variable)))
                 (emit 'dup) ; array
                 (emit-push-constant-int (variable-closure-index variable))
                 (emit 'aload (variable-register variable))
                 (emit 'aastore)
                 (setf (variable-register variable) nil)) ; The variable has moved.
                ((variable-index variable)
                 (emit 'dup) ; array
                 (emit-push-constant-int (variable-closure-index variable))
                 (emit 'aload (compiland-argument-register compiland))
                 (emit-push-constant-int (variable-index variable))
                 (emit 'aaload)
                 (emit 'aastore)
                 (setf (variable-index variable) nil))))) ; The variable has moved.
      (aver (not (null (compiland-closure-register compiland))))
      (cond (*child-p*
             (when (some #'variable-closure-index parameters)
               (emit 'pop)))
            (t
             (emit 'astore (compiland-closure-register compiland))))
      (dformat t "~S done moving arguments to closure array~%"
               (compiland-name compiland)))

    ;; If applicable, move args from arg array to registers.
    (when *using-arg-array*
      (unless (or *closure-variables* *child-p*)
        (dolist (variable (reverse parameters))
          (when (variable-reserved-register variable)
            (aver (not (variable-special-p variable)))
            (emit 'aload (compiland-argument-register compiland))
            (emit-push-constant-int (variable-index variable))
            (emit 'aaload)
            (emit 'astore (variable-reserved-register variable))
            (setf (variable-register variable) (variable-reserved-register variable))
            (setf (variable-index variable) nil)))))

    (generate-type-checks-for-variables (reverse parameters))

    ;; Unbox variables.
    (dolist (variable (reverse parameters))
      (p2-compiland-unbox-variable variable))

    ;; Establish dynamic bindings for any variables declared special.
    (dolist (variable parameters)
      (when (variable-special-p variable)
        (cond ((variable-register variable)
               (emit-push-current-thread)
               (emit-push-variable-name variable)
               (emit 'aload (variable-register variable))
               (emit-invokevirtual +lisp-thread-class+ "bindSpecial"
                                   (list +lisp-symbol+ +lisp-object+) nil)
               (setf (variable-register variable) nil))
              ((variable-index variable)
               (emit-push-current-thread)
               (emit-push-variable-name variable)
               (emit 'aload (compiland-argument-register compiland))
               (emit-push-constant-int (variable-index variable))
               (emit 'aaload)
               (emit-invokevirtual +lisp-thread-class+ "bindSpecial"
                                   (list +lisp-symbol+ +lisp-object+) nil)
               (setf (variable-index variable) nil)))))

    (compile-progn-body body 'stack)

    (unless *code*
      (emit-push-nil))

    (emit 'areturn)

    (resolve-variables)

    ;; Warn if any unused args. (Is this the right place?)
    (check-for-unused-variables (compiland-arg-vars compiland))

    ;; Go back and fill in prologue.
    (let ((code *code*))
      (setf *code* ())
      (let ((arity (compiland-arity compiland)))
        (when arity
          (generate-arg-count-check arity)))

      (when *hairy-arglist-p*
        (emit 'aload_0) ; this
        (aver (not (null (compiland-argument-register compiland))))
        (emit 'aload (compiland-argument-register compiland)) ; arg vector
        (cond ((or (memq '&OPTIONAL args) (memq '&KEY args))
               (ensure-thread-var-initialized)
               (maybe-initialize-thread-var)
               (emit 'aload *thread*)
               (emit-invokevirtual *this-class* "processArgs"
                                   (list +lisp-object-array+ +lisp-thread+)
                                   +lisp-object-array+))
              (t
               (emit-invokevirtual *this-class* "fastProcessArgs"
                                   (list +lisp-object-array+)
                                   +lisp-object-array+)))
        (emit 'astore (compiland-argument-register compiland)))

      (maybe-initialize-thread-var)
      (setf *code* (nconc code *code*)))

    (finalize-code)
    (optimize-code)

    (setf *code* (resolve-instructions *code*))
    (setf (method-max-stack execute-method) (analyze-stack))
    (setf (method-code execute-method) (code-bytes *code*))

    ;; Remove handler if its protected range is empty.
    (setf *handlers*
          (delete-if (lambda (handler) (eql (symbol-value (handler-from handler))
                                            (symbol-value (handler-to handler))))
                     *handlers*))

    (setf (method-max-locals execute-method) *registers-allocated*)
    (setf (method-handlers execute-method) (nreverse *handlers*))

    (setf (class-file-superclass class-file)
          (cond (super
                 super)
                (*child-p*
                 (if *closure-variables*
                     (progn
                       (setf execute-method-name
                             (setf (method-name execute-method) "_execute"))
                       (setf (method-name-index execute-method)
                             (pool-name (method-name execute-method)))
                       (setf (method-descriptor-index execute-method)
                             (pool-name (method-descriptor execute-method)))
                       +lisp-ctf-class+)
                     (if *hairy-arglist-p*
                         +lisp-compiled-function-class+
                         +lisp-primitive-class+)))
                (*hairy-arglist-p*
                 +lisp-compiled-function-class+)
                (t
                 +lisp-primitive-class+)))

    (setf (class-file-lambda-list class-file) args)

    (push execute-method (class-file-methods class-file)))
  t)

(defun compile-1 (compiland)
  (let ((*all-variables* nil)
        (*closure-variables* nil)
        (*undefined-variables* nil)
        (*local-functions* nil)
        (*current-compiland* compiland))
    (with-saved-compiler-policy
      ;; Pass 1.
      (p1-compiland compiland)
      (setf *closure-variables*
            (remove-if-not #'variable-used-non-locally-p *all-variables*))
      (when *closure-variables*
        (setf *closure-variables*
              (remove-if #'variable-special-p *closure-variables*))
        (when *closure-variables*
          (let ((i 0))
            (dolist (var (reverse *closure-variables*))
              (setf (variable-closure-index var) i)
              (dformat t "var = ~S closure index = ~S~%" (variable-name var)
                       (variable-closure-index var))
              (incf i)))))
      ;; Pass 2.
      (with-class-file (compiland-class-file compiland)
        (p2-compiland compiland)
        (write-class-file (compiland-class-file compiland)))
      (class-file-pathname (compiland-class-file compiland)))))

(defvar *compiler-error-bailout*)

(defun make-compiler-error-form (form)
  `(lambda ,(cadr form)
     (error 'program-error :format-control "Execution of a form compiled with errors.")))

(defun compile-defun (name form environment filespec)
  (aver (eq (car form) 'LAMBDA))
  (unless (or (null environment) (empty-environment-p environment))
    (compiler-unsupported "COMPILE-DEFUN: unable to compile LAMBDA form defined in non-null lexical environment."))
  (catch 'compile-defun-abort
    (let* ((class-file (make-class-file :pathname filespec
                                        :lambda-name name
                                        :lambda-list (cadr form)))
           (*compiler-error-bailout*
            `(lambda ()
               (compile-1 (make-compiland :name ',name
                                          :lambda-expression (make-compiler-error-form ',form)
                                          :class-file
                                          (make-class-file :pathname ,filespec
                                                           :lambda-name ',name
                                                           :lambda-list (cadr ',form)))))))
        (compile-1 (make-compiland :name name
                                   :lambda-expression (precompile-form form t)
                                   :class-file class-file)))))

(defvar *catch-errors* t)

(defvar *in-compilation-unit* nil)

(defmacro with-compilation-unit (options &body body)
  `(%with-compilation-unit (lambda () ,@body) ,@options))

(defun %with-compilation-unit (fn &key override)
  (handler-bind ((style-warning 'handle-style-warning)
                 (warning 'handle-warning)
                 (compiler-error 'handle-compiler-error))
    (if (and *in-compilation-unit* (not override))
        (funcall fn)
        (let ((*style-warnings* 0)
              (*warnings* 0)
              (*errors* 0)
              (*defined-functions* nil)
              (*undefined-functions* nil)
              (*in-compilation-unit* t))
          (unwind-protect
              (funcall fn)
            (unless (or (and *suppress-compiler-warnings* (zerop *errors*))
                        (and (zerop (+ *errors* *warnings* *style-warnings*))
                             (null *undefined-functions*)))
              (format *error-output* "~%; Compilation unit finished~%")
              (unless (zerop *errors*)
                (format *error-output* ";   Caught ~D ERROR condition~P~%"
                        *errors* *errors*))
              (unless *suppress-compiler-warnings*
                (unless (zerop *warnings*)
                  (format *error-output* ";   Caught ~D WARNING condition~P~%"
                          *warnings* *warnings*))
                (unless (zerop *style-warnings*)
                  (format *error-output* ";   Caught ~D STYLE-WARNING condition~P~%"
                          *style-warnings* *style-warnings*))
                (when *undefined-functions*
                  (format *error-output* ";   The following functions were used but not defined:~%")
                  (dolist (name *undefined-functions*)
                    (format *error-output* ";     ~S~%" name))))
              (terpri *error-output*)))))))

(defun get-lambda-to-compile (thing)
  (if (and (consp thing)
           (eq (%car thing) 'LAMBDA))
      thing
      (multiple-value-bind (lambda-expression environment)
          (function-lambda-expression (if (typep thing 'standard-generic-function)
                                          (mop::funcallable-instance-function thing)
                                          thing))
        (unless lambda-expression
          (error "Can't find a definition for ~S." thing))
        (values lambda-expression environment))))

(defun %jvm-compile (name definition)
  (unless definition
    (resolve name)
    (setf definition (fdefinition name)))
  (when (compiled-function-p definition)
    (return-from %jvm-compile (values name nil nil)))
  (multiple-value-bind (expr env)
      (get-lambda-to-compile definition)
    (let* ((*package* (if (and name (symbol-package name))
                          (symbol-package name)
                          *package*))
           compiled-function
           (warnings-p t)
           (failure-p t))
      (with-compilation-unit ()
        (with-saved-compiler-policy
          (let* ((tempfile (make-temp-file)))
            (unwind-protect
                 (setf compiled-function
                       (load-compiled-function (compile-defun name expr env tempfile)))
              (delete-file tempfile))))
        (when (and name (functionp compiled-function))
          (sys::%set-lambda-name compiled-function name)
          (sys:set-call-count compiled-function (sys:call-count definition))
          (sys::%set-arglist compiled-function (sys::arglist definition))
          (let ((*warn-on-redefinition* nil))
            (cond ((typep definition 'standard-generic-function)
                   (mop:set-funcallable-instance-function definition compiled-function))
                  (t
                   (setf (fdefinition name)
                         (if (macro-function name)
                             (make-macro name compiled-function)
                             compiled-function))))))
        (cond ((zerop (+ *errors* *warnings* *style-warnings*))
               (setf warnings-p nil failure-p nil))
              ((zerop (+ *errors* *warnings*))
               (setf failure-p nil))))
      (values (or name compiled-function) warnings-p failure-p))))

(defun jvm-compile (name &optional definition)
  (if *catch-errors*
      (handler-case
          (%jvm-compile name definition)
        (compiler-unsupported-feature-error
         (c)
         (fresh-line)
         (sys::%format t "; UNSUPPORTED FEATURE: ~A~%" c)
         (if name
             (sys::%format t "; Unable to compile ~S.~%" name)
             (sys::%format t "; Unable to compile top-level form.~%"))
         (precompiler::precompile name definition)))
      (%jvm-compile name definition)))

(defun jvm-compile-package (package-designator)
  (let ((pkg (if (packagep package-designator)
                 package-designator
                 (find-package package-designator))))
      (dolist (sym (sys::package-symbols pkg))
        (when (fboundp sym)
          (unless (or (special-operator-p sym) (macro-function sym))
            ;; Force autoload to be resolved.
            (resolve sym)
            (let ((f (fdefinition sym)))
              (unless (compiled-function-p f)
                (jvm-compile sym)))))))
  t)

(defun initialize-p2-handlers ()
  (mapc #'install-p2-handler '(declare
                               multiple-value-call
                               multiple-value-list
                               multiple-value-prog1
                               nth
                               progn))
  (install-p2-handler '%call-internal      'p2-%call-internal)
  (install-p2-handler '%ldb                'p2-%ldb)
  (install-p2-handler '%make-structure     'p2-%make-structure)
  (install-p2-handler '*                   'p2-times)
  (install-p2-handler '+                   'p2-plus)
  (install-p2-handler '-                   'p2-minus)
  (install-p2-handler '<                   'p2-numeric-comparison)
  (install-p2-handler '<=                  'p2-numeric-comparison)
  (install-p2-handler '=                   'p2-numeric-comparison)
  (install-p2-handler '>                   'p2-numeric-comparison)
  (install-p2-handler '>=                  'p2-numeric-comparison)
  (install-p2-handler 'and                 'p2-and)
  (install-p2-handler 'aref                'p2-aref)
  (install-p2-handler 'aset                'p2-aset)
  (install-p2-handler 'ash                 'p2-ash)
  (install-p2-handler 'atom                'p2-atom)
  (install-p2-handler 'bit-vector-p        'p2-bit-vector-p)
  (install-p2-handler 'car                 'p2-car)
  (install-p2-handler 'cdr                 'p2-cdr)
  (install-p2-handler 'char                'p2-char/schar)
  (install-p2-handler 'char-code           'p2-char-code)
  (install-p2-handler 'java:jclass         'p2-java-jclass)
  (install-p2-handler 'java:jconstructor   'p2-java-jconstructor)
  (install-p2-handler 'java:jmethod        'p2-java-jmethod)
  (install-p2-handler 'char=               'p2-char=)
  (install-p2-handler 'characterp          'p2-characterp)
  (install-p2-handler 'classp              'p2-classp)
  (install-p2-handler 'coerce-to-function  'p2-coerce-to-function)
  (install-p2-handler 'cons                'p2-cons)
  (install-p2-handler 'sys::backq-cons     'p2-cons)
  (install-p2-handler 'consp               'p2-consp)
  (install-p2-handler 'delete              'p2-delete)
  (install-p2-handler 'elt                 'p2-elt)
  (install-p2-handler 'eq                  'p2-eq/neq)
  (install-p2-handler 'eql                 'p2-eql)
  (install-p2-handler 'eval-when           'p2-eval-when)
  (install-p2-handler 'find-class          'p2-find-class)
  (install-p2-handler 'fixnump             'p2-fixnump)
  (install-p2-handler 'flet                'p2-flet)
  (install-p2-handler 'funcall             'p2-funcall)
  (install-p2-handler 'function            'p2-function)
  (install-p2-handler 'gensym              'p2-gensym)
  (install-p2-handler 'get                 'p2-get)
  (install-p2-handler 'getf                'p2-getf)
  (install-p2-handler 'gethash             'p2-gethash)
  (install-p2-handler 'gethash1            'p2-gethash)
  (install-p2-handler 'go                  'p2-go)
  (install-p2-handler 'if                  'p2-if)
  (install-p2-handler 'labels              'p2-labels)
  (install-p2-handler 'length              'p2-length)
  (install-p2-handler 'list                'p2-list)
  (install-p2-handler 'sys::backq-list     'p2-list)
  (install-p2-handler 'list*               'p2-list*)
  (install-p2-handler 'sys::backq-list*    'p2-list*)
  (install-p2-handler 'load-time-value     'p2-load-time-value)
  (install-p2-handler 'locally             'p2-locally)
  (install-p2-handler 'logand              'p2-logand)
  (install-p2-handler 'logior              'p2-logior)
  (install-p2-handler 'lognot              'p2-lognot)
  (install-p2-handler 'logxor              'p2-logxor)
  (install-p2-handler 'make-array          'p2-make-array)
  (install-p2-handler 'make-hash-table     'p2-make-hash-table)
  (install-p2-handler 'make-sequence       'p2-make-sequence)
  (install-p2-handler 'make-string         'p2-make-string)
  (install-p2-handler 'make-structure      'p2-make-structure)
  (install-p2-handler 'max                 'p2-min/max)
  (install-p2-handler 'memq                'p2-memq)
  (install-p2-handler 'memql               'p2-memql)
  (install-p2-handler 'min                 'p2-min/max)
  (install-p2-handler 'mod                 'p2-mod)
  (install-p2-handler 'neq                 'p2-eq/neq)
  (install-p2-handler 'not                 'p2-not/null)
  (install-p2-handler 'nthcdr              'p2-nthcdr)
  (install-p2-handler 'null                'p2-not/null)
  (install-p2-handler 'or                  'p2-or)
  (install-p2-handler 'packagep            'p2-packagep)
  (install-p2-handler 'progv               'p2-progv)
  (install-p2-handler 'puthash             'p2-puthash)
  (install-p2-handler 'quote               'p2-quote)
  (install-p2-handler 'read-line           'p2-read-line)
  (install-p2-handler 'readtablep          'p2-readtablep)
  (install-p2-handler 'return-from         'p2-return-from)
  (install-p2-handler 'rplacd              'p2-rplacd)
  (install-p2-handler 'schar               'p2-char/schar)
  (install-p2-handler 'set                 'p2-set)
  (install-p2-handler 'set-car             'p2-set-car/cdr)
  (install-p2-handler 'set-cdr             'p2-set-car/cdr)
  (install-p2-handler 'set-char            'p2-set-char/schar)
  (install-p2-handler 'set-schar           'p2-set-char/schar)
  (install-p2-handler 'set-std-slot-value  'p2-set-std-slot-value)
  (install-p2-handler 'setq                'p2-setq)
  (install-p2-handler 'simple-vector-p     'p2-simple-vector-p)
  (install-p2-handler 'std-slot-value      'p2-std-slot-value)
  (install-p2-handler 'stream-element-type 'p2-stream-element-type)
  (install-p2-handler 'stringp             'p2-stringp)
  (install-p2-handler 'structure-ref       'p2-structure-ref)
  (install-p2-handler 'structure-set       'p2-structure-set)
  (install-p2-handler 'svref               'p2-svref)
  (install-p2-handler 'svset               'p2-svset)
  (install-p2-handler 'sxhash              'p2-sxhash)
  (install-p2-handler 'symbol-name         'p2-symbol-name)
  (install-p2-handler 'symbol-package      'p2-symbol-package)
  (install-p2-handler 'symbol-value        'p2-symbol-value)
  (install-p2-handler 'symbolp             'p2-symbolp)
  (install-p2-handler 'the                 'p2-the)
  (install-p2-handler 'throw               'p2-throw)
  (install-p2-handler 'truly-the           'p2-truly-the)
  (install-p2-handler 'truncate            'p2-truncate)
  (install-p2-handler 'values              'p2-values)
  (install-p2-handler 'vectorp             'p2-vectorp)
  (install-p2-handler 'vector-push-extend  'p2-vector-push-extend)
  (install-p2-handler 'write-8-bits        'p2-write-8-bits)
  (install-p2-handler 'zerop               'p2-zerop)
  t)

(initialize-p2-handlers)


(provide "COMPILER-PASS2")
