;;; compiler-pass2.lisp
;;;
;;; Copyright (C) 2003-2008 Peter Graves
;;; Copyright (C) 2008 Ville Voutilainen
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

(defknown pool-float (single-float) (integer 1 65535))
(defun pool-float (n)
  (declare (optimize speed))
  (pool-get (list 4 (%float-bits n))))

(defun pool-long/double (entry)
  (let* ((ht *pool-entries*)
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

(defknown pool-long (integer) (integer 1 65535))
(defun pool-long (n)
  (declare (optimize speed))
  (declare (type java-long n))
  (let* ((entry (list 5
                      (logand (ash n -32) #xffffffff)
                      (logand n #xffffffff))))
    (pool-long/double entry)))

(defknown pool-double (double-float) (integer 1 65535))
(defun pool-double (n)
  (declare (optimize speed))
  (let* ((n (%float-bits n))
         (entry (list 6
                      (logand (ash n -32) #xffffffff)
                      (logand n #xffffffff))))
    (pool-long/double entry)))

(defknown u2 (fixnum) cons)
(defun u2 (n)
  (declare (optimize speed))
  (declare (type (unsigned-byte 16) n))
  (when (not (<= 0 n 65535))
    (error "u2 argument ~A out of 65k range." n))
  (list (logand (ash n -8) #xff)
        (logand n #xff)))

(defknown s1 (fixnum) fixnum)
(defun s1 (n)
  (declare (optimize speed))
  (declare (type (signed-byte 8) n))
  (when (not (<= -128 n 127))
    (error "s2 argument ~A out of 16-bit signed range." n))
  (if (< n 0)
      (1+ (logxor (- n) #xFF))
      n))


(defknown s2 (fixnum) cons)
(defun s2 (n)
  (declare (optimize speed))
  (declare (type (signed-byte 16) n))
  (when (not (<= -32768 n 32767))
    (error "s2 argument ~A out of 16-bit signed range." n))
  (u2 (if (< n 0) (1+ (logxor (- n) #xFFFF))
          n)))

(defconstant +fasl-loader-class+
  "org/armedbear/lisp/FaslClassLoader")
(defconstant +java-string+ "Ljava/lang/String;")
(defconstant +java-object+ "Ljava/lang/Object;")
(defconstant +lisp-class+ "org/armedbear/lisp/Lisp")
(defconstant +lisp-nil-class+ "org/armedbear/lisp/Nil")
(defconstant +lisp-class-class+ "org/armedbear/lisp/LispClass")
(defconstant +lisp-object-class+ "org/armedbear/lisp/LispObject")
(defconstant +lisp-object+ "Lorg/armedbear/lisp/LispObject;")
(defconstant +lisp-object-array+ "[Lorg/armedbear/lisp/LispObject;")
(defconstant +closure-binding-array+ "[Lorg/armedbear/lisp/ClosureBinding;")
(defconstant +closure-binding-class+ "org/armedbear/lisp/ClosureBinding")
(defconstant +lisp-symbol-class+ "org/armedbear/lisp/Symbol")
(defconstant +lisp-symbol+ "Lorg/armedbear/lisp/Symbol;")
(defconstant +lisp-structure-object-class+ "org/armedbear/lisp/StructureObject")
(defconstant +lisp-thread-class+ "org/armedbear/lisp/LispThread")
(defconstant +lisp-thread+ "Lorg/armedbear/lisp/LispThread;")
(defconstant +lisp-load-class+ "org/armedbear/lisp/Load")
(defconstant +lisp-cons-class+ "org/armedbear/lisp/Cons")
(defconstant +lisp-cons+ "Lorg/armedbear/lisp/Cons;")
(defconstant +lisp-integer-class+ "org/armedbear/lisp/LispInteger")
(defconstant +lisp-integer+ "Lorg/armedbear/lisp/LispInteger;")
(defconstant +lisp-fixnum-class+ "org/armedbear/lisp/Fixnum")
(defconstant +lisp-fixnum+ "Lorg/armedbear/lisp/Fixnum;")
(defconstant +lisp-fixnum-array+ "[Lorg/armedbear/lisp/Fixnum;")
(defconstant +lisp-function-proxy-class+
  "org/armedbear/lisp/AutoloadedFunctionProxy")
(defconstant +lisp-bignum-class+ "org/armedbear/lisp/Bignum")
(defconstant +lisp-bignum+ "Lorg/armedbear/lisp/Bignum;")
(defconstant +lisp-single-float-class+ "org/armedbear/lisp/SingleFloat")
(defconstant +lisp-single-float+ "Lorg/armedbear/lisp/SingleFloat;")
(defconstant +lisp-double-float-class+ "org/armedbear/lisp/DoubleFloat")
(defconstant +lisp-double-float+ "Lorg/armedbear/lisp/DoubleFloat;")
(defconstant +lisp-character-class+ "org/armedbear/lisp/LispCharacter")
(defconstant +lisp-character+ "Lorg/armedbear/lisp/LispCharacter;")
(defconstant +lisp-character-array+ "[Lorg/armedbear/lisp/LispCharacter;")
(defconstant +lisp-abstract-bit-vector-class+ "org/armedbear/lisp/AbstractBitVector")
(defconstant +lisp-abstract-vector-class+ "org/armedbear/lisp/AbstractVector")
(defconstant +lisp-abstract-string-class+ "org/armedbear/lisp/AbstractString")
(defconstant +lisp-abstract-string+ "Lorg/armedbear/lisp/AbstractString;")
(defconstant +lisp-simple-vector-class+ "org/armedbear/lisp/SimpleVector")
(defconstant +lisp-simple-string-class+ "org/armedbear/lisp/SimpleString")
(defconstant +lisp-simple-string+ "Lorg/armedbear/lisp/SimpleString;")
(defconstant +lisp-environment+ "Lorg/armedbear/lisp/Environment;")
(defconstant +lisp-environment-class+ "org/armedbear/lisp/Environment")
(defconstant +lisp-special-binding+ "Lorg/armedbear/lisp/SpecialBinding;")
(defconstant +lisp-special-binding-class+ "org/armedbear/lisp/SpecialBinding")
(defconstant +lisp-special-bindings-mark+ "Lorg/armedbear/lisp/SpecialBindingsMark;")
(defconstant +lisp-special-bindings-mark-class+ "org/armedbear/lisp/SpecialBindingsMark")
(defconstant +lisp-throw-class+ "org/armedbear/lisp/Throw")
(defconstant +lisp-return-class+ "org/armedbear/lisp/Return")
(defconstant +lisp-go-class+ "org/armedbear/lisp/Go")
(defconstant +lisp-compiled-closure-class+ "org/armedbear/lisp/CompiledClosure")
(defconstant +lisp-primitive-class+ "org/armedbear/lisp/Primitive")
(defconstant +lisp-hash-table-class+ "org/armedbear/lisp/HashTable")
(defconstant +lisp-eql-hash-table-class+ "org/armedbear/lisp/EqlHashTable")
(defconstant +lisp-package-class+ "org/armedbear/lisp/Package")
(defconstant +lisp-readtable-class+ "org/armedbear/lisp/Readtable")
(defconstant +lisp-stream-class+ "org/armedbear/lisp/Stream")
(defconstant +lisp-closure-class+ "org/armedbear/lisp/Closure")
(defconstant +lisp-closure-parameter-class+ "org/armedbear/lisp/Closure$Parameter")
(defconstant +lisp-closure-parameter-array+ "[Lorg/armedbear/lisp/Closure$Parameter;")

(defstruct (instruction (:constructor %make-instruction (opcode args)))
  (opcode 0 :type (integer 0 255))
  args
  stack
  depth
  wide)

(defun make-instruction (opcode args)
  (let ((inst (apply #'%make-instruction
                     (list opcode
                           (remove :wide-prefix args)))))
    (when (memq :wide-prefix args)
      (setf (inst-wide inst) t))
    inst))

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

(defknown aload (fixnum) t)
(defun aload (index)
  (case index
    (0 (emit 'aload_0))
    (1 (emit 'aload_1))
    (2 (emit 'aload_2))
    (3 (emit 'aload_3))
    (t (emit 'aload index))))

(defknown astore (fixnum) t)
(defun astore (index)
  (case index
    (0 (emit 'astore_0))
    (1 (emit 'astore_1))
    (2 (emit 'astore_2))
    (3 (emit 'astore_3))
    (t (emit 'astore index))))

(defknown emit-push-nil () t)
(declaim (inline emit-push-nil))
(defun emit-push-nil ()
  (emit 'getstatic +lisp-class+ "NIL" +lisp-object+))

(defknown emit-push-nil-symbol () t)
(declaim (inline emit-push-nil-symbol))
(defun emit-push-nil-symbol ()
  (emit 'getstatic +lisp-nil-class+ "NIL" +lisp-symbol+))

(defknown emit-push-t () t)
(declaim (inline emit-push-t))
(defun emit-push-t ()
  (emit 'getstatic +lisp-class+ "T" +lisp-symbol+))

(defknown emit-push-false (t) t)
(defun emit-push-false (representation)
  (declare (optimize speed (safety 0)))
  (ecase representation
    (:boolean
     (emit 'iconst_0))
    ((nil)
     (emit-push-nil))))

(defknown emit-push-true (t) t)
(defun emit-push-true (representation)
  (declare (optimize speed (safety 0)))
  (ecase representation
    (:boolean
     (emit 'iconst_1))
    ((nil)
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
  (case n
    (0 (emit 'lconst_0))
    (1 (emit 'lconst_1))
    (t
     (emit 'ldc2_w (pool-long n)))))

(defknown emit-push-constant-float (single-float) t)
(defun emit-push-constant-float (n)
  (case n
    (0.0s0 (emit 'fconst_0))
    (1.0s0 (emit 'fconst_1))
    (2.0s0 (emit 'fconst_2))
    (t (emit 'ldc (pool-float n)))))

(defknown emit-push-constant-double (double-float) t)
(defun emit-push-constant-double (n)
  (case n
    (0.0d0 (emit 'dconst_0))
    (1.0d0 (emit 'dconst_1))
    (t (emit 'ldc2_w (pool-double n)))))

(defknown emit-dup (symbol) t)
(defun emit-dup (representation &key (past nil past-supplied-p))
  "Emits the 'dup' instruction required to duplicate `representation'.

If `past' is specified, the newly duplicated value is inserted on the
stack past the top-most value, which is assumed to be of the representation
passed in `past'."
  (emit
   (nth (if past-supplied-p
            (representation-size past) 0)
        (ecase (representation-size representation)
          (1 '(dup  dup_x1  dup_x2))
          (2 '(dup2 dup2_x1 dup2_x2))))))

(defknown emit-swap (symbol symbol) t)
(defun emit-swap (rep1 rep2)
  "Swaps 2 values on the stack,
the top-most value's representation being 'rep1'."
  (let ((r1-size (representation-size rep1))
        (r2-size (representation-size rep2)))
    (cond ((and (= 1 r1-size)
                (= 1 r2-size))
           (emit 'swap))
          ((and (= 1 r1-size)
                (= 2 r2-size))
           (emit 'dup2_x1)
           (emit 'pop2))
          ((and (= 2 r1-size)
                (= 1 r2-size))
           (emit 'dup_x2)
           (emit 'pop))
          ((and (= 2 r1-size)
                (= 2 r2-size))
           (emit 'dup2_x2)
           (emit 'pop2)))))

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
                                          ((or (equal return-type "J")
                                               (equal return-type "D")) 2)
                                          (t 1))))
                        (dolist (type arg-types result)
                          (decf result (if (or (equal type "J")
                                               (equal type "D"))
                                           2 1))))))
    (cons descriptor stack-effect)))

(defparameter *descriptors* (make-hash-table :test #'equal))

;; Just an experiment...
(defmacro defsubst (name lambda-list &rest body)
  (let* ((block-name (fdefinition-block-name name))
         (expansion (generate-inline-expansion block-name lambda-list body)))
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

(defvar type-representations '((:int fixnum)
                               (:long (integer #.most-negative-java-long
                                               #.most-positive-java-long))
                               (:float single-float)
                               (:double double-float)
                               (:char base-char character)
                               (:boolean boolean)
                               )
  "Lists the widest Lisp types to be stored in each of the Java primitives
supported (and used) by the compiler.")

(defun type-representation (the-type)
  "Converts a type specification or compiler type into a representation."
  (when (null the-type)
    (return-from type-representation))
  (do* ((types type-representations (cdr types)))
       ((endp types) nil)
    (do* ((type-list (cdr (car types)) (cdr type-list))
          (type (car type-list) (car type-list)))
         ((endp type-list))
      (when (or (subtypep the-type type)
                (compiler-subtypep the-type (make-compiler-type type)))
        (return-from type-representation (caar types))))))

(defun representation-size (representation)
  (ecase representation
    ((NIL :int :boolean :float :char) 1)
    ((:long :double) 2)))


(defknown emit-unbox-boolean () t)
(defun emit-unbox-boolean ()
  (emit 'instanceof +lisp-nil-class+)
  (emit 'iconst_1)
  (emit 'ixor))  ;; 1 -> 0 && 0 -> 1: in other words, negate the low bit

(defknown emit-unbox-character () t)
(defun emit-unbox-character ()
  (cond ((> *safety* 0)
         (emit-invokestatic +lisp-character-class+ "getValue"
                            (lisp-object-arg-types 1) "C"))
        (t
         (emit 'checkcast +lisp-character-class+)
         (emit 'getfield +lisp-character-class+ "value" "C"))))

;;                     source type /
;;                         targets   :boolean :char    :int :long :float :double
(defvar rep-conversion `((NIL      . #( ,#'emit-unbox-boolean
                                        ,#'emit-unbox-character
                                       "intValue" "longValue"
                                       "floatValue" "doubleValue"))
                         (:boolean . #( NIL    :err    :err  :err  :err   :err))
                         (:char    . #(  1     NIL     :err  :err  :err   :err))
                         (:int     . #(  1     :err     NIL  i2l   i2f    i2d))
                         (:long    . #(  1     :err     l2i  NIL   l2f    l2d))
                         (:float   . #(  1     :err    :err  :err  NIL    f2d))
                         (:double  . #(  1     :err    :err  :err  d2f    NIL)))
  "Contains a table with operations to be performed to do
internal representation conversion.")

(defvar rep-classes
  '((:boolean  #.+lisp-object-class+        #.+lisp-object+)
    (:char     #.+lisp-character-class+     #.+lisp-character+)
    (:int      #.+lisp-integer-class+       #.+lisp-integer+)
    (:long     #.+lisp-integer-class+       #.+lisp-integer+)
    (:float    #.+lisp-single-float-class+  #.+lisp-single-float+)
    (:double   #.+lisp-double-float-class+  #.+lisp-double-float+))
  "Lists the class on which to call the `getInstance' method on,
when converting the internal representation to a LispObject.")

(defvar rep-arg-chars
  '((:boolean . "Z")
    (:char    . "C")
    (:int     . "I")
    (:long    . "J")
    (:float   . "F")
    (:double  . "D"))
  "Lists the argument type identifiers for each
of the internal representations.")

(defun convert-representation (in out)
  "Converts the value on the stack in the `in' representation
to a value on the stack in the `out' representation."
  (when (eql in out)
    ;; no-op
    (return-from convert-representation))
  (when (null out)
    ;; Convert back to a lisp object
    (when in
      (let ((class (cdr (assoc in rep-classes)))
            (arg-spec (cdr (assoc in rep-arg-chars))))
        (emit-invokestatic (first class) "getInstance" (list arg-spec)
                           (second class))))
    (return-from convert-representation))
  (let* ((in-map (cdr (assoc in rep-conversion)))
         (op-num (position out '(:boolean :char :int :long :float :double)))
         (op (aref in-map op-num)))
    (when op
      ;; Convert from one internal representation into another
      (assert (neq op :err))
      (cond ((eql op 1)
             (emit-move-from-stack nil in)
             (emit 'iconst_1))
            ((functionp op)
             (funcall op))
            ((stringp op)
             (emit-invokevirtual +lisp-object-class+ op nil
                                 (cdr (assoc out rep-arg-chars))))
            (t
             (emit op))))))

(defvar common-representations '((:int :long :long)
                                 (:int :float :double)
                                 (:int :double :double)
                                 (:float :int :double)
                                 (:float :double :double)
                                 (:double :int :double)
                                 (:double :float :double))
  "Representations to convert unequal representations to, in order
to get the correct (exact where required) comparisons.")

(defun common-representation (rep1 rep2)
  (when (eq rep1 rep2)
    (return-from common-representation rep1))
  (do* ((remaining common-representations (cdr remaining))
        (rep (car remaining) (car remaining)))
       ((endp remaining))
    (destructuring-bind
          (r1 r2 result) rep
      (when (and (eq rep1 r1) (eq rep2 r2))
        (return-from common-representation result)))))



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
    (astore *thread*)
    (setf *initialize-thread-var* nil)))

(defknown ensure-thread-var-initialized () t)
(declaim (inline ensure-thread-var-initialized))
(defun ensure-thread-var-initialized ()
  (setf *initialize-thread-var* t))

(defknown emit-push-current-thread () t)
(defun emit-push-current-thread ()
  (declare (optimize speed))
  (ensure-thread-var-initialized)
  (aload *thread*))

(defun local-variable-p (variable)
  "Return non-NIL if `variable' is a local variable.

Special variables are not considered local."
  (or (variable-register variable) ;; either register or index
      (variable-index variable)))  ;; is non-nil for local variables

(defun emit-load-local-variable (variable)
  "Loads a local variable in the top stack position."
  (aver (local-variable-p variable))
  (if (variable-register variable)
      (aload (variable-register variable))
      (progn
        (aload (compiland-argument-register *current-compiland*))
        (emit-push-constant-int (variable-index variable))
        (emit 'aaload))))

(defun emit-push-variable-name (variable)
  (emit-load-externalized-object (variable-name variable)))

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
              ;###
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
    (aload (compiland-argument-register *current-compiland*))
    (emit 'arraylength)
    (emit-push-constant-int arity)
    (emit 'if_icmpeq label1)
    (aload 0) ; this
    (emit-invokevirtual *this-class* "argCountError" nil nil)
    (label label1)))

(defun maybe-generate-interrupt-check ()
  (unless (> *speed* *safety*)
    (let ((label1 (gensym)))
      (emit 'getstatic +lisp-class+ "interrupted" "Z")
      (emit 'ifeq label1)
      (emit-invokestatic +lisp-class+ "handleInterrupt" nil nil)
      (label label1))))

(defknown single-valued-p (t) t)
(defun single-valued-p (form)
  (cond ((node-p form)
         (if (tagbody-node-p form)
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
      (ensure-thread-var-initialized)
      (emit 'clear-values)
      (return))))

(defun compile-forms-and-maybe-emit-clear-values (&rest forms-and-compile-args)
  (let ((forms-for-emit-clear
	 (loop for (form arg1 arg2) on forms-and-compile-args by #'cdddr
	    do (compile-form form arg1 arg2)
	    collecting form)))
    (apply #'maybe-emit-clear-values forms-for-emit-clear)))

(defknown emit-unbox-fixnum () t)
(defun emit-unbox-fixnum ()
  (declare (optimize speed))
  (cond ((= *safety* 3)
         (emit-invokestatic +lisp-fixnum-class+ "getValue"
                            (lisp-object-arg-types 1) "I"))
        (t
         (emit 'checkcast +lisp-fixnum-class+)
         (emit 'getfield +lisp-fixnum-class+ "value" "I"))))

(defknown emit-unbox-long () t)
(defun emit-unbox-long ()
  (emit-invokestatic +lisp-bignum-class+ "longValue"
                     (lisp-object-arg-types 1) "J"))

(defknown emit-unbox-float () t)
(defun emit-unbox-float ()
  (declare (optimize speed))
  (cond ((= *safety* 3)
         (emit-invokestatic +lisp-single-float-class+ "getValue"
                            (lisp-object-arg-types 1) "F"))
        (t
         (emit 'checkcast +lisp-single-float-class+)
         (emit 'getfield +lisp-single-float-class+ "value" "F"))))

(defknown emit-unbox-double () t)
(defun emit-unbox-double ()
  (declare (optimize speed))
  (cond ((= *safety* 3)
         (emit-invokestatic +lisp-double-float-class+ "getValue"
                            (lisp-object-arg-types 1) "D"))
        (t
         (emit 'checkcast +lisp-double-float-class+)
         (emit 'getfield +lisp-double-float-class+ "value" "D"))))

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
         (emit-invokevirtual +lisp-object-class+ "longValue" nil "J"))
        ((eq required-representation :float)
         (emit-invokevirtual +lisp-object-class+ "floatValue" nil "F"))
        ((eq required-representation :double)
         (emit-invokevirtual +lisp-object-class+ "doubleValue" nil "D"))
        (t (assert nil))))

(defknown emit-move-from-stack (t &optional t) t)
(defun emit-move-from-stack (target &optional representation)
  (declare (optimize speed))
  (cond ((null target)
         (ecase representation
           ((:long :double)
            (emit 'pop2))
           ((NIL :int :boolean :char :float)
            (emit 'pop))))
        ((eq target 'stack)) ; Nothing to do.
        ((fixnump target)
         ;; A register.
         (emit (ecase representation
                 ((:int :boolean :char)
                          'istore)
                 (:long   'lstore)
                 (:float  'fstore)
                 (:double 'dstore)
                 ((nil)   'astore))
               target))
        (t
         (sys::%format t "emit-move-from-stack general case~%")
         (aver nil))))

;; Expects value on stack.
(defknown emit-invoke-method (t t t) t)
(defun emit-invoke-method (method-name target representation)
  (emit-invokevirtual +lisp-object-class+ method-name nil +lisp-object+)
  (fix-boxing representation nil)
  (emit-move-from-stack target representation))

;; "In addition to situations for which the standard specifies that conditions
;; of type WARNING must or might be signaled, warnings might be signaled in
;; situations where the compiler can determine that the consequences are
;; undefined or that a run-time error will be signaled. Examples of this
;; situation are as follows: violating type declarations, altering or assigning
;; the value of a constant defined with DEFCONSTANT, calling built-in Lisp
;; functions with a wrong number of arguments or malformed keyword argument
;; lists, and using unrecognized declaration specifiers." (3.2.5)
(defun check-number-of-args (form n &optional minimum)
  (declare (type fixnum n))
  (let* ((op (car form))
         (args (cdr form))
         (ok (if minimum
		 (>= (length args) n)
	       (= (length args) n))))
    (declare (type boolean ok))
    (unless ok
      (funcall (if (eq (symbol-package op) +cl-package+)
                   #'compiler-warn ; See above!
                   #'compiler-style-warn)
               "Wrong number of arguments for ~A (expected~:[~; at least~] ~D, but received ~D)."
               op minimum n (length args)))
    ok))

(defknown check-arg-count (t fixnum) t)
(defun check-arg-count (form n)
  (check-number-of-args form n))

(declaim (ftype (function (t fixnum) t) check-min-args))
(defun check-min-args (form n)
  (check-number-of-args form n t))

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
                 11 ; fconst_0
                 12 ; fconst_1
                 13 ; fconst_2
                 14 ; dconst_0
                 15 ; dconst_1
                 42 ; aload_0
                 43 ; aload_1
                 44 ; aload_2
                 45 ; aload_3
                 46 ; iaload
                 47 ; laload
                 48 ; faload
                 49 ; daload
                 50 ; aaload
                 75 ; astore_0
                 76 ; astore_1
                 77 ; astore_2
                 78 ; astore_3
                 79 ; iastore
                 80 ; lastore
                 81 ; fastore
                 82 ; dastore
                 83 ; aastore
                 87 ; pop
                 88 ; pop2
                 89 ; dup
                 90 ; dup_x1
                 91 ; dup_x2
                 92 ; dup2
                 93 ; dup2_x1
                 94 ; dup2_x2
                 95 ; swap
                 96 ; iadd
                 97 ; ladd
                 98 ; fadd
                 99 ; dadd
                 100 ; isub
                 101 ; lsub
                 102 ; fsub
                 103 ; dsub
                 104 ; imul
                 105 ; lmul
                 106 ; fmul
                 107 ; dmul
                 116 ; ineg
                 117 ; lneg
                 118 ; fneg
                 119 ; dneg
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
                 134 ; i2f
                 135 ; i2d
                 136 ; l2i
                 137 ; l2f
                 138 ; l2d
                 141 ; f2d
                 144 ; d2f
                 148 ; lcmp
                 149 ; fcmpd
                 150 ; fcmpg
                 151 ; dcmpd
                 152 ; dcmpg
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
                 176 ; areturn
                 177 ; return
                 190 ; arraylength
                 191 ; athrow
                 194 ; monitorenter
                 195 ; monitorexit
                 198 ; ifnull
                 202 ; label
                 ))
      (setf (gethash n ht) nil))))

(initialize-resolvers)

(defmacro define-resolver (opcodes args &body body)
  (let ((name (gensym)))
    `(progn
       (defun ,name ,args ,@body)
       (eval-when (:load-toplevel :execute)
	 ,(if (listp opcodes)
	      `(dolist (op ',opcodes)
		 (setf (gethash op +resolvers+) (symbol-function ',name)))
	      `(setf (gethash ,opcodes +resolvers+) (symbol-function ',name)))))))

(defun load/store-resolver (instruction inst-index inst-index2 error-text)
 (let* ((args (instruction-args instruction))
        (index (car args)))
   (declare (type (unsigned-byte 16) index))
   (cond ((<= 0 index 3)
          (inst (+ index inst-index)))
         ((<= 0 index 255)
          (inst inst-index2 index))
         (t
          (error error-text)))))

;; aload
(define-resolver 25 (instruction)
  (load/store-resolver instruction 42 25 "ALOAD unsupported case"))

;; astore
(define-resolver 58 (instruction)
  (load/store-resolver instruction 75 58 "ASTORE unsupported case"))

;; iload
(define-resolver 21 (instruction)
  (load/store-resolver instruction 26 21 "ILOAD unsupported case"))

;; istore
(define-resolver 54 (instruction)
  (load/store-resolver instruction 59 54 "ISTORE unsupported case"))

;; lload
(define-resolver 22 (instruction)
  (load/store-resolver instruction 30 22 "LLOAD unsupported case"))

;; lstore
(define-resolver 55 (instruction)
  (load/store-resolver instruction 63 55 "LSTORE unsupported case"))

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
           (inst 17 (s2 n))))))

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
  (let* ((args (instruction-args instruction)))
    (unless (= (length args) 1)
      (error "Wrong number of args for LDC2_W."))
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
    (when (not (<= -128 n 127))
      (error "IINC argument ~A out of bounds." n))
    (inst 132 (list register (s1 n)))))

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
                                         +lisp-object-array+)))))
             (dolist (instruction instructions)
               (vector-push-extend (resolve-instruction instruction) vector))))
          (t
           (vector-push-extend (resolve-instruction instruction) vector)))))))

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
          (internal-compiler-error 
           "Stack inconsistency detected in ~A at index ~D: found ~S, expected ~S." 
           (compiland-name *current-compiland*)
           i instruction-depth (+ depth instruction-stack)))
        (return-from walk-code))
      (let ((opcode (instruction-opcode instruction)))
        (setf depth (+ depth instruction-stack))
        (setf (instruction-depth instruction) depth)
        (when (branch-opcode-p opcode)
          (let ((label (car (instruction-args instruction))))
            (declare (type symbol label))
            (walk-code code (symbol-value label) depth)))
        (when (member opcode '(167 176 191)) ; GOTO ARETURN ATHROW
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
      max-stack)))


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
  (and instruction
       (= (the fixnum (instruction-opcode (the instruction instruction))) 202)))

(declaim (ftype (function (t) t) instruction-label))
(defun instruction-label (instruction)
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
                           ((eq (car (instruction-args next-instruction))
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
              (setf (instruction-args instruction) (s2 offset))))
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
  (write-8-bits (logand (ash n -8) #xFF) stream)
  (write-8-bits (logand n #xFF) stream))

(defknown write-u4 (integer stream) t)
(defun write-u4 (n stream)
  (declare (optimize speed))
  (declare (type (unsigned-byte 32) n))
  (write-u2 (logand (ash n -16) #xFFFF) stream)
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
      ((3 4) ; int
       (write-u4 (second entry) stream))
      ((5 6) ; long double
       (write-u4 (second entry) stream)
       (write-u4 (third entry) stream))
      ((9 10 11 12) ; fieldref methodref InterfaceMethodref nameAndType
       (write-u2 (second entry) stream)
       (write-u2 (third entry) stream))
      ((7 8) ; class string
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

(defstruct (java-method (:conc-name method-) (:constructor %make-method))
  access-flags
  name
  descriptor
  name-index
  descriptor-index
  max-stack
  max-locals
  code
  handlers)

(defun make-method (&rest args &key descriptor name
                                    descriptor-index name-index
                               &allow-other-keys)
  (apply #'%make-method
         (list* :descriptor-index (or descriptor-index (pool-name descriptor))
                :name-index (or name-index (pool-name name))
                args)))

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

(defun emit-read-from-string (object)
  (emit-constructor-lambda-list object))

(defun make-constructor (super lambda-name args)
  (let* ((*compiler-debug* nil)
         ;; We don't normally need to see debugging output for constructors.
         (constructor (make-method :name "<init>"
                                   :descriptor "()V"))
         req-params-register
         opt-params-register
         key-params-register
         rest-p
         keys-p
         more-keys-p
         (*code* ())
         (*handlers* nil))
    (setf (method-max-locals constructor) 1)
    (unless (equal super +lisp-primitive-class+)
      (multiple-value-bind
            (req opt key key-p rest
                 allow-other-keys-p)
          (parse-lambda-list args)
        (setf rest-p rest
              more-keys-p allow-other-keys-p
              keys-p key-p)
        (macrolet
            ((parameters-to-array ((param params register) &body body)
               (let ((count-sym (gensym)))
                 `(progn
                    (emit-push-constant-int (length ,params))
                    (emit 'anewarray +lisp-closure-parameter-class+)
                    (astore (setf ,register (method-max-locals constructor)))
                    (incf (method-max-locals constructor))
                    (do* ((,count-sym 0 (1+ ,count-sym))
                          (,params ,params (cdr ,params))
                          (,param (car ,params) (car ,params)))
                        ((endp ,params))
                      (declare (ignorable ,param))
                      (aload ,register)
                      (emit-push-constant-int ,count-sym)
                      (emit 'new +lisp-closure-parameter-class+)
                      (emit 'dup)
                      ,@body
                      (emit 'aastore))))))
          ;; process required args
          (parameters-to-array (ignore req req-params-register)
             (emit-push-t) ;; we don't need the actual symbol
             (emit-invokespecial-init +lisp-closure-parameter-class+
                                      (list +lisp-symbol+)))

          (parameters-to-array (param opt opt-params-register)
             (emit-push-t) ;; we don't need the actual variable-symbol
             (emit-read-from-string (second param)) ;; initform
             (if (null (third param))               ;; supplied-p
                 (emit-push-nil)
                 (emit-push-t)) ;; we don't need the actual supplied-p symbol
             (emit 'getstatic +lisp-closure-class+ "OPTIONAL" "I")
             (emit-invokespecial-init +lisp-closure-parameter-class+
                                      (list +lisp-symbol+ +lisp-object+
                                            +lisp-object+ "I")))

          (parameters-to-array (param key key-params-register)
             (let ((keyword (fourth param)))
               (if (keywordp keyword)
                   (progn
                     (emit 'ldc (pool-string (symbol-name keyword)))
                     (emit-invokestatic +lisp-class+ "internKeyword"
                                        (list +java-string+) +lisp-symbol+))
                   ;; symbol is not really a keyword; yes, that's allowed!
                   (progn
                     (emit 'ldc (pool-string (symbol-name keyword)))
                     (emit 'ldc (pool-string
                                 (package-name (symbol-package keyword))))
                     (emit-invokestatic +lisp-class+ "internInPackage"
                                        (list +java-string+ +java-string+)
                                        +lisp-symbol+))))
             (emit-push-t) ;; we don't need the actual variable-symbol
             (emit-read-from-string (second (car key)))
             (if (null (third param))
                 (emit-push-nil)
                 (emit-push-t)) ;; we don't need the actual supplied-p symbol
             (emit-invokespecial-init +lisp-closure-parameter-class+
                                      (list +lisp-symbol+ +lisp-symbol+
                                            +lisp-object+ +lisp-object+))))))
    (aload 0) ;; this
    (cond ((equal super +lisp-primitive-class+)
           (emit-constructor-lambda-name lambda-name)
           (emit-constructor-lambda-list args)
           (emit-invokespecial-init super (lisp-object-arg-types 2)))
          ((equal super +lisp-compiled-closure-class+)
           (aload req-params-register)
           (aload opt-params-register)
           (aload key-params-register)
           (if keys-p
               (emit-push-t)
               (emit-push-nil-symbol))
           (if rest-p
               (emit-push-t)
               (emit-push-nil-symbol))
           (if more-keys-p
               (emit-push-t)
               (emit-push-nil-symbol))
           (emit-invokespecial-init super
                                    (list +lisp-closure-parameter-array+
                                          +lisp-closure-parameter-array+
                                          +lisp-closure-parameter-array+
                                          +lisp-symbol+
                                          +lisp-symbol+ +lisp-symbol+)))
          (t
           (aver nil)))
    (setf *code* (append *static-code* *code*))
    (emit 'return)
    (finalize-code)
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

(defconst +field-flag-final+       #x10) ;; final field
(defconst +field-flag-static+      #x08) ;; static field
(defconst +field-access-protected+ #x04) ;; subclass accessible
(defconst +field-access-private+   #x02) ;; class-only accessible
(defconst +field-access-public+    #x01) ;; generally accessible
(defconst +field-access-default+   #x00) ;; package accessible, used for LABELS

(defknown declare-field (t t t) t)
(defun declare-field (name descriptor access-flags)
  (let ((field (make-field name descriptor)))
    ;; final static <access-flags>
    (setf (field-access-flags field)
          (logior +field-flag-final+ +field-flag-static+ access-flags))
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

(defvar *declare-inline* nil)

(defmacro declare-with-hashtable (declared-item hashtable hashtable-var
				  item-var &body body)
  `(let* ((,hashtable-var ,hashtable)
	  (,item-var (gethash1 ,declared-item ,hashtable-var)))
     (declare (type hash-table ,hashtable-var))
     (unless ,item-var
       ,@body)
     ,item-var))

;; The protocol of the serialize-* functions is to serialize
;; the type to which they apply and emit code which leaves the
;; restored object on the stack.

;; The functions may generate only Java code, or decide to defer
;; some of the process of restoring the object to the reader. The
;; latter is generally applicable to more complex structures.

;; This way, the serialize-* functions can be used to depend on
;; each other to serialize nested constructs. They are also the
;; building blocks of the EMIT-LOAD-EXTERNALIZED-OBJECT function,
;; which is called from the compiler.

(defun serialize-integer (n)
  "Generates code to restore a serialized integer."
  (cond((<= 0 n 255)
        (emit 'getstatic +lisp-fixnum-class+ "constants" +lisp-fixnum-array+)
        (emit-push-constant-int n)
        (emit 'aaload))
       ((<= most-negative-fixnum n most-positive-fixnum)
        (emit-push-constant-int n)
        (emit-invokestatic +lisp-fixnum-class+ "getInstance"
                           '("I") +lisp-fixnum+))
       ((<= most-negative-java-long n most-positive-java-long)
        (emit-push-constant-long n)
        (emit-invokestatic +lisp-bignum-class+ "getInstance"
                           '("J") +lisp-integer+))
       (t
        (let* ((*print-base* 10)
               (s (with-output-to-string (stream) (dump-form n stream))))
          (emit 'ldc (pool-string s))
          (emit-push-constant-int 10)
          (emit-invokestatic +lisp-bignum-class+ "getInstance"
                             (list +java-string+ "I") +lisp-integer+)))))

(defun serialize-character (c)
  "Generates code to restore a serialized character."
  (emit-push-constant-int (char-code c))
  (emit-invokestatic +lisp-character-class+ "getInstance" '("C")
                     +lisp-character+))

(defun serialize-float (s)
  "Generates code to restore a serialized single-float."
  (emit 'new +lisp-single-float-class+)
  (emit 'dup)
  (emit 'ldc (pool-float s))
  (emit-invokespecial-init +lisp-single-float-class+ '("F")))

(defun serialize-double (d)
  "Generates code to restore a serialized double-float."
  (emit 'new +lisp-double-float-class+)
  (emit 'dup)
  (emit 'ldc2_w (pool-double d))
  (emit-invokespecial-init +lisp-double-float-class+ '("D")))

(defun serialize-string (string)
  "Generate code to restore a serialized string."
  (emit 'new +lisp-simple-string-class+)
  (emit 'dup)
  (emit 'ldc (pool-string string))
  (emit-invokespecial-init +lisp-simple-string-class+ (list +java-string+)))

(defun serialize-package (pkg)
  "Generate code to restore a serialized package."
  (emit 'ldc (pool-string (concatenate 'string "#.(FIND-PACKAGE \""
                                       (package-name pkg) "\")")))
  (emit-invokestatic +lisp-class+ "readObjectFromString"
                     (list +java-string+) +lisp-object+))

(defun serialize-object (object)
  "Generate code to restore a serialized object which is not of any
of the other types."
  (let ((s (with-output-to-string (stream)
             (dump-form object stream))))
    (emit 'ldc (pool-string s))
    (emit-invokestatic +lisp-class+ "readObjectFromString"
                       (list +java-string+) +lisp-object+)))

(defun serialize-symbol (symbol)
  "Generate code to restore a serialized symbol."
  (multiple-value-bind
        (name class)
      (lookup-known-symbol symbol)
    (cond
      (name
       (emit 'getstatic class name +lisp-symbol+))
      ((null (symbol-package symbol))
       (emit-push-constant-int (dump-uninterned-symbol-index symbol))
       (emit-invokestatic +lisp-load-class+ "getUninternedSymbol" '("I")
                          +lisp-object+)
       (emit 'checkcast +lisp-symbol-class+))
      ((keywordp symbol)
       (emit 'ldc (pool-string (symbol-name symbol)))
       (emit-invokestatic +lisp-class+ "internKeyword"
                          (list +java-string+) +lisp-symbol+))
      (t
       (emit 'ldc (pool-string (symbol-name symbol)))
       (emit 'ldc (pool-string (package-name (symbol-package symbol))))
       (emit-invokestatic +lisp-class+ "internInPackage"
                          (list +java-string+ +java-string+)
                          +lisp-symbol+)))))

(defvar serialization-table
  `((integer "INT" ,#'eql ,#'serialize-integer ,+lisp-integer+)
    (character "CHR" ,#'eql ,#'serialize-character ,+lisp-character+)
    (single-float "FLT" ,#'eql ,#'serialize-float ,+lisp-single-float+)
    (double-float "DBL" ,#'eql ,#'serialize-double ,+lisp-double-float+)
    (string "STR" ,#'equal ,#'serialize-string
            ,+lisp-abstract-string+) ;; because of (not compile-file)
    (package "PKG" ,#'eq ,#'serialize-package ,+lisp-object+)
    (symbol "SYM" ,#'eq ,#'serialize-symbol ,+lisp-symbol+)
    (T "OBJ" ,#'eq ,#'serialize-object ,+lisp-object+))
  "A list of 5-element lists. The elements of the sublists mean:

1. The type of the value to be serialized
2. The string to be used as a field prefix
3. The function to be used to determine equality (coalescing or not)
4. The function to dispatch serialization to
5. The type of the field to save the serialized result to")

(defknown emit-load-externalized-object (t) string)
(defun emit-load-externalized-object (object &optional cast)
  "Externalizes `object' for use in a FASL.

Returns the name of the field (in `*this-class*') from which
the value of the object can be loaded. Objects may be coalesced based
on the equality indicator in the `serialization-table'.

Code to restore the serialized object is inserted into `*code' or
`*static-code*' if `*declare-inline*' is non-nil.
"
  ;; TODO: rewrite to become EMIT-LOAD-EXTERNALIZED-OBJECT which
  ;; - instead of returning the name of the field - returns the type
  ;; of the field it just loaded (to allow casting and what not).
  ;; The function should still do what it does today: de-serialize the
  ;; object and storing its value.

  (destructuring-bind
        (type prefix similarity-fn dispatch-fn field-type)
      (assoc-if #'(lambda (x)
                    (typep object x))
                serialization-table)
    (declare (ignore type)) ;; the type has been used in the selection process
    (when (not *file-compilation*) ;; in-memory compilation wants object EQ-ness
      (setf similarity-fn #'eq))
    (let ((existing (assoc object *externalized-objects* :test similarity-fn)))
      (when existing
        (emit 'getstatic *this-class* (cdr existing) field-type)
        (when cast
          (emit 'checkcast cast))
        (return-from emit-load-externalized-object field-type)))

    ;; We need to set up the serialized value
    (let ((field-name (symbol-name (gensym prefix))))
      (declare-field field-name field-type +field-access-private+)
      (push (cons object field-name) *externalized-objects*)

      (cond
        ((not *file-compilation*)
         (let ((*code* *static-code*))
           (remember field-name object)
           (emit 'ldc (pool-string field-name))
           (emit-invokestatic +lisp-class+ "recall"
                              (list +java-string+) +lisp-object+)
           (when (string/= field-type +lisp-object+)
             (emit 'checkcast (subseq field-type 1 (1- (length field-type)))))
           (emit 'putstatic *this-class* field-name field-type)
           (setf *static-code* *code*)))
        (*declare-inline*
         (funcall dispatch-fn object)
         (emit 'putstatic *this-class* field-name field-type))
        (t
         (let ((*code* *static-code*))
           (funcall dispatch-fn object)
           (emit 'putstatic *this-class* field-name field-type)
           (setf *static-code* *code*))))

      (emit 'getstatic *this-class* field-name field-type)
      (when cast
        (emit 'checkcast cast))
      field-type)))

(defknown declare-function (symbol &optional setf) string)
(defun declare-function (symbol &optional setf)
  (declare (type symbol symbol))
  (declare-with-hashtable
   symbol *declared-functions* ht f
   (setf f (symbol-name (if setf (gensym "SETF") (gensym "FUN"))))
   (let ((s (sanitize symbol)))
     (when s
       (setf f (concatenate 'string f "_" s))))
   (declare-field f +lisp-object+ +field-access-private+)
   (multiple-value-bind
         (name class)
       (lookup-known-symbol symbol)
     ;; This is a work-around for the fact that
     ;; EMIT-LOAD-EXTERNALIZED-OBJECT can't be used due to the fact that
     ;; here we won't know where to send the code yet (the LET
     ;; selects between *code* and *static-code*, while
     ;; EMIT-LOAD-EXTERNALIZED-OBJECT wants to modify those specials too
     (unless name
        (setf name (if *file-compilation*
                       (declare-object-as-string symbol)
                       (declare-object symbol))
              class *this-class*))
     (let (saved-code)
       (let ((*code* (if *declare-inline* *code* *static-code*)))
         (if (eq class *this-class*)
             (progn ;; generated by the DECLARE-OBJECT*'s above
               (emit 'getstatic class name +lisp-object+)
               (emit 'checkcast +lisp-symbol-class+))
             (emit 'getstatic class name +lisp-symbol+))
         (emit-invokevirtual +lisp-symbol-class+
                             (if setf
                                 "getSymbolSetfFunctionOrDie"
                                 "getSymbolFunctionOrDie")
                             nil +lisp-object+)
         ;; make sure we're not cacheing a proxied function
         ;; (AutoloadedFunctionProxy) by allowing it to resolve itself
         (emit-invokevirtual +lisp-object-class+
                             "resolve" nil +lisp-object+)
         (emit 'putstatic *this-class* f +lisp-object+)
         (if *declare-inline*
             (setf saved-code *code*)
             (setf *static-code* *code*))
         (setf (gethash symbol ht) f))
       (when *declare-inline*
         (setf *code* saved-code))
       f))))

(defknown declare-setf-function (name) string)
(defun declare-setf-function (name)
  (declare-function (cadr name) t))


(defknown declare-local-function (local-function) string)
(defun declare-local-function (local-function)
  (declare-with-hashtable
   local-function *declared-functions* ht g
   (setf g (symbol-name (gensym "LFUN")))
   (let* ((pathname (abcl-class-file-pathname (local-function-class-file local-function)))
	  (class-name (concatenate 'string "org/armedbear/lisp/" (pathname-name pathname)))
	  (*code* *static-code*))
     ;; fixme *declare-inline*
     (declare-field g +lisp-object+ +field-access-private+)
     (emit 'new class-name)
     (emit 'dup)
     (emit-invokespecial-init class-name '())

     ;(emit 'ldc (pool-string (pathname-name pathname)))
     ;(emit-invokestatic +fasl-loader-class+ "faslLoadFunction"
     ;(list +java-string+) +lisp-object+)

;     (emit 'ldc (pool-string (file-namestring pathname)))
     
;     (emit-invokestatic +lisp-function-proxy-class+ "loadPreloadedFunction"
;			(list +java-string+) +lisp-object+)
     (emit 'putstatic *this-class* g +lisp-object+)
     (setf *static-code* *code*)
     (setf (gethash local-function ht) g))))


(defknown declare-object-as-string (t) string)
(defun declare-object-as-string (obj)
  ;; TODO: replace with emit-load-externalized-object
  ;; just replacing won't work however:
  ;;  field identification in Java includes the field type
  ;;  and we're not letting the caller know about the type of
  ;;  field we're creating in emit-load-externalized-object.
  ;;  The solution is to rewrite externalize-object to
  ;;  EMIT-LOAD-EXTERNALIZED-OBJECT, which serializes *and*
  ;;  emits the right loading code (not just de-serialization anymore)
  (let (saved-code
        (g (symbol-name (gensym "OBJSTR"))))
    (let* ((s (with-output-to-string (stream) (dump-form obj stream)))
           (*code* (if *declare-inline* *code* *static-code*)))
      ;; strings may contain evaluated bits which may depend on
      ;; previous statements
      (declare-field g +lisp-object+ +field-access-private+)
      (emit 'ldc (pool-string s))
      (emit-invokestatic +lisp-class+ "readObjectFromString"
                         (list +java-string+) +lisp-object+)
      (emit 'putstatic *this-class* g +lisp-object+)
      (if *declare-inline*
          (setf saved-code *code*)
          (setf *static-code* *code*)))
    (when *declare-inline*
      (setf *code* saved-code))
    g))

(defun declare-load-time-value (obj)
  (let ((g (symbol-name (gensym "LTV")))
        saved-code)
    (let* ((s (with-output-to-string (stream) (dump-form obj stream)))
           (*code* (if *declare-inline* *code* *static-code*)))
      ;; The readObjectFromString call may require evaluation of
      ;; lisp code in the string (think #.() syntax), of which the outcome
      ;; may depend on something which was declared inline
      (declare-field g +lisp-object+ +field-access-private+)
      (emit 'ldc (pool-string s))
      (emit-invokestatic +lisp-class+ "readObjectFromString"
                         (list +java-string+) +lisp-object+)
      (emit-invokestatic +lisp-class+ "loadTimeValue"
                         (lisp-object-arg-types 1) +lisp-object+)
      (emit 'putstatic *this-class* g +lisp-object+)
      (if *declare-inline*
          (setf saved-code *code*)
          (setf *static-code* *code*)))
    (when *declare-inline*
      (setf *code* saved-code))
    g))

(declaim (ftype (function (t &optional t) string) declare-object))
(defun declare-object (obj &optional (obj-ref +lisp-object+)
                           obj-class)
  "Stores the object OBJ in the object-lookup-table,
loading the object value into a field upon class-creation time.

The field type of the object is specified by OBJ-REF."
  (let ((g (symbol-name (gensym "OBJ"))))
    ;; fixme *declare-inline*?
    (remember g obj)
    (let* ((*code* *static-code*))
      (declare-field g obj-ref +field-access-private+)
      (emit 'ldc (pool-string g))
      (emit-invokestatic +lisp-class+ "recall"
                         (list +java-string+) +lisp-object+)
      (when (and obj-class (string/= obj-class +lisp-object-class+))
        (emit 'checkcast obj-class))
      (emit 'putstatic *this-class* g obj-ref)
      (setf *static-code* *code*)
      g)))

(defknown compile-constant (t t t) t)
(defun compile-constant (form target representation)
  (unless target
    (return-from compile-constant))
  (ecase representation
    (:int
     (cond ((fixnump form)
            (emit-push-constant-int form))
           ((integerp form)
            (emit-load-externalized-object form)
            (emit-invokevirtual +lisp-object-class+ "intValue" nil "I"))
           (t
            (sys::%format t "compile-constant int representation~%")
            (assert nil)))
     (emit-move-from-stack target representation)
     (return-from compile-constant))
    (:long
     (cond ((<= most-negative-java-long form most-positive-java-long)
            (emit-push-constant-long form))
           ((integerp form)
            (emit-load-externalized-object form)
            (emit-invokevirtual +lisp-object-class+ "longValue" nil "J"))
           (t
            (sys::%format t "compile-constant long representation~%")
            (assert nil)))
     (emit-move-from-stack target representation)
     (return-from compile-constant))
    (:char
     (cond ((characterp form)
            (emit-push-constant-int (char-code form))
            (emit-move-from-stack target representation)
            (return-from compile-constant))
           (t
            (sys::%format t "compile-constant :char representation~%")
            (assert nil))))
    (:boolean
     (emit (if form 'iconst_1 'iconst_0))
     (emit-move-from-stack target representation)
     (return-from compile-constant))
    (:float
     (cond ((integerp form)
            (emit-push-constant-float (coerce form 'single-float)))
           ((typep form 'single-float)
            (emit-push-constant-float form))
           ((typep form 'double-float)
            (emit-push-constant-double form)
            (emit 'd2f))
           (t
            (sys::%format t "compile-constant :float representation~%")
            (assert nil)))
     (emit-move-from-stack target representation)
     (return-from compile-constant))
    (:double
     (cond ((or (integerp form)
                (typep form 'single-float))
            (emit-push-constant-double (coerce form 'double-float)))
           ((typep form 'double-float)
            (emit-push-constant-double form))
           (t
            (sys::%format t "compile-constant :double representation~%")
            (assert nil)))
     (emit-move-from-stack target representation)
     (return-from compile-constant))
    ((NIL)))
  (cond ((or (numberp form)
             (typep form 'single-float)
             (typep form 'double-float)
             (characterp form)
             (stringp form)
             (packagep form)
             (pathnamep form)
             (vectorp form)
             (structure-object-p form)
             (standard-object-p form)
             (java:java-object-p form))
         (emit-load-externalized-object form))
        (t
         (if *file-compilation*
             (error "COMPILE-CONSTANT unhandled case ~S" form)
             (emit-load-externalized-object form))))
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
                    (SYS::%LENGTH    "LENGTH")
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

(defmacro define-inlined-function (name params preamble-and-test &body body)
  (let* ((test (second preamble-and-test))
	 (preamble (and test (first preamble-and-test)))
	 (test (or test (first preamble-and-test))))
    `(defun ,name ,params
       ,preamble
       (unless ,test
	 (compile-function-call ,@params)
	 (return-from ,name))
       ,@body)))

(defknown p2-predicate (t t t) t)
(define-inlined-function p2-predicate (form target representation)
  ((= (length form) 2))
  (let* ((op (car form))
         (info (gethash op *predicates*))
         (boxed-method-name (car info))
         (unboxed-method-name (cdr info)))
    (cond ((and boxed-method-name unboxed-method-name)
           (let ((arg (cadr form)))
	     (compile-forms-and-maybe-emit-clear-values arg 'stack nil)
             (ecase representation
               (:boolean
                (emit-invokevirtual +lisp-object-class+
                                    unboxed-method-name
                                    nil "Z"))
               ((NIL)
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
            (progn
              (sys::%format t "emit-push-int~%")
              (aver nil))))))

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
(define-inlined-function p2-eq/neq (form target representation)
  ((aver (or (null representation) (eq representation :boolean)))
   (check-arg-count form 2))
  (let* ((op (%car form))
         (args (%cdr form))
         (arg1 (%car args))
         (arg2 (%cadr args)))
    (compile-forms-and-maybe-emit-clear-values arg1 'stack nil
					       arg2 'stack nil)
     (let ((LABEL1 (gensym))
           (LABEL2 (gensym)))
       (emit (if (eq op 'EQ) 'if_acmpne 'if_acmpeq) LABEL1)
       (emit-push-true representation)
       (emit 'goto LABEL2)
       (label LABEL1)
       (emit-push-false representation)
       (label LABEL2))
     (emit-move-from-stack target representation))
   t)

(defun emit-ifne-for-eql (representation instruction-type)
  (emit-invokevirtual +lisp-object-class+ "eql" instruction-type "Z")
  (convert-representation :boolean representation))

(defknown p2-eql (t t t) t)
(define-inlined-function p2-eql (form target representation)
  ((aver (or (null representation) (eq representation :boolean)))
   (check-arg-count form 2))
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
             (emit 'if_icmpeq label1)
             (emit-push-false representation)
             (emit 'goto label2)
             (label label1)
             (emit-push-true representation)
             (label label2)))
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
           (ecase representation
             (:boolean
              (emit-invokevirtual +lisp-object-class+ "eql"
                                  (lisp-object-arg-types 1) "Z"))
             ((NIL)
              (emit-invokevirtual +lisp-object-class+ "EQL"
                                  (lisp-object-arg-types 1) +lisp-object+)))))
    (emit-move-from-stack target representation)))

(defknown p2-memq (t t t) t)
(define-inlined-function p2-memq (form target representation)
  ((check-arg-count form 2))
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
(define-inlined-function p2-memql (form target representation)
  ((check-arg-count form 2))
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
                 (emit-load-externalized-object op)
                 (aload 0)))
            (t
             (emit-load-externalized-object op)))
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
  (fix-boxing representation nil)
  (emit-move-from-stack target))


(defun duplicate-closure-array (compiland)
  (let* ((*register* *register*)
         (register (allocate-register)))
    (aload (compiland-closure-register compiland))        ;; src
    (emit-push-constant-int 0)                            ;; srcPos
    (emit-push-constant-int (length *closure-variables*))
    (emit 'anewarray +closure-binding-class+)             ;; dest
    (emit 'dup)
    (astore register)  ;; save dest value
    (emit-push-constant-int 0)                            ;; destPos
    (emit-push-constant-int (length *closure-variables*)) ;; length
    (emit-invokestatic "java/lang/System" "arraycopy"
                       (list +java-object+ "I"
                             +java-object+ "I" "I") nil)
    (aload register))) ;; reload dest value



(defknown compile-local-function-call (t t t) t)
(defun compile-local-function-call (form target representation)
  "Compiles a call to a function marked as `*child-p*'; a local function.

Functions this applies to can be FLET, LABELS, LAMBDA or NAMED-LAMBDA.
Note: DEFUN implies a named lambda."
  (let* ((compiland *current-compiland*)
         (op (car form))
         (args (cdr form))
         (local-function (find-local-function op))
         (*register* *register*))
    (cond ((local-function-variable local-function)
           ;; LABELS
           (dformat t "compile-local-function-call LABELS case variable = ~S~%"
                   (variable-name (local-function-variable local-function)))
           (compile-var-ref (make-var-ref
                             (local-function-variable local-function))
                            'stack nil))
          ((local-function-environment local-function)
           (assert (local-function-references-allowed-p local-function))
           (assert (not *file-compilation*))
           (emit-load-externalized-object
            (local-function-environment local-function)
            +lisp-environment-class+)
           (emit-load-externalized-object (local-function-name local-function))
           (emit-invokevirtual +lisp-environment-class+ "lookupFunction"
                               (list +lisp-object+)
                               +lisp-object+))
          (t
           (dformat t "compile-local-function-call default case~%")
           (let* ((g (if *file-compilation*
                         (declare-local-function local-function)
                         (declare-object
                          (local-function-function local-function)))))
             (emit 'getstatic *this-class* g +lisp-object+)
                                        ; Stack: template-function
             (when *closure-variables*
               (emit 'checkcast +lisp-compiled-closure-class+)
               (duplicate-closure-array compiland)
               (emit-invokestatic +lisp-class+ "makeCompiledClosure"
                                  (list +lisp-object+ +closure-binding-array+)
                                  +lisp-object+)))))
    (process-args args)
    (emit-call-execute (length args))
    (fix-boxing representation nil)
    (emit-move-from-stack target representation))
  t)


;;                            <        <=         >        >=         =
(defvar comparison-ops '(< <= > >= =))
(defvar comparison-ins
  '((:int  . #(if_icmpge if_icmpgt if_icmple if_icmplt if_icmpne))
    (:long . #((lcmp ifge) (lcmp ifgt) (lcmp ifle)
               (lcmp iflt) (lcmp ifne)))
    (:float . #((fcmpg ifge) (fcmpg ifgt) (fcmpl ifle)
                (fcmpl iflt) (fcmpl ifne)))
    (:double . #((dcmpg ifge) (dcmpg ifgt) (dcmpl ifle)
                 (dcmpl iflt) (dcmpl ifne))))
  "Instructions to be generated upon each comparison operation,
given a specific common representation.")


(defun emit-numeric-comparison (op representation false-LABEL)
  (let* ((pos (position op comparison-ops))
         (ops-table (cdr (assoc representation comparison-ins)))
         (ops (aref ops-table pos)))
    (if (listp ops)
        (progn
          (emit (car ops))
          (emit (cadr ops) false-LABEL))
        (emit ops false-LABEL))))

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
              (type2 (derive-compiler-type arg2))
              (common-rep (common-representation (type-representation type1)
                                                 (type-representation type2))))
         (cond ((and (integerp arg1) (integerp arg2))
                (let ((result (funcall op arg1 arg2)))
                  (if result
                      (emit-push-true representation)
                      (emit-push-false representation)))
                (emit-move-from-stack target representation)
                (return-from p2-numeric-comparison))
               (common-rep
                (let ((LABEL1 (gensym))
                      (LABEL2 (gensym)))
		  (compile-forms-and-maybe-emit-clear-values
                          arg1 'stack common-rep
                          arg2 'stack common-rep)
                  (emit-numeric-comparison op common-rep LABEL1)
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
                (convert-representation :boolean representation)
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

(defmacro p2-test-integer-predicate (form predicate &body instructions)
  (let ((tmpform (gensym)))
    `(let ((,tmpform ,form))
       (when (check-arg-count ,tmpform 1)
	 (let ((arg (%cadr ,tmpform)))
	   (cond ((fixnum-type-p (derive-compiler-type arg))
		  (compile-forms-and-maybe-emit-clear-values arg 'stack :int)
		  ,@instructions)
		 (t
		  (p2-test-predicate ,tmpform ,predicate))))))))

(defun p2-test-evenp (form)
  (p2-test-integer-predicate form "evenp"
			     (emit-push-constant-int 1)
			     (emit 'iand)
			     'ifne))

(defun p2-test-oddp (form)
  (p2-test-integer-predicate form "oddp"
			     (emit-push-constant-int 1)
			     (emit 'iand)
			     'ifeq))

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
  (p2-test-integer-predicate form "minusp" 'ifge))

(defun p2-test-plusp (form)
  (p2-test-integer-predicate form "plusp" 'ifle))

(defun p2-test-zerop (form)
  (p2-test-integer-predicate form "zerop" 'ifne))

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
  (when (check-arg-count form 2)
    (let* ((op (%car form))
           (translated-op (ecase op
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
	   (compile-forms-and-maybe-emit-clear-values arg 'stack :boolean)
	   (emit 'ifeq LABEL1)
           )
         (compile-form consequent target representation)
         (emit 'goto LABEL2)
         (label LABEL1)
         (compile-form alternate target representation)
         (label LABEL2))))))

(defknown p2-if-not-and (t t t) t)
(defun p2-if-not-and (form target representation)
  (let* ((inverted-test (second (second form)))
         (consequent (third form))
         (alternate (fourth form))
         (LABEL1 (gensym))
         (LABEL2 (gensym)))
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
    (emit 'getfield +lisp-thread-class+ "_values" +lisp-object-array+)
    (astore values-register)
    (dolist (subform subforms)
      (compile-form subform nil nil))
    ;; Restore multiple values returned by first subform.
    (emit-push-current-thread)
    (aload values-register)
    (emit 'putfield +lisp-thread-class+ "_values" +lisp-object-array+)
    ;; Result.
    (aload result-register)
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
       (aload function-register)
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
       (astore values-register)
       (dolist (values-form (cddr form))
         (compile-form values-form 'stack nil)
         (emit-push-current-thread)
         (emit 'swap)
         (aload values-register)
         (emit-invokevirtual +lisp-thread-class+ "accumulateValues"
                             (list +lisp-object+ +lisp-object-array+)
                             +lisp-object-array+)
         (astore values-register)
         (maybe-emit-clear-values values-form))
       (aload function-register)
       (aload values-register)
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

(declaim (ftype (function (t) t) emit-new-closure-binding))
(defun emit-new-closure-binding (variable)
  ""
  (emit 'new +closure-binding-class+)            ;; value c-b
  (emit 'dup_x1)                                 ;; c-b value c-b
  (emit 'swap)                                   ;; c-b c-b value
  (emit-invokespecial-init +closure-binding-class+
                           (list +lisp-object+)) ;; c-b
  (aload (compiland-closure-register *current-compiland*))
                                                 ;; c-b array
  (emit 'swap)                                   ;; array c-b
  (emit-push-constant-int (variable-closure-index variable))
                                                 ;; array c-b int
  (emit 'swap) ; array index value
  (emit 'aastore))

;; Generates code to bind variable to value at top of runtime stack.
(declaim (ftype (function (t) t) compile-binding))
(defun compile-binding (variable)
  (cond ((variable-register variable)
         (astore (variable-register variable)))
        ((variable-special-p variable)
         (emit-push-current-thread)
         (emit 'swap)
         (emit-push-variable-name variable)
         (emit 'swap)
         (emit-invokevirtual +lisp-thread-class+ "bindSpecial"
                             (list +lisp-symbol+ +lisp-object+)
                             +lisp-special-binding+)
         (if (variable-binding-register variable)
             (astore (variable-binding-register variable))
             (emit 'pop)))
        ((variable-closure-index variable)              ;; stack:
         (emit-new-closure-binding variable))
        (t
         (sys::%format t "compile-binding~%")
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

(defun restore-dynamic-environment (register)
   (emit-push-current-thread)
   (aload register)
   (emit-invokevirtual +lisp-thread-class+ "resetSpecialBindings"
                       (list +lisp-special-bindings-mark+) nil)
  )

(defun save-dynamic-environment (register)
   (emit-push-current-thread)
   (emit-invokevirtual +lisp-thread-class+ "markSpecialBindings"
                       nil +lisp-special-bindings-mark+)
   (astore register)
  )

(defun restore-environment-and-make-handler (register label-START)
  (let ((label-END (gensym))
	(label-EXIT (gensym)))
    (emit 'goto label-EXIT)
    (label label-END)
    (restore-dynamic-environment register)
    (emit 'athrow)
    ;; Restore dynamic environment.
    (label label-EXIT)
    (restore-dynamic-environment register)
    (push (make-handler :from label-START
			:to label-END
			:code label-END
			:catch-type 0) *handlers*)))

(defun p2-m-v-b-node (block target)
  (let* ((*register* *register*)
         (form (m-v-b-form block))
         (*visible-variables* *visible-variables*)
         (vars (second form))
         (bind-special-p nil)
         (variables (m-v-b-vars block))
         (label-START (gensym)))
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
      (setf (m-v-b-environment-register block) (allocate-register))
      (save-dynamic-environment (m-v-b-environment-register block))
      (label label-START))
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
             (emit 'getfield +lisp-thread-class+ "_values" +lisp-object-array+)
             (emit-move-from-stack values-register)
             ;; Did we get just one value?
             (aload values-register)
             (emit 'ifnull LABEL1)
             ;; Reaching here, we have multiple values (or no values at all).
             ;; We need the slow path if we have more variables than values.
             (aload values-register)
             (emit 'arraylength)
             (emit-push-constant-int (length vars))
             (emit 'if_icmplt LABEL1)
             ;; Reaching here, we have enough values for all the variables.
             ;; We can use the values we have. This is the fast path.
             (aload values-register)
             (emit 'goto LABEL2)
             (label LABEL1)
             (emit-push-current-thread)
             (aload result-register)
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
    (dolist (variable (m-v-b-free-specials block))
      (push variable *visible-variables*))
    ;; Body.
    (let ((*blocks* (cons block *blocks*)))
      (compile-progn-body (cdddr form) target))
    (when bind-special-p
      (restore-environment-and-make-handler (m-v-b-environment-register block)
					    label-START))))

(defun propagate-vars (block)
  (let ((removed '()))
    (dolist (variable (let-vars block))
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
                                (aver (= (variable-reads variable)
                                         (length (variable-references variable))))
                                (dolist (ref (variable-references variable))
                                  (aver (eq (var-ref-variable ref) variable))
                                  (setf (var-ref-variable ref) source-var))
                                ;; Check for DOTIMES limit variable.
                                (when (get (variable-name variable)
                                           'sys::dotimes-limit-variable-p)
                                  (let* ((symbol (get (variable-name variable)
                                                      'sys::dotimes-index-variable-name))
                                         (index-variable (find-variable symbol (let-vars block))))
                                    (when index-variable
                                      (setf (get (variable-name index-variable)
                                                 'sys::dotimes-limit-variable-name)
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
        (setf (let-vars block) (remove variable (let-vars block)))))))

(defun derive-variable-representation (variable block
                                       &key (type nil type-supplied-p))
  (when (not (null (variable-representation variable)))
    ;; representation already derived
    (return-from derive-variable-representation))
  (when type-supplied-p
    (setf (variable-declared-type variable) type))
  (when (or (variable-closure-index variable)
            (variable-index variable))
    ;; variables in one of the arrays cannot be represented
    ;; other than by the boxed representation LispObject
    (return-from derive-variable-representation))
  (let ((type (variable-declared-type variable)))
    (when (and (eq (variable-declared-type variable) :none)
               (eql (variable-writes variable) 0))
      (variable-derived-type variable))
    (cond ((neq type :none)
           (setf (variable-representation variable)
                 (type-representation type))
           (unless (memq (variable-representation variable) '(:int :long))
             ;; We don't support unboxed variables other than INT and LONG (yet)
             (setf (variable-representation variable) NIL)))
          ((zerop (variable-writes variable))
           (when (eq :none (variable-derived-type variable))
             (setf (variable-derived-type variable)
                   (derive-compiler-type (variable-initform variable))))
           (let ((derived-type (variable-derived-type variable)))
             (setf (variable-derived-type variable) derived-type)
             (setf (variable-representation variable)
                   (type-representation derived-type))
             (unless (memq (variable-representation variable) '(:int :long))
               ;; We don't support unboxed variables other than INT and LONG (yet)
               (setf (variable-representation variable) NIL))))
          ((and block
                (get (variable-name variable) 'sys::dotimes-index-variable-p))
           ;; DOTIMES index variable.
           (let* ((name (get (variable-name variable)
                             'sys::dotimes-limit-variable-name))
                  (limit-variable (and name
                                       (or (find-variable name
                                                          (let-vars block))
                                           (find-visible-variable name)))))
             (when limit-variable
               (derive-variable-representation limit-variable block)
               (setf (variable-representation variable)
                     (variable-representation limit-variable))
               (let ((limit-type (variable-derived-type limit-variable)))
                 (when (integer-type-p limit-type)
                   (setf (variable-derived-type variable)
                         (%make-integer-type 0 (integer-type-high limit-type)))))))))))

(defun allocate-variable-register (variable)
  (setf (variable-register variable)
        (if (= 2 (representation-size (variable-representation variable)))
            (allocate-register-pair)
            (allocate-register))))

(defun emit-move-to-variable (variable)
  (let ((representation (variable-representation variable)))
    (cond ((variable-register variable)
           (emit (ecase (variable-representation variable)
                   ((:int :boolean :char)
                    'istore)
                   (:long   'lstore)
                   (:float  'fstore)
                   (:double 'dstore)
                   ((nil)   'astore))
                 (variable-register variable)))
          ((variable-index variable)
           (aload (compiland-argument-register *current-compiland*))
           (emit-swap representation nil)
           (emit-push-constant-int (variable-index variable))
           (emit-swap representation :int)
           (emit 'aastore))
          ((variable-closure-index variable)
           (aload (compiland-closure-register *current-compiland*))
           (emit-push-constant-int (variable-closure-index variable))
           (emit 'aaload)
           (emit-swap representation nil)
           (emit 'putfield +closure-binding-class+ "value" +lisp-object+))
          ((variable-environment variable)
           (assert (not *file-compilation*))
           (emit-load-externalized-object (variable-environment variable)
                                          +lisp-environment-class+)
           (emit 'swap)
           (emit-push-variable-name variable)
           (emit 'swap)
           (emit-invokevirtual +lisp-environment-class+ "rebind"
                               (list +lisp-symbol+ +lisp-object+)
                               nil))
          (t
           (assert nil)))))

(defun emit-push-variable (variable)
  (cond ((variable-register variable)
         (emit (ecase (variable-representation variable)
                 ((:int :boolean :char)
                  'iload)
                 (:long   'lload)
                 (:float  'fload)
                 (:double 'dload)
                 ((nil)   'aload))
               (variable-register variable)))
        ((variable-index variable)
         (aload (compiland-argument-register *current-compiland*))
         (emit-push-constant-int (variable-index variable))
         (emit 'aaload))
        ((variable-closure-index variable)
         (aload (compiland-closure-register *current-compiland*))
         (emit-push-constant-int (variable-closure-index variable))
         (emit 'aaload)
         (emit 'getfield +closure-binding-class+ "value" +lisp-object+))
        ((variable-environment variable)
         (assert (not *file-compilation*))
         (emit-load-externalized-object (variable-environment variable)
                                        +lisp-environment-class+)
         (emit-push-variable-name variable)
         (emit-invokevirtual +lisp-environment-class+ "lookup"
                             (list +lisp-object+)
                             +lisp-object+))
        (t
         (assert nil))))


(defknown p2-let-bindings (t) t)
(defun p2-let-bindings (block)
  (dolist (variable (let-vars block))
    (unless (or (variable-special-p variable)
                (variable-closure-index variable)
                (zerop (variable-reads variable)))
      (aver (null (variable-register variable)))
      (setf (variable-register variable) t)))
  (let (must-clear-values
        temporary-storage)
    (declare (type boolean must-clear-values))
    ;; Evaluate each initform. If the variable being bound is special, allocate
    ;; a temporary register for the result; LET bindings must be done in
    ;; parallel, so we can't modify any specials until all the initforms have
    ;; been evaluated. Note that we can't just push the values on the stack
    ;; because we'll lose JVM stack consistency if there is a non-local
    ;; transfer of control from one of the initforms.
    (dolist (variable (let-vars block))
      (let* ((initform (variable-initform variable))
             (unused-p (and (not (variable-special-p variable))
                            ;; If it's never read, we don't care about writes.
                            (zerop (variable-reads variable)))))
        (cond (unused-p
               (compile-form initform nil nil)) ; for effect
              (t
               (cond (initform
                      (when (eq (variable-register variable) t)
                        (derive-variable-representation variable block))
                      (compile-form initform 'stack
                                    (variable-representation variable))
                      (unless must-clear-values
                        (unless (single-valued-p initform)
                          (setf must-clear-values t))))
                     (t
                      ;; No initform.
                      (emit-push-nil)))
               (when (eq (variable-register variable) t)
                 ;; Now allocate the register.
                 (allocate-variable-register variable))
               (when (variable-special-p variable)
                 (setf (variable-binding-register variable)
                       (allocate-register)))
               (cond ((variable-special-p variable)
                      (let ((temp-register (allocate-register)))
                        ;; FIXME: this permanently allocates a register
                        ;; which has only a single local use
                        (push (cons temp-register variable)
                              temporary-storage)
                        (emit-move-from-stack temp-register)))
                     ((variable-representation variable)
                      (emit-move-to-variable variable))
                     (t
                      (compile-binding variable)))))))
    (when must-clear-values
      (emit-clear-values))
    ;; Now that all the initforms have been evaluated, move the results from
    ;; the temporary registers (if any) to their proper destinations.
    (dolist (temp temporary-storage)
      (aload (car temp))
      (compile-binding (cdr temp))))
  ;; Now make the variables visible.
  (dolist (variable (let-vars block))
    (push variable *visible-variables*))
  t)

(defknown p2-let*-bindings (t) t)
(defun p2-let*-bindings (block)
  (let ((must-clear-values nil))
    (declare (type boolean must-clear-values))
    ;; Generate code to evaluate initforms and bind variables.
    (dolist (variable (let-vars block))
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
                                     +lisp-special-binding+)
                 (if (variable-binding-register variable)
                     (astore (variable-binding-register variable))
                     (emit 'pop))
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
                        (when (and (eq (variable-declared-type variable) :none)
                                   (eql (variable-writes variable) 0))
                          (setf (variable-derived-type variable)
                                (derive-compiler-type initform)))
                        (derive-variable-representation variable block)
                        (allocate-variable-register variable)
                        (compile-form initform 'stack
                                      (variable-representation variable))
                        (update-must-clear-values)
                        (emit-move-to-variable variable)
                        (setf boundp t))
                       (t
                        (compile-form initform 'stack nil)
                        (update-must-clear-values))))))
        (unless (or boundp (variable-special-p variable))
          (unless (or (variable-closure-index variable)
                      (variable-register variable))
            (setf (variable-register variable) (allocate-register))))
        (push variable *visible-variables*)
        (unless boundp
          (when (variable-special-p variable)
            (setf (variable-binding-register variable) (allocate-register)))
          (compile-binding variable))
        (maybe-generate-type-check variable)))
    (when must-clear-values
      (emit-clear-values)))
  t)

(defun p2-let/let*-node (block target representation)
  (let* (
         (*register* *register*)
         (form (let-form block))
         (*visible-variables* *visible-variables*)
         (specialp nil)
         (label-START (gensym)))
    ;; Walk the variable list looking for special bindings and unused lexicals.
    (dolist (variable (let-vars block))
      (cond ((variable-special-p variable)
             (setf specialp t))
            ((zerop (variable-reads variable))
             (unused-variable variable))))
    ;; If there are any special bindings...
    (when specialp
      ;; We need to save current dynamic environment.
      (setf (let-environment-register block) (allocate-register))
      (save-dynamic-environment (let-environment-register block))
      (label label-START))
    (propagate-vars block)
    (ecase (car form)
      (LET
       (p2-let-bindings block))
      (LET*
       (p2-let*-bindings block)))
    ;; Make declarations of free specials visible.
    (dolist (variable (let-free-specials block))
      (push variable *visible-variables*))
    ;; Body of LET/LET*.
    (with-saved-compiler-policy
      (process-optimization-declarations (cddr form))
      (let ((*blocks* (cons block *blocks*)))
        (compile-progn-body (cddr form) target representation)))
    (when specialp
      (restore-environment-and-make-handler (let-environment-register block)
					    label-START))))

(defknown p2-locally-node (t t t) t)
(defun p2-locally-node (block target representation)
  (with-saved-compiler-policy
    (let* ((body (cdr (locally-form block)))
           (*visible-variables* (append (locally-free-specials block)
                                        *visible-variables*))
           (*blocks* (cons block *blocks*)))
      (process-optimization-declarations body)
      (compile-progn-body body target representation))))

(defknown p2-tagbody-node (t t) t)
(defun p2-tagbody-node (block target)
  (let* ((*blocks* (cons block *blocks*))
         (*visible-tags* *visible-tags*)
         (*register* *register*)
         (form (tagbody-form block))
         (body (cdr form))
         (BEGIN-BLOCK (gensym))
         (END-BLOCK (gensym))
         (RETHROW (gensym))
         (EXIT (gensym))
         (must-clear-values nil))
    ;; Scan for tags.
    (dolist (tag (tagbody-tags block))
      (push tag *visible-tags*))

    (when (tagbody-id-variable block)
      ;; we have a block variable; that should be a closure variable
      (assert (not (null (variable-closure-index (tagbody-id-variable block)))))
      (emit 'new +lisp-object-class+)
      (emit 'dup)
      (emit-invokespecial-init +lisp-object-class+ '())
      (emit-new-closure-binding (tagbody-id-variable block)))
    (label BEGIN-BLOCK)
    (do* ((rest body (cdr rest))
          (subform (car rest) (car rest)))
         ((null rest))
      (cond ((or (symbolp subform) (integerp subform))
             (let ((tag (find subform (tagbody-tags block) :key #'tag-name
                              :test #'eql)))
               (unless tag
                 (error "COMPILE-TAGBODY: tag not found: ~S~%" subform))
               (when (tag-used tag)
                 (label (tag-label tag)))))
            (t
             (compile-form subform nil nil)
             (unless must-clear-values
               (unless (single-valued-p subform)
                 (setf must-clear-values t))))))
    (label END-BLOCK)
    (emit 'goto EXIT)
    (when (tagbody-non-local-go-p block)
      ; We need a handler to catch non-local GOs.
      (let* ((HANDLER (gensym))
             (EXTENT-EXIT-HANDLER (gensym))
             (*register* *register*)
             (go-register (allocate-register))
             (tag-register (allocate-register)))
        (label HANDLER)
        ;; The Go object is on the runtime stack. Stack depth is 1.
        (emit 'dup)
        (astore go-register)
        ;; Get the tag.
        (emit 'getfield +lisp-go-class+ "tagbody" +lisp-object+) ; Stack depth is still 1.
        (emit-push-variable (tagbody-id-variable block))
        (emit 'if_acmpne RETHROW) ;; Not this TAGBODY
        (aload go-register)
        (emit 'getfield +lisp-go-class+ "tag" +lisp-object+) ; Stack depth is still 1.
        (astore tag-register)
        ;; Don't actually generate comparisons for tags
        ;; to which there is no non-local GO instruction
        (dolist (tag (remove-if-not #'tag-used-non-locally
                                    (tagbody-tags block)))
          (aload tag-register)
          (emit-load-externalized-object (tag-label tag))
          ;; Jump if EQ.
          (emit 'if_acmpeq (tag-label tag)))
        (label RETHROW)
        ;; Not found. Re-throw Go.
        (aload go-register)
        (emit 'aconst_null) ;; load null value
        (emit-move-to-variable (tagbody-id-variable block))
        (emit 'athrow)
        (label EXTENT-EXIT-HANDLER)
        (emit 'aconst_null) ;; load null value
        (emit-move-to-variable (tagbody-id-variable block))
        (emit 'athrow)
        ;; Finally...
        (push (make-handler :from BEGIN-BLOCK
                            :to END-BLOCK
                            :code HANDLER
                            :catch-type (pool-class +lisp-go-class+))
              *handlers*)
        (push (make-handler :from BEGIN-BLOCK
                            :to END-BLOCK
                            :code EXTENT-EXIT-HANDLER
                            :catch-type 0)
              *handlers*)))
    (label EXIT)
    (when (tagbody-non-local-go-p block)
      (emit 'aconst_null) ;; load null value
      (emit-move-to-variable (tagbody-id-variable block)))
    (when must-clear-values
      (emit-clear-values))
    ;; TAGBODY returns NIL.
    (when target
      (emit-push-nil)
      (emit-move-from-stack target)))
  )

(defknown p2-go (t t t) t)
(defun p2-go (form target representation)
  ;; FIXME What if we're called with a non-NIL representation?
  (declare (ignore target representation))
  (let* ((name (cadr form))
         (tag (find-tag name))
         (tag-block (when tag (tag-block tag))))
    (unless tag
      (error "p2-go: tag not found: ~S" name))
    (when (and (eq (tag-compiland tag) *current-compiland*)
               (not (enclosed-by-protected-block-p tag-block)))
      ;; Local case with local transfer of control
      ;;   Note: Local case with non-local transfer of control handled below
      (when (and (tagbody-needs-environment-restoration tag-block)
                 (enclosed-by-environment-setting-block-p tag-block))
        ;; If there's a dynamic environment to restore, do it.
	(restore-dynamic-environment (environment-register-to-restore tag-block)))
      (maybe-generate-interrupt-check)
      (emit 'goto (tag-label tag))
      (return-from p2-go))
    ;; Non-local GO.
    (emit-push-variable (tagbody-id-variable tag-block))
    (emit-load-externalized-object (tag-label tag)) ; Tag.
    (emit-invokestatic +lisp-class+ "nonLocalGo" (lisp-object-arg-types 2)
                       +lisp-object+)
    ;; Following code will not be reached, but is needed for JVM stack
    ;; consistency.
    (emit 'areturn)))

(defknown p2-atom (t t t) t)
(define-inlined-function p2-atom (form target representation)
  ((aver (or (null representation) (eq representation :boolean)))
   (check-arg-count form 1))
  (compile-forms-and-maybe-emit-clear-values (cadr form) 'stack nil)
  (emit 'instanceof +lisp-cons-class+)
  (let ((LABEL1 (gensym))
        (LABEL2 (gensym)))
    (emit 'ifeq LABEL1)
    (ecase representation
      (:boolean
       (emit 'iconst_0))
      ((nil)
       (emit-push-nil)))
    (emit 'goto LABEL2)
    (label LABEL1)
    (ecase representation
      (:boolean
       (emit 'iconst_1))
      ((nil)
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
           (convert-representation :boolean representation)
           (emit-move-from-stack target representation)))))

(defun p2-bit-vector-p (form target representation)
  (p2-instanceof-predicate form target representation +lisp-abstract-bit-vector-class+))

(defun p2-characterp (form target representation)
  (p2-instanceof-predicate form target representation +lisp-character-class+))

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

(define-inlined-function p2-coerce-to-function (form target representation)
  ((check-arg-count form 1))
  (compile-forms-and-maybe-emit-clear-values (%cadr form) 'stack nil)
  (emit-invokestatic +lisp-class+ "coerceToFunction"
                     (lisp-object-arg-types 1) +lisp-object+)
  (emit-move-from-stack target))

(defun p2-block-node (block target representation)
  (unless (block-node-p block)
    (sys::%format t "type-of block = ~S~%" (type-of block))
    (aver (block-node-p block)))
  (let* ((*blocks* (cons block *blocks*))
         (*register* *register*)
         (BEGIN-BLOCK (gensym))
         (END-BLOCK (gensym))
         (BLOCK-EXIT (block-exit block)))
    (setf (block-target block) target)
    (when (block-id-variable block)
      ;; we have a block variable; that should be a closure variable
      (assert (not (null (variable-closure-index (block-id-variable block)))))
      (emit 'new +lisp-object-class+)
      (emit 'dup)
      (emit-invokespecial-init +lisp-object-class+ '())
      (emit-new-closure-binding (block-id-variable block)))
    (dformat t "*all-variables* = ~S~%"
             (mapcar #'variable-name *all-variables*))
    (label BEGIN-BLOCK) ; Start of protected range, for non-local returns
    ;; Implicit PROGN.
    (compile-progn-body (cddr (block-form block)) target)
    (label END-BLOCK) ; End of protected range, for non-local returns
    (when (block-non-local-return-p block)
      ;; We need a handler to catch non-local RETURNs.
      (emit 'goto BLOCK-EXIT) ; Jump over handler, when inserting one
      (let ((HANDLER (gensym))
            (EXTENT-EXIT-HANDLER (gensym))
            (THIS-BLOCK (gensym)))
        (label HANDLER)
        ;; The Return object is on the runtime stack. Stack depth is 1.
        (emit 'dup) ; Stack depth is 2.
        (emit 'getfield +lisp-return-class+ "tag" +lisp-object+) ; Still 2.
        (emit-push-variable (block-id-variable block))
        ;; If it's not the block we're looking for...
        (emit 'if_acmpeq THIS-BLOCK) ; Stack depth is 1.
        (label EXTENT-EXIT-HANDLER)
        ;; Not the tag we're looking for.
        (emit 'aconst_null) ;; load null value
        (emit-move-to-variable (block-id-variable block))
        (emit 'athrow)
        (label THIS-BLOCK)
        (emit 'getfield +lisp-return-class+ "result" +lisp-object+)
        (emit-move-from-stack target) ; Stack depth is 0.
        ;; Finally...
        (push (make-handler :from BEGIN-BLOCK
                            :to END-BLOCK
                            :code HANDLER
                            :catch-type (pool-class +lisp-return-class+))
              *handlers*)
        (push (make-handler :from BEGIN-BLOCK
                            :to END-BLOCK
                            :code EXTENT-EXIT-HANDLER
                            :catch-type 0)
              *handlers*)))
    (label BLOCK-EXIT)
    (when (block-id-variable block)
      (emit 'aconst_null) ;; load null value
      (emit-move-to-variable (block-id-variable block)))
    (fix-boxing representation nil)))

(defknown p2-return-from (t t t) t)
(defun p2-return-from (form target representation)
  ;; FIXME What if we're called with a non-NIL representation?
  (declare (ignore target representation))
  (let* ((name (second form))
         (result-form (third form))
         (block (find-block name)))
    (when (null block)
      (error "No block named ~S is currently visible." name))
    (let ((compiland *current-compiland*))
      (when (eq (block-compiland block) compiland)
        ;; Local case. Is the RETURN nested inside an UNWIND-PROTECT which is
        ;; inside the block we're returning from?
        (unless (enclosed-by-protected-block-p block)
          (unless (compiland-single-valued-p *current-compiland*)
            (emit-clear-values))
          (compile-form result-form (block-target block) nil)
          (when (and (block-needs-environment-restoration block)
                     (enclosed-by-environment-setting-block-p block))
            (restore-dynamic-environment (environment-register-to-restore block)))
          (emit 'goto (block-exit block))
          (return-from p2-return-from))))
    ;; Non-local RETURN.
    (aver (block-non-local-return-p block))
    (emit-push-variable (block-id-variable block))
    (emit-load-externalized-object (block-name block))
    (emit-clear-values)
    (compile-form result-form 'stack nil)
    (emit-invokestatic +lisp-class+ "nonLocalReturn" (lisp-object-arg-types 3)
                       +lisp-object+)
    ;; Following code will not be reached, but is needed for JVM stack
    ;; consistency.
    (emit 'areturn)))

(defun emit-car/cdr (arg target representation field)
  (compile-forms-and-maybe-emit-clear-values arg 'stack nil)
  (emit-invoke-method field target representation))

(define-inlined-function p2-car (form target representation)
  ((check-arg-count form 1))
  (let ((arg (%cadr form)))
    (cond ((and (null target) (< *safety* 3))
           (compile-form arg target nil))
          ((and (consp arg) (eq (%car arg) 'cdr) (= (length arg) 2))
	   (compile-forms-and-maybe-emit-clear-values (second arg) 'stack nil)
           (emit-invoke-method "cadr" target representation))
          (t
	   (emit-car/cdr arg target representation "car")))))

(define-inlined-function p2-cdr (form target representation)
  ((check-arg-count form 1))
  (let ((arg (%cadr form)))
    (emit-car/cdr arg target representation "cdr")))

(define-inlined-function p2-cons (form target representation)
  ((check-arg-count form 2))
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
  (cond (*file-compilation*
         (emit 'getstatic *this-class*
               (declare-load-time-value (second form)) +lisp-object+)
         (fix-boxing representation nil)
         (emit-move-from-stack target representation))
        (t
         (compile-constant (eval (second form)) target representation))))

(defun p2-progv-node (block target representation)
  (let* ((form (progv-form block))
         (symbols-form (cadr form))
         (values-form (caddr form))
         (*register* *register*)
         (environment-register
          (setf (progv-environment-register block) (allocate-register)))
         (label-START (gensym)))
    (compile-form symbols-form 'stack nil)
    (compile-form values-form 'stack nil)
    (unless (and (single-valued-p symbols-form)
                 (single-valued-p values-form))
      (emit-clear-values))
    (save-dynamic-environment environment-register)
    (label label-START)
    ;; Compile call to Lisp.progvBindVars().
    (emit-push-current-thread)
    (emit-invokestatic +lisp-class+ "progvBindVars"
                       (list +lisp-object+ +lisp-object+ +lisp-thread+) nil)
      ;; Implicit PROGN.
    (let ((*blocks* (cons block *blocks*)))
      (compile-progn-body (cdddr form) target representation))
    (restore-environment-and-make-handler environment-register label-START)))

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
          ((symbolp obj)
           (emit-load-externalized-object obj)
           (emit-move-from-stack target representation))
          ((listp obj)
           (emit-load-externalized-object obj)
           (emit-move-from-stack target representation))
          ((constantp obj)
           (compile-constant obj target representation))
          (t
           (compiler-unsupported "COMPILE-QUOTE: unsupported case: ~S" form)))))

(define-inlined-function p2-rplacd (form target representation)
  ((check-arg-count form 2))
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

(define-inlined-function p2-set-car/cdr (form target representation)
  ((check-arg-count form 2))
  (let ((op (%car form))
        (args (%cdr form)))
    (compile-form (%car args) 'stack nil)
    (compile-form (%cadr args) 'stack nil)
    (when target
      (emit-dup nil :past nil))
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

(defun compile-and-write-to-stream (class-file compiland stream)
  (with-class-file class-file
    (let ((*current-compiland* compiland))
      (with-saved-compiler-policy
	  (p2-compiland compiland)
	(write-class-file (compiland-class-file compiland) stream)))))

(defun set-compiland-and-write-class (class-file compiland stream)
  (setf (compiland-class-file compiland) class-file)
  (compile-and-write-to-stream class-file compiland stream))


(defmacro with-temp-class-file (pathname class-file lambda-list &body body)
  `(let* ((,pathname (make-temp-file))
	  (,class-file (make-class-file :pathname ,pathname
				       :lambda-list ,lambda-list)))
     (unwind-protect
	  (progn ,@body)
       (delete-file pathname))))

(defknown p2-flet-process-compiland (t) t)
(defun p2-flet-process-compiland (local-function)
  (let* ((compiland (local-function-compiland local-function))
         (lambda-list (cadr (compiland-lambda-expression compiland))))
    (cond (*file-compilation*
           (let* ((pathname (funcall *pathnames-generator*))
                  (class-file (make-class-file :pathname pathname
                                               :lambda-list lambda-list)))
             (with-open-class-file (f class-file)
               (set-compiland-and-write-class class-file compiland f))
             (setf (local-function-class-file local-function) class-file)))
          (t
           (let ((class-file (make-class-file :lambda-list lambda-list)))
             (with-open-stream (stream (sys::%make-byte-array-output-stream))
               (set-compiland-and-write-class class-file compiland stream)
               (setf (local-function-class-file local-function) class-file)
               (setf (local-function-function local-function)
                     (load-compiled-function
                      (sys::%get-output-stream-bytes stream)))))))))

(defun emit-make-compiled-closure-for-labels
    (local-function compiland declaration)
  (emit 'getstatic *this-class* declaration +lisp-object+)
  (let ((parent (compiland-parent compiland)))
    (when (compiland-closure-register parent)
      (dformat t "(compiland-closure-register parent) = ~S~%"
	       (compiland-closure-register parent))
      (emit 'checkcast +lisp-compiled-closure-class+)
      (duplicate-closure-array parent)
      (emit-invokestatic +lisp-class+ "makeCompiledClosure"
			 (list +lisp-object+ +closure-binding-array+)
			 +lisp-object+)))
  (emit-move-to-variable (local-function-variable local-function)))

(defknown p2-labels-process-compiland (t) t)
(defun p2-labels-process-compiland (local-function)
  (let* ((compiland (local-function-compiland local-function))
         (lambda-list (cadr (compiland-lambda-expression compiland))))
    (cond (*file-compilation*
           (let* ((pathname (funcall *pathnames-generator*))
                  (class-file (make-class-file :pathname pathname
                                               :lambda-list lambda-list)))
             (with-open-class-file (f class-file)
               (set-compiland-and-write-class class-file compiland f))
             (setf (local-function-class-file local-function) class-file)
             (let ((g (declare-local-function local-function)))
               (emit-make-compiled-closure-for-labels
                local-function compiland g))))
          (t
           (let ((class-file (make-class-file :lambda-list lambda-list)))
             (with-open-stream (stream (sys::%make-byte-array-output-stream))
               (set-compiland-and-write-class class-file compiland stream)
               (setf (local-function-class-file local-function) class-file)
               (let ((g (declare-object
                         (load-compiled-function
                          (sys::%get-output-stream-bytes stream)))))
                 (emit-make-compiled-closure-for-labels
                  local-function compiland g))))))))

(defknown p2-flet-node (t t t) t)
(defun p2-flet-node (block target representation)
  (let* ((form (flet-form block))
         (*local-functions* *local-functions*)
         (*visible-variables* *visible-variables*)
         (local-functions (cadr form))
         (body (cddr form)))
    (dolist (local-function local-functions)
      (p2-flet-process-compiland local-function))
    (dolist (local-function local-functions)
      (push local-function *local-functions*))
    (dolist (special (flet-free-specials block))
      (push special *visible-variables*))
    (let ((*blocks* (cons block *blocks*)))
      (compile-progn-body body target representation))))

(defknown p2-labels-node (t t t) t)
(defun p2-labels-node (block target representation)
  (let* ((form (labels-form block))
         (*local-functions* *local-functions*)
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
    (dolist (special (labels-free-specials block))
      (push special *visible-variables*))
    (let ((*blocks* (cons block *blocks*)))
      (compile-progn-body body target representation))))

(defun p2-lambda (compiland target)
  (let* ((lambda-list (cadr (compiland-lambda-expression compiland))))
    (aver (null (compiland-class-file compiland)))
    (cond (*file-compilation*
           (setf (compiland-class-file compiland)
                 (make-class-file :pathname (funcall *pathnames-generator*)
                                  :lambda-list lambda-list))
           (let ((class-file (compiland-class-file compiland)))
	     (with-open-class-file (f class-file)
	       (compile-and-write-to-stream class-file compiland f))
             (emit 'getstatic *this-class*
                   (declare-local-function (make-local-function :class-file
                                                                class-file))
                   +lisp-object+)))
          (t
           (setf (compiland-class-file compiland)
                 (make-class-file :lambda-list lambda-list))
           (with-open-stream (stream (sys::%make-byte-array-output-stream))
             (compile-and-write-to-stream (compiland-class-file compiland)
                                          compiland stream)
             (emit-load-externalized-object (load-compiled-function
                                    (sys::%get-output-stream-bytes stream))))))
    (cond ((null *closure-variables*))  ; Nothing to do.
          ((compiland-closure-register *current-compiland*)
           (duplicate-closure-array *current-compiland*)
           (emit-invokestatic +lisp-class+ "makeCompiledClosure"
                              (list +lisp-object+ +closure-binding-array+)
                              +lisp-object+))
                                        ; Stack: compiled-closure
          (t
           (aver nil))) ;; Shouldn't happen.
    (emit-move-from-stack target)))

(defknown p2-function (t t t) t)
(defun p2-function (form target representation)
  ;; FIXME What if we're called with a non-NIL representation?
  (declare (ignore representation))
  (let ((name (second form))
        local-function)
    (cond
      ((symbolp name)
       (dformat t "p2-function case 1~%")
       (cond
         ((setf local-function (find-local-function name))
          (dformat t "p2-function 1~%")
          (cond
            ((local-function-variable local-function)
             (dformat t "p2-function 2 emitting var-ref~%")
             (compile-var-ref (make-var-ref
                               (local-function-variable local-function))
                              'stack nil))
            (t
             (let ((g (if *file-compilation*
                          (declare-local-function local-function)
                          (declare-object
                           (local-function-function local-function)))))
               (emit 'getstatic *this-class* g +lisp-object+)
                                        ; Stack: template-function

               (when (compiland-closure-register *current-compiland*)
                 (emit 'checkcast +lisp-compiled-closure-class+)
                 (duplicate-closure-array *current-compiland*)
                 (emit-invokestatic +lisp-class+ "makeCompiledClosure"
                                    (list +lisp-object+ +closure-binding-array+)
                                    +lisp-object+)))))
          (emit-move-from-stack target))
         ((inline-ok name)
          (emit 'getstatic *this-class*
                (declare-function name) +lisp-object+)
          (emit-move-from-stack target))
         (t
          (emit-load-externalized-object name)
          (emit-invokevirtual +lisp-object-class+ "getSymbolFunctionOrDie"
                              nil +lisp-object+)
          (emit-move-from-stack target))))
      ((and (consp name) (eq (%car name) 'SETF))
       (dformat t "p2-function case 2~%")
       ;; FIXME Need to check for NOTINLINE declaration!
       (cond
         ((setf local-function (find-local-function name))
          (dformat t "p2-function 1~%")
          (when (eq (local-function-compiland local-function)
                    *current-compiland*)
            (aload 0) ; this
            (emit-move-from-stack target)
            (return-from p2-function))
          (cond
            ((local-function-variable local-function)
             (dformat t "p2-function 2~%")
             (compile-var-ref (make-var-ref
                               (local-function-variable local-function))
                              'stack nil))
            (t
             (let ((g (if *file-compilation*
                          (declare-local-function local-function)
                          (declare-object
                           (local-function-function local-function)))))
               (emit 'getstatic *this-class*
                     g +lisp-object+))))) ; Stack: template-function
         ((and (member name *functions-defined-in-current-file* :test #'equal)
	       (not (notinline-p name)))
          (emit 'getstatic *this-class*
                (declare-setf-function name) +lisp-object+)
          (emit-move-from-stack target))
         ((and (null *file-compilation*)
               (fboundp name)
               (fdefinition name))
          (emit-load-externalized-object (fdefinition name))
          (emit-move-from-stack target))
         (t
          (emit-load-externalized-object (cadr name))
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
(define-inlined-function p2-ash (form target representation)
  ((check-arg-count form 2))
  (let* ((args (%cdr form))
         (arg1 (%car args))
         (arg2 (%cadr args))
         (type1 (derive-compiler-type arg1))
         (type2 (derive-compiler-type arg2))
         (low2 (and (fixnum-type-p type2) (integer-type-low type2)))
         (high2 (and (fixnum-type-p type2) (integer-type-high type2)))
         (constant-shift (fixnum-constant-value type2))
         (result-type (derive-compiler-type form)))
    (cond ((and (integerp arg1) (integerp arg2))
           (compile-constant (ash arg1 arg2) target representation))
          ((and constant-shift
                ;; ishl/ishr only use the low five bits of the mask.
                (<= -31 constant-shift 31)
                (fixnum-type-p type1)
                (fixnum-type-p result-type))
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
           (convert-representation :int representation)
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
           (convert-representation :long representation)
           (emit-move-from-stack target representation))
          ((and (fixnum-type-p type1)
                low2 high2 (<= -31 low2 high2 0)) ; Negative shift.
	   (compile-forms-and-maybe-emit-clear-values arg1 'stack :int
						      arg2 'stack :int)
           (emit 'ineg)
           (emit 'ishr)
           (convert-representation :int representation)
           (emit-move-from-stack target representation))
          ((fixnum-type-p type2)
           (cond ((and low2 high2 (<= 0 low2 high2 63) ; Non-negative shift.
                       (java-long-type-p type1)
                       (java-long-type-p result-type))
		  (compile-forms-and-maybe-emit-clear-values arg1 'stack :long
							     arg2 'stack :int)
                  (emit 'lshl)
                  (convert-representation :long representation))
                 ((and low2 high2 (<= -63 low2 high2 0) ; Negative shift.
                       (java-long-type-p type1)
                       (java-long-type-p result-type))
		  (compile-forms-and-maybe-emit-clear-values arg1 'stack :long
							     arg2 'stack :int)
                  (emit 'ineg)
                  (emit 'lshr)
                  (convert-representation :long representation))
                 (t
		  (compile-forms-and-maybe-emit-clear-values arg1 'stack nil
							     arg2 'stack :int)
                  (emit-invokevirtual +lisp-object-class+ "ash" '("I") +lisp-object+)
                  (fix-boxing representation result-type)))
           (emit-move-from-stack target representation))
          (t
           (compile-function-call form target representation)))))

(defknown p2-logand (t t t) t)
(defun p2-logand (form target representation)
  (let* ((args (cdr form)))
    (case (length args)
      (2
       (let* ((arg1 (%car args))
              (arg2 (%cadr args))
              (type1 (derive-compiler-type arg1))
              (type2 (derive-compiler-type arg2))
              (result-type (derive-compiler-type form)))
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
                ;; Both arguments are fixnums.
		(compile-forms-and-maybe-emit-clear-values arg1 'stack :int
							   arg2 'stack :int)
                (emit 'iand)
                (convert-representation :int representation)
                (emit-move-from-stack target representation))
               ((or (and (fixnum-type-p type1)
                         (compiler-subtypep type1 'unsigned-byte))
                    (and (fixnum-type-p type2)
                         (compiler-subtypep type2 'unsigned-byte)))
                ;; One of the arguments is a positive fixnum.
		(compile-forms-and-maybe-emit-clear-values arg1 'stack :int
							   arg2 'stack :int)
                (emit 'iand)
                (convert-representation :int representation)
                (emit-move-from-stack target representation))
               ((and (java-long-type-p type1) (java-long-type-p type2))
                ;; Both arguments are longs.
		(compile-forms-and-maybe-emit-clear-values arg1 'stack :long
							   arg2 'stack :long)
                (emit 'land)
                (convert-representation :long representation)
                (emit-move-from-stack target representation))
               ((or (and (java-long-type-p type1)
                         (compiler-subtypep type1 'unsigned-byte))
                    (and (java-long-type-p type2)
                         (compiler-subtypep type2 'unsigned-byte)))
                ;; One of the arguments is a positive long.
		(compile-forms-and-maybe-emit-clear-values arg1 'stack :long
							   arg2 'stack :long)
                (emit 'land)
                (convert-representation :long representation)
                (emit-move-from-stack target representation))
               ((fixnum-type-p type2)
		(compile-forms-and-maybe-emit-clear-values arg1 'stack nil
							   arg2 'stack :int)
                (emit-invokevirtual +lisp-object-class+ "LOGAND" '("I") +lisp-object+)
                (fix-boxing representation result-type)
                (emit-move-from-stack target representation))
               ((fixnum-type-p type1)
                ;; arg1 is a fixnum, but arg2 is not
		(compile-forms-and-maybe-emit-clear-values arg1 'stack :int
							   arg2 'stack nil)
                ;; swap args
                (emit 'swap)
                (emit-invokevirtual +lisp-object-class+ "LOGAND" '("I") +lisp-object+)
                (fix-boxing representation result-type)
                (emit-move-from-stack target representation))
               (t
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
		(compile-forms-and-maybe-emit-clear-values arg1 'stack :int
							   arg2 'stack :int)
                (emit 'ior)
                (convert-representation :int representation)
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
                (convert-representation :long representation)
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
		(compile-forms-and-maybe-emit-clear-values arg1 'stack :int
							   arg2 'stack :int)
                (emit 'ixor)
                (convert-representation :int representation))
               ((and (java-long-type-p type1) (java-long-type-p type2))
		(compile-forms-and-maybe-emit-clear-values arg1 'stack :long
							   arg2 'stack :long)
                (emit 'lxor)
                (convert-representation :long representation))
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
(define-inlined-function p2-lognot (form target representation)
  ((check-arg-count form 1))
  (cond ((and (fixnum-type-p (derive-compiler-type form)))
         (let ((arg (%cadr form)))
	   (compile-forms-and-maybe-emit-clear-values arg 'stack :int)
           (emit 'iconst_m1)
           (emit 'ixor)
           (convert-representation :int representation)
           (emit-move-from-stack target representation)))
        (t
         (let ((arg (%cadr form)))
	   (compile-forms-and-maybe-emit-clear-values arg 'stack nil))
         (emit-invokevirtual +lisp-object-class+ "LOGNOT" nil +lisp-object+)
         (fix-boxing representation nil)
         (emit-move-from-stack target representation))))

;; %ldb size position integer => byte
(defknown p2-%ldb (t t t) t)
(define-inlined-function p2-%ldb (form target representation)
  ((check-arg-count form 3))
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
		  (compile-forms-and-maybe-emit-clear-values size-arg nil nil
							     position-arg nil nil
							     arg3 'stack :int)
                  (unless (zerop position)
                    (emit-push-constant-int position)
                    (emit 'ishr))
                  (emit-push-constant-int (1- (expt 2 size))) ; mask
                  (emit 'iand)
                  (convert-representation :int representation)
                  (emit-move-from-stack target representation))
                 ((<= (+ position size) 63)
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
                         (convert-representation :int representation))
                        (t
                         (emit-push-constant-long (1- (expt 2 size))) ; mask
                         (emit 'land)
                         (convert-representation :long representation)))
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
           (emit 'dup_x2) ;; use not supported by emit-dup: 3 values involved
           (emit 'pop)
           (emit-invokevirtual +lisp-object-class+ "LDB" '("I" "I") +lisp-object+)
           (fix-boxing representation nil)
           (emit-move-from-stack target representation))
          (t
           (compile-function-call form target representation)))))

(defknown p2-mod (t t t) t)
(define-inlined-function p2-mod (form target representation)
  ((check-arg-count form 2))
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

(defknown p2-zerop (t t t) t)
(define-inlined-function p2-zerop (form target representation)
  ((aver (or (null representation) (eq representation :boolean)))
   (check-arg-count form 1))
  (let* ((arg (cadr form))
         (type (derive-compiler-type arg)))
    (cond ((fixnum-type-p type)
	   (compile-forms-and-maybe-emit-clear-values arg 'stack :int)
           (let ((LABEL1 (gensym))
                 (LABEL2 (gensym)))
             (emit 'ifne LABEL1)
             (ecase representation
               (:boolean
                (emit 'iconst_1))
               ((nil)
                (emit-push-t)))
             (emit 'goto LABEL2)
             (label LABEL1)
             (ecase representation
               (:boolean
                (emit 'iconst_0))
               ((nil)
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
(define-inlined-function p2-std-slot-value (form target representation)
  ((check-arg-count form 2))
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
(define-inlined-function p2-set-std-slot-value (form target representation)
  ((check-arg-count form 3))
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
      (astore value-register))
    (emit-invokevirtual +lisp-object-class+ "setSlotValue"
                        (lisp-object-arg-types 2) nil)
    (when value-register
      (aload value-register)
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
(define-inlined-function p2-make-sequence (form target representation)
  ;; In safe code, we want to make sure the requested length does not exceed
  ;; ARRAY-DIMENSION-LIMIT.
  ((and (< *safety* 3)
               (= (length form) 3)
               (null representation)))
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
(define-inlined-function p2-stream-element-type (form target representation)
  ((check-arg-count form 1))
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
(define-inlined-function p2-write-8-bits (form target representation)
  ((check-arg-count form 2))
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
  (let* ((args (cdr form))
         (len (length args)))
    (case len
      (1
       (let* ((arg1 (%car args))
              (type1 (derive-compiler-type arg1)))
         (cond ((compiler-subtypep type1 'stream)
		(compile-forms-and-maybe-emit-clear-values arg1 'stack nil)
                (emit 'checkcast +lisp-stream-class+)
                (emit-push-constant-int 1)
                (emit-push-nil)
                (emit-invokevirtual +lisp-stream-class+ "readLine"
                                    (list "Z" +lisp-object+) +lisp-object+)
                (emit-move-from-stack target))
               (t
                (compile-function-call form target representation)))))
      (2
       (let* ((arg1 (%car args))
              (type1 (derive-compiler-type arg1))
              (arg2 (%cadr args)))
         (cond ((and (compiler-subtypep type1 'stream) (null arg2))
		(compile-forms-and-maybe-emit-clear-values arg1 'stack nil)
                (emit 'checkcast +lisp-stream-class+)
                (emit-push-constant-int 0)
                (emit-push-nil)
                (emit-invokevirtual +lisp-stream-class+ "readLine"
                                    (list "Z" +lisp-object+) +lisp-object+)
                (emit-move-from-stack target)
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
                                      (or high1 high2))))
               ((and low1 (>= low1 0))
                ;; arg1 is non-negative
                (dformat t "arg1 is non-negative~%")
                (setf result-low 0)
                (setf result-high high1))
               ((and low2 (>= low2 0))
                ;; arg2 is non-negative
                (dformat t "arg2 is non-negative~%")
                (setf result-low 0)
                (setf result-high high2)))
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


(defmacro define-int-bounds-derivation (name (low1 high1 low2 high2)
                                        &body body)
  "Associates an integer-bounds calculation function with a numeric
operator `name', assuming 2 integer arguments."
  `(setf (get ',name 'int-bounds)
         #'(lambda (,low1 ,high1 ,low2 ,high2)
             (declare (ignorable ,low1 ,high1 ,low2 ,high2))
             ,@body)))

(defun derive-integer-type (op type1 type2)
  "Derives the composed integer type of operation `op' given integer
types `type1' and `type2'."
  (let ((low1 (integer-type-low type1))
        (high1 (integer-type-high type1))
        (low2 (integer-type-low type2))
        (high2 (integer-type-high type2))
        (op-fn (get op 'int-bounds)))
    (assert op-fn)
    (multiple-value-bind
          (low high non-int-p)
        (funcall op-fn low1 high1 low2 high2)
      (if non-int-p
          non-int-p
          (%make-integer-type low high)))))

(defvar numeric-op-type-derivation
  `(((+ - *)
     (integer integer ,#'derive-integer-type)
     (integer single-float single-float)
     (integer double-float double-float)
     (single-float integer single-float)
     (single-float double-float double-float)
     (double-float integer double-float)
     (double-float single-float double-float))
    ((/)
     (integer single-float single-float)
     (integer double-float double-float)
     (single-float integer single-float)
     (single-float double-float double-float)
     (double-float integer double-float)
     (double-float single-float double-float))
    ((ash)
     (integer integer ,#'derive-integer-type))
    ((min max)
     (integer integer ,#'derive-integer-type)
     (integer single-float single-float)
     (integer double-float double-float)
     (single-float double-float double-float)
     (double-float single-float double-float)))
  "Table used to derive the return type of a numeric operation,
based on the types of the arguments.")

(defun derive-type-numeric-op (op &rest types)
  "Returns the result type of the numeric operation `op' and the types
of the operation arguments given in `types'."
  (let ((types-table
         (cdr (assoc op numeric-op-type-derivation :test #'member))))
    (assert types-table)
    (flet ((match (type1 type2)
             (do* ((remaining-types types-table (cdr remaining-types)))
                  ((endp remaining-types)
                   ;; when we don't find a matching type, return T
                   T)
               (destructuring-bind
                     (t1 t2 result-type)
                   (car remaining-types)
                 (when (and (or (subtypep type1 t1)
                                (compiler-subtypep type1 t1))
                            (or (subtypep type2 t2)
                                (compiler-subtypep type2 t2)))
                   (return-from match
                     (if (functionp result-type)
                         (funcall result-type op type1 type2)
                         result-type)))))))
      (let ((type1 (car types))
            (type2 (cadr types)))
        (when (and (eq type1 type2)
                   (memq type1 '(SINGLE-FLOAT DOUBLE-FLOAT)))
          (return-from derive-type-numeric-op type1))
        (match type1 type2)))))

(defvar zero-integer-type (%make-integer-type 0 0)
  "Integer type representing the 0 (zero)
value for use with derive-type-minus and derive-type-plus.")

(define-int-bounds-derivation - (low1 high1 low2 high2)
  (values (when (and low1 high2) ;; low1 or high2 undefined: no lower bound
            (if low2
                (min (- low1 low2)
                     (- low1 high2))
                ;; low2 undefined: no effect on lower bound
                (- low1 high2)))
          (when (and high1 low2) ;; high1 or low2 undefined: no upper bound
            (if high2
                (max (- high1 low2)
                     (- high1 high2))
                ;; high2 undefined: no effect on upper bound
                (- high1 low2)))))

(defun derive-compiler-types (args op)
  (flet ((combine (x y)
		  (derive-type-numeric-op op x y)))
    (reduce #'combine (cdr args) :key #'derive-compiler-type
	    :initial-value (derive-compiler-type (car args)))))

(defknown derive-type-minus (t) t)
(defun derive-type-minus (form)
  (let ((op (car form))
        (args (cdr form)))
    (case (length args)
      (1 (derive-type-numeric-op (car form)
                                 zero-integer-type
                                 (derive-compiler-type (%car args))))
      (2 (derive-compiler-types args op)))))

(define-int-bounds-derivation + (low1 high1 low2 high2)
    (values (and low1 low2 (+ low1 low2))
            (and high1 high2 (+ high1 high2))))

(defknown derive-type-plus (t) t)
(defun derive-type-plus (form)
  (let ((op (car form))
        (args (cdr form)))
    (if (null args)
        zero-integer-type
      (derive-compiler-types args op))))

(define-int-bounds-derivation * (low1 high1 low2 high2)
  (cond ((or (null low1) (null low2))
         (values nil nil))
        ((or (null high1) (null high2))
         (values (if (or (minusp low1) (minusp low2))
                     (- (* (abs low1) (abs low2)))
                     (* low1 low2))
                 nil))
        ((or (minusp low1) (minusp low2))
         (let ((max (* (max (abs low1) (abs high1))
                       (max (abs low2) (abs high2)))))
           (values (- max) max)))
        (t
         (values (* low1 low2) (* high1 high2)))))

(defvar one-integer-type (%make-integer-type 1 1)
  "Integer type representing the value 1 (one)
for use with derive-type-times.")

(defun derive-type-times (form)
  (let ((op (car form))
        (args (cdr form)))
    (if (null args)
        one-integer-type
      (derive-compiler-types args op))))

(define-int-bounds-derivation max (low1 low2 high1 high2)
  (values (or (when (and low1 low2) (max low1 low2)) low1 low2)
          (or (when (and high1 high2) (max high1 high2)) high1 high2)))

(declaim (ftype (function (t) t) derive-type-max))
(defun derive-type-max (form)
  (let ((op (car form))
        (args (cdr form)))
    (derive-compiler-types args op)))

(define-int-bounds-derivation min (low1 high1 low2 high2)
  (values (or (when (and low1 low2) (min low1 low2)) low1 low2)
          (or (when (and high1 high2) (min high1 high2)) high1 high2)))

(defknown derive-type-min (t) t)
(defun derive-type-min (form)
  (let ((op (car form))
        (args (cdr form)))
    (derive-compiler-types args op)))

;; read-char &optional input-stream eof-error-p eof-value recursive-p => char
(declaim (ftype (function (t) t) derive-type-read-char))
(defun derive-type-read-char (form)
  (if (< (length form) 3) ; no EOF-ERROR-P arg
      'CHARACTER
      t))


(define-int-bounds-derivation ash (low1 high1 low2 high2)
  (when (and low1 high1 low2 high2)
    (cond
      ((and (>= low1 0) (>= high1 0) (>= low2 0) (>= high2 0))
       ;; Everything is non-negative.
       (values (ash low1 low2)
               (unless (<= 64 high2)
                 (ash high1 high2))))
      ((and (>= low1 0) (>= high1 0) (<= low2 0) (<= high2 0))
       ;; Negative (or zero) second argument.
       (values (ash low1 low2)
               (ash high1 high2))))))

;; ash integer count => shifted-integer
(defknown derive-type-ash (t) t)
(defun derive-type-ash (form)
  (derive-type-numeric-op (car form)
                          (derive-compiler-type (cadr form))
                          (derive-compiler-type (caddr form))))

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
        ((typep form 'single-float)
         'SINGLE-FLOAT)
        ((typep form 'double-float)
         'DOUBLE-FLOAT)
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
                        ((= 0 (variable-writes variable))
                         (derive-type (variable-initform variable)))
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
        ((node-p form)
         (let ((result t))
;;; ### FIXME
#|
the statements below used to work, maybe ...
We need more thought here.
           (cond ((and (block-node-p form)
                       (equal (block-name form) '(LET)))
                  ;;              (format t "derive-type LET/LET* node case~%")
                  (let* ((forms (cddr (node-form form)))
                         (last-form (car (last forms)))
                         (derived-type (derive-compiler-type last-form)))
                    ;;                (unless (eq derived-type t)
                    ;;                  (let ((*print-structure* nil))
                    ;;                    (format t "last-form = ~S~%" last-form))
                    ;;                  (format t "derived-type = ~S~%" derived-type)
                    ;;                  )
                    (setf result derived-type)))
                 ((and (block-node-p form)
                       (symbolp (block-name form)))
                  (unless (block-return-p form)
                    (let* ((forms (cddr (block-form form)))
                           (last-form (car (last forms)))
                           (derived-type (derive-compiler-type last-form)))
;;                       (unless (eq derived-type t)
;;                         (let ((*print-structure* nil))
;;                           (format t "last-form = ~S~%" last-form))
;;                         (format t "derived-type = ~S~%" derived-type)
;;                         )
                      (setf result derived-type))))) |#
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

(define-inlined-function p2-length (form target representation)
  ((check-arg-count form 1))
  (let ((arg (cadr form)))
    (compile-forms-and-maybe-emit-clear-values arg 'stack nil)
    (ecase representation
      (:int
       (emit-invokevirtual +lisp-object-class+ "length" nil "I"))
      ((:long :float :double)
       (emit-invokevirtual +lisp-object-class+ "length" nil "I")
       (convert-representation :int representation))
      (:boolean
       ;; FIXME We could optimize this all away in unsafe calls.
       (emit-invokevirtual +lisp-object-class+ "length" nil "I")
       (emit 'pop)
       (emit 'iconst_1))
      (:char
       (sys::%format t "p2-length: :char case~%")
       (aver nil))
      ((nil)
       (emit-invokevirtual +lisp-object-class+ "LENGTH" nil +lisp-object+)))
    (emit-move-from-stack target representation)))

(defun cons-for-list/list* (form target representation &optional list-star-p)
  (let* ((args (cdr form))
	 (length (length args))
	 (cons-heads (if list-star-p
			 (butlast args 1)
		       args)))
    (cond ((>= 4 length 1)
	   (dolist (cons-head cons-heads)
	     (emit 'new +lisp-cons-class+)
	     (emit 'dup)
	     (compile-form cons-head 'stack nil))
	   (if list-star-p
	       (compile-form (first (last args)) 'stack nil)
	     (progn
	       (emit-invokespecial-init 
		+lisp-cons-class+ (lisp-object-arg-types 1))
	       (pop cons-heads))) ; we've handled one of the args, so remove it
	   (dolist (cons-head cons-heads)
	     (declare (ignore cons-head))
	     (emit-invokespecial-init 
	      +lisp-cons-class+ (lisp-object-arg-types 2)))
	   (if list-star-p 
	       (progn
		 (apply #'maybe-emit-clear-values args)
		 (emit-move-from-stack target representation))
	     (progn 
	       (unless (every 'single-valued-p args)
		 (emit-clear-values))
	       (emit-move-from-stack target))))
	  (t 
	   (compile-function-call form target representation)))))
	   
	

(defun p2-list (form target representation)
  (cons-for-list/list* form target representation))

(defun p2-list* (form target representation)
  (cons-for-list/list* form target representation t))

(define-inlined-function compile-nth (form target representation)
  ((check-arg-count form 2))
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
    (1 (compile-constant 1 target representation))
    (2 (compile-form (cadr form) target representation))
    (3
     (let* ((args (cdr form))
            (arg1 (%car args))
            (arg2 (%cadr args))
            result-type result-rep value)
       (when (fixnump arg1)
         (rotatef arg1 arg2))
       (setf result-type (derive-compiler-type form)
             result-rep (type-representation result-type))
       (cond ((and (numberp arg1) (numberp arg2))
              (dformat t "p2-times case 1~%")
              (compile-constant (* arg1 arg2) target representation))
             ((setf value (fixnum-constant-value result-type))
              (dformat t "p2-times case 1a~%")
              (compile-constant value target representation))
             (result-rep
              (compile-forms-and-maybe-emit-clear-values
                          arg1 'stack result-rep
                          arg2 'stack result-rep)
              (emit (case result-rep
                      (:int    'imul)
                      (:long   'lmul)
                      (:float  'fmul)
                      (:double 'dmul)
                      (t
                       (sys::format t "p2-times: unsupported rep case"))))
              (convert-representation result-rep representation)
	      (emit-move-from-stack target representation))
             ((fixnump arg2)
	      (compile-forms-and-maybe-emit-clear-values arg1 'stack nil)
              (emit-push-int arg2)
              (emit-invokevirtual +lisp-object-class+ "multiplyBy" '("I") +lisp-object+)
              (fix-boxing representation result-type)
              (emit-move-from-stack target representation))
             (t
              (dformat t "p2-times case 4~%")
              (compile-binary-operation "multiplyBy" args target representation)))))
    (t
     (dformat t "p2-times case 5~%")
     (p2-times `(,(car form) (,(car form) ,(second form) ,(third form))
                    ,@(nthcdr 3 form)) target representation))))

(defknown p2-min/max (t t t) t)
(defun p2-min/max (form target representation)
  (case (length form)
    (1 (error 'program-error "Wrong number of arguments for ~A." (car form)))
    (2 (compile-form (cadr form) target representation))
    (3 (let* ((op (%car form))
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
           (cond ((and (java-long-type-p type1) (java-long-type-p type2))
                  (let ((common-rep (if (and (fixnum-type-p type1)
                                             (fixnum-type-p type2))
                                        :int :long))
                        (LABEL1 (gensym)))
                    (compile-form arg1 'stack common-rep)
                    (emit-dup common-rep)
                    (compile-form arg2 'stack common-rep)
                    (emit-dup common-rep :past common-rep)
                    (emit-numeric-comparison (if (eq op 'max) '<= '>=)
                                             common-rep LABEL1)
                    (emit-swap common-rep common-rep)
                    (label LABEL1)
                    (emit-move-from-stack nil common-rep)
                    (convert-representation common-rep representation)
                    (emit-move-from-stack target representation)))
                 (t
                  (compile-form arg1 'stack nil)
                  (emit-dup nil)
                  (compile-form arg2 'stack nil)
                  (emit-dup nil :past nil)
                  (emit-invokevirtual +lisp-object-class+
                                      (if (eq op 'max)
                                          "isLessThanOrEqualTo"
                                          "isGreaterThanOrEqualTo")
                                      (lisp-object-arg-types 1) "Z")
                  (let ((LABEL1 (gensym)))
                    (emit 'ifeq LABEL1)
                    (emit 'swap)
                    (label LABEL1)
                    (emit 'pop))
                  (fix-boxing representation nil)
                  (emit-move-from-stack target representation))))))
    (t
     (p2-min/max `(,(car form) (,(car form) ,(second form) ,(third form))
                    ,@(nthcdr 3 form)) target representation))))

(defun p2-plus (form target representation)
  (case (length form)
    (1
     (compile-constant 0 target representation))
    (2
     (compile-form (cadr form) target representation))
    (3
     (let* ((args (%cdr form))
            (arg1 (%car args))
            (arg2 (%cadr args))
            (type1 (derive-compiler-type arg1))
            (type2 (derive-compiler-type arg2))
            (result-type (derive-compiler-type form))
            (result-rep (type-representation result-type)))
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
             (result-rep
              (compile-forms-and-maybe-emit-clear-values
                        arg1 'stack result-rep
                        arg2 'stack result-rep)
              (emit (case result-rep
                      (:int    'iadd)
                      (:long   'ladd)
                      (:float  'fadd)
                      (:double 'dadd)
                      (t
                       (sys::format
                        t "p2-plus: Unexpected result-rep ~S for form ~S."
                        result-rep form)
                       (assert nil))))
              (convert-representation result-rep representation)
              (emit-move-from-stack target representation))
             ((eql arg2 1)
	      (compile-forms-and-maybe-emit-clear-values arg1 'stack nil)
              (emit-invoke-method "incr" target representation))
             ((eql arg1 1)
	      (compile-forms-and-maybe-emit-clear-values arg2 'stack nil)
              (emit-invoke-method "incr" target representation))
             ((or (fixnum-type-p type1) (fixnum-type-p type2))
	      (compile-forms-and-maybe-emit-clear-values
                    arg1 'stack (when (fixnum-type-p type1) :int)
                    arg2 'stack (when (null (fixnum-type-p type1)) :int))
              (when (fixnum-type-p type1)
                (emit 'swap))
              (emit-invokevirtual +lisp-object-class+ "add"
                                  '("I") +lisp-object+)
              (fix-boxing representation result-type)
              (emit-move-from-stack target representation))
             (t
              (compile-binary-operation "add" args target representation)))))
    (t
     ;; (+ a b c) => (+ (+ a b) c)
     (let ((new-form `(+ (+ ,(second form) ,(third form)) ,@(nthcdr 3 form))))
       (p2-plus new-form target representation)))))

(defun p2-minus (form target representation)
  (case (length form)
    (1
     ;; generates "Insufficient arguments" error
     (compile-function-call form target representation))
    (2
     (let* ((arg (%cadr form))
            (type (derive-compiler-type form))
            (type-rep (type-representation type)))
       (cond ((numberp arg)
              (compile-constant (- arg) 'stack representation)
              (emit-move-from-stack target representation))
             (type-rep
              (compile-form arg 'stack type-rep)
              (emit (case type-rep
                      (:int    'ineg)
                      (:long   'lneg)
                      (:float  'fneg)
                      (:double 'dneg)
                      (t
                       (sys::format t
                                    "p2-minus: unsupported rep (~S) for '~S'~%"
                                    type-rep form)
                       (assert nil))))
              (convert-representation type-rep representation)
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
            (type2 (derive-compiler-type arg2))
            (result-type (derive-compiler-type form))
            (result-rep (type-representation result-type)))
       (cond ((and (numberp arg1) (numberp arg2))
              (compile-constant (- arg1 arg2) target representation))
             (result-rep
	      (compile-forms-and-maybe-emit-clear-values
                        arg1 'stack result-rep
                        arg2 'stack result-rep)
              (emit (case result-rep
                      (:int    'isub)
                      (:long   'lsub)
                      (:float  'fsub)
                      (:double 'dsub)
                      (t
                       (sys::%format t "p2-minus sub-instruction (rep: ~S); form: ~S~%"
                                     result-rep form)
                       (assert nil))))
              (convert-representation result-rep representation)
              (emit-move-from-stack target representation))
             ((fixnum-type-p type2)
	      (compile-forms-and-maybe-emit-clear-values
                    arg1 'stack nil
                    arg2 'stack :int)
              (emit-invokevirtual +lisp-object-class+
                                  "subtract"
                                  '("I") +lisp-object+)
              (fix-boxing representation result-type)
              (emit-move-from-stack target representation))
             (t
              (compile-binary-operation "subtract" args target representation)))))
    (t
     (let ((new-form `(- (- ,(second form) ,(third form)) ,@(nthcdr 3 form))))
       (p2-minus new-form target representation)))))

;; char/schar string index => character
(defknown p2-char/schar (t t t) t)
(define-inlined-function p2-char/schar (form target representation)
  ((check-arg-count form 2))
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
(define-inlined-function p2-set-char/schar (form target representation)
  ((check-arg-count form 3))
  (let* ((op (%car form))
         (args (%cdr form))
         (arg1 (first args))
         (arg2 (second args))
         (arg3 (third args))
         (type1 (derive-compiler-type arg1))
         (type2 (derive-compiler-type arg2))
         (type3 (derive-compiler-type arg3)))
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
               (emit 'iload value-register)
               (convert-representation :char representation)
               (emit-move-from-stack target representation))))
          (t
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
             (aload value-register)
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
    (fix-boxing representation nil) ; FIXME use derived result type
    (emit-move-from-stack target representation)))

(defun p2-elt (form target representation)
  (cond ((and (check-arg-count form 2)
              (fixnum-type-p (derive-compiler-type (third form)))
              (neq representation :char)) ; FIXME
         (compile-form (second form) 'stack nil)
         (compile-form (third form) 'stack :int)
         (emit-invokevirtual +lisp-object-class+ "elt" '("I") +lisp-object+)
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
       (ecase representation
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
         ((nil :float :double :boolean)
          ;;###FIXME for float and double, we probably want
          ;; separate java methods to retrieve the values.
	  (compile-forms-and-maybe-emit-clear-values arg1 'stack nil
						     arg2 'stack :int)
          (emit-invokevirtual +lisp-object-class+ "AREF" '("I") +lisp-object+)
          (convert-representation nil representation)))
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
                (value-register (unless (null target) (allocate-register))))
           ;; array
           (compile-form arg1 'stack nil)
           ;; index
           (compile-form arg2 'stack :int)
           ;; value
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
           (maybe-emit-clear-values arg1 arg2 arg3)
           (cond ((fixnum-type-p type3)
                  (emit-invokevirtual +lisp-object-class+ "aset" '("I" "I") nil))
                 (t
                  (emit-invokevirtual +lisp-object-class+ "aset" (list "I" +lisp-object+) nil)))
           (when value-register
             (cond ((fixnum-type-p type3)
                    (emit 'iload value-register)
                    (convert-representation :int representation))
                   (t
                    (aload value-register)
                    (fix-boxing representation type3)))
             (emit-move-from-stack target representation))))
        (t
         (compile-function-call form target representation))))

(defknown p2-structure-ref (t t t) t)
(define-inlined-function p2-structure-ref (form target representation)
  ((check-arg-count form 2))
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
           (ecase representation
             (:int
              (emit-invokevirtual +lisp-object-class+ "getFixnumSlotValue"
                                  '("I") "I"))
             ((nil :char :long :float :double)
              (emit-invokevirtual +lisp-object-class+ "getSlotValue"
                                  '("I") +lisp-object+)
              ;; (convert-representation NIL NIL) is a no-op
              (convert-representation nil representation))
             (:boolean
              (emit-invokevirtual +lisp-object-class+ "getSlotValueAsBoolean"
                                  '("I") "Z")))
           (emit-move-from-stack target representation))
          (t
           (compile-function-call form target representation)))))

(defknown p2-structure-set (t t t) t)
(define-inlined-function p2-structure-set (form target representation)
  ((check-arg-count form 3))
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
              (astore value-register))
            (emit-invokevirtual +lisp-object-class+
                                (format nil "setSlotValue_~D" arg2)
                                (lisp-object-arg-types 1) nil)
            (when value-register
              (aload value-register)
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
              (astore value-register))
            (emit-invokevirtual +lisp-object-class+ "setSlotValue"
                                (list "I" +lisp-object+) nil)
            (when value-register
              (aload value-register)
              (fix-boxing representation nil)
              (emit-move-from-stack target representation))))
         (t
          (compile-function-call form target representation)))))


(define-inlined-function p2-not/null (form target representation)
  ((aver (or (null representation) (eq representation :boolean)))
   (check-arg-count form 1))
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
             (label LABEL1)
             (emit-push-false representation)
             (label LABEL2)))
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
             (label LABEL1)
             (emit-push-t)
             (label LABEL2)))
          (t
	   (compile-forms-and-maybe-emit-clear-values arg 'stack nil)
           (let ((LABEL1 (gensym))
                 (LABEL2 (gensym)))
             (emit-push-nil)
             (emit 'if_acmpeq LABEL1)
             (emit-push-nil)
             (emit 'goto LABEL2)
             (label LABEL1)
             (emit-push-t)
             (label LABEL2)))))
  (emit-move-from-stack target representation))

(define-inlined-function p2-nthcdr (form target representation)
  ((check-arg-count form 2))
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
         (ecase representation
           (:boolean
	    (compile-forms-and-maybe-emit-clear-values arg2 'stack :boolean)
            (emit 'goto DONE)
            (label FAIL)
            (emit 'iconst_0))
           ((nil)
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
         (label LABEL1)
         (fix-boxing representation nil) ; FIXME use derived result type
         (label LABEL2)
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

(defun compile-special-reference (variable target representation)
  (let ((name (variable-name variable)))
    (when (constantp name)
      (let ((value (symbol-value name)))
        (when (or (null *file-compilation*)
                  (stringp value)
                  (numberp value)
                  (packagep value))
          (compile-constant value target representation)
          (return-from compile-special-reference))))
    (unless (and (variable-binding-register variable)
                 (eq (variable-compiland variable) *current-compiland*)
                 (not (enclosed-by-runtime-bindings-creating-block-p
                       (variable-block variable))))
      (emit-load-externalized-object name))
    (cond ((constantp name)
           ;; "... a reference to a symbol declared with DEFCONSTANT always
           ;; refers to its global value."
           (emit-invokevirtual +lisp-symbol-class+ "getSymbolValue"
                               nil +lisp-object+))
          ((and (variable-binding-register variable)
                (eq (variable-compiland variable) *current-compiland*)
                (not (enclosed-by-runtime-bindings-creating-block-p
                      (variable-block variable))))
           (aload (variable-binding-register variable))
           (emit 'getfield +lisp-special-binding-class+ "value"
                 +lisp-object+))
          (t
           (emit-push-current-thread)
           (emit-invokevirtual +lisp-symbol-class+ "symbolValue"
                               (list +lisp-thread+) +lisp-object+)))
    (fix-boxing representation nil)
    (emit-move-from-stack target representation)))

(defknown compile-var-ref (t t t) t)
(defun compile-var-ref (ref target representation)
  (when target
    (if (var-ref-constant-p ref)
        (compile-constant (var-ref-constant-value ref) target representation)
        (let ((variable (var-ref-variable ref)))
          (cond ((variable-special-p variable)
                 (compile-special-reference variable target representation))
                ((or (variable-representation variable)
                     (variable-register variable)
                     (variable-closure-index variable)
                     (variable-index variable)
                     (variable-environment variable))
                 (emit-push-variable variable)
                 (convert-representation (variable-representation variable)
                                         representation)
                 (emit-move-from-stack target representation))
                (t
                 (sys::%format t "compile-var-ref general case~%")
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
    (return-from p2-setq (compile-form (precompiler:precompile-form form t
                                                        *compile-file-environment*)
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
      (cond ((and variable
                  (variable-binding-register variable)
                  (eq (variable-compiland variable) *current-compiland*)
                  (not (enclosed-by-runtime-bindings-creating-block-p
                        (variable-block variable))))
             (aload (variable-binding-register variable))
             (compile-forms-and-maybe-emit-clear-values value-form 'stack nil)
             (emit 'dup_x1) ;; copy past th
             (emit 'putfield +lisp-special-binding-class+ "value"
                   +lisp-object+))
            ((and (consp value-form)
                  (eq (first value-form) 'CONS)
                  (= (length value-form) 3)
                  (var-ref-p (third value-form))
                  (eq (variable-name (var-ref-variable (third value-form))) name))
             (emit-push-current-thread)
             (emit-load-externalized-object name)
	     (compile-forms-and-maybe-emit-clear-values (second value-form) 'stack nil)
             (emit-invokevirtual +lisp-thread-class+ "pushSpecial"
                                 (list +lisp-symbol+ +lisp-object+) +lisp-object+))
            (t
             (emit-push-current-thread)
             (emit-load-externalized-object name)
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
          (emit 'iload (variable-register variable))
          (convert-representation :int representation)
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
             (convert-representation :int representation)
             (emit-move-from-stack target representation)))
          ((and (eq (variable-representation variable) :int)
                (or (equal value-form (list '1- (variable-name variable)))
                    (equal value-form (list '- (variable-name variable) 1))))
           (dformat t "p2-setq decf :int case~%")
           (emit 'iinc (variable-register variable) -1)
           (when target
             (convert-representation :int representation)
             (emit-move-from-stack target representation)))
          (t
           (let ((rep (variable-representation variable)))
             (dformat t "p2-setq ~A case value-form = ~S~%" rep value-form)
             (compile-forms-and-maybe-emit-clear-values value-form 'stack rep)
             (when target
               (emit-dup rep))
             (emit-move-to-variable variable)
             (when target
               (convert-representation rep representation)
               (emit-move-from-stack target representation)))))))

(defun p2-sxhash (form target representation)
  (cond ((check-arg-count form 1)
         (let ((arg (%cadr form)))
	   (compile-forms-and-maybe-emit-clear-values arg 'stack nil)
           (emit-invokevirtual +lisp-object-class+ "sxhash" nil "I")
           (convert-representation :int representation)
           (emit-move-from-stack target representation)))
        (t
         (compile-function-call form target representation))))

(defknown p2-symbol-name (t t t) t)
(define-inlined-function p2-symbol-name (form target representation)
  ((check-arg-count form 1))
  (let ((arg (%cadr form)))
    (cond ((and (eq (derive-compiler-type arg) 'SYMBOL) (< *safety* 3))
	   (compile-forms-and-maybe-emit-clear-values arg 'stack nil)
           (emit 'checkcast +lisp-symbol-class+)
           (emit 'getfield  +lisp-symbol-class+ "name" +lisp-simple-string+)
           (emit-move-from-stack target representation))
          (t
           (compile-function-call form target representation)))))

(defknown p2-symbol-package (t t t) t)
(define-inlined-function p2-symbol-package (form target representation)
  ((check-arg-count form 1))
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
    (cond ((and (subtypep type-form 'FIXNUM)
                (consp value-form)
                (eq (car value-form) 'structure-ref))
           ;; Special case for structure slot references: getFixnumSlotValue()
           ;; signals an error if the slot's value is not a fixnum.
           (compile-form value-form target representation))
          ((and (> *safety* 0)
                (not (compiler-subtypep (derive-type value-form) type-form)))
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
(define-inlined-function p2-char-code (form target representation)
  ((check-arg-count form 1))
  (let ((arg (second form)))
    (cond ((characterp arg)
           (compile-constant (char-code arg) target representation))
          ((and (< *safety* 3)
                (eq (derive-compiler-type arg) 'character))
           (compile-form arg 'stack :char)
           ;; we change the representation between the above and here
           ;;  ON PURPOSE!
	   (convert-representation :int representation)
           (emit-move-from-stack target representation))
          (t
           (compile-function-call form target representation)))))

(defknown p2-java-jclass (t t t) t)
(define-inlined-function p2-java-jclass (form target representation)
  ((and (= 2 (length form))
	(stringp (cadr form))))
  (let ((c (ignore-errors (java:jclass (cadr form)))))
    (if c (compile-constant c target representation)
      ;; delay resolving the method to run-time; it's unavailable now
      (compile-function-call form target representation))))

(defknown p2-java-jconstructor (t t t) t)
(define-inlined-function p2-java-jconstructor (form target representation)
  ((and (< 1 (length form))
	(every #'stringp (cdr form))))
  (let ((c (ignore-errors (apply #'java:jconstructor (cdr form)))))
    (if c (compile-constant c target representation)
      ;; delay resolving the method to run-time; it's unavailable now
      (compile-function-call form target representation))))

(defknown p2-java-jmethod (t t t) t)
(define-inlined-function p2-java-jmethod (form target representation)
  ((and (< 1 (length form))
	(every #'stringp (cdr form))))
  (let ((m (ignore-errors (apply #'java:jmethod (cdr form)))))
    (if m (compile-constant m target representation)
      ;; delay resolving the method to run-time; it's unavailable now
      (compile-function-call form target representation))))

#|(defknown p2-java-jcall (t t t) t)
(define-inlined-function p2-java-jcall (form target representation)
  ((and (> *speed* *safety*)
	(< 1 (length form))
	(eq 'jmethod (car (cadr form)))
	(every #'stringp (cdr (cadr form)))))
  (let ((m (ignore-errors (eval (cadr form)))))
    (if m 
	(let ((must-clear-values nil)
	      (arg-types (raw-arg-types (jmethod-params m))))
	  (declare (type boolean must-clear-values))
	  (dolist (arg (cddr form))
	    (compile-form arg 'stack nil)
	    (unless must-clear-values
	      (unless (single-valued-p arg)
		(setf must-clear-values t))))
	  (when must-clear-values
	    (emit-clear-values))
	  (dotimes (i (jarray-length raw-arg-types))
	    (push (jarray-ref raw-arg-types i) arg-types))
	  (emit-invokevirtual (jclass-name (jmethod-declaring-class m))
			      (jmethod-name m)
			      (nreverse arg-types)
			      (jmethod-return-type m)))
      ;; delay resolving the method to run-time; it's unavailable now
      (compile-function-call form target representation))))|#

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

(defknown p2-threads-synchronized-on (t t) t)
(defun p2-threads-synchronized-on (block target)
  (let* ((form (synchronized-form block))
         (*register* *register*)
         (object-register (allocate-register))
         (BEGIN-PROTECTED-RANGE (gensym))
         (END-PROTECTED-RANGE (gensym))
         (EXIT (gensym)))
    (compile-form (cadr form) 'stack nil)
    (emit-invokevirtual +lisp-object-class+ "lockableInstance" nil
                        +java-object+) ; value to synchronize
    (emit 'dup)
    (astore object-register)
    (emit 'monitorenter)
    (label BEGIN-PROTECTED-RANGE)
    (let ((*blocks* (cons block *blocks*)))
      (compile-progn-body (cddr form) target))
    (emit 'goto EXIT)
    (label END-PROTECTED-RANGE)
    (aload object-register)
    (emit 'monitorexit)
    (emit 'athrow)

    (label EXIT)
    (aload object-register)
    (emit 'monitorexit)
    (push (make-handler :from BEGIN-PROTECTED-RANGE
                        :to END-PROTECTED-RANGE
                        :code END-PROTECTED-RANGE
                        :catch-type 0) *handlers*)))


(defknown p2-catch-node (t t) t)
(defun p2-catch-node (block target)
  (let ((form (catch-form block)))
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
      (aload tag-register)
      (emit-invokevirtual +lisp-thread-class+ "pushCatchTag"
                          (lisp-object-arg-types 1) nil)
      (let ((*blocks* (cons block *blocks*)))
        ; Stack depth is 0.
        (label BEGIN-PROTECTED-RANGE) ; Start of protected range.
        (compile-progn-body (cddr form) target) ; Implicit PROGN.
        (label END-PROTECTED-RANGE) ; End of protected range.
        (emit 'goto EXIT)) ; Jump over handlers.
      (label THROW-HANDLER) ; Start of handler for THROW.
      ;; The Throw object is on the runtime stack. Stack depth is 1.
      (emit 'dup) ; Stack depth is 2.
      (emit 'getfield +lisp-throw-class+ "tag" +lisp-object+) ; Still 2.
      (aload tag-register) ; Stack depth is 3.
      ;; If it's not the tag we're looking for, we branch to the start of the
      ;; catch-all handler, which will do a re-throw.
      (emit 'if_acmpne DEFAULT-HANDLER) ; Stack depth is 1.
      (emit-push-current-thread)
      (emit-invokevirtual +lisp-throw-class+ "getResult"
                          (list +lisp-thread+) +lisp-object+)
      (emit-move-from-stack target) ; Stack depth is 0.
      (emit 'goto EXIT)
      (label DEFAULT-HANDLER) ; Start of handler for all other Throwables.
      ;; A Throwable object is on the runtime stack here. Stack depth is 1.
      (emit-push-current-thread)
      (emit-invokevirtual +lisp-thread-class+ "popCatchTag" nil nil)
      (emit 'athrow) ; Re-throw.
      (label EXIT)
      ;; Finally...
      (emit-push-current-thread)
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
  (let ((form (unwind-protect-form block)))
    (when (= (length form) 2) ; No cleanup form.
      (compile-form (second form) target nil)
      (return-from p2-unwind-protect-node))

    ;; The internal representation of UNWIND-PROTECT
    ;; as generated by P1-UNWIND-PROTECT differs a bit
    ;; from what the spec says; ours is:
    ;; (UNWIND-PROTECT protected-form (progn cleanup-forms) cleanup-forms),
    ;; because we need to compile the cleanup forms twice and
    ;; we can compile a p1 outcome only once.
    ;;
    ;; We used to use JSR and RET JVM instructions to prevent
    ;; duplication of output code. However, this led to JVM stack
    ;; inconsistency errors
    ;; (see http://trac.common-lisp.net/armedbear/ticket/21)
    (let* ((protected-form (cadr form))
           (unwinding-form (caddr form))
           (cleanup-forms (cdddr form))
           (*register* *register*)
           (exception-register (allocate-register))
           (result-register (allocate-register))
           (values-register (allocate-register))
           (BEGIN-PROTECTED-RANGE (gensym))
           (END-PROTECTED-RANGE (gensym))
           (HANDLER (gensym))
           (EXIT (gensym)))
      ;; Make sure there are no leftover multiple return values from previous calls.
      (emit-clear-values)

      (let* ((*blocks* (cons block *blocks*)))
        (label BEGIN-PROTECTED-RANGE)
        (compile-form protected-form result-register nil)
        (unless (single-valued-p protected-form)
          (emit-push-current-thread)
          (emit 'getfield +lisp-thread-class+ "_values" +lisp-object-array+)
          (astore values-register))
        (label END-PROTECTED-RANGE))
      (let ((*register* *register*))
        (compile-form unwinding-form nil nil))
      (when (single-valued-p protected-form)
        ;; otherwise, we'll load the values register below
        (maybe-emit-clear-values unwinding-form))
      (emit 'goto EXIT) ; Jump over handler.
      (label HANDLER) ; Start of exception handler.
      ;; The Throwable object is on the runtime stack. Stack depth is 1.
      (astore exception-register)
      (emit-push-current-thread)
      (emit 'getfield +lisp-thread-class+ "_values" +lisp-object-array+)
      (astore values-register)
      (let ((*register* *register*))
        (dolist (subform cleanup-forms)
          (compile-form subform nil nil)))
      (maybe-emit-clear-values cleanup-forms)
      (emit-push-current-thread)
      (aload values-register)
      (emit 'putfield +lisp-thread-class+ "_values" +lisp-object-array+)
      (aload exception-register)
      (emit 'athrow) ; Re-throw exception.
      (label EXIT)
      ;; Restore multiple values returned by protected form.
      (unless (single-valued-p protected-form)
        (emit-push-current-thread)
        (aload values-register)
        (emit 'putfield +lisp-thread-class+ "_values" +lisp-object-array+))
      ;; Result.
      (aload result-register)
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
                (ecase representation
                  (:boolean
                   (emit 'iconst_1))
                  ((nil)
                   (emit-load-externalized-object form)))
                (emit-move-from-stack target representation))
               (t
                ;; Shouldn't happen.
                (aver nil))))
        ((var-ref-p form)
         (compile-var-ref form target representation))
        ((node-p form)
         (cond
           ((block-node-p form)
            (p2-block-node form target representation))
           ((let/let*-node-p form)
            (p2-let/let*-node form target representation))
           ((tagbody-node-p form)
            (p2-tagbody-node form target)
            (fix-boxing representation nil))
           ((unwind-protect-node-p form)
            (p2-unwind-protect-node form target)
            (fix-boxing representation nil))
           ((m-v-b-node-p form)
            (p2-m-v-b-node form target)
            (fix-boxing representation nil))
           ((flet-node-p form)
            (p2-flet-node form target representation))
           ((labels-node-p form)
            (p2-labels-node form target representation))
           ((locally-node-p form)
            (p2-locally-node form target representation))
           ((catch-node-p form)
            (p2-catch-node form target)
            (fix-boxing representation nil))
           ((progv-node-p form)
            (p2-progv-node form target representation))
           ((synchronized-node-p form)
            (p2-threads-synchronized-on form target)
            (fix-boxing representation nil))
           (t
            (aver (not "Can't happen")))
))
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
          (get-descriptor (list +lisp-object-array+) +lisp-object+)))
      (return-from analyze-args
        (cond ((<= arg-count call-registers-limit)
               (get-descriptor (lisp-object-arg-types arg-count) +lisp-object+))
              (t (setf *using-arg-array* t)
                 (setf (compiland-arity compiland) arg-count)
                 (get-descriptor (list +lisp-object-array+) +lisp-object+)))))
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

(defmacro with-open-class-file ((var class-file) &body body)
  `(with-open-file (,var (abcl-class-file-pathname ,class-file)
			 :direction :output
			 :element-type '(unsigned-byte 8)
			 :if-exists :supersede)
     ,@body))

(defun write-class-file (class-file stream)
  (let* ((super (abcl-class-file-superclass class-file))
         (this-index (pool-class (abcl-class-file-class class-file)))
         (super-index (pool-class super))
         (constructor (make-constructor super
                                        (abcl-class-file-lambda-name class-file)
                                        (abcl-class-file-lambda-list class-file))))
    (pool-name "Code") ; Must be in pool!

    (when *file-compilation*
      (pool-name "SourceFile") ; Must be in pool!
      (pool-name (file-namestring *compile-file-truename*)))
    (when (and (boundp '*source-line-number*)
               (fixnump *source-line-number*))
      (pool-name "LineNumberTable")) ; Must be in pool!
    
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
    (write-u2 (1+ (length (abcl-class-file-methods class-file))) stream)
    ;; methods
    (dolist (method (abcl-class-file-methods class-file))
      (write-method method stream))
    (write-method constructor stream)
    ;; attributes count
    (cond (*file-compilation*
	   ;; attributes count
	   (write-u2 1 stream)
	   ;; attributes table
	   (write-source-file-attr (file-namestring *compile-file-truename*)
				   stream))
	  (t
	   ;; attributes count
	   (write-u2 0 stream)))
    stream))

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
      (when (memq (type-representation (variable-declared-type variable))
                  '(:int :long))
        (emit-push-variable variable)
        (derive-variable-representation variable nil)
        (when (< 1 (representation-size (variable-representation variable)))
          (allocate-variable-register variable))
        (convert-representation nil (variable-representation variable))
        (emit-move-to-variable variable))))
  t)

(defknown p2-compiland (t) t)
(defun p2-compiland (compiland)
  (let* ((p1-result (compiland-p1-result compiland))
         (class-file (compiland-class-file compiland))
         (*this-class* (abcl-class-file-class class-file))
         (args (cadr p1-result))
         (closure-args (intersection *closure-variables*
                                     (compiland-arg-vars compiland)))
         (local-closure-vars
          (find compiland *closure-variables* :key #'variable-compiland))
         (body (cddr p1-result))
         (*using-arg-array* nil)
         (*hairy-arglist-p* nil)
         ;; *hairy-arglist-p* != NIL --> *using-arglist-array* != NIL

         (*child-p* (not (null (compiland-parent compiland))))

         (descriptor (analyze-args compiland))
         (execute-method (make-method :name "execute"
                                      :descriptor descriptor))
         (*code* ())
         (*register* 1) ;; register 0: "this" pointer
         (*registers-allocated* 1)
         (*handlers* ())
         (*visible-variables* *visible-variables*)

         (*thread* nil)
         (*initialize-thread-var* nil)
         (label-START (gensym)))

    (dolist (var (compiland-arg-vars compiland))
      (push var *visible-variables*))
    (dolist (var (compiland-free-specials compiland))
      (push var *visible-variables*))

    (when *using-arg-array*
      (setf (compiland-argument-register compiland) (allocate-register)))

    ;; Assign indices or registers, depending on where the args are
    ;; located: the arg-array or the call-stack
    (let ((index 0))
      (dolist (variable (compiland-arg-vars compiland))
        (aver (null (variable-register variable)))
        (aver (null (variable-index variable)))
        (if *using-arg-array*
            (setf (variable-index variable) index)
            (setf (variable-register variable) (allocate-register)))
        (incf index)))

    ;; Reserve the next available slot for the thread register.
    (setf *thread* (allocate-register))

    (when *closure-variables*
      (setf (compiland-closure-register compiland) (allocate-register))
      (dformat t "p2-compiland 2 closure register = ~S~%"
               (compiland-closure-register compiland)))

    (when *closure-variables*
      (if (not *child-p*)
          (progn
            ;; if we're the ultimate parent: create the closure array
            (emit-push-constant-int (length *closure-variables*))
            (emit 'anewarray +closure-binding-class+))
        (progn
          (aload 0)
          (emit 'getfield +lisp-compiled-closure-class+ "ctx"
                +closure-binding-array+)
          (when local-closure-vars
            ;; in all other cases, it gets stored in the register below
            (emit 'astore (compiland-closure-register compiland))
            (duplicate-closure-array compiland)))))

    ;; Move args from their original registers to the closure variables array
    (when (or closure-args
              (and *closure-variables* (not *child-p*)))
      (dformat t "~S moving arguments to closure array~%"
               (compiland-name compiland))
      (dotimes (i (length *closure-variables*))
        ;; Loop over all slots, setting their value
        ;;  unconditionally if we're the parent creating it (using null
        ;;  values if no real value is available)
        ;; or selectively if we're a child binding certain slots.
        (let ((variable (find i closure-args
                              :key #'variable-closure-index
                              :test #'eql)))
          (when (or (not *child-p*) variable)
            ;; we're the parent, or we have a variable to set.
            (emit 'dup) ; array
            (emit-push-constant-int i)
            (emit 'new +closure-binding-class+)
            (emit 'dup)
            (cond
              ((null variable)
               (assert (not *child-p*))
               (emit 'aconst_null))
              ((variable-register variable)
               (assert (not (eql (variable-register variable)
                                 (compiland-closure-register compiland))))
               (aload (variable-register variable))
               (setf (variable-register variable) nil))
              ((variable-index variable)
               (aload (compiland-argument-register compiland))
               (emit-push-constant-int (variable-index variable))
               (emit 'aaload)
               (setf (variable-index variable) nil))
              (t
               (assert (not "Can't happen!!"))))
            (emit-invokespecial-init +closure-binding-class+
                                     (list +lisp-object+))
            (emit 'aastore)))))

    (when *closure-variables*
      (aver (not (null (compiland-closure-register compiland))))
      (astore (compiland-closure-register compiland))
      (dformat t "~S done moving arguments to closure array~%"
               (compiland-name compiland)))

    ;; If applicable, move args from arg array to registers.
    (when *using-arg-array*
      (dolist (variable (compiland-arg-vars compiland))
        (unless (or (variable-special-p variable)
                    (null (variable-index variable)) ;; not in the array anymore
                    (< (+ (variable-reads variable)
                          (variable-writes variable)) 2))
          (let ((register (allocate-register)))
            (aload (compiland-argument-register compiland))
            (emit-push-constant-int (variable-index variable))
            (emit 'aaload)
            (astore register)
            (setf (variable-register variable) register)
            (setf (variable-index variable) nil)))))

    (p2-compiland-process-type-declarations body)
    (generate-type-checks-for-variables (compiland-arg-vars compiland))

    ;; Unbox variables.
    (dolist (variable (compiland-arg-vars compiland))
      (p2-compiland-unbox-variable variable))

    ;; Establish dynamic bindings for any variables declared special.
    (when (some #'variable-special-p (compiland-arg-vars compiland))
      ;; Save the dynamic environment
      (setf (compiland-environment-register compiland)
            (allocate-register))
      (save-dynamic-environment (compiland-environment-register compiland))
      (label label-START)
      (dolist (variable (compiland-arg-vars compiland))
        (when (variable-special-p variable)
          (setf (variable-binding-register variable) (allocate-register))
          (emit-push-current-thread)
          (emit-push-variable-name variable)
          (cond ((variable-register variable)
                 (aload (variable-register variable))
                 (setf (variable-register variable) nil))
                ((variable-index variable)
                 (aload (compiland-argument-register compiland))
                 (emit-push-constant-int (variable-index variable))
                 (emit 'aaload)
                 (setf (variable-index variable) nil)))
          (emit-invokevirtual +lisp-thread-class+ "bindSpecial"
                              (list +lisp-symbol+ +lisp-object+)
                              +lisp-special-binding+)
          (astore (variable-binding-register variable)))))

    (compile-progn-body body 'stack)

    (when (compiland-environment-register compiland)
      (restore-environment-and-make-handler
       (compiland-environment-register compiland) label-START))

    (unless *code*
      (emit-push-nil))
    (emit 'areturn)

    ;; Warn if any unused args. (Is this the right place?)
    (check-for-unused-variables (compiland-arg-vars compiland))

    ;; Go back and fill in prologue.
    (let ((code *code*))
      (setf *code* ())
      (let ((arity (compiland-arity compiland)))
        (when arity
          (generate-arg-count-check arity)))

      (when *hairy-arglist-p*
        (aload 0) ; this
        (aver (not (null (compiland-argument-register compiland))))
        (aload (compiland-argument-register compiland)) ; arg vector
        (cond ((or (memq '&OPTIONAL args) (memq '&KEY args))
               (ensure-thread-var-initialized)
               (maybe-initialize-thread-var)
	       (emit-push-current-thread)
               (emit-invokevirtual *this-class* "processArgs"
                                   (list +lisp-object-array+ +lisp-thread+)
                                   +lisp-object-array+))
              (t
               (emit-invokevirtual *this-class* "fastProcessArgs"
                                   (list +lisp-object-array+)
                                   +lisp-object-array+)))
        (astore (compiland-argument-register compiland)))

      (maybe-initialize-thread-var)
      (setf *code* (nconc code *code*)))

    (setf (abcl-class-file-superclass class-file)
          (if (or *hairy-arglist-p*
		  (and *child-p* *closure-variables*))
	      +lisp-compiled-closure-class+
	    +lisp-primitive-class+))

    (setf (abcl-class-file-lambda-list class-file) args)
    (setf (method-max-locals execute-method) *registers-allocated*)
    (push execute-method (abcl-class-file-methods class-file))


    ;;;  Move here
    (finalize-code)
    (optimize-code)

    (setf *code* (resolve-instructions *code*))
    (setf (method-max-stack execute-method) (analyze-stack))
    (setf (method-code execute-method) (code-bytes *code*))

    ;; Remove handler if its protected range is empty.
    (setf *handlers*
          (delete-if (lambda (handler)
                       (eql (symbol-value (handler-from handler))
                            (symbol-value (handler-to handler))))
                     *handlers*))
    ;;; to here
    ;;; To a separate function which is part of class file finalization
    ;;;  when we have a section of class-file-generation centered code


    (setf (method-handlers execute-method) (nreverse *handlers*)))
  t)

(defun p2-with-inline-code (form target representation)
  ;;form = (with-inline-code (&optional target-var repr-var) ...body...)
  (destructuring-bind (&optional target-var repr-var) (cadr form)
    (eval `(let (,@(when target-var `((,target-var ,target)))
		 ,@(when repr-var `((,repr-var ,representation))))
	     ,@(cddr form)))))

(defun compile-1 (compiland stream)
  (let ((*all-variables* nil)
        (*closure-variables* nil)
        (*undefined-variables* nil)
        (*local-functions* *local-functions*)
        (*current-compiland* compiland))
    (with-saved-compiler-policy
      ;; Pass 1.
      (p1-compiland compiland)
      ;; *all-variables* doesn't contain variables which
      ;; are in an enclosing lexical environment (variable-environment)
      ;; so we don't need to filter them out
      (setf *closure-variables*
            (remove-if #'variable-special-p
                       (remove-if-not #'variable-used-non-locally-p
                                                 *all-variables*)))
      (let ((i 0))
        (dolist (var (reverse *closure-variables*))
          (setf (variable-closure-index var) i)
          (dformat t "var = ~S closure index = ~S~%" (variable-name var)
                   (variable-closure-index var))
          (incf i)))

      ;; Assert that we're not refering to any variables
      ;; we're not allowed to use
      (assert (= 0
                 (length (remove-if (complement #'variable-references)
                                    (remove-if #'variable-references-allowed-p
                                               *visible-variables*)))))

      ;; Pass 2.
      (with-class-file (compiland-class-file compiland)
        (p2-compiland compiland)
        (write-class-file (compiland-class-file compiland) stream)))))

(defvar *compiler-error-bailout*)

(defun make-compiler-error-form (form)
  `(lambda ,(cadr form)
     (error 'program-error :format-control "Execution of a form compiled with errors.")))

(defun compile-defun (name form environment filespec stream *declare-inline*)
  "Compiles a lambda expression `form'. If `filespec' is NIL,
a random Java class name is generated, if it is non-NIL, it's used
to derive a Java class name from."
  (aver (eq (car form) 'LAMBDA))
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
                                                           :lambda-list (cadr ',form)))
			  ,stream)))
           (*compile-file-environment* environment))
        (compile-1 (make-compiland :name name
                                   :lambda-expression
                                   (precompiler:precompile-form form t
                                                                environment)
                                   :class-file class-file)
		   stream))))

(defvar *catch-errors* t)

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


(defvar *resignal-compiler-warnings* nil
  "Bind this to t inside slime compilation")

(defun handle-warning (condition)
  (cond (*resignal-compiler-warnings*
         (signal condition))
        (t
         (unless *suppress-compiler-warnings*
           (fresh-line *error-output*)
           (note-error-context)
           (format *error-output* "; Caught ~A:~%;   ~A~2%"
                   (type-of condition) condition))
         (muffle-warning))))

(defun handle-compiler-error (condition)
  (fresh-line *error-output*)
  (note-error-context)
  (format *error-output* "; Caught ERROR:~%;   ~A~2%" condition)
  (throw 'compile-defun-abort (funcall *compiler-error-bailout*)))

(defvar *in-compilation-unit* nil)

(defmacro with-compilation-unit (options &body body)
  `(%with-compilation-unit (lambda () ,@body) ,@options))

(defun %with-compilation-unit (fn &key override)
  (if (and *in-compilation-unit* (not override))
      (funcall fn)
      (let ((style-warnings 0)
            (warnings 0)
            (errors 0)
            (*defined-functions* nil)
            (*undefined-functions* nil)
            (*in-compilation-unit* t))
        (unwind-protect
             (handler-bind ((style-warning #'(lambda (c)
                                               (incf style-warnings)
                                               (handle-warning c)))
                            (warning #'(lambda (c)
                                         (incf warnings)
                                         (handle-warning c)))
                            (compiler-error #'(lambda (c)
                                                (incf errors)
                                                (handle-compiler-error c))))
               (funcall fn))
          (unless (or (and *suppress-compiler-warnings* (zerop errors))
                      (and (zerop (+ errors warnings style-warnings))
                           (null *undefined-functions*)))
            (format *error-output*
                    "~%; Compilation unit finished~%")
            (unless (zerop errors)
              (format *error-output*
                      ";   Caught ~D ERROR condition~P~%"
                      errors errors))
            (unless *suppress-compiler-warnings*
              (unless (zerop warnings)
                (format *error-output*
                        ";   Caught ~D WARNING condition~P~%"
                        warnings warnings))
              (unless (zerop style-warnings)
                (format *error-output*
                        ";   Caught ~D STYLE-WARNING condition~P~%"
                        style-warnings style-warnings))
              (when *undefined-functions*
                (format *error-output*
                        ";   The following functions were used but not defined:~%")
                (dolist (name *undefined-functions*)
                  (format *error-output* ";     ~S~%" name))))
            (terpri *error-output*))))))


(defun %jvm-compile (name definition expr env)
  ;; This function is part of the call chain from COMPILE, but
  ;; not COMPILE-FILE
  (let* (compiled-function)
    (with-compilation-unit ()
      (with-saved-compiler-policy
          (setf compiled-function
                (load-compiled-function
                 (with-open-stream (s (sys::%make-byte-array-output-stream))
                   (compile-defun name expr env nil s nil)
                   (finish-output s)
                   (sys::%get-output-stream-bytes s))))))
    (when (and name (functionp compiled-function))
      (sys::set-function-definition name compiled-function definition))
    (or name compiled-function)))


(defun jvm-compile (name &optional definition)
  ;; This function is part of the call chain from COMPILE, but
  ;; not COMPILE-FILE
  (unless definition
    (resolve name) ;; Make sure the symbol has been resolved by the autoloader
    (setf definition (fdefinition name)))
  (when (compiled-function-p definition)
    (return-from jvm-compile (values name nil nil)))
  (let ((catch-errors *catch-errors*)
        (warnings-p nil)
        (failure-p nil)
        (*package* (or (and name (symbol-package name)) *package*))
        (expression definition)
        (*file-compilation* nil)
        (*visible-variables* nil)
        (*local-functions* nil)
        (*pathnames-generator* (constantly nil))
        environment)
    (unless (and (consp definition) (eq (car definition) 'LAMBDA))
      (let ((function definition))
        (when (typep definition 'standard-generic-function)
          (setf function (mop::funcallable-instance-function function)))
        (multiple-value-setq
            (expression environment)
          (function-lambda-expression function))))
    (unless expression
      (error "Can't find a definition for ~S." definition))
    (when environment
      (dolist (var (reverse (environment-all-variables environment)))
        ;; We need to add all variables, even symbol macros,
        ;; because the latter may shadow other variables by the same name
        ;; The precompiler should have resolved all symbol-macros, so
        ;; later we assert we didn't get any references to the symbol-macro.
        (push (make-variable :name (if (symbolp var) var (car var))
                             :special-p (symbolp var)
                             :environment environment
                             :references-allowed-p
                             (not (sys:symbol-macro-p (cdr var)))
                             :compiland NIL) *visible-variables*))
      (dolist (fun (reverse (environment-all-functions environment)))
        (push (make-local-function :name (car fun)
                                   :references-allowed-p
                                   (not (macro-function-p (cdr fun)))
                                   :environment environment)
              *local-functions*)))
    (handler-bind
        ((compiler-unsupported-feature-error
          #'(lambda (c)
              (when catch-errors
                (fresh-line)
                (sys::%format t "; UNSUPPORTED FEATURE: ~A~%" c)
                (sys::%format t "; Unable to compile ~S.~%"
                              (or name "top-level form"))
                (return-from jvm-compile
                  (sys:precompile name definition)))))
         (style-warning
          #'(lambda (c) (declare (ignore c))
              (setf warnings-p t) nil))
         ((or warning compiler-error)
          #'(lambda (c) (declare (ignore c))
              (setf warnings-p t
                    failure-p t)
              nil)))
      (values (%jvm-compile name definition expression environment)
              warnings-p failure-p))))

(defvar *file-compilation* nil)
(defvar *pathnames-generator* #'make-temp-file)

(defun compile (name &optional definition)
  (jvm-compile name definition))

(defmacro with-file-compilation (&body body)
  `(let ((*file-compilation* t)
         (*pathnames-generator* #'sys::next-classfile-name))
     ,@body))



(defun jvm-compile-package (package-designator)
  (let ((pkg (if (packagep package-designator)
                 package-designator
                 (find-package package-designator))))
      (dolist (sym (sys::package-symbols pkg))
        (when (fboundp sym)
          (unless (or (special-operator-p sym) (macro-function sym))
            (jvm-compile sym)))))
  t)

(defun initialize-p2-handlers ()
  (mapc #'install-p2-handler '(declare
                               multiple-value-call
                               multiple-value-list
                               multiple-value-prog1
                               nth
                               progn))
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
;  (install-p2-handler 'java:jcall          'p2-java-jcall)
  (install-p2-handler 'char=               'p2-char=)
  (install-p2-handler 'characterp          'p2-characterp)
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
  (install-p2-handler 'funcall             'p2-funcall)
  (install-p2-handler 'function            'p2-function)
  (install-p2-handler 'gensym              'p2-gensym)
  (install-p2-handler 'get                 'p2-get)
  (install-p2-handler 'getf                'p2-getf)
  (install-p2-handler 'gethash             'p2-gethash)
  (install-p2-handler 'gethash1            'p2-gethash)
  (install-p2-handler 'go                  'p2-go)
  (install-p2-handler 'if                  'p2-if)
  (install-p2-handler 'sys::%length        'p2-length)
  (install-p2-handler 'list                'p2-list)
  (install-p2-handler 'sys::backq-list     'p2-list)
  (install-p2-handler 'list*               'p2-list*)
  (install-p2-handler 'sys::backq-list*    'p2-list*)
  (install-p2-handler 'load-time-value     'p2-load-time-value)
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
  (install-p2-handler 'with-inline-code    'p2-with-inline-code)
  t)

(initialize-p2-handlers)


(defvar sys:*enable-autocompile*)

(defun sys:autocompile (function)
  (when sys:*enable-autocompile*
    (let ((sys:*enable-autocompile* nil))
      (values (compile nil function)))))

(setf sys:*enable-autocompile* t)

(provide "COMPILER-PASS2")
