;;; jvm-instructions.lisp
;;;
;;; Copyright (C) 2003-2006 Peter Graves
;;; Copyright (C) 2010 Erik Huelsmann
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

(in-package #:jvm)


;;    OPCODES

(defconst *opcode-table* (make-array 256))

(defconst *opcodes* (make-hash-table :test 'equalp))

(defstruct jvm-opcode name number size stack-effect register-used)

(defun %define-opcode (name number size stack-effect register)
  (declare (type fixnum number size))
  (let* ((name (string name))
         (opcode (make-jvm-opcode :name name
                                  :number number
                                  :size size
                                  :stack-effect stack-effect
                                  :register-used register)))
     (setf (svref *opcode-table* number) opcode)
     (setf (gethash name *opcodes*) opcode)
     (setf (gethash number *opcodes*) opcode)))

(defmacro define-opcode (name number size stack-effect register)
  `(%define-opcode ',name ,number ,size ,stack-effect ,register))

;; name number size stack-effect register-used
(define-opcode nop 0 1 0 nil)
(define-opcode aconst_null 1 1 1 nil)
(define-opcode iconst_m1 2 1 1 nil)
(define-opcode iconst_0 3 1 1 nil)
(define-opcode iconst_1 4 1 1 nil)
(define-opcode iconst_2 5 1 1 nil)
(define-opcode iconst_3 6 1 1 nil)
(define-opcode iconst_4 7 1 1 nil)
(define-opcode iconst_5 8 1 1 nil)
(define-opcode lconst_0 9 1 2 nil)
(define-opcode lconst_1 10 1 2 nil)
(define-opcode fconst_0 11 1 1 nil)
(define-opcode fconst_1 12 1 1 nil)
(define-opcode fconst_2 13 1 1 nil)
(define-opcode dconst_0 14 1 2 nil)
(define-opcode dconst_1 15 1 2 nil)
(define-opcode bipush 16 2 1 nil)
(define-opcode sipush 17 3 1 nil)
(define-opcode ldc 18 2 1 nil)
(define-opcode ldc_w 19 3 1 nil)
(define-opcode ldc2_w 20 3 2 nil)
(define-opcode iload 21 2 1 t)
(define-opcode lload 22 2 2 t)
(define-opcode fload 23 2 nil t)
(define-opcode dload 24 2 nil t)
(define-opcode aload 25 2 1 t)
(define-opcode iload_0 26 1 1 0)
(define-opcode iload_1 27 1 1 1)
(define-opcode iload_2 28 1 1 2)
(define-opcode iload_3 29 1 1 3)
(define-opcode lload_0 30 1 2 0)
(define-opcode lload_1 31 1 2 1)
(define-opcode lload_2 32 1 2 2)
(define-opcode lload_3 33 1 2 3)
(define-opcode fload_0 34 1 nil 0)
(define-opcode fload_1 35 1 nil 1)
(define-opcode fload_2 36 1 nil 2)
(define-opcode fload_3 37 1 nil 3)
(define-opcode dload_0 38 1 nil 0)
(define-opcode dload_1 39 1 nil 1)
(define-opcode dload_2 40 1 nil 2)
(define-opcode dload_3 41 1 nil 3)
(define-opcode aload_0 42 1 1 0)
(define-opcode aload_1 43 1 1 1)
(define-opcode aload_2 44 1 1 2)
(define-opcode aload_3 45 1 1 3)
(define-opcode iaload 46 1 -1 nil)
(define-opcode laload 47 1 0 nil)
(define-opcode faload 48 1 -1 nil)
(define-opcode daload 49 1 0 nil)
(define-opcode aaload 50 1 -1 nil)
(define-opcode baload 51 1 nil nil)
(define-opcode caload 52 1 nil nil)
(define-opcode saload 53 1 nil nil)
(define-opcode istore 54 2 -1 t)
(define-opcode lstore 55 2 -2 t)
(define-opcode fstore 56 2 nil t)
(define-opcode dstore 57 2 nil t)
(define-opcode astore 58 2 -1 t)
(define-opcode istore_0 59 1 -1 0)
(define-opcode istore_1 60 1 -1 1)
(define-opcode istore_2 61 1 -1 2)
(define-opcode istore_3 62 1 -1 3)
(define-opcode lstore_0 63 1 -2 0)
(define-opcode lstore_1 64 1 -2 1)
(define-opcode lstore_2 65 1 -2 2)
(define-opcode lstore_3 66 1 -2 3)
(define-opcode fstore_0 67 1 nil 0)
(define-opcode fstore_1 68 1 nil 1)
(define-opcode fstore_2 69 1 nil 2)
(define-opcode fstore_3 70 1 nil 3)
(define-opcode dstore_0 71 1 nil 0)
(define-opcode dstore_1 72 1 nil 1)
(define-opcode dstore_2 73 1 nil 2)
(define-opcode dstore_3 74 1 nil 3)
(define-opcode astore_0 75 1 -1 0)
(define-opcode astore_1 76 1 -1 1)
(define-opcode astore_2 77 1 -1 2)
(define-opcode astore_3 78 1 -1 3)
(define-opcode iastore 79 1 -3 nil)
(define-opcode lastore 80 1 -4 nil)
(define-opcode fastore 81 1 -3 nil)
(define-opcode dastore 82 1 -4 nil)
(define-opcode aastore 83 1 -3 nil)
(define-opcode bastore 84 1 nil nil)
(define-opcode castore 85 1 nil nil)
(define-opcode sastore 86 1 nil nil)
(define-opcode pop 87 1 -1 nil)
(define-opcode pop2 88 1 -2 nil)
(define-opcode dup 89 1 1 nil)
(define-opcode dup_x1 90 1 1 nil)
(define-opcode dup_x2 91 1 1 nil)
(define-opcode dup2 92 1 2 nil)
(define-opcode dup2_x1 93 1 2 nil)
(define-opcode dup2_x2 94 1 2 nil)
(define-opcode swap 95 1 0 nil)
(define-opcode iadd 96 1 -1 nil)
(define-opcode ladd 97 1 -2 nil)
(define-opcode fadd 98 1 -1 nil)
(define-opcode dadd 99 1 -2 nil)
(define-opcode isub 100 1 -1 nil)
(define-opcode lsub 101 1 -2 nil)
(define-opcode fsub 102 1 -1 nil)
(define-opcode dsub 103 1 -2 nil)
(define-opcode imul 104 1 -1 nil)
(define-opcode lmul 105 1 -2 nil)
(define-opcode fmul 106 1 -1 nil)
(define-opcode dmul 107 1 -2 nil)
(define-opcode idiv 108 1 nil nil)
(define-opcode ldiv 109 1 nil nil)
(define-opcode fdiv 110 1 nil nil)
(define-opcode ddiv 111 1 nil nil)
(define-opcode irem 112 1 nil nil)
(define-opcode lrem 113 1 nil nil)
(define-opcode frem 114 1 nil nil)
(define-opcode drem 115 1 nil nil)
(define-opcode ineg 116 1 0 nil)
(define-opcode lneg 117 1 0 nil)
(define-opcode fneg 118 1 0 nil)
(define-opcode dneg 119 1 0 nil)
(define-opcode ishl 120 1 -1 nil)
(define-opcode lshl 121 1 -1 nil)
(define-opcode ishr 122 1 -1 nil)
(define-opcode lshr 123 1 -1 nil)
(define-opcode iushr 124 1 nil nil)
(define-opcode lushr 125 1 nil nil)
(define-opcode iand 126 1 -1 nil)
(define-opcode land 127 1 -2 nil)
(define-opcode ior 128 1 -1 nil)
(define-opcode lor 129 1 -2 nil)
(define-opcode ixor 130 1 -1 nil)
(define-opcode lxor 131 1 -2 nil)
(define-opcode iinc 132 3 0 t)
(define-opcode i2l 133 1 1 nil)
(define-opcode i2f 134 1 0 nil)
(define-opcode i2d 135 1 1 nil)
(define-opcode l2i 136 1 -1 nil)
(define-opcode l2f 137 1 -1 nil)
(define-opcode l2d 138 1 0 nil)
(define-opcode f2i 139 1 nil nil)
(define-opcode f2l 140 1 nil nil)
(define-opcode f2d 141 1 1 nil)
(define-opcode d2i 142 1 nil nil)
(define-opcode d2l 143 1 nil nil)
(define-opcode d2f 144 1 -1 nil)
(define-opcode i2b 145 1 nil nil)
(define-opcode i2c 146 1 nil nil)
(define-opcode i2s 147 1 nil nil)
(define-opcode lcmp 148 1 -3 nil)
(define-opcode fcmpl 149 1 -1 nil)
(define-opcode fcmpg 150 1 -1 nil)
(define-opcode dcmpl 151 1 -3 nil)
(define-opcode dcmpg 152 1 -3 nil)
(define-opcode ifeq 153 3 -1 nil)
(define-opcode ifne 154 3 -1 nil)
(define-opcode iflt 155 3 -1 nil)
(define-opcode ifge 156 3 -1 nil)
(define-opcode ifgt 157 3 -1 nil)
(define-opcode ifle 158 3 -1 nil)
(define-opcode if_icmpeq 159 3 -2 nil)
(define-opcode if_icmpne 160 3 -2 nil)
(define-opcode if_icmplt 161 3 -2 nil)
(define-opcode if_icmpge 162 3 -2 nil)
(define-opcode if_icmpgt 163 3 -2 nil)
(define-opcode if_icmple 164 3 -2 nil)
(define-opcode if_acmpeq 165 3 -2 nil)
(define-opcode if_acmpne 166 3 -2 nil)
(define-opcode goto 167 3 0 nil)
;;(define-opcode jsr 168 3 1) Don't use these 2 opcodes: deprecated
;;(define-opcode ret 169 2 0) their use results in JVM verifier errors
(define-opcode tableswitch 170 0 nil nil)
(define-opcode lookupswitch 171 0 nil nil)
(define-opcode ireturn 172 1 nil nil)
(define-opcode lreturn 173 1 nil nil)
(define-opcode freturn 174 1 nil nil)
(define-opcode dreturn 175 1 nil nil)
(define-opcode ireturn 172 1 -1 nil)
(define-opcode areturn 176 1 -1 nil)
(define-opcode return 177 1 0 nil)
(define-opcode getstatic 178 3 1 nil)
(define-opcode putstatic 179 3 -1 nil)
(define-opcode getfield 180 3 0 nil)
(define-opcode putfield 181 3 -2 nil)
(define-opcode invokevirtual 182 3 nil nil)
(define-opcode invokespecial 183 3 nil nil)
(define-opcode invokestatic 184 3 nil nil)
(define-opcode invokeinterface 185 5 nil nil)
(define-opcode unused 186 0 nil nil)
(define-opcode new 187 3 1 nil)
(define-opcode newarray 188 2 nil nil)
(define-opcode anewarray 189 3 0 nil)
(define-opcode arraylength 190 1 0 nil)
(define-opcode athrow 191 1 0 nil)
(define-opcode checkcast 192 3 0 nil)
(define-opcode instanceof 193 3 0 nil)
(define-opcode monitorenter 194 1 -1 nil)
(define-opcode monitorexit 195 1 -1 nil)
(define-opcode wide 196 0 nil nil)
(define-opcode multianewarray 197 4 nil nil)
(define-opcode ifnull 198 3 -1 nil)
(define-opcode ifnonnull 199 3 nil nil)
(define-opcode goto_w 200 5 nil nil)
;; (define-opcode jsr_w 201 5 nil) Don't use: deprecated
(define-opcode label 202 0 0 nil)  ;; virtual: does not exist in the JVM
;; (define-opcode push-value 203 nil 1)
;; (define-opcode store-value 204 nil -1)
(define-opcode clear-values 205 0 0 t)  ;; virtual: does not exist in the JVM
;;(define-opcode var-ref 206 0 0)

(defparameter *last-opcode* 206)

(declaim (ftype (function (t) t) opcode-name))
(defun opcode-name (opcode-number)
  (let ((opcode (gethash opcode-number *opcodes*)))
    (and opcode (jvm-opcode-name opcode))))

(declaim (ftype (function (t) (integer 0 255)) opcode-number))
(defun opcode-number (opcode-name)
  (declare (optimize speed))
  (let ((opcode (gethash (string opcode-name) *opcodes*)))
    (if opcode
        (jvm-opcode-number opcode)
        (error "Unknown opcode ~S." opcode-name))))

(declaim (ftype (function (t) fixnum) opcode-size))
(defun opcode-size (opcode-number)
  (declare (optimize speed (safety 0)))
  (declare (type (integer 0 255) opcode-number))
  (jvm-opcode-size (svref *opcode-table* opcode-number)))

(declaim (ftype (function (t) t) opcode-stack-effect))
(defun opcode-stack-effect (opcode-number)
  (declare (optimize speed))
  (jvm-opcode-stack-effect (svref *opcode-table* opcode-number)))




;;   INSTRUCTION

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

(declaim (ftype (function (t) t) instruction-label))
(defun instruction-label (instruction)
  (and instruction
       (= (instruction-opcode (the instruction instruction)) 202)
       (car (instruction-args instruction))))



(defknown inst * t)
(defun inst (instr &optional args)
  (declare (optimize speed))
  (let ((opcode (if (fixnump instr)
                    instr
                    (opcode-number instr))))
    (unless (listp args)
      (setf args (list args)))
    (make-instruction opcode args)))


;; Having %emit and %%emit output their code to *code*
;; is currently an implementation detail exposed to all users.
;; We need to have APIs to address this, but for now pass2 is
;; our only user and we'll hard-code the use of *code*.
(defvar *code* nil)

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
  (when (and (consp instr)
             (eq (car instr) 'QUOTE)
             (symbolp (cadr instr)))
    (setf instr (opcode-number (cadr instr))))
  (if (fixnump instr)
      `(%%emit ,instr ,@args)
      `(%emit ,instr ,@args)))


;;  Helper routines

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

(declaim (ftype (function (t) t) branch-p)
         (inline branch-p))
(defun branch-p (opcode)
;;  (declare (optimize speed))
;;  (declare (type '(integer 0 255) opcode))
  (or (<= 153 opcode 167)
      (<= 198 opcode 200))) ;; ifnull / ifnonnull / goto_w

(declaim (ftype (function (t) t) unconditional-control-transfer-p)
         (inline unconditional-control-transfer-p))
(defun unconditional-control-transfer-p (opcode)
  (or (= 167 opcode) ;; goto
      (= 200 opcode) ;; goto_w
      (<= 172 opcode 177) ;; ?return
      (= 191 opcode) ;; athrow
      ))

(declaim (ftype (function (t) boolean) label-p)
         (inline label-p))
(defun label-p (instruction)
  (and instruction
       (= (the fixnum (instruction-opcode (the instruction instruction))) 202)))

(defun print-code (code)
  (dotimes (i (length code))
    (let ((instruction (elt code i)))
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

(defun expand-virtual-instructions (code)
  (let* ((len (length code))
         (vector (make-array (ash len 1) :fill-pointer 0 :adjustable t)))
    (dotimes (index len vector)
      (declare (type (unsigned-byte 16) index))
      (let ((instruction (svref code index)))
        (case (instruction-opcode instruction)
          (205 ; CLEAR-VALUES
           (dolist (instruction
                     (list
                      (inst 'aload (car (instruction-args instruction)))
                      (inst 'aconst_null)
                      (inst 'putfield (u2 (pool-field +lisp-thread+ "_values"
                                                      +lisp-object-array+)))))
             (vector-push-extend instruction vector)))
          (t
           (vector-push-extend instruction vector)))))))


;;   RESOLVERS

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
                 172 ; ireturn
                 176 ; areturn
                 177 ; return
                 178 ; getstatic
                 179 ; putstatic
                 180 ; getfield
                 181 ; putfield
                 182 ; invokevirtual
                 183 ; invockespecial
                 184 ; invokestatic
                 187 ; new
                 189 ; anewarray
                 190 ; arraylength
                 191 ; athrow
                 192 ; checkcast
                 193 ; instanceof
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
                 (setf (gethash op +resolvers+)
                       (symbol-function ',name)))
              `(setf (gethash ,opcodes +resolvers+)
                     (symbol-function ',name)))))))

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

;; ldc
(define-resolver 18 (instruction)
  (let* ((args (instruction-args instruction)))
    (unless (= (length args) 1)
      (error "Wrong number of args for LDC."))
    (if (> (car args) 255)
        (inst 19 (u2 (car args))) ; LDC_W
        (inst 18 args))))

;; ldc_w
(define-resolver 19 (instruction)
  (let* ((args (instruction-args instruction)))
    (unless (= (length args) 1)
      (error "Wrong number of args for LDC_W."))
    (inst 19 (u2 (car args)))))

;; ldc2_w
(define-resolver 20 (instruction)
  (let* ((args (instruction-args instruction)))
    (unless (= (length args) 1)
      (error "Wrong number of args for LDC2_W."))
    (inst 20 (u2 (car args)))))

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
  (let* ((len (length code))
         (vector (make-array len :fill-pointer 0 :adjustable t)))
    (dotimes (index len vector)
      (declare (type (unsigned-byte 16) index))
      (let ((instruction (aref code index)))
        (vector-push-extend (resolve-instruction instruction) vector)))))



;; BYTE CODE ANALYSIS AND OPTIMIZATION

(declaim (ftype (function (t t t) t) analyze-stack-path))
(defun analyze-stack-path (code start-index depth)
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
        (unless (= (the fixnum instruction-depth)
                   (the fixnum (+ depth instruction-stack)))
          (internal-compiler-error "Stack inconsistency detected ~
                                    in ~A at index ~D: ~
                                    found ~S, expected ~S."
                                   (if *current-compiland*
                                       (compiland-name *current-compiland*)
                                       "<unknown>")
                                   i instruction-depth
                                   (+ depth instruction-stack)))
        (return-from analyze-stack-path))
      (let ((opcode (instruction-opcode instruction)))
        (setf depth (+ depth instruction-stack))
        (setf (instruction-depth instruction) depth)
        (unless (<= 0 depth)
          (internal-compiler-error "Stack inconsistency detected ~
                                    in ~A at index ~D: ~
                                    negative depth ~S."
                                   (if *current-compiland*
                                       (compiland-name *current-compiland*)
                                       "<unknown>")
                                   i depth))
        (when (branch-p opcode)
          (let ((label (car (instruction-args instruction))))
            (declare (type symbol label))
            (analyze-stack-path code (symbol-value label) depth)))
        (when (unconditional-control-transfer-p opcode)
          ;; Current path ends.
          (return-from analyze-stack-path))))))

(declaim (ftype (function (t) t) analyze-stack))
(defun analyze-stack (code exception-entry-points)
  (declare (optimize speed))
  (let* ((code-length (length code)))
    (declare (type vector code))
    (dotimes (i code-length)
      (let* ((instruction (aref code i))
             (opcode (instruction-opcode instruction)))
        (when (eql opcode 202) ; LABEL
          (let ((label (car (instruction-args instruction))))
            (set label i)))
        (if (instruction-stack instruction)
            (when (opcode-stack-effect opcode)
              (unless (eql (instruction-stack instruction)
                           (opcode-stack-effect opcode))
                (sys::%format t "instruction-stack = ~S ~
                                 opcode-stack-effect = ~S~%"
                              (instruction-stack instruction)
                              (opcode-stack-effect opcode))
                (sys::%format t "index = ~D instruction = ~A~%" i
                              (print-instruction instruction))))
            (setf (instruction-stack instruction)
                  (opcode-stack-effect opcode)))
        (unless (instruction-stack instruction)
          (sys::%format t "no stack information for instruction ~D~%"
                        (instruction-opcode instruction))
          (aver nil))))
    (analyze-stack-path code 0 0)
    (dolist (entry-point exception-entry-points)
      ;; Stack depth is always 1 when handler is called.
      (analyze-stack-path code (symbol-value entry-point) 1))
    (let ((max-stack 0))
      (declare (type fixnum max-stack))
      (dotimes (i code-length)
        (let* ((instruction (aref code i))
               (instruction-depth (instruction-depth instruction)))
          (when instruction-depth
            (setf max-stack (max max-stack (the fixnum instruction-depth))))))
      max-stack)))

(defun analyze-locals (code)
  (let ((code-length (length code))
        (max-local 0))
    (dotimes (i code-length max-local)
      (let* ((instruction (aref code i))
             (opcode (instruction-opcode instruction)))
        (setf max-local
              (max max-local
                   (or (let ((opcode-register
                                (jvm-opcode-register-used opcode)))
                         (if (eq t opcode-register)
                             (car (instruction-args instruction))
                             opcode-register))
                       0)))))))

(defun delete-unused-labels (code handler-labels)
  (declare (optimize speed))
  (let ((code (coerce code 'vector))
        (changed nil)
        (marker (gensym)))
    ;; Mark the labels that are actually branched to.
    (dotimes (i (length code))
      (let ((instruction (aref code i)))
        (when (branch-p (instruction-opcode instruction))
          (let ((label (car (instruction-args instruction))))
            (set label marker)))))
    ;; Add labels used for exception handlers.
    (dolist (label handler-labels)
      (set label marker))
    ;; Remove labels that are not used as branch targets.
    (dotimes (i (length code))
      (let ((instruction (aref code i)))
        (when (= (instruction-opcode instruction) 202) ; LABEL
          (let ((label (car (instruction-args instruction))))
            (declare (type symbol label))
            (unless (eq (symbol-value label) marker)
              (setf (aref code i) nil)
              (setf changed t))))))
    (values (if changed (delete nil code) code)
            changed)))

(defun delete-unreachable-code (code)
  ;; Look for unreachable code after GOTO.
  (declare (optimize speed))
  (let* ((code (coerce code 'vector))
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
              ((unconditional-control-transfer-p opcode)
               (setf after-goto/areturn t)))))
    (values (if changed (delete nil code) code)
            changed)))


(declaim (ftype (function (t) label-target-instructions) hash-labels))
(defun label-target-instructions (code)
  (let ((ht (make-hash-table :test 'eq))
        (code (coerce code 'vector))
        (pending-labels '()))
    (dotimes (i (length code))
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

(defun optimize-jumps (code)
  (declare (optimize speed))
  (let* ((code (coerce code 'vector))
         (ht (label-target-instructions code))
         (changed nil))
    (dotimes (i (length code))
      (let* ((instruction (aref code i))
             (opcode (and instruction (instruction-opcode instruction))))
        (when (and opcode (branch-p opcode))
          (let* ((target-label (car (instruction-args instruction)))
                 (next-instruction (gethash1 target-label ht)))
            (when next-instruction
              (case (instruction-opcode next-instruction)
                ((167 200)                  ;; GOTO
                 (setf (instruction-args instruction)
                       (instruction-args next-instruction)
                       changed t))
                (176 ; ARETURN
                 (when (unconditional-control-transfer-p opcode)
                   (setf (instruction-opcode instruction) 176
                         (instruction-args instruction) nil
                         changed t)))))))))
    (values code changed)))


(defun optimize-instruction-sequences (code)
  (let* ((code (coerce code 'vector))
         (changed nil))
    (dotimes (i (1- (length code)))
      (let* ((this-instruction (aref code i))
             (this-opcode (and this-instruction
                               (instruction-opcode this-instruction)))
             (labels-skipped-p nil)
             (next-instruction (do ((j (1+ i) (1+ j)))
                                   ((or (>= j (length code))
                                        (/= 202 ; LABEL
                                            (instruction-opcode (aref code j))))
                                    (when (< j (length code))
                                      (aref code j)))
                                 (setf labels-skipped-p t)))
             (next-opcode (and next-instruction
                               (instruction-opcode next-instruction))))
        (case this-opcode
          (205 ; CLEAR-VALUES
           (when (eql next-opcode 205)       ; CLEAR-VALUES
             (setf (aref code i) nil)
             (setf changed t)))
          (178 ; GETSTATIC
           (when (and (eql next-opcode 87)   ; POP
                      (not labels-skipped-p))
             (setf (aref code i) nil)
             (setf (aref code (1+ i)) nil)
             (setf changed t)))
          (176 ; ARETURN
           (when (eql next-opcode 176)       ; ARETURN
             (setf (aref code i) nil)
             (setf changed t)))
          ((200 167)                         ; GOTO GOTO_W
           (when (and (or (eql next-opcode 202)  ; LABEL
                          (eql next-opcode 200)  ; GOTO_W
                          (eql next-opcode 167)) ; GOTO
                      (eq (car (instruction-args this-instruction))
                          (car (instruction-args next-instruction))))
             (setf (aref code i) nil)
             (setf changed t))))))
    (values (if changed (delete nil code) code)
            changed)))

(defvar *enable-optimization* t)

(defknown optimize-code (t t) t)
(defun optimize-code (code handler-labels)
  (unless *enable-optimization*
    (format t "optimizations are disabled~%"))
  (when *enable-optimization*
    (when *compiler-debug*
      (format t "----- before optimization -----~%")
      (print-code code))
    (loop
       (let ((changed-p nil))
         (multiple-value-setq
             (code changed-p)
           (delete-unused-labels code handler-labels))
         (if changed-p
             (setf code (optimize-instruction-sequences code))
             (multiple-value-setq
                 (code changed-p)
               (optimize-instruction-sequences code)))
         (if changed-p
             (setf code (optimize-jumps code))
             (multiple-value-setq
                 (code changed-p)
               (optimize-jumps code)))
         (if changed-p
             (setf code (delete-unreachable-code code))
             (multiple-value-setq
                 (code changed-p)
               (delete-unreachable-code code)))
         (unless changed-p
           (return))))
    (unless (vectorp code)
      (setf code (coerce code 'vector)))
    (when *compiler-debug*
      (sys::%format t "----- after optimization -----~%")
      (print-code code)))
  code)




(defun code-bytes (code)
  (let ((length 0)
        labels ;; alist
        )
    (declare (type (unsigned-byte 16) length))
    ;; Pass 1: calculate label offsets and overall length.
    (dotimes (i (length code))
      (declare (type (unsigned-byte 16) i))
      (let* ((instruction (aref code i))
             (opcode (instruction-opcode instruction)))
        (if (= opcode 202) ; LABEL
            (let ((label (car (instruction-args instruction))))
              (set label length)
              (setf labels
                    (acons label length labels)))
            (incf length (opcode-size opcode)))))
    ;; Pass 2: replace labels with calculated offsets.
    (let ((index 0))
      (declare (type (unsigned-byte 16) index))
      (dotimes (i (length code))
        (declare (type (unsigned-byte 16) i))
        (let ((instruction (aref code i)))
          (when (branch-p (instruction-opcode instruction))
            (let* ((label (car (instruction-args instruction)))
                   (offset (- (the (unsigned-byte 16)
                                (symbol-value (the symbol label)))
                              index)))
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
      (values bytes labels))))

(defun finalize-code (code handler-labels optimize)
  (setf code (coerce (nreverse code) 'vector))
  (when optimize
    (setf code (optimize-code code handler-labels)))
  (resolve-instructions (expand-virtual-instructions code)))

(provide '#:opcodes)
