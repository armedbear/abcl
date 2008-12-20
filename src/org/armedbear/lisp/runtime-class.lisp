;;; runtime-class.lisp
;;;
;;; Copyright (C) 2004 Peter Graves
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

(in-package :java)

(require :format)

;; jparse generated definitions, somewhat simplified

(defclass java-class nil ((java-instance :initarg :java-instance :reader java-instance)))
(defclass jboolean (java-class) nil)
(defmethod initialize-instance :after ((b jboolean) &key &allow-other-keys)
  (setf (slot-value b 'java-instance) (make-immediate-object (java-instance b) :boolean)))
(defclass jarray (java-class) nil)
(defclass |java.lang.Object| (java-class) nil)
(defclass output-stream (java-class) nil)
(defclass file-output-stream (output-stream java-class) nil)
(defclass class-visitor (java-class) nil)
(defclass class-writer (class-visitor java-class) nil)
(defclass code-visitor (java-class) nil)
(defclass code-writer (code-visitor java-class) nil)
(defclass attribute (java-class) nil)
(defclass constants (java-class) nil)
(defclass label (java-class) nil)
(defmethod make-file-output-stream-1 ((v1 string))
  (make-instance 'file-output-stream :java-instance
                 (jnew (jconstructor "java.io.FileOutputStream" "java.lang.String") v1)))
(defmethod write-1 ((instance file-output-stream) (v1 jarray))
  (jcall (jmethod "java.io.FileOutputStream" "write" "[B") (java-instance instance) (java-instance v1)))
(defmethod close-0 ((instance file-output-stream))
  (jcall (jmethod "java.io.FileOutputStream" "close") (java-instance instance)))
(defmethod make-class-writer-1 ((v1 jboolean))
  (make-instance 'class-writer :java-instance
                 (jnew (jconstructor "org.objectweb.asm.ClassWriter" "boolean") (java-instance v1))))
(defmethod visit-end-0 ((instance class-writer))
  (jcall (jmethod "org.objectweb.asm.ClassWriter" "visitEnd") (java-instance instance)))
(defmethod to-byte-array-0 ((instance class-writer))
  (make-instance 'jarray :java-instance
                 (jcall (jmethod "org.objectweb.asm.ClassWriter" "toByteArray") (java-instance instance))))
(defmethod visit-insn-1 ((instance code-visitor) (v1 fixnum))
  (jcall (jmethod "org.objectweb.asm.CodeVisitor" "visitInsn" "int") (java-instance instance) v1))
(defmethod visit-int-insn-2 ((instance code-visitor) (v1 fixnum) (v2 fixnum))
  (jcall (jmethod "org.objectweb.asm.CodeVisitor" "visitIntInsn" "int" "int") (java-instance instance) v1
         v2))
(defmethod visit-var-insn-2 ((instance code-visitor) (v1 fixnum) (v2 fixnum))
  (jcall (jmethod "org.objectweb.asm.CodeVisitor" "visitVarInsn" "int" "int") (java-instance instance) v1
         v2))
(defmethod visit-type-insn-2 ((instance code-visitor) (v1 fixnum) (v2 string))
  (jcall (jmethod "org.objectweb.asm.CodeVisitor" "visitTypeInsn" "int" "java.lang.String")
         (java-instance instance) v1 v2))
(defmethod visit-field-insn-4 ((instance code-visitor) (v1 fixnum) (v2 string) (v3 string) (v4 string))
  (jcall (jmethod "org.objectweb.asm.CodeVisitor" "visitFieldInsn" "int" "java.lang.String"
                  "java.lang.String" "java.lang.String")
         (java-instance instance) v1 v2 v3 v4))
(defmethod visit-method-insn-4 ((instance code-visitor) (v1 fixnum) (v2 string) (v3 string) (v4 string))
  (jcall (jmethod "org.objectweb.asm.CodeVisitor" "visitMethodInsn" "int" "java.lang.String"
                  "java.lang.String" "java.lang.String")
         (java-instance instance) v1 v2 v3 v4))
(defmethod visit-jump-insn-2 ((instance code-visitor) (v1 fixnum) (v2 label))
  (jcall (jmethod "org.objectweb.asm.CodeVisitor" "visitJumpInsn" "int" "org.objectweb.asm.Label")
         (java-instance instance) v1 (java-instance v2)))
(defmethod visit-label-1 ((instance code-visitor) (v1 label))
  (jcall (jmethod "org.objectweb.asm.CodeVisitor" "visitLabel" "org.objectweb.asm.Label")
         (java-instance instance) (java-instance v1)))
(defmethod visit-ldc-insn-1 ((instance code-visitor) (v1 |java.lang.Object|))
  (jcall (jmethod "org.objectweb.asm.CodeVisitor" "visitLdcInsn" "java.lang.Object")
         (java-instance instance) (java-instance v1)))
(defmethod visit-try-catch-block-4 ((instance code-visitor) (v1 label) (v2 label) (v3 label) (v4 string))
  (jcall (jmethod "org.objectweb.asm.CodeVisitor" "visitTryCatchBlock" "org.objectweb.asm.Label"
                  "org.objectweb.asm.Label" "org.objectweb.asm.Label" "java.lang.String")
         (java-instance instance) (java-instance v1) (java-instance v2) (java-instance v3) v4))
(defmethod visit-maxs-2 ((instance code-visitor) (v1 fixnum) (v2 fixnum))
  (jcall (jmethod "org.objectweb.asm.CodeVisitor" "visitMaxs" "int" "int") (java-instance instance) v1 v2))
(defconstant constants.ifnonnull (jfield "org.objectweb.asm.Constants" "IFNONNULL"))
(defconstant constants.ifnull (jfield "org.objectweb.asm.Constants" "IFNULL"))
(defconstant constants.multianewarray (jfield "org.objectweb.asm.Constants" "MULTIANEWARRAY"))
(defconstant constants.monitorexit (jfield "org.objectweb.asm.Constants" "MONITOREXIT"))
(defconstant constants.monitorenter (jfield "org.objectweb.asm.Constants" "MONITORENTER"))
(defconstant constants.instanceof (jfield "org.objectweb.asm.Constants" "INSTANCEOF"))
(defconstant constants.checkcast (jfield "org.objectweb.asm.Constants" "CHECKCAST"))
(defconstant constants.athrow (jfield "org.objectweb.asm.Constants" "ATHROW"))
(defconstant constants.arraylength (jfield "org.objectweb.asm.Constants" "ARRAYLENGTH"))
(defconstant constants.anewarray (jfield "org.objectweb.asm.Constants" "ANEWARRAY"))
(defconstant constants.newarray (jfield "org.objectweb.asm.Constants" "NEWARRAY"))
(defconstant constants.new (jfield "org.objectweb.asm.Constants" "NEW"))
(defconstant constants.invokeinterface (jfield "org.objectweb.asm.Constants" "INVOKEINTERFACE"))
(defconstant constants.invokestatic (jfield "org.objectweb.asm.Constants" "INVOKESTATIC"))
(defconstant constants.invokespecial (jfield "org.objectweb.asm.Constants" "INVOKESPECIAL"))
(defconstant constants.invokevirtual (jfield "org.objectweb.asm.Constants" "INVOKEVIRTUAL"))
(defconstant constants.putfield (jfield "org.objectweb.asm.Constants" "PUTFIELD"))
(defconstant constants.getfield (jfield "org.objectweb.asm.Constants" "GETFIELD"))
(defconstant constants.putstatic (jfield "org.objectweb.asm.Constants" "PUTSTATIC"))
(defconstant constants.getstatic (jfield "org.objectweb.asm.Constants" "GETSTATIC"))
(defconstant constants.return (jfield "org.objectweb.asm.Constants" "RETURN"))
(defconstant constants.areturn (jfield "org.objectweb.asm.Constants" "ARETURN"))
(defconstant constants.dreturn (jfield "org.objectweb.asm.Constants" "DRETURN"))
(defconstant constants.freturn (jfield "org.objectweb.asm.Constants" "FRETURN"))
(defconstant constants.lreturn (jfield "org.objectweb.asm.Constants" "LRETURN"))
(defconstant constants.ireturn (jfield "org.objectweb.asm.Constants" "IRETURN"))
(defconstant constants.lookupswitch (jfield "org.objectweb.asm.Constants" "LOOKUPSWITCH"))
(defconstant constants.tableswitch (jfield "org.objectweb.asm.Constants" "TABLESWITCH"))
(defconstant constants.ret (jfield "org.objectweb.asm.Constants" "RET"))
(defconstant constants.jsr (jfield "org.objectweb.asm.Constants" "JSR"))
(defconstant constants.goto (jfield "org.objectweb.asm.Constants" "GOTO"))
(defconstant constants.if-acmpne (jfield "org.objectweb.asm.Constants" "IF_ACMPNE"))
(defconstant constants.if-acmpeq (jfield "org.objectweb.asm.Constants" "IF_ACMPEQ"))
(defconstant constants.if-icmple (jfield "org.objectweb.asm.Constants" "IF_ICMPLE"))
(defconstant constants.if-icmpgt (jfield "org.objectweb.asm.Constants" "IF_ICMPGT"))
(defconstant constants.if-icmpge (jfield "org.objectweb.asm.Constants" "IF_ICMPGE"))
(defconstant constants.if-icmplt (jfield "org.objectweb.asm.Constants" "IF_ICMPLT"))
(defconstant constants.if-icmpne (jfield "org.objectweb.asm.Constants" "IF_ICMPNE"))
(defconstant constants.if-icmpeq (jfield "org.objectweb.asm.Constants" "IF_ICMPEQ"))
(defconstant constants.ifle (jfield "org.objectweb.asm.Constants" "IFLE"))
(defconstant constants.ifgt (jfield "org.objectweb.asm.Constants" "IFGT"))
(defconstant constants.ifge (jfield "org.objectweb.asm.Constants" "IFGE"))
(defconstant constants.iflt (jfield "org.objectweb.asm.Constants" "IFLT"))
(defconstant constants.ifne (jfield "org.objectweb.asm.Constants" "IFNE"))
(defconstant constants.ifeq (jfield "org.objectweb.asm.Constants" "IFEQ"))
(defconstant constants.dcmpg (jfield "org.objectweb.asm.Constants" "DCMPG"))
(defconstant constants.dcmpl (jfield "org.objectweb.asm.Constants" "DCMPL"))
(defconstant constants.fcmpg (jfield "org.objectweb.asm.Constants" "FCMPG"))
(defconstant constants.fcmpl (jfield "org.objectweb.asm.Constants" "FCMPL"))
(defconstant constants.lcmp (jfield "org.objectweb.asm.Constants" "LCMP"))
(defconstant constants.i2s (jfield "org.objectweb.asm.Constants" "I2S"))
(defconstant constants.i2c (jfield "org.objectweb.asm.Constants" "I2C"))
(defconstant constants.i2b (jfield "org.objectweb.asm.Constants" "I2B"))
(defconstant constants.d2f (jfield "org.objectweb.asm.Constants" "D2F"))
(defconstant constants.d2l (jfield "org.objectweb.asm.Constants" "D2L"))
(defconstant constants.d2i (jfield "org.objectweb.asm.Constants" "D2I"))
(defconstant constants.f2d (jfield "org.objectweb.asm.Constants" "F2D"))
(defconstant constants.f2l (jfield "org.objectweb.asm.Constants" "F2L"))
(defconstant constants.f2i (jfield "org.objectweb.asm.Constants" "F2I"))
(defconstant constants.l2d (jfield "org.objectweb.asm.Constants" "L2D"))
(defconstant constants.l2f (jfield "org.objectweb.asm.Constants" "L2F"))
(defconstant constants.l2i (jfield "org.objectweb.asm.Constants" "L2I"))
(defconstant constants.i2d (jfield "org.objectweb.asm.Constants" "I2D"))
(defconstant constants.i2f (jfield "org.objectweb.asm.Constants" "I2F"))
(defconstant constants.i2l (jfield "org.objectweb.asm.Constants" "I2L"))
(defconstant constants.iinc (jfield "org.objectweb.asm.Constants" "IINC"))
(defconstant constants.lxor (jfield "org.objectweb.asm.Constants" "LXOR"))
(defconstant constants.ixor (jfield "org.objectweb.asm.Constants" "IXOR"))
(defconstant constants.lor (jfield "org.objectweb.asm.Constants" "LOR"))
(defconstant constants.ior (jfield "org.objectweb.asm.Constants" "IOR"))
(defconstant constants.land (jfield "org.objectweb.asm.Constants" "LAND"))
(defconstant constants.iand (jfield "org.objectweb.asm.Constants" "IAND"))
(defconstant constants.lushr (jfield "org.objectweb.asm.Constants" "LUSHR"))
(defconstant constants.iushr (jfield "org.objectweb.asm.Constants" "IUSHR"))
(defconstant constants.lshr (jfield "org.objectweb.asm.Constants" "LSHR"))
(defconstant constants.ishr (jfield "org.objectweb.asm.Constants" "ISHR"))
(defconstant constants.lshl (jfield "org.objectweb.asm.Constants" "LSHL"))
(defconstant constants.ishl (jfield "org.objectweb.asm.Constants" "ISHL"))
(defconstant constants.dneg (jfield "org.objectweb.asm.Constants" "DNEG"))
(defconstant constants.fneg (jfield "org.objectweb.asm.Constants" "FNEG"))
(defconstant constants.lneg (jfield "org.objectweb.asm.Constants" "LNEG"))
(defconstant constants.ineg (jfield "org.objectweb.asm.Constants" "INEG"))
(defconstant constants.drem (jfield "org.objectweb.asm.Constants" "DREM"))
(defconstant constants.frem (jfield "org.objectweb.asm.Constants" "FREM"))
(defconstant constants.lrem (jfield "org.objectweb.asm.Constants" "LREM"))
(defconstant constants.irem (jfield "org.objectweb.asm.Constants" "IREM"))
(defconstant constants.ddiv (jfield "org.objectweb.asm.Constants" "DDIV"))
(defconstant constants.fdiv (jfield "org.objectweb.asm.Constants" "FDIV"))
(defconstant constants.ldiv (jfield "org.objectweb.asm.Constants" "LDIV"))
(defconstant constants.idiv (jfield "org.objectweb.asm.Constants" "IDIV"))
(defconstant constants.dmul (jfield "org.objectweb.asm.Constants" "DMUL"))
(defconstant constants.fmul (jfield "org.objectweb.asm.Constants" "FMUL"))
(defconstant constants.lmul (jfield "org.objectweb.asm.Constants" "LMUL"))
(defconstant constants.imul (jfield "org.objectweb.asm.Constants" "IMUL"))
(defconstant constants.dsub (jfield "org.objectweb.asm.Constants" "DSUB"))
(defconstant constants.fsub (jfield "org.objectweb.asm.Constants" "FSUB"))
(defconstant constants.lsub (jfield "org.objectweb.asm.Constants" "LSUB"))
(defconstant constants.isub (jfield "org.objectweb.asm.Constants" "ISUB"))
(defconstant constants.dadd (jfield "org.objectweb.asm.Constants" "DADD"))
(defconstant constants.fadd (jfield "org.objectweb.asm.Constants" "FADD"))
(defconstant constants.ladd (jfield "org.objectweb.asm.Constants" "LADD"))
(defconstant constants.iadd (jfield "org.objectweb.asm.Constants" "IADD"))
(defconstant constants.swap (jfield "org.objectweb.asm.Constants" "SWAP"))
(defconstant constants.dup2_x2 (jfield "org.objectweb.asm.Constants" "DUP2_X2"))
(defconstant constants.dup2_x1 (jfield "org.objectweb.asm.Constants" "DUP2_X1"))
(defconstant constants.dup2 (jfield "org.objectweb.asm.Constants" "DUP2"))
(defconstant constants.dup_x2 (jfield "org.objectweb.asm.Constants" "DUP_X2"))
(defconstant constants.dup_x1 (jfield "org.objectweb.asm.Constants" "DUP_X1"))
(defconstant constants.dup (jfield "org.objectweb.asm.Constants" "DUP"))
(defconstant constants.pop2 (jfield "org.objectweb.asm.Constants" "POP2"))
(defconstant constants.pop (jfield "org.objectweb.asm.Constants" "POP"))
(defconstant constants.sastore (jfield "org.objectweb.asm.Constants" "SASTORE"))
(defconstant constants.castore (jfield "org.objectweb.asm.Constants" "CASTORE"))
(defconstant constants.bastore (jfield "org.objectweb.asm.Constants" "BASTORE"))
(defconstant constants.aastore (jfield "org.objectweb.asm.Constants" "AASTORE"))
(defconstant constants.dastore (jfield "org.objectweb.asm.Constants" "DASTORE"))
(defconstant constants.fastore (jfield "org.objectweb.asm.Constants" "FASTORE"))
(defconstant constants.lastore (jfield "org.objectweb.asm.Constants" "LASTORE"))
(defconstant constants.iastore (jfield "org.objectweb.asm.Constants" "IASTORE"))
(defconstant constants.astore (jfield "org.objectweb.asm.Constants" "ASTORE"))
(defconstant constants.dstore (jfield "org.objectweb.asm.Constants" "DSTORE"))
(defconstant constants.fstore (jfield "org.objectweb.asm.Constants" "FSTORE"))
(defconstant constants.lstore (jfield "org.objectweb.asm.Constants" "LSTORE"))
(defconstant constants.istore (jfield "org.objectweb.asm.Constants" "ISTORE"))
(defconstant constants.saload (jfield "org.objectweb.asm.Constants" "SALOAD"))
(defconstant constants.caload (jfield "org.objectweb.asm.Constants" "CALOAD"))
(defconstant constants.baload (jfield "org.objectweb.asm.Constants" "BALOAD"))
(defconstant constants.aaload (jfield "org.objectweb.asm.Constants" "AALOAD"))
(defconstant constants.daload (jfield "org.objectweb.asm.Constants" "DALOAD"))
(defconstant constants.faload (jfield "org.objectweb.asm.Constants" "FALOAD"))
(defconstant constants.laload (jfield "org.objectweb.asm.Constants" "LALOAD"))
(defconstant constants.iaload (jfield "org.objectweb.asm.Constants" "IALOAD"))
(defconstant constants.aload (jfield "org.objectweb.asm.Constants" "ALOAD"))
(defconstant constants.dload (jfield "org.objectweb.asm.Constants" "DLOAD"))
(defconstant constants.fload (jfield "org.objectweb.asm.Constants" "FLOAD"))
(defconstant constants.lload (jfield "org.objectweb.asm.Constants" "LLOAD"))
(defconstant constants.iload (jfield "org.objectweb.asm.Constants" "ILOAD"))
(defconstant constants.ldc (jfield "org.objectweb.asm.Constants" "LDC"))
(defconstant constants.sipush (jfield "org.objectweb.asm.Constants" "SIPUSH"))
(defconstant constants.bipush (jfield "org.objectweb.asm.Constants" "BIPUSH"))
(defconstant constants.dconst_1 (jfield "org.objectweb.asm.Constants" "DCONST_1"))
(defconstant constants.dconst_0 (jfield "org.objectweb.asm.Constants" "DCONST_0"))
(defconstant constants.fconst_2 (jfield "org.objectweb.asm.Constants" "FCONST_2"))
(defconstant constants.fconst_1 (jfield "org.objectweb.asm.Constants" "FCONST_1"))
(defconstant constants.fconst_0 (jfield "org.objectweb.asm.Constants" "FCONST_0"))
(defconstant constants.lconst_1 (jfield "org.objectweb.asm.Constants" "LCONST_1"))
(defconstant constants.lconst_0 (jfield "org.objectweb.asm.Constants" "LCONST_0"))
(defconstant constants.iconst_5 (jfield "org.objectweb.asm.Constants" "ICONST_5"))
(defconstant constants.iconst_4 (jfield "org.objectweb.asm.Constants" "ICONST_4"))
(defconstant constants.iconst_3 (jfield "org.objectweb.asm.Constants" "ICONST_3"))
(defconstant constants.iconst_2 (jfield "org.objectweb.asm.Constants" "ICONST_2"))
(defconstant constants.iconst_1 (jfield "org.objectweb.asm.Constants" "ICONST_1"))
(defconstant constants.iconst_0 (jfield "org.objectweb.asm.Constants" "ICONST_0"))
(defconstant constants.iconst_m1 (jfield "org.objectweb.asm.Constants" "ICONST_M1"))
(defconstant constants.aconst-null (jfield "org.objectweb.asm.Constants" "ACONST_NULL"))
(defconstant constants.nop (jfield "org.objectweb.asm.Constants" "NOP"))
(defconstant constants.t-long (jfield "org.objectweb.asm.Constants" "T_LONG"))
(defconstant constants.t-int (jfield "org.objectweb.asm.Constants" "T_INT"))
(defconstant constants.t-short (jfield "org.objectweb.asm.Constants" "T_SHORT"))
(defconstant constants.t-byte (jfield "org.objectweb.asm.Constants" "T_BYTE"))
(defconstant constants.t-double (jfield "org.objectweb.asm.Constants" "T_DOUBLE"))
(defconstant constants.t-float (jfield "org.objectweb.asm.Constants" "T_FLOAT"))
(defconstant constants.t-char (jfield "org.objectweb.asm.Constants" "T_CHAR"))
(defconstant constants.t-boolean (jfield "org.objectweb.asm.Constants" "T_BOOLEAN"))
(defconstant constants.acc-deprecated (jfield "org.objectweb.asm.Constants" "ACC_DEPRECATED"))
(defconstant constants.acc-synthetic (jfield "org.objectweb.asm.Constants" "ACC_SYNTHETIC"))
(defconstant constants.acc-super (jfield "org.objectweb.asm.Constants" "ACC_SUPER"))
(defconstant constants.acc-strict (jfield "org.objectweb.asm.Constants" "ACC_STRICT"))
(defconstant constants.acc-abstract (jfield "org.objectweb.asm.Constants" "ACC_ABSTRACT"))
(defconstant constants.acc-interface (jfield "org.objectweb.asm.Constants" "ACC_INTERFACE"))
(defconstant constants.acc-enum (jfield "org.objectweb.asm.Constants" "ACC_ENUM"))
(defconstant constants.acc-native (jfield "org.objectweb.asm.Constants" "ACC_NATIVE"))
(defconstant constants.acc-transient (jfield "org.objectweb.asm.Constants" "ACC_TRANSIENT"))
(defconstant constants.acc-varargs (jfield "org.objectweb.asm.Constants" "ACC_VARARGS"))
(defconstant constants.acc-bridge (jfield "org.objectweb.asm.Constants" "ACC_BRIDGE"))
(defconstant constants.acc-volatile (jfield "org.objectweb.asm.Constants" "ACC_VOLATILE"))
(defconstant constants.acc-synchronized (jfield "org.objectweb.asm.Constants" "ACC_SYNCHRONIZED"))
(defconstant constants.acc-final (jfield "org.objectweb.asm.Constants" "ACC_FINAL"))
(defconstant constants.acc-static (jfield "org.objectweb.asm.Constants" "ACC_STATIC"))
(defconstant constants.acc-protected (jfield "org.objectweb.asm.Constants" "ACC_PROTECTED"))
(defconstant constants.acc-private (jfield "org.objectweb.asm.Constants" "ACC_PRIVATE"))
(defconstant constants.acc-public (jfield "org.objectweb.asm.Constants" "ACC_PUBLIC"))
(defconstant constants.v1-1 (jfield "org.objectweb.asm.Constants" "V1_1"))
(defmethod make-label-0 nil
  (make-instance 'label :java-instance (jnew (jconstructor "org.objectweb.asm.Label"))))

;;end of jparse generated definitions


(defmethod visit-4 ((instance class-writer) (v1 fixnum) (v2 string) (v3 string) v4)
  (jcall
   (jmethod "org.objectweb.asm.ClassWriter" "visit" "int" "int" "java.lang.String" "java.lang.String" "[Ljava.lang.String;" "java.lang.String")
   (java-instance instance) constants.v1-1 v1 v2 v3 v4 nil))

(defmethod visit-field-3 ((instance class-writer) (v1 fixnum) (v2 string) (v3 string))
  (jcall
   (jmethod "org.objectweb.asm.ClassWriter" "visitField" "int" "java.lang.String" "java.lang.String" "java.lang.Object" "org.objectweb.asm.Attribute")
   (java-instance instance) v1 v2 v3 nil nil))

(defmethod visit-method-3 ((instance class-writer) (v1 fixnum) (v2 string) (v3 string))
  (make-instance 'code-visitor :java-instance
                 (jcall
                  (jmethod "org.objectweb.asm.ClassWriter" "visitMethod" "int" "java.lang.String" "java.lang.String" "[Ljava.lang.String;" "org.objectweb.asm.Attribute")
                  (java-instance instance) v1 v2 v3 nil nil)))

(defun make-java-string (string)
  (make-instance '|java.lang.Object|
                 :java-instance (jnew (jconstructor "java.lang.String" "[C") (jnew-array-from-array "char" string))))

(defparameter *primitive-types*
  (acons 
   "void" (list "V" (list "" "" "") -1 constants.return -1)
   (acons 
    "byte"
    (list "B" (list "org/armedbear/lisp/Fixnum" "java/lang/Byte" "byteValue")
          constants.iload constants.ireturn constants.iconst_0)
    (acons 
     "short"
     (list "S" (list "org/armedbear/lisp/Fixnum" "java/lang/Short" "shortValue")
           constants.iload constants.ireturn constants.iconst_0)
     (acons 
      "int"
      (list "I" (list "org/armedbear/lisp/Fixnum" "java/lang/Integer" "intValue")
            constants.iload constants.ireturn constants.iconst_0)
      (acons 
       "long"
       (list "J" (list "org/armedbear/lisp/Fixnum" "java/lang/Long" "longValue")
             constants.lload constants.lreturn constants.lconst_0)
       (acons 
        "float"
        (list "F" (list "org/armedbear/lisp/SingleFloat" "java/lang/Float" "floatValue")
              constants.fload constants.freturn constants.fconst_0)
        (acons 
         "double"
         (list "D" (list "org/armedbear/lisp/DoubleFloat" "java/lang/Double" "doubleValue")
               constants.dload constants.dreturn constants.dconst_0)
         (acons 
          "char"
          (list "C" (list "org/armedbear/lisp/LispCharacter" "java/lang/Character" "charValue")
                constants.iload constants.ireturn constants.iconst_0)
          (acons 
           "boolean"
           (list "Z" (list "org/armedbear/lisp/LispObject" "" "")
                 constants.iload constants.ireturn constants.iconst_0)
           nil))))))))))

(defun primitive-type-p (type)
  (assoc type *primitive-types* :test #'string=))

(defun type-name (type)
  (let* ((dim (count #\[ type :test #'char=))
         (prefix (make-string dim :initial-element #\[))
         (base-type (string-right-trim "[ ]" type))
         (base-name (assoc base-type *primitive-types* :test #'string=)))
    (concatenate 'string prefix
                 (if base-name (cadr base-name)
                     (substitute #\/ #\.
                                 (if (zerop dim) base-type (decorate-type-name base-type)))))))


(defun decorate-type-name (type)
  (if (char= (char type 0) #\[) type
      (format nil "L~a;" type)))

(defun decorated-type-name (type)
  (let ((name (type-name type)))
    (if (primitive-type-p type) name (decorate-type-name name))))

(defun arg-type-for-make-lisp-object (type)
  (if (primitive-type-p type)
      (decorated-type-name type)
      "Ljava/lang/Object;"))

(defun return-type-for-make-lisp-object (type)
  (let ((name (assoc type *primitive-types* :test #'string=)))
    (if name (caaddr name) "org/armedbear/lisp/LispObject")))

(defun cast-type (type)
  (let ((name (assoc type *primitive-types* :test #'string=)))
    (if name (cadr (caddr name)) (type-name type))))

(defun converter-for-primitive-return-type (type)
  (assert (and (primitive-type-p type) 
               (not (or (string= type "void")(string= type "boolean")))))
  (caddr (caddr (assoc type *primitive-types* :test #'string=))))

(defun load-instruction (type)
  (let ((name (assoc type *primitive-types* :test #'string=)))
    (if name (cadddr name) constants.aload)))

(defun return-instruction (type)
  (let ((name (assoc type *primitive-types* :test #'string=)))
    (if name (car (cddddr name)) constants.areturn)))

(defun error-constant (type)
  (let ((name (assoc type *primitive-types* :test #'string=)))
    (if name (cadr (cddddr name)) constants.aconst-null)))


(defun size (type)
  (if (or (string= type "long") (string= type "double")) 2 1))

(defun modifier (m)
  (cond ((string= "public" m) constants.acc-public)
        ((string= "protected" m) constants.acc-protected)
        ((string= "private" m) constants.acc-private)
        ((string= "static" m) constants.acc-static)
        ((string= "abstract" m) constants.acc-abstract)
        ((string= "final" m) constants.acc-final)
        ((string= "transient" m) constants.acc-transient)
        ((string= "volatile" m) constants.acc-volatile)
        ((string= "synchronized" m) constants.acc-synchronized)
        (t (error "Invalid modifier ~s." m))))


(defun write-method
  (class-writer class-name class-type-name method-name unique-method-name modifiers result-type arg-types &optional super-invocation)

  (let* ((args-size (reduce #'+ arg-types :key #'size))
         (index (+ 2 args-size))
         (cv (visit-method-3
              class-writer
              (reduce #'+ modifiers :key #'modifier)
              method-name
              (format nil "(~{~a~})~a"
                      (mapcar #'decorated-type-name arg-types) (decorated-type-name result-type)))))

    (when super-invocation
      (visit-var-insn-2 cv constants.aload 0)
      (loop for arg-number in (cdr super-invocation)
        with super-arg-types = (make-string-output-stream)
        do
        (visit-var-insn-2 cv
                          (load-instruction (nth (1- arg-number) arg-types))
                          (reduce #'+ arg-types :end (1- arg-number) :key #'size :initial-value 1))
        (write-string (decorated-type-name (nth (1- arg-number) arg-types)) super-arg-types)
        finally
        (visit-method-insn-4 cv constants.invokespecial
                             (type-name (car super-invocation)) "<init>"
                             (format nil "(~a)~a"
                                     (get-output-stream-string super-arg-types) "V"))))
    (visit-ldc-insn-1 cv (make-java-string class-name))
    (visit-method-insn-4 cv constants.invokestatic
                         "org/armedbear/lisp/RuntimeClass"
                         "getRuntimeClass"
                         "(Ljava/lang/String;)Lorg/armedbear/lisp/RuntimeClass;")
    (visit-field-insn-4 cv constants.putstatic
                        class-type-name "rc" "Lorg/armedbear/lisp/RuntimeClass;")
    (visit-field-insn-4 cv constants.getstatic
                        class-type-name "rc" "Lorg/armedbear/lisp/RuntimeClass;")
    (visit-ldc-insn-1 cv (make-java-string unique-method-name))
    (visit-method-insn-4 cv constants.invokevirtual
                         "org/armedbear/lisp/RuntimeClass"
                         "getLispMethod"
                         "(Ljava/lang/String;)Lorg/armedbear/lisp/Function;")
    (visit-var-insn-2 cv constants.astore (1+ args-size))
    (visit-field-insn-4 cv constants.getstatic
                        "org/armedbear/lisp/Lisp" "NIL" "Lorg/armedbear/lisp/LispObject;")
    (visit-var-insn-2 cv constants.astore (+ 2 args-size))


    (let ((l0 (make-label-0))(l1 (make-label-0))(l2 (make-label-0))(l3 (make-label-0)))
      (visit-label-1 cv l0)

      (visit-var-insn-2 cv constants.aload index)
      (visit-var-insn-2 cv constants.aload 0) ; (visit-var-insn-2 cv constants.aload 0)
      (visit-method-insn-4 cv constants.invokestatic
                           "org/armedbear/lisp/RuntimeClass" "makeLispObject"
                           (format nil "(~a)~a"
                                   (arg-type-for-make-lisp-object "java.lang.Object")
                                   (decorate-type-name (return-type-for-make-lisp-object "java.lang.Object"))))
      (visit-method-insn-4 cv constants.invokevirtual
                           "org/armedbear/lisp/LispObject"
                           "push"
                           "(Lorg/armedbear/lisp/LispObject;)Lorg/armedbear/lisp/LispObject;")
      (visit-var-insn-2 cv constants.astore (+ 2 args-size))

      (loop for arg-type in (reverse arg-types) and j = args-size then (- j (size arg-type))
        do
        (visit-var-insn-2 cv constants.aload index)

        (visit-var-insn-2 cv (load-instruction arg-type) j)
        (visit-method-insn-4 cv constants.invokestatic
                             "org/armedbear/lisp/RuntimeClass" "makeLispObject"
                             (format nil "(~a)~a"
                                     (arg-type-for-make-lisp-object arg-type)
                                     (decorate-type-name (return-type-for-make-lisp-object arg-type))))
        (visit-method-insn-4 cv constants.invokevirtual
                           "org/armedbear/lisp/LispObject"
                           "push"
                           "(Lorg/armedbear/lisp/LispObject;)Lorg/armedbear/lisp/LispObject;") ;uj
        (visit-var-insn-2 cv constants.astore (+ 2 args-size)))
      
      
      (visit-var-insn-2 cv constants.aload (1- index))
      (visit-var-insn-2 cv constants.aload index)

      (visit-type-insn-2 cv constants.new "org/armedbear/lisp/Environment")
      (visit-insn-1 cv constants.dup)
      (visit-method-insn-4 cv constants.invokespecial "org/armedbear/lisp/Environment" "<init>" "()V")
      (visit-method-insn-4 cv constants.invokestatic 
                           "org/armedbear/lisp/LispThread"
                           "currentThread"
                           "()Lorg/armedbear/lisp/LispThread;")
      (visit-method-insn-4 cv constants.invokestatic 
                           "org/armedbear/lisp/RuntimeClass"
                           "evalC"
                           "(Lorg/armedbear/lisp/LispObject;Lorg/armedbear/lisp/LispObject;Lorg/armedbear/lisp/Environment;Lorg/armedbear/lisp/LispThread;)Lorg/armedbear/lisp/LispObject;")
      (cond
       ((string= "void" result-type)
        (visit-insn-1 cv constants.pop))
       ((string= "boolean" result-type)
        (visit-method-insn-4 cv constants.invokevirtual
                             (return-type-for-make-lisp-object result-type)
                             "getBooleanValue"
                             (concatenate 'string "()" (type-name result-type))))
       ((primitive-type-p result-type)
        (visit-method-insn-4 cv constants.invokevirtual
                             "org/armedbear/lisp/LispObject"
                             "javaInstance"
                             "()Ljava/lang/Object;")
        (visit-type-insn-2 cv constants.checkcast (cast-type result-type))
        (visit-method-insn-4 cv constants.invokevirtual
                             (cast-type result-type)
                             (converter-for-primitive-return-type result-type)
                             (concatenate 'string "()" (type-name result-type))
                             ))
       (t
        (visit-method-insn-4 cv constants.invokevirtual
                             "org/armedbear/lisp/LispObject" "javaInstance" "()Ljava/lang/Object;")
        (visit-type-insn-2 cv constants.checkcast (cast-type result-type))))


      (visit-label-1 cv l1)
      (if (string= "void" result-type)
          (visit-jump-insn-2 cv constants.goto l3)
          (visit-insn-1 cv (return-instruction result-type)))
      (visit-label-1 cv l2)
      (visit-var-insn-2 cv constants.astore (1+ index))
      (visit-var-insn-2 cv constants.aload (1+ index))
      (visit-method-insn-4 cv constants.invokevirtual
                           "org/armedbear/lisp/ConditionThrowable" "printStackTrace" "()V")

      (if (string= "void" result-type)
          (progn (visit-insn-1 cv (return-instruction result-type))(visit-label-1 cv l3) )
          (visit-insn-1 cv (error-constant result-type)))

      (visit-insn-1 cv (return-instruction result-type))
      (visit-try-catch-block-4 cv l0 l1 l2 "org/armedbear/lisp/ConditionThrowable")

      (visit-maxs-2 cv 0 0))))



(defun jnew-runtime-class (class-name super-name interfaces constructors methods fields &optional filename)
  "Creates and loads a Java class with methods calling Lisp closures
   as given in METHODS.  CLASS-NAME and SUPER-NAME are strings,
   INTERFACES is a list of strings, CONSTRUCTORS, METHODS and FIELDS are
   lists of constructor, method and field definitions.

   Constructor definitions are lists of the form
   (argument-types function &optional super-invocation-arguments)
   where argument-types is a list of strings and function is a lisp function of
   (1+ (length argument-types)) arguments; the instance (`this') is passed in as
   the last argument. The optional super-invocation-arguments is a list of numbers
   between 1 and (length argument-types), where the number k stands for the kth argument
   to the just defined constructor. If present, the constructor of the superclass
   will be called with the appropriate arguments. E.g., if the constructor definition is
   ((\"java.lang.String\" \"int\") #'(lambda (string i this) ...) (2 1))
   then the constructor of the superclass with argument types (int, java.lang.String) will
   be called with the second and first arguments.

   Method definitions are lists of the form
   (method-name return-type argument-types function modifier*)
   where method-name and return-type are strings, argument-types is a list of strings and function
   is a lisp function of (1+ (length argument-types)) arguments; the instance (`this') is
   passed in as the last argument.

   Field definitions are lists of the form
   (field-name type modifier*)

   If FILE-NAME is given, a .class file will be written; this is useful for debugging only."

  (let ((cw (make-class-writer-1 (make-instance 'jboolean :java-instance t)))
        (class-type-name (type-name class-name))
        (super-type-name (type-name super-name))
        (interface-type-names 
         (when interfaces 
           (let* ((no-of-interfaces (length interfaces))
                  (ifarray (jnew-array "java.lang.String" no-of-interfaces)))
             (dotimes (i no-of-interfaces ifarray) 
               (setf (jarray-ref ifarray i) (type-name (nth i interfaces)))))))
        (args-for-%jnew))
    (visit-4 cw (+ constants.acc-public constants.acc-super)
             class-type-name super-type-name interface-type-names)
    (visit-field-3 cw (+ constants.acc-private constants.acc-static)
                   "rc" "Lorg/armedbear/lisp/RuntimeClass;")

    (dolist (field-def fields)
      (visit-field-3 cw
                     (reduce #'+ (cddr field-def) :key #'modifier)
                     (car field-def)
                     (decorated-type-name (cadr field-def))))


    (if constructors
        (loop for (arg-types constr-def super-invocation-args) in constructors
          for unique-method-name = (apply #'concatenate 'string "<init>|" arg-types)
          then (apply #'concatenate 'string "<init>|" arg-types)
          collect unique-method-name into args
          collect (coerce constr-def 'function) into args
          do
          (write-method 
           cw class-name class-type-name "<init>" unique-method-name '("public") "void" arg-types
           (cons super-type-name super-invocation-args))
          finally
          (setf args-for-%jnew (append args-for-%jnew args)))
        (let ((cv (visit-method-3 cw constants.acc-public "<init>" "()V")))
          (visit-var-insn-2 cv constants.aload 0)
          (visit-method-insn-4 cv constants.invokespecial super-type-name "<init>" "()V")
          (visit-insn-1 cv constants.return)
          (visit-maxs-2 cv 1 1)))

    (loop for (method-name ret-type arg-types method-def . modifiers) in methods
      for unique-method-name = (apply #'concatenate 'string method-name "|" arg-types)
      then (apply #'concatenate 'string method-name "|" arg-types)
      collect unique-method-name into args
      collect (coerce method-def 'function) into args
      do
      (write-method 
       cw class-name class-type-name method-name unique-method-name modifiers ret-type arg-types)
      finally
      (apply #'java::%jnew-runtime-class class-name (append args-for-%jnew args)))

    (visit-end-0 cw)

    (when filename
      (let ((os (make-file-output-stream-1 filename)))
        (write-1 os (to-byte-array-0 cw))
        (close-0 os)))

    (java::%load-java-class-from-byte-array class-name (java-instance (to-byte-array-0 cw)))))

(defun jredefine-method (class-name method-name arg-types method-def)
  "Replace the definition of the method named METHDO-NAME (or
   constructor, if METHD-NAME is nil) of argument types ARG-TYPES of the
   class named CLASS-NAME defined with JNEW-RUNTIME-CLASS with
   METHOD-DEF. See the documentation of JNEW-RUNTIME-CLASS."
  (assert (jruntime-class-exists-p class-name) (class-name)
          "Can't redefine methods of undefined runtime class ~a" class-name)
  (let ((unique-method-name 
         (apply #'concatenate 'string (if method-name method-name "<init>") "|" arg-types)))
    (java::%jredefine-method class-name unique-method-name  (compile nil method-def))))

(defun jruntime-class-exists-p (class-name)
  "Returns true if a class named CLASS-NAME has been created and loaded by JNEW-RUNTIME-CLASS.
   Needed because Java classes cannot be reloaded."
  (when
    (jstatic (jmethod "org.armedbear.lisp.RuntimeClass" "getRuntimeClass" "java.lang.String")
             "org.armedbear.lisp.RuntimeClass"
             class-name)
    t))
