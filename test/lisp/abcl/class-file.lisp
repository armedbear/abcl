;;; compiler-tests.lisp
;;;
;;; Copyright (C) 2010 Erik Huelsmann
;;;
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

#+abcl
(require '#:jvm)

(in-package #:abcl.test.lisp)


(deftest fieldtype.1a
    (string= (jvm::internal-field-type :int) "I")
  T)

(deftest fieldtype.1b
    (string= (jvm::internal-field-type :long) "J")
  T)

(deftest fieldtype.1c
    (string= (jvm::internal-field-type :float) "F")
  T)

(deftest fieldtype.1d
    (string= (jvm::internal-field-type :double) "D")
  T)

(deftest fieldtype.1e
    (string= (jvm::internal-field-type :boolean) "Z")
  T)

(deftest fieldtype.1f
    (string= (jvm::internal-field-type :char) "C")
  T)

(deftest fieldtype.1g
    (string= (jvm::internal-field-type :byte) "B")
  T)

(deftest fieldtype.1h
    (string= (jvm::internal-field-type :short) "S")
  T)

(deftest fieldtype.1i
    (string= (jvm::internal-field-type :void) "V")
  T)

(deftest fieldtype.1j
    (string= (jvm::internal-field-type nil) "V")
  T)

(deftest fieldtype.2
    (string= (jvm::internal-field-type jvm::+lisp-object+)
             "org/armedbear/lisp/LispObject")
  T)


(deftest fieldref.1a
    (string= (jvm::internal-field-ref :int) "I")
  T)

(deftest fieldref.1b
    (string= (jvm::internal-field-ref :long) "J")
  T)

(deftest fieldref.1c
    (string= (jvm::internal-field-ref :float) "F")
  T)

(deftest fieldref.1d
    (string= (jvm::internal-field-ref :double) "D")
  T)

(deftest fieldref.1e
    (string= (jvm::internal-field-ref :boolean) "Z")
  T)

(deftest fieldref.1f
    (string= (jvm::internal-field-ref :char) "C")
  T)

(deftest fieldref.1g
    (string= (jvm::internal-field-ref :byte) "B")
  T)

(deftest fieldref.1h
    (string= (jvm::internal-field-ref :short) "S")
  T)

(deftest fieldref.1i
    (string= (jvm::internal-field-ref :void) "V")
  T)

(deftest fieldref.1j
    (string= (jvm::internal-field-ref nil) "V")
  T)

(deftest fieldref.2
    (string= (jvm::internal-field-ref jvm::+lisp-object+)
             "Lorg/armedbear/lisp/LispObject;")
  T)

(deftest descriptor.1
    (and
     (string= (jvm::descriptor :void :int :long :boolean)
              "(IJZ)V")
     (string= (jvm::descriptor nil :int :long :boolean)
              "(IJZ)V"))
  T)

(deftest descriptor.2
    (string= (jvm::descriptor jvm::+lisp-object+ jvm::+lisp-object+)
             "(Lorg/armedbear/lisp/LispObject;)Lorg/armedbear/lisp/LispObject;")
  T)

(deftest map-flags.1
    (eql (jvm::map-flags '(:public)) #x0001)
  T)

(deftest pool.1
    (let* ((pool (jvm::make-pool)))
      (jvm::pool-add-class pool jvm::+lisp-readtable+)
      (jvm::pool-add-field-ref pool jvm::+lisp-readtable+ "ABC" :int)
      (jvm::pool-add-field-ref pool
                               jvm::+lisp-readtable+ "ABD"
                               jvm::+lisp-readtable+)
      (jvm::pool-add-method-ref pool jvm::+lisp-readtable+ "MBC" :int)
      (jvm::pool-add-method-ref pool jvm::+lisp-readtable+ "MBD"
                                jvm::+lisp-readtable+)
      (jvm::pool-add-interface-method-ref pool
                                          jvm::+lisp-readtable+ "MBD" :int)
      (jvm::pool-add-interface-method-ref pool
                                          jvm::+lisp-readtable+ "MBD"
                                          jvm::+lisp-readtable+)
      (jvm::pool-add-string pool "string")
      (jvm::pool-add-int pool 1)
      (jvm::pool-add-float pool 1.0f0)
      (jvm::pool-add-long pool 1)
      (jvm::pool-add-double pool 1.0d0)
      (jvm::pool-add-name/type pool "name1" :int)
      (jvm::pool-add-name/type pool "name2" jvm::+lisp-object+)
      (jvm::pool-add-utf8 pool "utf8")
      T)
  T)

(deftest make-class-file.1
    (let* ((class (jvm::make-jvm-class-name "org/armedbear/lisp/mcf_1"))
           (file (jvm::make-class-file class jvm::+lisp-object+ '(:public))))
      (jvm::class-add-field file (jvm::make-field "ABC" :int))
      (jvm::class-add-field file (jvm::make-field "ABD" jvm::+lisp-object+))
      (jvm::class-add-method file (jvm::make-jvm-method "MBC" nil :int))
      (jvm::class-add-method file (jvm::make-jvm-method "MBD" nil jvm::+lisp-object+))
      (jvm::class-add-method file (jvm::make-jvm-method :constructor :void nil))
      (jvm::class-add-method file (jvm::make-jvm-method :static-initializer :void nil))
      T)
  T)

(deftest finalize-class-file.1
    (let* ((class (jvm::make-jvm-class-name "org/armedbear/lisp/fcf_1"))
           (file (jvm::make-class-file class jvm::+lisp-object+ '(:public))))
      (jvm::class-add-field file (jvm::make-field "ABC" :int))
      (jvm::class-add-field file (jvm::make-field "ABD" jvm::+lisp-object+))
      (jvm::class-add-method file (jvm::make-jvm-method "MBC" nil '(:int)))
      (jvm::class-add-method file
                             (jvm::make-jvm-method "MBD" nil
                                                (list jvm::+lisp-object+)))
      (jvm::finalize-class-file file)
      file
      T)
  T)

(deftest generate-method.1
    (let* ((class (jvm::make-jvm-class-name "org/armedbear/lisp/gm_1"))
           (file (jvm::make-class-file class jvm::+lisp-object+ '(:public)))
           (method (jvm::make-jvm-method :static-initializer :void nil
                                      :flags '(:static))))
      (jvm::class-add-method file method)
      (jvm::with-code-to-method (file method)
        (jvm::emit 'return))
      (jvm::finalize-class-file file)
      (with-open-stream (stream (sys::%make-byte-array-output-stream))
        (jvm::write-class-file file stream)
        (sys::load-compiled-function (sys::%get-output-stream-bytes stream)))
      T)
  T)

(deftest generate-method.2
    (let* ((class (jvm::make-jvm-class-name "org/armedbear/lisp/gm_2"))
           (file (jvm::make-class-file class jvm::+lisp-object+ '(:public)))
           (method (jvm::make-jvm-method "doNothing" :void nil)))
      (jvm::class-add-method file method)
      (jvm::with-code-to-method (file method)
        (let ((label1 (gensym))
              (label2 (gensym))
              (label3 (gensym)))
          (jvm::label label1)
          (jvm::emit 'jvm::iconst_1)
          (jvm::label label2)
          (jvm::emit 'return)
          (jvm::label label3)
          (jvm::code-add-exception-handler (jvm::method-attribute method "Code")
                                           label1 label2 label3 nil))
        (jvm::emit 'return))
      (jvm::finalize-class-file file)
      (with-open-stream (stream (sys::%make-byte-array-output-stream))
        (jvm::write-class-file file stream)
        (sys::load-compiled-function (sys::%get-output-stream-bytes stream)))
      T)
  T)

;; generation of an ABCL-like function class
(deftest generate-method.3
    (let* ((class (jvm::make-jvm-class-name "org.armedbear.lisp.gm_3"))
           (file (jvm::make-class-file class jvm::+lisp-primitive+ '(:public)))
           )
      (let ((method (jvm::make-jvm-method :constructor :void nil)))
        (jvm::class-add-method file method)
        (jvm::with-code-to-method (file method)
          (jvm::emit 'aload 0)
          (jvm::emit-getstatic jvm::+lisp+ "NIL" jvm::+lisp-object+)
          (jvm::emit-getstatic jvm::+lisp+ "NIL" jvm::+lisp-object+)
          (jvm::emit-invokespecial-init jvm::+lisp-primitive+
                                        (list jvm::+lisp-object+
                                              jvm::+lisp-object+))
          (jvm::emit 'return)))
      (let ((method (jvm::make-jvm-method "execute" jvm::+lisp-object+ nil)))
        (jvm::class-add-method file method)
        (jvm::with-code-to-method (file method)
          (jvm::emit-getstatic jvm::+lisp+ "NIL" jvm::+lisp-object+)
          (jvm::emit 'jvm::areturn)))
      (jvm::finalize-class-file file)
      (with-open-stream (stream (sys::%make-byte-array-output-stream))
        (jvm::write-class-file file stream)
        (funcall (sys::load-compiled-function (sys::%get-output-stream-bytes stream)))))
  NIL)

;; generation of an ABCL-like function class with static init function and
;; static field
(deftest generate-method.4
    (let* ((class (jvm::make-jvm-class-name "org.armedbear.lisp.gm_4"))
           (file (jvm::make-class-file class jvm::+lisp-primitive+ '(:public)))
           )
      (jvm::class-add-field file (jvm::make-field "N1" jvm::+lisp-object+
                                                  :flags '(:static :private)))
      (let ((method (jvm::make-jvm-method :static-initializer :void nil :flags '(:static))))
        (jvm::class-add-method file method)
        (jvm::with-code-to-method (file method)
          (jvm::emit-getstatic jvm::+lisp+ "NIL" jvm::+lisp-object+)
          (jvm::emit-putstatic class "N1" jvm::+lisp-object+)
          (jvm::emit 'return)))
      (let ((method (jvm::make-jvm-method :constructor :void nil)))
        (jvm::class-add-method file method)
        (jvm::with-code-to-method (file method)
          (jvm::emit 'aload 0)
          (jvm::emit-getstatic class "N1" jvm::+lisp-object+)
          (jvm::emit-getstatic class "N1" jvm::+lisp-object+)
          (jvm::emit-invokespecial-init jvm::+lisp-primitive+
                                        (list jvm::+lisp-object+
                                              jvm::+lisp-object+))
          (jvm::emit 'return)))
      (let ((method (jvm::make-jvm-method "execute" jvm::+lisp-object+ nil)))
        (jvm::class-add-method file method)
        (jvm::with-code-to-method (file method)
          (jvm::emit-getstatic class "N1" jvm::+lisp-object+)
          (jvm::emit 'jvm::areturn)))
      (jvm::finalize-class-file file)
      (with-open-stream (stream (sys::%make-byte-array-output-stream))
        (jvm::write-class-file file stream)
        (funcall (sys::load-compiled-function (sys::%get-output-stream-bytes stream)))))
  NIL)


;; generation of ABCL-like function class with multiple 'execute' methods
(deftest generate-method.5
    (let* ((class (jvm::make-jvm-class-name "org.armedbear.lisp.gm_5"))
           (file (jvm::make-class-file class jvm::+lisp-primitive+ '(:public)))
           )
      (let ((method (jvm::make-jvm-method :constructor :void nil)))
        (jvm::class-add-method file method)
        (jvm::with-code-to-method (file method)
          (jvm::emit 'aload 0)
          (jvm::emit-getstatic jvm::+lisp+ "NIL" jvm::+lisp-object+)
          (jvm::emit-getstatic jvm::+lisp+ "NIL" jvm::+lisp-object+)
          (jvm::emit-invokespecial-init jvm::+lisp-primitive+
                                        (list jvm::+lisp-object+
                                              jvm::+lisp-object+))
          (jvm::emit 'return)))
      (let ((method (jvm::make-jvm-method "execute" jvm::+lisp-object+ nil)))
        (jvm::class-add-method file method)
        (jvm::with-code-to-method (file method)
          (jvm::emit-getstatic jvm::+lisp+ "NIL" jvm::+lisp-object+)
          (jvm::emit 'jvm::areturn)))
      (let ((method (jvm::make-jvm-method "execute" jvm::+lisp-object+
                                       (list jvm::+lisp-object+))))
        (jvm::class-add-method file method)
        (jvm::with-code-to-method (file method)
          (jvm::emit-getstatic jvm::+lisp+ "T" jvm::+lisp-symbol+)
          (jvm::emit 'jvm::areturn)))
      (jvm::finalize-class-file file)
      (with-open-stream (stream (sys::%make-byte-array-output-stream))
        (jvm::write-class-file file stream)
        (let* ((bytes (sys::%get-output-stream-bytes stream))
               (fn (sys::load-compiled-function bytes)))
          (values (funcall fn) (funcall fn NIL)))))
  NIL T)

;;Nested with-code-to-method
(deftest with-code-to-method.1
    (let* ((class (jvm::make-jvm-class-name "org/armedbear/lisp/gm_6"))
           (file (jvm::make-class-file class jvm::+lisp-object+ '(:public)))
           (method (jvm::make-jvm-method :static-initializer :void nil
				      :flags '(:static)))
	   (registers nil))
      (jvm::class-add-method file method)
      (jvm::with-code-to-method (file method)
	(jvm::allocate-register :int)
	(push jvm::*register* registers)
	(jvm::with-code-to-method (file method)
	  (jvm::allocate-register :int)
	  (push jvm::*register* registers)
	  (jvm::with-code-to-method (file method)
	    (jvm::allocate-register :int)
	    (push jvm::*register* registers))
	  (jvm::allocate-register :int)
	  (push jvm::*register* registers))
	(jvm::allocate-register :int)
	(push jvm::*register* registers))
      (jvm::finalize-class-file file)
      (nreverse registers))
  (1 2 3 4 5))

(deftest with-code-to-method.2
    (let* ((class (jvm::make-jvm-class-name "org/armedbear/lisp/gm_7"))
           (file (jvm::make-class-file class jvm::+lisp-object+ '(:public)))
           (method1 (jvm::make-jvm-method :static-initializer :void nil
				       :flags '(:static)))
	   (method2 (jvm::make-jvm-method "method2" :void nil))
	   (registers nil))
      (jvm::class-add-method file method1)
      (jvm::class-add-method file method2)
      (jvm::with-code-to-method (file method1)
	(jvm::allocate-register :int)
	(push jvm::*register* registers)
	(jvm::with-code-to-method (file method2)
	  (jvm::allocate-register :int)
	  (push jvm::*register* registers)
	  (jvm::with-code-to-method (file method1)
	    (jvm::allocate-register :int)
	    (push jvm::*register* registers))
	  (jvm::allocate-register :int)
	  (push jvm::*register* registers))
	(jvm::allocate-register :int)
	(push jvm::*register* registers))
      (jvm::finalize-class-file file)
      (nreverse registers))
  (1 1 2 2 3))

;; ;;  generation of an ABCL-like function, with mixed output to constructor,
;; ;;  static initializer and function method(s)
;; (deftest generate-method.6
;;     (let* ((class (jvm::make-jvm-class-name "org.armedbear.lisp.gm_6"))
;;            (file (jvm::make-class-file class jvm::+lisp-primitive+ '(:public)))
;;            )
;;       (let ((method (jvm::make-method :constructor :void nil)))
;;         (jvm::class-add-method file method)
;;         (jvm::with-code-to-method (file method)
;;           (jvm::emit 'aload 0)
;;           (jvm::emit-getstatic jvm::+lisp+ "NIL" jvm::+lisp-object+)
;;           (jvm::emit-getstatic jvm::+lisp+ "NIL" jvm::+lisp-object+)
;;           (jvm::emit-invokespecial-init jvm::+lisp-primitive+
;;                                         (list jvm::+lisp-object+
;;                                               jvm::+lisp-object+))
;;           (jvm::emit 'return)))
;;       (let ((method (jvm::make-method "execute" jvm::+lisp-object+ nil)))
;;         (jvm::class-add-method file method)
;;         (jvm::with-code-to-method (file method)
;;           (jvm::emit-getstatic jvm::+lisp+ "NIL" jvm::+lisp-object+)
;;           (jvm::emit 'jvm::areturn)))
;;       (jvm::finalize-class-file file)
;;       (with-open-stream (stream (sys::%make-byte-array-output-stream))
;;         (jvm::write-class-file file stream)
;;         (ignore-errors (sys::load-compiled-function nil))
;;         (funcall (sys::load-compiled-function (sys::%get-output-stream-bytes stream))))
;;       T
;;       )
;;   T)

