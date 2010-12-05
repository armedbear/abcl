(in-package :cl-user)
;;;; Copyright (C) 2010 by Mark Evenson

#|

A tour of the ABCL Java FFI by defining a Java interface at return,
creating a Java proxy implementation that provides a Lisp
implementation, and then use of the Java Reflection API to actually
invoke the Lisp implementation.

This needs abcl-0.24.0-dev or later.

|#

(defun define-java-interface (name package methods 
                              &optional (superinterfaces nil))
"Define a class for a Java interface called NAME in PACKAGE with METHODS.

METHODS is a list of (NAME RETURN-TYPE (ARG-TYPES)) entries.  NAME is
a string.  The values of RETURN-TYPE and the list of ARG-TYPES for the
defined method follow the are either references to Java objects as
created by JVM::MAKE-JVM-CLASS-NAME, or keywords representing Java
primtive types as contained in JVM::MAP-PRIMITIVE-TYPE.

SUPERINTERFACES optionally contains a list of interfaces that this
interface extends specified as fully qualifed dotted Java names."
  (let* ((class-name-string (format nil "~A/~A" package name))
         (class-name (jvm::make-jvm-class-name class-name-string))
         (class (jvm::make-class-interface-file class-name)))
    (dolist (superinterface superinterfaces)
      (jvm::class-add-superinterface 
       class 
       (if (type-of superinterface 'jvm::jvm-class-name)
           superinterface
           (jvm::make-jvm-class-name superinterface))))
    (dolist (method methods)
      (let ((name (first method))
            (returns (second method))
            (args (third method)))
      (jvm::class-add-method
       class
       (jvm::make-jvm-method name returns args
                             :flags '(:public :abstract)))))
    (jvm::finalize-class-file class)
    (let ((s (sys::%make-byte-array-output-stream)))
      (jvm::write-class-file class s)
      (sys::%get-output-stream-bytes s))))
    
(defun load-class (class-bytes) 
  "Load the Java byte[] array CLASS-BYTES as a Java class."
  (let ((load-class-method 
         (jmethod "org.armedbear.lisp.JavaClassLoader"
                  "loadClassFromByteArray" "[B")))
    (jcall load-class-method java::*classloader* class-bytes)))

;;; Unused in the interface example, but useful to get at the class
;;; definition with javap or jad
(defun write-class (class-bytes pathname)
  "Write the Java byte[] array CLASS-BYTES to PATHNAME."
  (with-open-file (stream pathname 
                          :direction :output 
                          :element-type '(signed-byte 8))
    (dotimes (i (jarray-length class-bytes))
      (write-byte (jarray-ref class-bytes i) stream))))

;;;; The example begins here.  We store all the intermediate values as
;;;; parameters so they may be inspected by those that follow this example.

;;; Construct a Java interface as an array of bytes containing the
;;; Java class
;;;
;;; This corresponds to the Java source:
;;;
;;;   package org.not.tmp;
;;;   public interface Foo {
;;;     public int add(int a, int b);
;;;   }
(defparameter *foo-bytes*
  (define-java-interface "Foo" "org/not/tmp" 
    '(("add" :int (:int :int)))))

;;; Load the class definition into the JVM
(defparameter *foo-interface-class* (load-class *foo-bytes*))

;;; Create an implementation of the interface in Lisp. 
(defparameter *foo*
  (jinterface-implementation "org.not.tmp.Foo"
                             "add" 
                             (lambda (a b) 
                               (reduce  #'+
                                        (mapcar (lambda (n) 
                                                  (jcall "intValue" n))
                                                (list a b))))))

;;; To get the class of what we just defined, we have to use Proxy.getProxyClass()
(defparameter *foo-class*
       ;; XXX would prettier if something like 
       ;;   (jarray-from-array-raw `#(,*foo-class*)) 
       ;; existed.
       (let ((interface-array (jnew-array "java.lang.Class" 1)))
         (setf (jarray-ref interface-array 0) *foo-interface-class*)
         (jstatic-raw "getProxyClass" "java.lang.reflect.Proxy" 
                      java::*classloader* interface-array)))
         

;;; Get a reference to the callable instance of this method.
(defparameter *callable-foo* 
  (jstatic-raw "getInvocationHandler" "java.lang.reflect.Proxy"  *foo*))

;;; In order to use *callable-foo* we need to reflect the method we are
;;; going to invoke.

;;; First we construct a Java array of classes for the parameters
(defparameter *add-parameters*
  ;; XXX again a jnew-array-from-array-raw would help here.
  (let ((parameters (jnew-array "java.lang.Class" 2)))
    (setf (jarray-ref parameters 0)
          (jfield-raw "java.lang.Integer" "TYPE")
          (jarray-ref parameters 1)
          (jfield-raw "java.lang.Integer" "TYPE"))
    parameters))

;;; Then we get the reflected instance of the method.
(defparameter *add-method*
  (jcall "getMethod" *foo-class* "add" *add-parameters*))

;;; Now we construct the actual arguments we are going to call with
(defparameter *add-args*
  (let ((args (jnew-array "java.lang.Integer" 2)))
    (setf (jarray-ref args 0)
          (jnew "java.lang.Integer" 2)
          (jarray-ref args 1)
          (jnew "java.lang.Integer" 2))
    args))

;;; It isn't strictly necessary to define the method parameter to
;;; JCALL in this manner, but it is more efficient in that the runtime
;;; does not have to dynamically introspect for the correct method.  
(defconstant +invocation-handler-invoke+
  (jmethod "java.lang.reflect.InvocationHandler" 
           "invoke" "java.lang.Object" "java.lang.reflect.Method" "[Ljava.lang.Object;"))

;; And finally we can make the call
#|
(jcall +invocation-handler-invoke+ *callable-foo* *foo* *add-method* *add-args*)
|#

