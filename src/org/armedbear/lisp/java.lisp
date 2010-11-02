;;; java.lisp
;;;
;;; Copyright (C) 2003-2007 Peter Graves, Andras Simon
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

(in-package "JAVA")

(require "CLOS")
(require "PRINT-OBJECT")

(defvar *classloader* (get-default-classloader))

(defun add-url-to-classpath (url &optional (classloader *classloader*))
  (jcall "addUrl" classloader url))

(defun add-urls-to-classpath (&rest urls)
  (dolist (url urls)
    (add-url-to-classpath url)))

(defun jregister-handler (object event handler &key data count)
  (%jregister-handler object event handler data count))

(defun jinterface-implementation (interface &rest method-names-and-defs)
  "Creates and returns an implementation of a Java interface with
   methods calling Lisp closures as given in METHOD-NAMES-AND-DEFS.

   INTERFACE is either a Java interface or a string naming one.

   METHOD-NAMES-AND-DEFS is an alternating list of method names
   (strings) and method definitions (closures).

   For missing methods, a dummy implementation is provided that
   returns nothing or null depending on whether the return type is
   void or not. This is for convenience only, and a warning is issued
   for each undefined method."
  (let ((interface (jclass interface))
        (implemented-methods
         (loop for m in method-names-and-defs
           for i from 0
           if (evenp i)
           do (assert (stringp m) (m) "Method names must be strings: ~s" m) and collect m
           else
           do (assert (or (symbolp m) (functionp m)) (m) "Methods must be function designators: ~s" m)))
        (null (make-immediate-object nil :ref)))
    (loop for method across
      (jclass-methods interface :declared nil :public t)
      for method-name = (jmethod-name method)
      when (not (member method-name implemented-methods :test #'string=))
      do
      (let* ((void-p (string= (jclass-name (jmethod-return-type method)) "void"))
             (arglist (when (plusp (length (jmethod-params method))) '(&rest ignore)))
             (def `(lambda
                     ,arglist
                     ,(when arglist '(declare (ignore ignore)))
                     ,(if void-p '(values) null))))
        (warn "Implementing dummy method ~a for interface ~a"
              method-name (jclass-name interface))
        (push (coerce def 'function) method-names-and-defs)
        (push method-name method-names-and-defs)))
    (apply #'%jnew-proxy interface method-names-and-defs)))

(defun jmake-invocation-handler (function)
  (%jmake-invocation-handler function))

(when (autoloadp 'jmake-proxy)
  (fmakunbound 'jmake-proxy))

(defgeneric jmake-proxy (interface implementation &optional lisp-this)
  (:documentation "Returns a proxy Java object implementing the provided interface(s) using methods implemented in Lisp - typically closures, but implementations are free to provide other mechanisms. You can pass an optional 'lisp-this' object that will be passed to the implementing methods as their first argument. If you don't provide this object, NIL will be used. The second argument of the Lisp methods is the name of the Java method being implemented. This has the implication that overloaded methods are merged, so you have to manually discriminate them if you want to. The remaining arguments are java-objects wrapping the method's parameters."))

(defun canonicalize-jproxy-interfaces (ifaces)
  (if (listp ifaces)
      (mapcar #'jclass ifaces)
      (list (jclass ifaces))))


(defmethod jmake-proxy (interface invocation-handler &optional lisp-this)
  "Basic implementation that directly uses an invocation handler."
  (%jmake-proxy (canonicalize-jproxy-interfaces interface) invocation-handler lisp-this))

(defmethod jmake-proxy (interface (implementation function) &optional lisp-this)
  "Implements a Java interface forwarding method calls to a Lisp function."
  (%jmake-proxy (canonicalize-jproxy-interfaces interface) (jmake-invocation-handler implementation) lisp-this))

(defmethod jmake-proxy (interface (implementation package) &optional lisp-this)
  "Implements a Java interface mapping Java method names to symbols in a given package. javaMethodName is mapped to a JAVA-METHOD-NAME symbol. An error is signaled if no such symbol exists in the package, or if the symbol exists but does not name a function."
  (flet ((java->lisp (name)
	   (with-output-to-string (str)
	     (let ((last-lower-p nil))
	       (map nil (lambda (char)
			  (let ((upper-p (char= (char-upcase char) char)))
			    (when (and last-lower-p upper-p)
			      (princ "-" str))
			    (setf last-lower-p (not upper-p))
			    (princ (char-upcase char) str)))
		    name)))))
    (%jmake-proxy (canonicalize-jproxy-interfaces interface)
		  (jmake-invocation-handler 
		   (lambda (obj method &rest args)
		     (let ((sym (find-symbol
				 (java->lisp method)
				 implementation)))
		       (unless sym
			 (error "Symbol ~A, implementation of method ~A, not found in ~A"
				  (java->lisp method)
				  method
				  implementation))
			 (if (fboundp sym)
			     (apply (symbol-function sym) obj method args)
			     (error "Function ~A, implementation of method ~A, not found in ~A"
				    sym method implementation)))))
		  lisp-this)))

(defmethod jmake-proxy (interface (implementation hash-table) &optional lisp-this)
  "Implements a Java interface using closures in an hash-table keyed by Java method name."
  (%jmake-proxy (canonicalize-jproxy-interfaces interface)
		(jmake-invocation-handler 
		 (lambda (obj method &rest args)
		   (let ((fn (gethash method implementation)))
		     (if fn
			 (apply fn obj args)
			 (error "Implementation for method ~A not found in ~A"
				method implementation)))))
		lisp-this))

(defun jequal (obj1 obj2)
  "Compares obj1 with obj2 using java.lang.Object.equals()"
  (jcall (jmethod "java.lang.Object" "equals" "java.lang.Object")
	 obj1 obj2))

(defun jobject-class (obj)
  "Returns the Java class that OBJ belongs to"
  (jcall (jmethod "java.lang.Object" "getClass") obj))

(defun jclass-superclass (class)
  "Returns the superclass of CLASS, or NIL if it hasn't got one"
  (jcall (jmethod "java.lang.Class" "getSuperclass") (jclass class)))

(defun jclass-interfaces (class)
  "Returns the vector of interfaces of CLASS"
  (jcall (jmethod "java.lang.Class" "getInterfaces") (jclass class)))

(defun jclass-interface-p (class)
  "Returns T if CLASS is an interface"
  (jcall (jmethod "java.lang.Class" "isInterface") (jclass class)))

(defun jclass-superclass-p (class-1 class-2)
  "Returns T if CLASS-1 is a superclass or interface of CLASS-2"
  (jcall (jmethod "java.lang.Class" "isAssignableFrom" "java.lang.Class")
         (jclass class-1)
         (jclass class-2)))

(defun jclass-array-p (class)
  "Returns T if CLASS is an array class"
  (jcall (jmethod "java.lang.Class" "isArray") (jclass class)))

(defun jarray-component-type (atype)
  "Returns the component type of the array type ATYPE"
  (assert (jclass-array-p atype))
  (jcall (jmethod "java.lang.Class" "getComponentType") atype))

(defun jarray-length (java-array)
  (jstatic "getLength" "java.lang.reflect.Array" java-array)  )

(defun (setf jarray-ref) (new-value java-array &rest indices)
  (apply #'jarray-set java-array new-value indices))

(defun jnew-array-from-array (element-type array)
  "Returns a new Java array with base type ELEMENT-TYPE (a string or a class-ref)
   initialized from ARRAY"
  (flet
    ((row-major-to-index (dimensions n)
                         (loop for dims on dimensions
                           with indices
                           do
                           (multiple-value-bind (m r) (floor n (apply #'* (cdr dims)))
                             (push m indices)
                             (setq n r))
                           finally (return (nreverse indices)))))
    (let* ((fill-pointer (when (array-has-fill-pointer-p array) (fill-pointer array)))
           (dimensions (if fill-pointer (list fill-pointer) (array-dimensions array)))
           (jarray (apply #'jnew-array element-type dimensions)))
      (dotimes (i (if fill-pointer fill-pointer (array-total-size array)) jarray)
        #+maybe_one_day
        (setf (apply #'jarray-ref jarray (row-major-to-index dimensions i)) (row-major-aref array i))
        (apply #'(setf jarray-ref) (row-major-aref array i) jarray (row-major-to-index dimensions i))))))

(defun jnew-array-from-list (element-type list)
  (let ((jarray (jnew-array element-type (length list)))
	(i 0))
    (dolist (x list)
      (setf (jarray-ref jarray i) x
	    i (1+ i)))
    jarray))

(defun jclass-constructors (class)
  "Returns a vector of constructors for CLASS"
  (jcall (jmethod "java.lang.Class" "getConstructors") (jclass class)))

(defun jconstructor-params (constructor)
  "Returns a vector of parameter types (Java classes) for CONSTRUCTOR"
  (jcall (jmethod "java.lang.reflect.Constructor" "getParameterTypes") constructor))

(defun jclass-fields (class &key declared public)
  "Returns a vector of all (or just the declared/public, if DECLARED/PUBLIC is true) fields of CLASS"
  (let* ((getter (if declared "getDeclaredFields" "getFields"))
         (fields (jcall (jmethod "java.lang.Class" getter) (jclass class))))
    (if public (delete-if-not #'jmember-public-p fields) fields)))

(defun jclass-field (class field-name)
  "Returns the field named FIELD-NAME of CLASS"
  (jcall (jmethod "java.lang.Class" "getField" "java.lang.String")
         (jclass class) field-name))

(defun jfield-type (field)
  "Returns the type (Java class) of FIELD"
  (jcall (jmethod "java.lang.reflect.Field" "getType") field))

(defun jfield-name (field)
  "Returns the name of FIELD as a Lisp string"
  (jcall (jmethod "java.lang.reflect.Field" "getName") field))


(defun (setf jfield) (newvalue class-ref-or-field field-or-instance
		      &optional (instance nil instance-supplied-p) unused-value)
  (declare (ignore unused-value))
  (if instance-supplied-p
      (jfield class-ref-or-field field-or-instance instance newvalue)
      (jfield class-ref-or-field field-or-instance newvalue)))

(defun jclass-methods (class &key declared public)
  "Return a vector of all (or just the declared/public, if DECLARED/PUBLIC is true) methods of CLASS"
  (let* ((getter (if declared "getDeclaredMethods" "getMethods"))
         (methods (jcall (jmethod "java.lang.Class" getter) (jclass class))))
    (if public (delete-if-not #'jmember-public-p methods) methods)))

(defun jmethod-params (method)
  "Returns a vector of parameter types (Java classes) for METHOD"
  (jcall (jmethod "java.lang.reflect.Method" "getParameterTypes") method))

(defun jmethod-return-type (method)
  "Returns the result type (Java class) of the METHOD"
  (jcall (jmethod "java.lang.reflect.Method" "getReturnType") method))

(defun jmethod-declaring-class (method)
  "Returns the Java class declaring METHOD"
  (jcall (jmethod "java.lang.reflect.Method" "getDeclaringClass") method))

(defun jmethod-name (method)
  "Returns the name of METHOD as a Lisp string"
  (jcall (jmethod "java.lang.reflect.Method" "getName") method))

(defun jinstance-of-p (obj class)
  "OBJ is an instance of CLASS (or one of its subclasses)"
  (and (java-object-p obj)
       (jcall (jmethod "java.lang.Class" "isInstance" "java.lang.Object") (jclass class) obj)))

(defun jmember-static-p (member)
  "MEMBER is a static member of its declaring class"
  (jstatic (jmethod "java.lang.reflect.Modifier" "isStatic" "int")
           "java.lang.reflect.Modifier"
           (jcall (jmethod "java.lang.reflect.Member" "getModifiers") member)))

(defun jmember-public-p (member)
  "MEMBER is a public member of its declaring class"
  (jstatic (jmethod "java.lang.reflect.Modifier" "isPublic" "int")
           "java.lang.reflect.Modifier"
           (jcall (jmethod "java.lang.reflect.Member" "getModifiers") member)))

(defun jmember-protected-p (member)
  "MEMBER is a protected member of its declaring class"
  (jstatic (jmethod "java.lang.reflect.Modifier" "isProtected" "int")
           "java.lang.reflect.Modifier"
           (jcall (jmethod "java.lang.reflect.Member" "getModifiers") member)))

(defmethod make-load-form ((object java-object) &optional environment)
  (declare (ignore environment))
  (let ((class-name (jclass-name (jclass-of object))))
    (cond
     ((string= class-name "java.lang.reflect.Constructor")
      `(java:jconstructor ,(jclass-name
                            (jcall (jmethod "java.lang.reflect.Constructor"
                                            "getDeclaringClass") object))
                          ,@(loop for arg-type across
                              (jcall
                               (jmethod "java.lang.reflect.Constructor"
                                        "getParameterTypes")
                               object)
                              collecting
                              (jclass-name arg-type))))
     ((string= class-name "java.lang.reflect.Method")
      `(java:jmethod ,(jclass-name
                       (jcall (jmethod "java.lang.reflect.Method"
                                       "getDeclaringClass") object))
                     ,(jmethod-name object)
                     ,@(loop for arg-type across
                         (jcall
                          (jmethod "java.lang.reflect.Method"
                                   "getParameterTypes")
                          object)
                         collecting
                         (jclass-name arg-type))))
     ((jinstance-of-p object "java.lang.Class")
      `(java:jclass ,(jcall (jmethod "java.lang.Class" "getName") object)))
     (t
      (error "Unknown load-form for ~A" class-name)))))

(defun jproperty-value (obj prop)
  (%jget-property-value obj prop))

(defun (setf jproperty-value) (value obj prop)
  (%jset-property-value obj prop value))

;;; higher-level operators

(defmacro chain (target op &rest ops)
  "Performs chained method invocations. `target' is the receiver object (when the first call is a virtual method call) or a list in the form (:static <jclass>) when the first method call is a static method call. `op' and each of the `ops' are either method designators or lists in the form (<method designator> &rest args), where a method designator is either a string naming a method, or a jmethod object. `chain' will perform the method call specified by `op' on `target'; then, for each of the `ops', `chain' will perform the specified method call using the object returned by the previous method call as the receiver, and will ultimately return the result of the last method call.
  For example, the form:

  (chain (:static \"java.lang.Runtime\") \"getRuntime\" (\"exec\" \"ls\"))

  is equivalent to the following Java code:

  java.lang.Runtime.getRuntime().exec(\"ls\");"
  (labels ((canonicalize-op (op) (if (listp op) op (list op)))
	   (compose-arglist (target op) `(,(car op) ,target ,@(cdr op)))
	   (make-binding-for (form) `(,(gensym) ,form))
	   (make-binding (bindings next-op &aux (target (caar bindings)))
	     (cons (make-binding-for
		    `(jcall ,@(compose-arglist target
					       (canonicalize-op next-op))))
		   bindings)))
    (let* ((first (if (and (consp target) (eq (first target) :static))
		      `(jstatic ,@(compose-arglist (cadr target) (canonicalize-op op)))
		      `(jcall ,@(compose-arglist target (canonicalize-op op)))))
	   (bindings (nreverse
		      (reduce #'make-binding ops
			      :initial-value (list (make-binding-for first))))))
      `(let* ,bindings
	 (declare (ignore ,@(mapcar #'car bindings)))))))

(defmacro jmethod-let (bindings &body body)
  (let ((args (gensym)))
    `(let ,(mapcar (lambda (binding)
		     `(,(car binding) (jmethod ,@(cdr binding))))
		   bindings)
       (macrolet ,(mapcar (lambda (binding)
			    `(,(car binding) (&rest ,args)
			       `(jcall ,,(car binding) ,@,args)))
			  bindings)
	 ,@body))))

;;; print-object

(defmethod print-object ((obj java:java-object) stream)
  (write-string (sys::%write-to-string obj) stream))

(defmethod print-object ((e java:java-exception) stream)
  (if *print-escape*
      (print-unreadable-object (e stream :type t :identity t)
        (format stream "~A"
                (java:jcall (java:jmethod "java.lang.Object" "toString")
                            (java:java-exception-cause e))))
      (format stream "Java exception '~A'."
              (java:jcall (java:jmethod "java.lang.Object" "toString")
                          (java:java-exception-cause e)))))

;;; JAVA-CLASS support
(defconstant +java-lang-object+ (jclass "java.lang.Object"))

(defclass java-class (standard-class)
  ((jclass :initarg :java-class
	   :initform (error "class is required")
	   :reader java-class-jclass)))

;;init java.lang.Object class
(defconstant +java-lang-object-class+
  (%register-java-class +java-lang-object+
			(mop::ensure-class (make-symbol "java.lang.Object")
					   :metaclass (find-class 'java-class)
					   :direct-superclasses (list (find-class 'java-object))
					   :java-class +java-lang-object+)))

(defun jclass-additional-superclasses (jclass)
  "Extension point to put additional CLOS classes on the CPL of a CLOS Java class."
  (let ((supers nil))
    (when (jclass-interface-p jclass)
      (push (find-class 'java-object) supers))
    (when (jequal jclass (jclass "java.util.List"))
      (push (find-class 'sequence) supers))
    supers))

(defun ensure-java-class (jclass)
  (let ((class (%find-java-class jclass)))
    (if class
	class
	(%register-java-class
	 jclass (mop::ensure-class
		 (make-symbol (jclass-name jclass))
		 :metaclass (find-class 'java-class)
		 :direct-superclasses
		 (let ((supers
			(mapcar #'ensure-java-class
				(delete nil
					(concatenate 'list
						     (list (jclass-superclass jclass))
						     (jclass-interfaces jclass))))))
		   (append supers (jclass-additional-superclasses jclass)))
		 :java-class jclass)))))

(defmethod mop::compute-class-precedence-list ((class java-class))
  "Sort classes this way:
   1. Java classes (but not java.lang.Object)
   2. Java interfaces
   3. java.lang.Object
   4. other classes
   Rationale:
   1. Concrete classes are the most specific.
   2. Then come interfaces.
     So if a generic function is specialized both on an interface and a concrete class,
     the concrete class comes first.
   3. because everything is an Object.
   4. to handle base CLOS classes.
   Note: Java interfaces are not sorted among themselves in any way, so if a
   gf is specialized on two different interfaces and you apply it to an object that
   implements both, it is unspecified which method will be called."
  (let ((cpl (nreverse (mop::collect-superclasses* class))))
    (flet ((score (class)
	     (if (not (typep class 'java-class))
		 4
		 (cond
		   ((jcall (jmethod "java.lang.Object" "equals" "java.lang.Object")
			   (java-class-jclass class) +java-lang-object+) 3)
		   ((jclass-interface-p (java-class-jclass class)) 2)
		   (t 1)))))
      (stable-sort cpl #'(lambda (x y)
			   (< (score x) (score y)))))))
	  
(defmethod make-instance ((class java-class) &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (error "make-instance not supported for ~S" class))

(provide "JAVA")
