;    Copyright (c) Rich Hickey. All rights reserved.
;    The use and distribution terms for this software are covered by the
;    Common Public License 1.0 (http://opensource.org/licenses/cpl.php)
;    which can be found in the file CPL.TXT at the root of this distribution.
;    By using this software in any fashion, you are agreeing to be bound by
;    the terms of this license.
;    You must not remove this notice, or any other, from this software.

;    Ported to ABCL by asimon@math.bme.hu. 
;    Minor ABCL fixes by:
;    A. Vodonosov (avodonosov@yandex.ru).
;    Alex Mizrahi (alex.mizrahi@gmail.com)

(defpackage :jfli
  (:use :common-lisp :java)
  (:export

   :enable-java-proxies

   ;wrapper generation
   :def-java-class
   :get-jar-classnames
   :dump-wrapper-defs-to-file

   ;object creation etc
   :find-java-class
   :new
   :make-new
   :make-typed-ref
   :jeq

   ;array support
   :make-new-array
   :jlength
   :jref
   :jref-boolean
   :jref-byte
   :jref-char
   :jref-double
   :jref-float
   :jref-int
   :jref-short
   :jref-long

   ;proxy support
   :new-proxy
   :unregister-proxy

   ;conversions
   :box-boolean
   :box-byte
   :box-char
   :box-double
   :box-float
   :box-integer
   :box-long
   :box-short
   :box-string
   :unbox-boolean
   :unbox-byte
   :unbox-char
   :unbox-double
   :unbox-float
   :unbox-integer
   :unbox-long
   :unbox-short
   :unbox-string

;   :ensure-package
;   :member-symbol
;   :class-symbol
;   :constructor-symbol

   :*null*
   :new-class
   :super
   ))

(in-package :jfli)


(eval-when (:compile-toplevel :load-toplevel :execute)
(defun string-append (&rest strings)
  (apply #'concatenate 'string (mapcar #'(lambda (s) (if (symbolp s) (symbol-name s) s)) strings)))


(defun intern-and-unexport (string package)
  (multiple-value-bind (symbol status)
      (find-symbol string package)
    (when (and *compile-file-pathname* (eq status :external)) (unexport symbol package))
    (intern string package)))
)

(defun is-assignable-from (class-1 class-2)
  (jcall (jmethod "java.lang.Class" "isAssignableFrom" "java.lang.Class") 
	 class-2 class-1))  ;;not a typo

#+abcl_not_used
(defun new-object-array (len element-type initial-element)
  (jnew-array-from-array element-type (make-array (list len) :initial-element initial-element)))


(defun java-ref-p (x)
  (java-object-p x))

(deftype java-ref ()
  '(satisfies java-ref-p))
  
(defun split-package-and-class (name)
    (let ((p (position #\. name :from-end t)))
      (unless p (error "must supply package-qualified classname"))
      (values (subseq name 0 p)
              (subseq name (1+ p)))))

(defun is-name-of-primitive (s)
  (member s '("boolean" "byte" "char" "short" "int" "long" "float" "double" "void")
          :test #'string-equal))

(defun is-primitive-class (class)
  (is-name-of-primitive (jclass-name class)))

(defun convert-to-java-string (s)
  (jnew (jconstructor "java.lang.String" "java.lang.String") s))

(define-symbol-macro boolean.type (jfield "java.lang.Boolean" "TYPE"))
(define-symbol-macro byte.type (jfield "java.lang.Byte" "TYPE"))
(define-symbol-macro character.type (jfield "java.lang.Character" "TYPE"))
(define-symbol-macro short.type (jfield "java.lang.Short" "TYPE"))
(define-symbol-macro integer.type (jfield "java.lang.Integer" "TYPE"))
(define-symbol-macro long.type (jfield "java.lang.Long" "TYPE"))
(define-symbol-macro float.type (jfield "java.lang.Float" "TYPE"))
(define-symbol-macro double.type (jfield "java.lang.Double" "TYPE"))
(define-symbol-macro void.type (jfield "java.lang.Void" "TYPE"))

#|
(defconstant boolean.type (jfield "java.lang.Boolean" "TYPE"))
(defconstant byte.type (jfield "java.lang.Byte" "TYPE"))
(defconstant character.type (jfield "java.lang.Character" "TYPE"))
(defconstant short.type (jfield "java.lang.Short" "TYPE"))
(defconstant integer.type (jfield "java.lang.Integer" "TYPE"))
(defconstant long.type (jfield "java.lang.Long" "TYPE"))
(defconstant float.type (jfield "java.lang.Float" "TYPE"))
(defconstant double.type (jfield "java.lang.Double" "TYPE"))
|#

(defconstant *null* java:+null+)

(defun identity-or-nil (obj)
  (unless (equal obj *null*) obj))

;;;;;;;;;;;;;;;;;;;;;;;;;;; utilities ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun ensure-package (name)
    "find the package or create it if it doesn't exist"
    (or (find-package name)
        (make-package name :use '())))
  (intern "Object" (ensure-package "java.lang")) 
  (intern "String" (ensure-package "java.lang")))

(defun enumeration.hasmoreelements (enum)
  (jcall (jmethod "java.util.Enumeration" "hasMoreElements") enum))

(defun enumeration.nextelement (enum)
  (jcall (jmethod "java.util.Enumeration" "nextElement") enum))

(defmacro doenum ((e enum) &body body)
  "jni-based, so not safe and not exported, but used by the implementation"
  (let ((genum (gensym)))
    `(let ((,genum ,enum))
       (do ()
           ((not (enumeration.hasmoreelements ,genum)))
         (let ((,e (enumeration.nextelement ,genum)))
           ,@body)))))

;probably insufficiently general, works as used here
(defmacro get-or-init (place init-form)
  `(or ,place
       (setf ,place ,init-form)))


(eval-when (:compile-toplevel)
  (intern-and-unexport "OBJECT." "java.lang"))

;create object. to bootstrap the hierarchy
(defclass |java.lang|::object. ()
  ((ref :reader ref :initarg :ref)
   (lisp-allocated :reader lisp-allocated-p :initarg :lisp-allocated :initform nil))
  (:documentation "the superclass of all Java typed reference classes"))

(defun get-ref (x)
  "any function taking an object can be passed a raw java-ref ptr or a typed reference instance.
Will also convert strings for use as objects"
;; avodonosov: 
;; typecase instead of etypecase 
;; to allow not only jfli-wrapped objects
;; as a parameters of NEW-CLASS, but also native 
;; Lisp objects too (in case of ABCL they are java 
;; instances anyway).
;; For example that may be org.armedbear.lisp.Function.
  (typecase x
    (java-ref x)
    (|java.lang|::object. (ref x))
    (string (convert-to-java-string x))
    (null nil)
    ((or number character) x)
    ;; avodonosov: otherwise clause
	(otherwise x)))

(defun is-same-object (obj1 obj2)
  (equal obj1 obj2))

(defun jeq (obj1 obj2)
  "are these 2 java objects the same object? Note that is not the same as Object.equals()"
  (is-same-object (get-ref obj1) (get-ref obj2)))


;;;;;;;;;;;;;;;;;;;;;;;; names and symbols ;;;;;;;;;;;;;;;;;;;;;;;
#|
The library does a lot with names and symbols, needing at various times to:
 - find stuff in Java - full names w/case required
 - create hopefully non-conflicting packages and member names

When you (def-java-class "java.lang.String") you get a bunch of symbols/names:
a package named '|java.lang|
a class-symbol '|java.lang|:STRING. (note the dot and case), 
   which can usually be used where a typename is required
   it also serves as the name of the Lisp typed reference class for string
   its symbol-value is the canonic-class-symbol (see below)
a canonic-class-symbol '|java.lang|::|String|
   can be used to reconstitute the full class name

I've started trying to flesh out the notion of a Java class designator, which can either be
the full class name as a string, the class-symbol, or one of :boolean, :int etc
|#

(defun canonic-class-symbol (full-class-name)
  "(\"java.lang.Object\") -> '|java.lang|:|Object|"
  (multiple-value-bind (package class) (split-package-and-class full-class-name)
    (intern class (ensure-package package))))

(defun class-symbol (full-class-name)
  "(\"java.lang.Object\") -> '|java.lang|:object."
  (multiple-value-bind (package class) (split-package-and-class full-class-name)
    (intern (string-upcase (string-append class ".")) (ensure-package package))))

(defun unexported-class-symbol (full-class-name)
  "(\"java.lang.Object\") -> '|java.lang|::object."
  (multiple-value-bind (package class) (split-package-and-class full-class-name)
    (intern-and-unexport (string-upcase (string-append class ".")) (ensure-package package))))

(defun java-class-name (class-sym)
  "inverse of class-symbol, only valid on class-syms created by def-java-class"
  (let ((canonic-class-symbol (symbol-value class-sym)))
    (string-append (package-name (symbol-package canonic-class-symbol))
                                                "."
                                                canonic-class-symbol)))

(defun member-symbol (full-class-name member-name)
  "members are defined case-insensitively in case-sensitive packages,
prefixed by 'classname.' -
(member-symbol \"java.lang.Object\" \"toString\") -> '|java.lang|::OBJECT.TOSTRING"
  (multiple-value-bind (package class) (split-package-and-class full-class-name)
    (intern (string-upcase (string-append class "." member-name)) (ensure-package package))))

(defun unexported-member-symbol (full-class-name member-name)
  "members are defined case-insensitively in case-sensitive packages,
prefixed by 'classname.' -
(member-symbol \"java.lang.Object\" \"toString\") -> '|java.lang|::OBJECT.TOSTRING"
  (multiple-value-bind (package class) (split-package-and-class full-class-name)
    (intern-and-unexport (string-upcase (string-append class "." member-name)) (ensure-package package))))

(defun constructor-symbol (full-class-name)
  (member-symbol full-class-name "new"))

(defun unexported-constructor-symbol (full-class-name)
  (unexported-member-symbol full-class-name "new"))

(defun get-java-class-ref (canonic-class-symbol)
  "class-ref is cached on the plist of the canonic class symbol"
  (get-or-init (get canonic-class-symbol :class-ref)
               (let ((class-name (string-append (package-name
                                                 (symbol-package canonic-class-symbol))
                                                "."
                                                canonic-class-symbol)))
		  (jclass class-name)
		  )))

(defun find-java-class (class-sym-or-string)
  "Given a Java class designator, returns the Java Class object."
  (ctypecase class-sym-or-string
    (symbol (case class-sym-or-string
              (:int integer.type)
              (:char character.type)
              (:long long.type)
              (:float float.type)
              (:boolean boolean.type)
              (:short short.type)
              (:double double.type)
              (:byte byte.type)
	      (:void void.type)
              (otherwise (get-java-class-ref class-sym-or-string))))
    (string (get-java-class-ref (canonic-class-symbol class-sym-or-string)))))

;;;;;;;;;;;;;;;;;;;;;; typed reference support ;;;;;;;;;;;;;;;;;;;;;;;;
#|
The library maintains a hierarchy of typed reference classes that parallel the
class hierarchy on the Java side
new returns a typed reference, but other functions that return objects
return raw references (for efficiency) 
make-typed-ref can create fully-typed wrappers when desired
|#

(defun get-superclass-names (full-class-name)
  (let* ((class (get-java-class-ref (canonic-class-symbol full-class-name)))
         (super (jclass-superclass class))
         (interfaces (jclass-interfaces class))
         (supers ()))
    (loop for i across interfaces
      do (push i supers))
    ;hmmm - where should the base class go in the precedence list?
    ;is it more important than the interfaces? this says no
    (if super
        (push super supers)
      (push (jclass "java.lang.Object") supers))
    (setf supers (nreverse supers))
    ;now we need to fix up order so more derived classes are first
    ;but don't have a total ordering, so merge one at a time
    (let (result)
      (dolist (s supers)
        (setf result (merge 'list result (list s)
                            (lambda (x y)
                              (is-assignable-from x y)))))
      (mapcar #'jclass-name result))))
#|
(defun get-superclass-names (full-class-name)
  (let* ((class (get-java-class-ref (canonic-class-symbol full-class-name)))
         (super (class.getsuperclass class))
         (interfaces (class.getinterfaces class))
         (supers ()))
    (do-jarray (i interfaces)
      (push (class.getname i) supers))
    ;hmmm - where should the base class go in the precedence list?
    ;is it more important than the interfaces? this says no
    (if super
        (push (class.getname super) supers)
      (push "java.lang.Object" supers))
    (nreverse supers)))
|#

(defun %ensure-java-class (full-class-name)
  "walks the superclass hierarchy and makes sure all the classes are fully defined
(they may be undefined or just forward-referenced-class)
caches this has been done on the class-symbol's plist"
  (let* ((class-sym (class-symbol full-class-name))
         (class (find-class class-sym nil)))
    (if (or (eql class-sym '|java.lang|::object.)
            (get class-sym :ensured))
        class
	(let ((supers (get-superclass-names full-class-name)))
	  (dolist (super supers)
	    (%ensure-java-class super))
	  (unless (and class (subtypep class 'standard-object))
	    (setf class
		  #+abcl
		  (sys::ensure-class  class-sym :direct-superclasses (mapcar #'(lambda (c) (find-class (class-symbol c))) supers))))
	  (setf (get class-sym :ensured) t)
	  class))))


(defun ensure-java-hierarchy (class-sym)
  "Works off class-sym for efficient use in new
This will only work on class-syms created by def-java-class,
as it depends upon symbol-value being the canonic class symbol"
  (unless (get class-sym :ensured)
    (%ensure-java-class (java-class-name class-sym))))

(defun make-typed-ref (java-ref)
  "Given a raw java-ref, determines the full type of the object
and returns an instance of a typed reference wrapper"
  (when java-ref
    (let ((class (jobject-class java-ref)))
      (if (jclass-array-p class)
          (error "typed refs not supported for arrays (yet)")
        (make-instance (%ensure-java-class (jclass-name class)) :ref java-ref)))))


;;;;;;;;;;;;;;;;;;;;;;;;; Wrapper Generation ;;;;;;;;;;;;;;;;;;;;;;;;;;;
#|
In an effort to reduce the volume of stuff generated when wrapping entire libraries,
the wrappers just generate minimal stubs, which, if and when invoked at runtime,
complete the work of building thunking closures, so very little code is generated for
things never called (Java libraries have huge numbers of symbols).
Not sure if this approach matters, but that's how it works
|#

(defmacro def-java-class (full-class-name)
  "Given the package-qualified, case-correct name of a java class, will generate
wrapper functions for its contructors, fields and methods."
  (multiple-value-bind (pacakge class) (split-package-and-class full-class-name)
    (declare (ignore class))
    (let* ((class-sym (unexported-class-symbol full-class-name))
           (defs
	     (list*
	      #+nil `(format t "!!!!!!!!!!~a~%" ,full-class-name)
	      `(ensure-package ,pacakge)
					;build a path from the simple class symbol to the canonic
	      `(defconstant ,class-sym ',(canonic-class-symbol full-class-name))
	      `(export ',class-sym (symbol-package ',class-sym))
	      `(def-java-constructors ,full-class-name)
	      `(def-java-methods ,full-class-name)
	      `(def-java-fields ,full-class-name)
	      (unless (string= full-class-name "java.lang.Object")
		(let* ((supers (mapcar #'unexported-class-symbol (get-superclass-names full-class-name)))
		       (super-exports 
			(mapcar #'(lambda (class-sym) `(export ',class-sym (symbol-package ',class-sym)))
				supers)))
		  (append (mapcar 
			   (lambda (p) `(ensure-package ,(package-name p)))
			   (remove (symbol-package class-sym)
				   (remove-duplicates (mapcar #'symbol-package supers))))
			  super-exports
			  (list 
			   `(defclass ,(class-symbol full-class-name)
			      ,supers ()))))))))
      `(locally ,@defs))))

(defun jarfile.new (fn)
  (jnew (jconstructor "java.util.jar.JarFile" "java.lang.String") fn))

(defun jarfile.entries (jar)
  (jcall (jmethod "java.util.jar.JarFile" "entries") jar))

(defun zipentry.isdirectory (e)
  (jcall (jmethod "java.util.zip.ZipEntry" "isDirectory") e))

(defun zipentry.getname (e)
  (jcall (jmethod "java.util.zip.ZipEntry" "getName") e))

(defun get-jar-classnames (jar-file-name &rest packages)
  "returns a list of strings, packages should be of the form \"java/lang\"
  for recursive lookup and \"java/util/\" for non-recursive"
  (let* ((jar (jarfile.new jar-file-name))
         (entries (jarfile.entries jar))
         (names ()))
    (doenum (e entries)
      (unless (zipentry.isdirectory e)
        (let ((ename (zipentry.getname e)))
          (flet ((matches (package)
                   (and (eql 0 (search package ename))
                        (or (not (eql #\/ (schar package (1- (length package))))) ;recursive
                            (not (find #\/ ename :start (length package))))))) ;non-subdirectory
            (when (and (eql (search ".class" ename)
                            (- (length ename) 6)) ;classname
                       ;don't grab anonymous inner classes
                       (not (and (find #\$ ename)
                                 (digit-char-p (schar ename (1+ (position #\$ ename))))))
                       (some #'matches packages))
              (push (nsubstitute #\. #\/ (subseq ename 0 (- (length ename) 6)))
                    names))))))
    names))

(defun dump-wrapper-defs-to-file (filename classnames)
  "given a list of classnames (say from get-jar-classnames), writes
calls to def-java-class to a file"
  (with-open-file (s filename :direction :output :if-exists :supersede)
    (dolist (name (sort classnames #'string-lessp))
      (format s "(def-java-class ~S)~%" name))))

;;;;;;;;;;;;;;;;;;;;;;;;; constructors and new ;;;;;;;;;;;;;;;;;;;;;;;;;;
#|

Every non-interface class with a public ctor will get;
  a constructor, classname.new
  a method defined on make-new, ultimately calling classname.new,
   specialized on (the value of) it's class-symbol (e.g. canonic sym)

Note that if the ctor is overloaded, there is just one function (taking a rest arg), 
which handles overload resolution

The new macro expands into a call to make-new
|#

(defgeneric make-new (class-sym &rest args)
  (:documentation "Allows for definition of before/after methods on ctors.
The new macro expands into call to this"))

(defun build-ctor-doc-string (name ctors)
  (with-output-to-string (s)
    (dolist (c ctors)
      (format s "~A(~{~#[~;~A~:;~A,~]~})~%"
              name
              (mapcar #'class-name-for-doc (jarray-to-list (jconstructor-params c)))))))

(defmacro def-java-constructors (full-class-name)
"creates and exports a ctor func classname.new, defines a method of 
make-new specialized on the class-symbol"
  (let ((ctor-list (get-ctor-list full-class-name)))
    (when ctor-list
      (let ((ctor-sym (unexported-constructor-symbol full-class-name))
            (class-sym (class-symbol full-class-name)))
        `(locally
           (defun ,ctor-sym (&rest args)
             ,(build-ctor-doc-string full-class-name ctor-list)
             (apply #'install-constructors-and-call ,full-class-name args))
           (export ',ctor-sym (symbol-package ',ctor-sym))
           (defmethod make-new ((class-sym (eql ,class-sym)) &rest args)
             (apply (function ,ctor-sym) args)))))))

(defun get-ctor-list (full-class-name)
  (let* ((class-sym (canonic-class-symbol full-class-name))
         (class (get-java-class-ref class-sym))
         (ctor-array (jclass-constructors class))
         (ctor-list (jarray-to-list ctor-array)))
    ctor-list))

(defun install-constructors-and-call (full-class-name &rest args)
  "initially the constructor symbol for a class is bound to this function,
when first called it will replace itself with the appropriate direct thunk,
then call the requested ctor - subsequent calls will be direct"
  (install-constructors full-class-name)
  (apply (constructor-symbol full-class-name) args))

(defun install-constructors (full-class-name)
  (let* ((ctor-list (get-ctor-list full-class-name)))
    (when ctor-list
      (setf (fdefinition (constructor-symbol full-class-name))
            (make-ctor-thunk ctor-list (class-symbol full-class-name))))))

(defun make-ctor-thunk (ctors class-sym)
  (if (rest ctors) ;overloaded
      (make-overloaded-ctor-thunk ctors class-sym)
    (make-non-overloaded-ctor-thunk (first ctors) class-sym)))

(defun make-non-overloaded-ctor-thunk (ctor class-sym)
  (let ((arg-boxers (get-arg-boxers (jconstructor-params ctor))))
    (lambda (&rest args)
      (let ((arglist (build-arglist args arg-boxers)))
	(ensure-java-hierarchy class-sym)
	(make-instance class-sym
		       :ref (apply #'jnew ctor arglist)
		       :lisp-allocated t)))))

(defun make-overloaded-ctor-thunk (ctors class-sym)
  (let ((thunks (make-ctor-thunks-by-args-length ctors class-sym)))
    (lambda (&rest args)
      (let ((fn (cdr (assoc (length args) thunks))))
        (if fn
            (apply fn
                   args)
          (error "invalid arity"))))))

(defun make-ctor-thunks-by-args-length (ctors class-sym)
  "returns an alist of thunks keyed by number of args"
  (let ((ctors-by-args-length (make-hash-table))
        (thunks-by-args-length nil))
    (dolist (ctor ctors)
      (let ((params-len (length (jconstructor-params ctor))))
        (push ctor (gethash params-len ctors-by-args-length))))
    (maphash #'(lambda (args-len ctors)
                 (push (cons args-len
                             (if (rest ctors);truly overloaded
                                 (make-type-overloaded-ctor-thunk ctors class-sym)
                               ;only one ctor with this number of args
                               (make-non-overloaded-ctor-thunk (first ctors) class-sym)))
                       thunks-by-args-length))
             ctors-by-args-length)
    thunks-by-args-length))

(defun make-type-overloaded-ctor-thunk (ctors class-sym)
  "these methods have the same number of args and must be distinguished by type"
  (let ((thunks (mapcar #'(lambda (ctor)
                            (list (make-non-overloaded-ctor-thunk ctor class-sym)
                                  (jarray-to-list (jconstructor-params ctor))))
                        ctors)))
    (lambda (&rest args)
      (block fn
        (let ((arg-types (get-types-of-args args)))
          (dolist (thunk-info thunks)
            (destructuring-bind (thunk param-types) thunk-info
              (when (is-congruent-type-list param-types arg-types)
                (return-from fn (apply thunk args)))))
          (error "No matching constructor"))))))

(defmacro new (class-spec &rest args)
"new class-spec args
class-spec -> class-name | (class-name this-name)
class-name -> \"package.qualified.ClassName\" | classname.
args -> [actual-arg]* [init-arg-spec]*
init-arg-spec -> init-arg | (init-arg)
init-arg -> :settable-field-or-method [params]* value ;note keyword
            | 
            .method-name [args]*                      ;note dot

Creates a new instance of class-name, using make-new generic function,
then initializes it by setting fields or accessors and/or calling member functions
If this-name is supplied it will be bound to the newly-allocated object and available
to the init-args"
  (labels ((mem-sym? (x)
             (or (keywordp x)
                 (and (symbolp x) (eql 0 (position #\. (symbol-name x))))))
           (mem-form? (x)
             (and (listp x) (mem-sym? (first x))))
           (mem-init? (x)
             (or (mem-sym? x) (mem-form? x)))
           (init-forms (x)
             (if x
                 (if (mem-form? (first x))
                     (cons (first x) (init-forms (rest x)))
                   (let ((more (member-if #'mem-init? (rest x))))
                     (cons (ldiff x more) (init-forms more)))))))
    (let* ((inits (member-if #'mem-init? args))
           (real-args (ldiff args inits))
           (class-atom (if (atom class-spec)
                           class-spec
                         (first class-spec)))
           (class-sym (if (symbolp class-atom)
                          ;(find-symbol (string-append (symbol-name class-atom) "."))
                          class-atom
                        (multiple-value-bind (package class) (split-package-and-class class-atom)
                          (find-symbol (string-append (string-upcase class) ".") package))))
           (class-name (subseq (symbol-name class-sym) 0 (1- (length (symbol-name class-sym)))))
           (gthis (gensym)))
      (flet ((expand-init (x)
               (if (keywordp (first x)) ;setf field or property
                   `(setf (,(find-symbol (string-append class-name "." (symbol-name (first x))))
                           ,gthis ,@(butlast (rest x)))
                          ,@(last (rest x)))
                 ;.memfunc
                 `(,(find-symbol (string-append class-name (symbol-name (first x))))
                   ,gthis
                   ,@(rest x)))))
        `(let* ((,gthis (make-new ,class-sym ,@real-args))
                ,@(when (listp class-spec)
                    `((,(second class-spec) ,gthis))))
           ,@(mapcar #'expand-init (init-forms inits))
           ,gthis)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Fields ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
all public fields will get a getter function classname.fieldname and a setter - (setf classname.fieldname)
instance fields take an first arg which is the instance
static fields also get a symbol-macro *classname.fieldname*
|#

(defmacro def-java-fields (full-class-name)
"fields will get a getter function classname.fieldname and a setter - (setf classname.fieldname)
instance fields take an first arg which is the instance
static fields also get a symbol-macro *classname.fieldname*"
  (let* ((class-sym (canonic-class-symbol full-class-name))
         (class (get-java-class-ref class-sym))
         (fields (jarray-to-list (jclass-fields class)))
         (defs nil))
    (dolist (field fields)
      (let* ((field-name (jfield-name field))
             (field-sym (unexported-member-symbol full-class-name field-name))
             (is-static (jmember-static-p field)))
        (if is-static
            (let ((macsym (intern-and-unexport (string-append "*" (symbol-name field-sym) "*")
                                  (symbol-package field-sym))))
              (push `(defun ,field-sym ()
                       (install-static-field-and-get ,full-class-name ,field-name))
                    defs)
              (push `(defun (setf ,field-sym) (val)
                       (install-static-field-and-set ,full-class-name ,field-name val))
                    defs)
              (push `(export ',field-sym (symbol-package ',field-sym)) defs)
              (push `(define-symbol-macro ,macsym (,field-sym)) defs)
              (push `(export ',macsym (symbol-package ',macsym)) defs))
          (progn
            (push `(defun ,field-sym (obj)
                     (install-field-and-get ,full-class-name ,field-name obj))
                  defs)
            (push `(defun (setf ,field-sym) (val obj)
                     (install-field-and-set ,full-class-name ,field-name val obj))
                  defs)
            (push `(export ',field-sym (symbol-package ',field-sym)) defs)))))
    `(locally ,@(nreverse defs))))

(defun install-field-and-get (full-class-name field-name obj)
  (install-field full-class-name field-name)
  (funcall (member-symbol full-class-name field-name) obj))

(defun install-field-and-set (full-class-name field-name val obj)
  (install-field full-class-name field-name)
  (funcall (fdefinition `(setf ,(member-symbol full-class-name field-name))) val obj))

(defun install-static-field-and-get (full-class-name field-name)
  (install-field full-class-name field-name)
  (funcall (member-symbol full-class-name field-name)))

(defun install-static-field-and-set (full-class-name field-name val)
  (install-field full-class-name field-name)
  (funcall (fdefinition `(setf ,(member-symbol full-class-name field-name))) val))


(defun install-field (full-class-name field-name)
  (let* ((class-sym (canonic-class-symbol full-class-name))
         (class (get-java-class-ref class-sym))
         (field (jclass-field class field-name))
         (field-sym (member-symbol full-class-name field-name))
         (is-static (jmember-static-p field))
         (field-type-name (jclass-name (jfield-type field)))
         (boxer (get-boxer-fn field-type-name))
         (unboxer (get-unboxer-fn field-type-name)))
    (if is-static
        (progn
          (setf (fdefinition field-sym)
                (lambda ()
                  (funcall unboxer (jfield-raw class field-name) #+nil (field.get field nil))))
          (setf (fdefinition `(setf ,field-sym))
                (lambda (arg)
                  (jfield field-name nil
                             (get-ref (if (and boxer (not (boxed? arg)))
                                          (funcall boxer arg)
                                        arg)))
                  arg)))
      (progn 
        (setf (fdefinition field-sym)
              (lambda (obj)
                (funcall unboxer (jfield-raw class field-name (get-ref obj)) #+nil(field.get field (get-ref obj)))))
        (setf (fdefinition `(setf ,field-sym))
              (lambda (arg obj)
                (jfield field-name (get-ref obj)
                           (get-ref (if (and boxer (not (boxed? arg)))
                                        (funcall boxer arg)
                                      arg)))
                arg))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; methods ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#|
defines wrappers for all public methods of the class
As with ctors, if a method is overloaded a single wrapper is created that handles
overload resolution.
The wrappers have the name classname.methodname
If a method follows the JavaBeans property protocol (i.e. it is called getSomething or isSomething
and there is a corresponding setSomething, then a (setf classname.methodname) will be defined
that calls the latter
|#

(defun class-name-for-doc (class)
  (let ((name (jclass-name class)))
    (if (jclass-array-p class)
        (decode-array-name name)
      name)))

(defun build-method-doc-string (name methods)
  (with-output-to-string (s)
    (dolist (m methods)
      (format s "~A~A ~A(~{~#[~;~A~:;~A,~]~})~%"
              (if (jmember-static-p  m)
                  "static "
                "")
              (jclass-name (jmethod-return-type m))
              name
              (mapcar #'class-name-for-doc (jarray-to-list (jmethod-params m)))))))

(defmacro def-java-methods (full-class-name)
  (let ((methods-by-name (get-methods-by-name full-class-name))
        (defs nil))
    (maphash (lambda (name methods)
               (let ((method-sym (unexported-member-symbol full-class-name name)))
                 (push `(defun ,method-sym (&rest args)
                          ,(build-method-doc-string name methods)
                          (apply #'install-methods-and-call ,full-class-name ,name args))
                       defs)
                 (push `(export ',method-sym (symbol-package ',method-sym))
                       defs)
                 ;build setters when finding beans property protocol
                 (flet ((add-setter-if (prefix)
                          (when (eql 0 (search prefix name))
                            (let ((setname (string-append "set" (subseq name (length prefix)))))
                              (when (gethash setname methods-by-name)
                                (push `(defun (setf ,method-sym) (val &rest args)
                                         (progn
                                           (apply #',(member-symbol full-class-name setname)
                                                  (append args (list val)))
                                           val))
                                      defs))))))
                   (add-setter-if "get")
                   (add-setter-if "is"))))
             methods-by-name)
    `(locally ,@(nreverse defs))))

(defun install-methods-and-call (full-class-name method &rest args)
  "initially all the member function symbols for a class are bound to this function,
when first called it will replace them with the appropriate direct thunks,
then call the requested method - subsequent calls via those symbols will be direct"
  (install-methods full-class-name)
  (apply (member-symbol full-class-name method) args))

(defun decode-array-name (tn)
  (let ((prim (assoc tn
                     '(("Z" . "boolean")
                       ("B" . "byte")
                       ("C" . "char")
                       ("S" . "short")
                       ("I" . "int")
                       ("J" . "long")
                       ("F" . "float")
                       ("D" . "double")
                       ("V" . "void"))
                     :test #'string-equal)))
    (if prim
        (rest prim)
      (let ((array-depth (count #\[ tn)))
        (if (= 0 array-depth)
            (subseq tn 1 (1- (length tn))) ;strip leading L and trailing ;
          (with-output-to-string (s)
            (write-string (decode-array-name (subseq tn array-depth)) s)
            (dotimes (x array-depth)
              (write-string "[]" s))))))))

(defun jarray-to-list (array) 
  (coerce array 'list))


(defun jmethod-made-accessible (method)
  "Return a method made accessible"
    (jcall (jmethod "java.lang.reflect.AccessibleObject" "setAccessible" "boolean") 
	   method
           java:+true+)
    method)

(defun jclass-relevant-methods (class)
  "Return all public methods, and all protected declared methods"
  (append (jarray-to-list (jclass-methods class))
	 (map 'list #'jmethod-made-accessible 
	      (remove-if-not #'jmember-protected-p (jclass-methods class :declared t)))))

(defun get-methods-by-name (full-class-name)
  "returns an #'equal hashtable of lists of java.lang.Method refs keyed by name"
  (let* ((class-sym (canonic-class-symbol full-class-name))
         (class (get-java-class-ref class-sym))
         (methods (jclass-relevant-methods class))
         (methods-by-name (make-hash-table :test #'equal)))
    (loop for method in methods
	  do
	  (push method (gethash (jmethod-name method) methods-by-name)))
    methods-by-name))

(defun install-methods (full-class-name)
  (let ((methods-by-name (get-methods-by-name full-class-name)))
    (maphash
     (lambda (name methods)
       (setf (fdefinition (member-symbol full-class-name name))
             (make-method-thunk methods)))
     methods-by-name)))

(defun make-method-thunk (methods)
  (if (rest methods) ;overloaded
      (make-overloaded-thunk methods)
    (make-non-overloaded-thunk (first methods))))

(defun make-non-overloaded-thunk (method)
  (let* ((unboxer-fn (get-unboxer-fn (jclass-name (jmethod-return-type method))))
        (arg-boxers (get-arg-boxers (jmethod-params method)))
        (is-static (jmember-static-p method))
	(caller (if is-static #'jstatic-raw #'jcall-raw)))
    (lambda (&rest args)
      (let ((arglist (build-arglist (if is-static args (rest args)) arg-boxers)))
	(funcall unboxer-fn
		 (apply caller  method
			(if is-static nil (get-ref (first args)))
			arglist))))))

(defun make-overloaded-thunk (methods)
  (let ((thunks (make-thunks-by-args-length methods)))
    (lambda (&rest args)
      (let ((fn (cdr (assoc (length args) thunks))))
        (if fn
            (apply fn
                   args)
          (error "invalid arity"))))))

(defun make-thunks-by-args-length (methods)
  "returns an alist of thunks keyed by number of args"
  (let ((methods-by-args-length (make-hash-table))
        (thunks-by-args-length nil))
    (dolist (method methods)
      (let ((is-static (jmember-static-p method))
            (params-len (length (jmethod-params method))))
        (push method (gethash (if is-static params-len (1+ params-len))
                              methods-by-args-length))))
    (maphash #'(lambda (args-len methods)
                 (push (cons args-len
                             (if (rest methods);truly overloaded
                                 (make-type-overloaded-thunk methods)
                               ;only one method with this number of args
                               (make-non-overloaded-thunk (first methods))))
                       thunks-by-args-length))
             methods-by-args-length)
    thunks-by-args-length))

(defun make-type-overloaded-thunk (methods)
  "these methods have the same number of args and must be distinguished by type"
  (let ((thunks (mapcar #'(lambda (method)
                            (list (make-non-overloaded-thunk method)
                                  (jmember-static-p method)
                                  (jarray-to-list (jmethod-params method))))
                        methods)))
    (lambda (&rest args)
      (block fn
        (let ((arg-types (get-types-of-args args)))
          (dolist (thunk-info thunks)
            (destructuring-bind (thunk is-static param-types) thunk-info
              (when (is-congruent-type-list param-types (if is-static arg-types (rest arg-types)))
                (return-from fn (apply thunk args)))))
          (error "No matching method"))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;; array support ;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun jref (array &rest subscripts)
  (apply #'jarray-ref-raw array subscripts))


(defun (setf jref) (val array &rest subscripts)
  (apply #'jarray-set array val subscripts))



(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro def-refs (&rest types)
    `(locally
       ,@(mapcan
          (lambda (type)
            (let ((ref-sym (intern (string-upcase (string-append "jref-" (symbol-name type))))))
              (list 
               `(defun ,ref-sym (array &rest subscripts)
                  ,(format nil "like aref, for Java arrays of ~A, settable" (symbol-name type))
		 (assert (every #'integerp subscripts))
		 (apply #'jarray-ref array subscripts))

               `(defun (setf ,ref-sym) (val array &rest subscripts)
		 (assert (every #'integerp subscripts))
		 (apply #'jarray-set array ,(if (eql type 'boolean) '(box-boolean val) 'val) subscripts)
                  ))))
          types))))

;arrays of primitives have their own accessors
(def-refs boolean byte char double float int short long)

(defun jlength (array)
  "like length, for Java arrays"
  (jarray-length array)) ;(get-ref array)?

(defgeneric make-new-array (type &rest dimensions)
  (:documentation "generic function, with methods for all Java class designators")
  (:method (type &rest dims)
   (assert (every #'integerp dims))
   (apply #'jnew-array type dims)))

(defmethod make-new-array ((type symbol) &rest dimensions)
  (apply #'make-new-array (get-java-class-ref type) dimensions))

(defmethod make-new-array ((type string) &rest dimensions)
  (apply #'make-new-array (find-java-class type) dimensions))

(defmethod make-new-array ((type (eql :char)) &rest dimensions)
  (apply #'make-new-array character.type dimensions))

(defmethod make-new-array ((type (eql :int)) &rest dimensions)
  (apply #'make-new-array integer.type dimensions))

(defmethod make-new-array ((type (eql :boolean)) &rest dimensions)
  (apply #'make-new-array boolean.type dimensions))

(defmethod make-new-array ((type (eql :double)) &rest dimensions)
  (apply #'make-new-array double.type dimensions))

(defmethod make-new-array ((type (eql :byte)) &rest dimensions)
  (apply #'make-new-array byte.type dimensions))

(defmethod make-new-array ((type (eql :float)) &rest dimensions)
  (apply #'make-new-array float.type dimensions))

(defmethod make-new-array ((type (eql :short)) &rest dimensions)
  (apply #'make-new-array short.type dimensions))

(defmethod make-new-array ((type (eql :long)) &rest dimensions)
  (apply #'make-new-array long.type dimensions))

;;;;;;;;;;;;;;;;;;;;;;;;;; arg/param helpers ;;;;;;;;;;;;;;;;;;;;;;


(defun get-arg-boxers (param-types)
  "returns a list with one entry per param, either nil or a function that boxes the arg"
  (loop for param-type across param-types
	collecting (get-boxer-fn (jclass-name param-type))))



(defun build-arglist (args arg-boxers)
  (when args
    (loop for arg in args 
	  for boxer in arg-boxers
	  collecting
	  (get-ref (if (and boxer (not (boxed? arg)))
		       (funcall boxer arg)
		       arg)))))


(defun get-types-of-args (args)
  (let (ret)
    (dolist (arg args)
      (push (infer-box-type arg)
            ret))
    (nreverse ret)))

(defun is-congruent-type-list (param-types arg-types)
  (every #'(lambda (arg-type param-type)
             (if arg-type
                 (is-assignable-from arg-type param-type)
               ;nil was passed - must be boolean or non-primitive target type
               (or (not (is-primitive-class param-type))
                   (jclass-superclass-p boolean.type  param-type))))
         arg-types param-types))


;;;;;;;;;;;;;;;;;;;;;;;; argument conversion and boxing ;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun box-string (s)
  "Given a string or symbol, returns reference to a Java string"
  (convert-to-java-string s))

(defun unbox-string (ref &optional delete-local)
  "Given a reference to a Java string, returns a Lisp string" 
  (declare (ignore delete-local))
  (convert-from-java-string (get-ref ref)))



(defun get-boxer-fn (class-name)
  (if (string= class-name "boolean")
      #'box-boolean
      nil))

(defun get-boxer-fn-sym (class-name)
  (if (string= class-name "boolean")
      'box-boolean
      'identity))
  
(defun boxed? (x)
  (or (java-ref-p x)
      (typep x '|java.lang|::object.)))

(defun infer-box-type (x)
  (cond
   ((null x) nil)
   ((boxed? x) (jobject-class (get-ref x)))
   ((typep x '(integer -2147483648 +2147483647)) integer.type)
   ((typep x '(integer -9223372036854775808 +9223372036854775807)) long.type)
   ((numberp x) double.type)
   ; ((characterp x) character.type) ;;;FIXME!!
   ((eq x t) boolean.type)
   ((or (stringp x) (symbolp x))
    (get-java-class-ref '|java.lang|::|String|))
   (t (error "can't infer box type"))))


(defun get-unboxer-fn (class-name)
  (if (string= class-name "void")
      #'unbox-void
      (if (or (is-name-of-primitive class-name) (string= class-name "java.lang.String"))
	  #'jobject-lisp-value
	  #'identity-or-nil)))

(defun get-unboxer-fn-sym (class-name)
  (if (string= class-name "void")
      'unbox-void
      (if (or (is-name-of-primitive class-name) (string= class-name "java.lang.String"))
	  'jobject-lisp-value
	  'identity-or-nil)))


(defun unbox-void (x &optional delete-local)
  (declare (ignore x delete-local))
  nil)

(defun box-void (x)
  (declare (ignore x))
  nil)

(defun box-boolean (x)
  (if x java:+true+ java:+false+))

;;;;;;;;;;;;;;;;;;;;;;;; proxy support ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun enable-java-proxies ()
  t)

(defun find-java-class-in-macro (name)
  (find-java-class
   (if (symbolp name)
       (symbol-value name)
     name)))

(defmacro new-proxy (&rest interface-defs)
  "interface-def -> (interface-name method-defs+)
interface-name -> \"package.qualified.ClassName\" | classname. (must name a Java interface type)
method-def -> (method-name arg-defs* body)
arg-def -> arg-name | (arg-name arg-type)
arg-type -> \"package.qualified.ClassName\" | classname. | :primitive
method-name -> symbol | string (matched case-insensitively)

Creates, registers and returns a Java object that implements the supplied interfaces"
  (let (defined-method-names)
    (labels ((process-idefs (idefs)
	       (when (rest idefs)
		 (error "Sorry, only one interface def at a time"))
	       (process-idef (first idefs)))
	     (process-idef (idef)
	       (destructuring-bind (interface-name &rest method-defs) idef
		 (let* ((methods (jclass-methods (find-java-class-in-macro interface-name)))
			(ret `((find-java-class ,interface-name)
			       ,@(loop for method-def in method-defs appending (process-method-def method-def methods)))))
		   ;;check to make sure every function is defined
		   (loop for method across methods
			 for mname = (jmethod-name method)
			 unless (member mname defined-method-names :test #'string-equal)
			 do 
			 (warn (format nil "proxy doesn't define:~%~A" mname)))
		   ret)))
	     (process-method-def (method-def methods)
	       (destructuring-bind (method-name (&rest arg-defs) &body body) method-def
		 (push method-name defined-method-names)
		 (let ((method (matching-method method-name arg-defs methods))
		       (gargs (gensym)))
		   `(,(jmethod-name method)
		     (lambda (&rest ,gargs)
		       (,(get-boxer-fn-sym (jclass-name (jmethod-return-type method)))
			 (let ,(arg-lets arg-defs
					 (jarray-to-list (jmethod-params method))
					 gargs
					 0)
			   ,@body)))))))
	     (arg-lets (arg-defs params gargs idx)
	       (when arg-defs
		 (let ((arg (first arg-defs))
		       (param (first params)))
		   (cons `(,(if (atom arg) arg (first arg))
			   (,(get-unboxer-fn-sym (jclass-name param))
			    (nth ,idx ,gargs)))
			 (arg-lets (rest arg-defs) (rest params) gargs (1+ idx))))))
	     (matching-method (method-name arg-defs methods)
	       (let (match)
		 (loop for method across methods
		       when (method-matches method-name arg-defs method)
		       do
		       (if match
			   (error (format nil "more than one method matches ~A" method-name))
			   (setf match method)))
		 (or match (error (format nil "no method matches ~A" method-name)))))
	     (method-matches (method-name arg-defs method)
	       (when (string-equal method-name (jmethod-name method))
		 (let ((params (jmethod-params method)))
		   (when (= (length arg-defs) (length params))
		     (is-congruent arg-defs params)))))
	     (is-congruent (arg-defs params)
	       (every (lambda (arg param)
			(or (atom arg)	;no type spec matches anything
			    (jeq (find-java-class-in-macro (second arg)) param)))
		      arg-defs (jarray-to-list params))))
      `(java::%jnew-proxy ,@(process-idefs interface-defs)))))


#+nil
(defun jrc (class-name super-name interfaces constructors methods fields &optional filename)
  "A friendlier version of jnew-runtime-class."
  #+nil (format t "~s~%~s~%~s~%~s~%~s~%~s~%" class-name super-name interfaces constructors methods fields filename)
  (if (java:jruntime-class-exists-p class-name)
      (progn 
	(warn "Java class ~a already exists. Redefining methods." class-name)
	(loop for 
	      (argument-types function super-invocation-args) in constructors
	      do 
	      (java:jredefine-method class-name nil argument-types function))
	(loop for 
	      (method-name return-type argument-types function &rest modifiers)
	      in methods
	      do 
	      (java:jredefine-method class-name method-name argument-types function)))
      (java:jnew-runtime-class class-name super-name interfaces constructors methods fields filename)))


(defun get-modifiers (member)
  (jcall (jmethod "java.lang.reflect.Member" "getModifiers") member))

(defun get-modifier-list (member)
  (let ((mods (get-modifiers member)))
    (loop for (mod . mod-call) in 
	  '(("public" . "isPublic")
	    ("protected" . "isProtected")
	    ("private" . "isPrivate")
	    ("static"  . "isStatic")
	    ;("abstract" . "isAbstract")
	    ("final" . "isFinal")
	    ("transient" . "isTransient")
	    ("volatile" . "isVolatile")
	    ("synchronized" . "isSynchronized"))
	  when 
	  (jstatic (jmethod "java.lang.reflect.Modifier" mod-call "int")
           "java.lang.reflect.Modifier"
           mods)
	  collect mod)))


(defun get-java-object (x)
  (typecase x
    (|java.lang|::object. (ref x))
    (t x)))

(defun find-java-class-name-in-macro (c)
  (etypecase c
    (symbol (jclass-name (find-java-class (symbol-value c))))
    (string c)))

#+nil
(defmacro new-class (class-name super-and-interface-names constructor-defs method-defs field-defs)
  "class-name -> string
   super-and-interface-names -> class-name | (class-name interface-name*)
   constructor-defs -> (constructor-def*)
   constructor-def -> (ctr-arg-defs body) 
   /the first form in body may be (super arg-name+); this will call the constructor of the superclass
    with the listed arguments/
   ctr-arg-def -> (arg-name arg-type)
   method-def -> (method-name return-type access-modifiers arg-defs* body)
   /access-modifiers may be nil (to get the modifiers from the superclass), a keyword, or
   a list of keywords/
   method-name -> string 
arg-def -> arg-name | (arg-name arg-type)
arg-type -> \"package.qualified.ClassName\" | classname. | :primitive
class-name -> \"package.qualified.ClassName\" | classname. 
interface-name -> \"package.qualified.InterfaceName\" | interfacename. 

Creates, registers and returns a Java object that implements the supplied interfaces"
  (let ((this (intern "THIS" *package*))
        (defined-method-names))
    (labels ((process-ctr-def (ctr-def ctrs)
	       (destructuring-bind ((&rest arg-defs) &body body)
                   ctr-def
                 (let ((ctr-param-names 
			(mapcar 
			 #'(lambda (arg-def) (find-java-class-name-in-macro (cadr arg-def)))
			 arg-defs))
		       ;(ctr-param-names (mapcar #'cadr arg-defs))
		       (gargs (gensym))
		       (head (car body))
		       (sia))
		   (when (and (consp head) (eq (car head) 'super))
		     (setq sia (mapcar 
				#'(lambda (arg-name) 
				   (1+ (position arg-name arg-defs :key #'car)))
				(cdr head))
			   body (cdr body)))
                   `(,ctr-param-names
                     (lambda (&rest ,gargs)
			 (let ,(arg-lets (append arg-defs (list this))
					 (append 
					  ctr-param-names
					  (list class-name))
					 gargs
					 0)
			   ,@body))
                     ,sia))))
	     (process-method-def (method-def methods)
               (destructuring-bind (method-name return-type modifiers (&rest arg-defs) &body body)
                   method-def
                 (push method-name defined-method-names)
                 (let* ((method (matching-method method-name arg-defs methods))
                        (method-params 
                         (if method
                             (jarray-to-list (jmethod-params method))
                             (mapcar #'(lambda (arg-def) (find-java-class-in-macro (cadr arg-def))) arg-defs)))
                        (method-param-names 
                         #+nil
			  (if method
                             (mapcar #'jclass-name (jarray-to-list method-params))
                             (mapcar #'cadr arg-defs))
			  (mapcar #'jclass-name method-params))
                        (return-type-name
                         (jclass-name 
                          (if method (jmethod-return-type method) (find-java-class-in-macro return-type))))
                        (modifiers 
			 #+nil
			  (if method (get-modifier-list method) '("public"))
			  (cond ((and (null modifiers) method) (get-modifier-list method))
				((symbolp modifiers) (list (string-downcase (symbol-name modifiers))))
				((consp modifiers) (mapcar #'(lambda (m) (string-downcase (symbol-name m))) modifiers))
				(t (error (format t "Need to provide modifiers for method ~A" method-name)))))
                        (gargs (gensym))) 
                   `(,method-name ,return-type-name ,method-param-names
                     (lambda (&rest ,gargs)
		       ;;(,(get-boxer-fn-sym return-type-name)
		       (get-java-object  ;;check!
			 (let ,(arg-lets (append arg-defs (list this))
					 (append 
					  method-param-names 
					  #+nil (map 'list #'(lambda (p) (jclass-name p)) method-params)
					  (list class-name))
					 gargs
					 0)
			   ,@body))
		       )
                     ,@modifiers))))
             (arg-lets (arg-defs params gargs idx)
               (when arg-defs
                 (let ((arg (first arg-defs))
                       (param (first params)))
                   (cons `(,(if (atom arg) arg (first arg))
                           (,(get-unboxer-fn-sym param)
                            (nth ,idx ,gargs)))
                         (arg-lets (rest arg-defs) (rest params) gargs (1+ idx))))))
             (matching-method (method-name arg-defs methods)
               (let (match)
                 (loop for method across methods
                       when (method-matches method-name arg-defs method)
                       do
                       (if match
                           (error (format nil "more than one method matches ~A" method-name))
                           (setf match method)))
                 match))
             (method-matches (method-name arg-defs method)
               (when (string-equal method-name (jmethod-name method))
                 (let ((params (jmethod-params method)))
                   (when (= (length arg-defs) (length params))
                     (is-congruent arg-defs params)))))
             (is-congruent (arg-defs params)
               (every (lambda (arg param)
                        (or (atom arg)  ;no type spec matches anything
                            (jeq (find-java-class-in-macro (second arg)) param)))
                      arg-defs (jarray-to-list params))))
      (unless (consp super-and-interface-names)
	(setq super-and-interface-names (list super-and-interface-names)))
      (let* ((super-name (find-java-class-name-in-macro (car super-and-interface-names)))
	     (interfaces (mapcar #'find-java-class-name-in-macro (cdr super-and-interface-names)))
	     (super (jclass super-name))
             (super-ctrs (jclass-constructors super))
             (ctrs-ret (loop for ctr-def in constructor-defs collecting
                        (process-ctr-def ctr-def super-ctrs)))
	     (super-methods (jclass-methods super))
             (iface-methods 
              (apply #'concatenate 'vector 
                     (mapcar #'(lambda (ifn)
                                 (jclass-methods (jclass ifn)))
                             interfaces)))
             (methods-ret (loop for method-def in method-defs collecting
                        (process-method-def
                         method-def 
                         (concatenate 'vector super-methods iface-methods)))))
        ;;check to make sure every function is defined
        (loop for method across iface-methods
              for mname = (jmethod-name method)
              unless (member mname defined-method-names :test #'string-equal)
              do 
              (warn (format nil "class doesn't define:~%~A" mname)))
	`(progn 
	  (jrc ,class-name ,super-name ,interfaces 
	   ',ctrs-ret
	   ',methods-ret
	   (loop for (fn type . mods) in ',field-defs
	    collecting `(,fn ,(find-java-class-name-in-macro type)
			 ,@(mapcar #'(lambda (mod) (string-downcase (symbol-name mod))) mods)))
	  #+nil ,(namestring (merge-pathnames class-name "/tmp/")))
	  (eval '(def-java-class ,class-name)))))))


