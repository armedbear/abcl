(require "JVM")

;;The package is set to :jvm for convenience, since most of the symbols used
;;here come from that package. However, the functions we're definining belong
;;to the :java package.
(in-package :jvm)

(defconstant +abcl-java-object+ (make-jvm-class-name "org.armedbear.lisp.JavaObject"))

(defun java::make-memory-class-loader (&optional (parent (java:get-current-classloader)))
  (java:jnew "org.armedbear.lisp.MemoryClassLoader" parent))

(defun java:jnew-runtime-class
    (class-name &rest args &key (superclass "java.lang.Object")
     interfaces constructors methods fields (access-flags '(:public)) annotations
     (class-loader (java::make-memory-class-loader)))
  "Creates and loads a Java class with methods calling Lisp closures
   as given in METHODS.  CLASS-NAME and SUPER-NAME are strings,
   INTERFACES is a list of strings, CONSTRUCTORS, METHODS and FIELDS are
   lists of constructor, method and field definitions.

   Constructor definitions - currently NOT supported - are lists of the form
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

     (METHOD-NAME RETURN-TYPE ARGUMENT-TYPES FUNCTION &key MODIFIERS ANNOTATIONS)

   where
      METHOD-NAME is a string
      RETURN-TYPE denotes the type of the object returned by the method
      ARGUMENT-TYPES is a list of parameters to the method

        The types are either strings naming fully qualified java classes or Lisp keywords referring to
        primitive types (:void, :int, etc.).

     FUNCTION is a Lisp function of minimum arity (1+ (length
     argument-types)). The instance (`this') is passed as the first
     argument.

   Field definitions are lists of the form (field-name type &key modifiers annotations)."
  (declare (ignorable superclass interfaces constructors methods fields access-flags annotations))
  (let ((stream (sys::%make-byte-array-output-stream)))
    (multiple-value-bind (class-file method-implementation-fields)
        (apply #'java::%jnew-runtime-class class-name stream :allow-other-keys T args)
      (sys::put-memory-function class-loader
                                class-name (sys::%get-output-stream-bytes stream))
      (let ((jclass (java:jcall "loadClass" class-loader class-name)))
        (dolist (method method-implementation-fields)
          (setf (java:jfield jclass (car method)) (cdr method)))
        jclass))))

(defconstant +abcl-lisp-integer-object+ (make-jvm-class-name "org.armedbear.lisp.LispInteger"))
(defconstant +abcl-lisp-object-object+ (make-jvm-class-name "org.armedbear.lisp.LispObject"))
(defconstant +abcl-single-float-object+ (make-jvm-class-name "org.armedbear.lisp.SingleFloat"))
(defconstant +abcl-double-float-object+ (make-jvm-class-name "org.armedbear.lisp.DoubleFloat"))
(defconstant +abcl-lisp-character-object+ (make-jvm-class-name "org.armedbear.lisp.LispCharacter"))

(defun box-arguments (argument-types offset all-argc)
  ;;Box each argument
  (loop
    :for arg-type :in argument-types
    :for i :from offset
    :do (progn
          (cond
            ((member arg-type '(:int :short :byte))
             (iload i)
             (emit-invokestatic +abcl-lisp-integer-object+ "getInstance"
                                (list :int) +abcl-lisp-integer-object+))
            ((eq arg-type :long)
             (lload i)
             (emit-invokestatic +abcl-lisp-integer-object+ "getInstance"
                                (list :long) +abcl-lisp-integer-object+))
            ((eq arg-type :float)
             (fload i)
             (emit-invokestatic +abcl-single-float-object+ "getInstance"
                                (list :float) +abcl-single-float-object+))
            ((eq arg-type :double)
             (dload i)
             (emit-invokestatic +abcl-double-float-object+ "getInstance"
                                (list :double) +abcl-double-float-object+))
            ((eq arg-type :boolean)
             (iload i)
             (emit-invokestatic +abcl-lisp-object-object+ "getInstance"
                                (list :boolean) +abcl-lisp-object-object+))
            ((eq arg-type :char)
             (iload i)
             (emit-invokestatic +abcl-lisp-character-object+ "getInstance"
                                (list :char) +abcl-lisp-character-object+))
            ((keywordp arg-type)
             (error "Unsupported arg-type: ~A" arg-type))
            (t (aload i)
               (emit 'iconst_1) ;;true
               (emit-invokestatic +abcl-java-object+ "getInstance"
                                  (list +java-object+ :boolean) +lisp-object+)))
          (astore (+ i all-argc)))))

(defun java::%jnew-runtime-class
    (class-name stream &key (superclass "java.lang.Object")
     interfaces constructors methods fields (access-flags '(:public)) annotations)
  "Actual implementation of jnew-runtime-class. Writes the class bytes to a stream. Returns two values: the finalized class-file structure and the alist of method implementation fields."
  (let* ((jvm-class-name (make-jvm-class-name class-name))
         (class-file (make-class-file jvm-class-name (make-jvm-class-name superclass) access-flags))
         method-implementation-fields)
    (setf (class-file-interfaces class-file)
          (mapcar #'make-jvm-class-name interfaces))
    (when annotations
      (class-add-attribute class-file (make-runtime-visible-annotations-attribute
                                       :list (mapcar #'parse-annotation annotations))))
    (setf method-implementation-fields (java::runtime-class-add-methods class-file methods))
    (java::runtime-class-add-fields class-file fields)
    (if (null constructors)
      (let ((ctor (make-jvm-method :constructor :void nil :flags '(:public))))
        (class-add-method class-file ctor)
        (with-code-to-method (class-file ctor)
          (aload 0)
          (emit-invokespecial-init (class-file-superclass class-file) nil)
          (emit 'return)))
      (dolist (constructor constructors)
        (destructuring-bind (argument-types function
                             &key (modifiers '(:public)))
            constructor
          (let* ((argument-types (mapcar #'java::canonicalize-java-type argument-types))
                 (argc (length argument-types))
                 (ctor (make-jvm-method :constructor :void argument-types :flags modifiers))
                 (field-name (string (gensym "CONSTRUCTOR")))
                 (all-argc (1+ argc)))
            (class-add-method class-file ctor)
            (let ((field (make-field field-name +lisp-object+ :flags '(:public :static))))
              (class-add-field class-file field))
            (push (cons field-name function) method-implementation-fields)
            (with-code-to-method (class-file ctor)
              (dotimes (i (* 2 all-argc))
                (allocate-register nil))

              (aload 0)
              (emit-invokespecial-init (class-file-superclass class-file) nil)

              (aload 0)
              (emit 'iconst_1) ;;true
              (emit-invokestatic +abcl-java-object+ "getInstance"
                                 (list +java-object+ :boolean) +lisp-object+)
              (astore all-argc)

              (box-arguments argument-types 1 all-argc)

              ;;Load the Lisp function from its static field
              (emit-getstatic (class-file-class class-file) field-name +lisp-object+)
              (if (<= all-argc call-registers-limit)
                  (progn
                    ;;Load the boxed this
                    (aload all-argc)
                    ;;Load each boxed argument
                    (dotimes (i argc)
                      (aload (+ i 1 all-argc))))
                  (error "execute(LispObject[]) is currently not supported"))
              (emit-call-execute all-argc)

              (emit 'return))))))
    (finalize-class-file class-file)
    (write-class-file class-file stream)
    (finish-output stream)
    #+test-record-generated-class-file
    (let ((filename (merge-pathnames (format nil "~A.class" class-name))))
      (with-open-file (f filename :direction :output :element-type '(signed-byte 8))
        (write-sequence (java::list-from-jarray (sys::%get-output-stream-bytes stream)) f))
      (format *standard-output* "~&Wrote class file ~A.~%" filename))
    (values class-file method-implementation-fields)))

(defun java::make-accessor-name (prefix name)
  (let ((initial (char-upcase (aref name 0)))
        (rest (subseq name 1)))
    (format nil "~A~A~A" prefix initial rest)))

;;This is missing from compiler-pass2.lisp. Probably this and similar functions should reside
;;in a dedicated file, independent from both runtime-class and compiler-pass2.
(defun emit-invokespecial (class-name method-name arg-types return-type)
  (let* ((stack-effect (apply #'descriptor-stack-effect return-type arg-types))
         (index (pool-add-method-ref *pool* class-name
                                     method-name (cons return-type arg-types)))
         (instruction (apply #'%emit 'invokespecial (u2 index))))
    (declare (type (signed-byte 8) stack-effect))
    (setf (instruction-stack instruction) (1- stack-effect))))

(defun java::canonicalize-java-type (type)
  (cond
    ((stringp type) (make-jvm-class-name type))
    ((keywordp type) type)
    (t (error "Unrecognized Java type: ~A" type))))

(defun java::emit-unbox-and-return (return-type)
  (cond
    ((eq return-type :void)
     (emit 'pop)
     (emit 'return))
    ((member return-type '(:int :short :byte))
     (emit-invokevirtual +lisp-object+ "intValue" nil :int)
     (emit 'ireturn))
    ((eq return-type :long)
     (emit-invokevirtual +lisp-object+ "longValue" nil :long)
     (emit 'lreturn))
    ((eq return-type :float)
     (emit-invokevirtual +lisp-object+ "floatValue" nil :float)
     (emit 'freturn))
    ((eq return-type :double)
     (emit-invokevirtual +lisp-object+ "doubleValue" nil :double)
     (emit 'dreturn))
    ((eq return-type :boolean)
     (emit-invokevirtual +lisp-object+ "getBooleanValue" nil :boolean)
     (emit 'ireturn))
    ((eq return-type :char)
     ;; FIXME: how does this call not work?
     ;; (emit-invokevirtual +lisp-character+ "getValue" nil :char)
     (emit-invokestatic +lisp-character+ "getValue"
                        (list +lisp-object+) :char)
     (emit 'ireturn))
    ((jvm-class-name-p return-type)
     (emit 'ldc_w (pool-class return-type))
     (emit-invokevirtual +lisp-object+ "javaInstance" (list +java-class+) +java-object+)
     (emit-checkcast return-type)
     (emit 'areturn))
    (t
     (error "Unsupported return type: ~A" return-type))))

(defun java::runtime-class-add-methods (class-file methods)
  (let (method-implementation-fields)
    (dolist (method methods)
      (destructuring-bind (name return-type argument-types function
                           &key (modifiers '(:public)) annotations override)
          method
        (let* ((argument-types (mapcar #'java::canonicalize-java-type argument-types))
               (argc (length argument-types))
               (return-type (java::canonicalize-java-type return-type))
               (jmethod (make-jvm-method name return-type argument-types :flags modifiers))
               (field-name (string (gensym name)))
               (staticp (member :static modifiers))
               (offset (if staticp 0 1))
               (all-argc (+ argc offset)))
          (class-add-method class-file jmethod)
          (let ((field (make-field field-name +lisp-object+ :flags '(:public :static))))
            (class-add-field class-file field)
            (push (cons field-name function) method-implementation-fields))
          (when annotations
            (method-add-attribute jmethod (make-runtime-visible-annotations-attribute
                                           :list (mapcar #'parse-annotation annotations))))
          (with-code-to-method (class-file jmethod)
            ;;Allocate registers (2 * argc to load and store arguments + 2 to box "this")
            (dotimes (i (* 2 all-argc))
              (allocate-register nil))
            (unless staticp
              ;;Box "this" (to be passed as the first argument to the Lisp function)
              (aload 0)
              (emit 'iconst_1) ;;true
              (emit-invokestatic +abcl-java-object+ "getInstance"
                                 (list +java-object+ :boolean) +lisp-object+)
              (astore all-argc))
            (box-arguments argument-types offset all-argc)
            ;;Load the Lisp function from its static field
            (emit-getstatic (class-file-class class-file) field-name +lisp-object+)
            (if (<= all-argc call-registers-limit)
                (progn
                  ;;Load the boxed this
                  (unless staticp
                    (aload all-argc))
                  ;;Load each boxed argument
                  (dotimes (i argc)
                    (aload (+ i 1 all-argc))))
                (error "execute(LispObject[]) is currently not supported"))
            (emit-call-execute all-argc)
            (java::emit-unbox-and-return return-type))
          (cond
            ((eq override t)
             (let ((super-method
                    (make-jvm-method (format nil "super$~A" name)
                                     return-type argument-types :flags modifiers)))
               (class-add-method class-file super-method)
               (with-code-to-method (class-file super-method)
                 (dotimes (i (1+ (length argument-types)))
                   (allocate-register nil))
                 (aload 0)
                 (loop
                    :for arg-type :in argument-types
                    :for i :from 1
                    :do (progn
                          (cond
                            ((keywordp arg-type)
                             (error "Unsupported arg-type: ~A" arg-type))
                            ((eq arg-type :int) :todo)
                            (t (aload i)))))
                 (emit-invokespecial (class-file-superclass class-file) name
                                     argument-types return-type)
                 ;(emit 'pop)
                 (cond
                   ((eq return-type :void)
                    (emit 'return))
                   ((eq return-type :int)
                    (emit 'ireturn))
                   ((eq return-type :boolean)
                    (emit 'ireturn))
                   ((jvm-class-name-p return-type)
                    (emit 'areturn))
                   (t
                    (error "Unsupported return type: ~A" return-type))))))))))
    method-implementation-fields))

(defun java::runtime-class-add-fields (class-file fields)
  (dolist (field-spec fields)
    (destructuring-bind (name type &key (modifiers '(:public)) annotations
                              (getter nil getter-p) (setter nil setter-p)
                              (property (and (not getter-p) (not setter-p))))
        field-spec
      (let* ((type (if (keywordp type) type (make-jvm-class-name type)))
             (field (make-field name type :flags modifiers)))
        (when (member :static modifiers)
          (setf property nil getter nil setter nil))
        (when annotations
          (field-add-attribute field (make-runtime-visible-annotations-attribute
                                      :list (mapcar #'parse-annotation annotations))))
        (class-add-field class-file field)
        (when (or getter property)
          (unless (stringp getter)
            (setf getter (java::make-accessor-name "get" (if (stringp property) property name))))
          (let ((jmethod (make-jvm-method getter type nil :flags '(:public))))
            (class-add-method class-file jmethod)
            (with-code-to-method (class-file jmethod)
              (aload 0)
              (emit-getfield (class-file-class class-file) name type)
              (cond
                ((jvm-class-name-p type) (emit 'areturn))
                ((eq type :int) (emit 'ireturn))
                (t (error "Unsupported getter return type: ~A" type))))))
        (when (or setter property)
          (unless (stringp setter)
            (setf setter (java::make-accessor-name "set" (if (stringp property) property name))))
          (let ((jmethod (make-jvm-method setter :void (list type) :flags '(:public))))
            (class-add-method class-file jmethod)
            (with-code-to-method (class-file jmethod)
              (aload 0)
              (cond
                ((jvm-class-name-p type) (aload 1))
                ((eq type :int) (emit 'iload 1))
                (t (error "Unsupported setter parameter type: ~A" type)))
              (emit-putfield (class-file-class class-file) name type)
              (emit 'return))))))))

(defmacro java:define-java-class () :todo)

(defun parse-annotation (annotation)
  (when (annotation-p annotation)
    (return-from parse-annotation annotation))
  (destructuring-bind (class &rest elements) (if (listp annotation) annotation (list annotation))
    (let (actual-elements)
      (dolist (elem elements)
        (push (parse-annotation-element elem) actual-elements))
      (make-annotation :type class :elements (nreverse actual-elements)))))

(defun parse-annotation-element (elem)
  (cond
    ((annotation-element-p elem) elem)
    ((atom elem) (make-primitive-or-string-annotation-element :name nil :value elem))
    ((keywordp (car elem)) (parse-annotation-element `("value" ,@elem)))
    (t
     (destructuring-bind (name &key value enum annotation) elem
       (cond
         (enum (make-enum-value-annotation-element :name name :type enum :value value))
         (annotation
          (make-annotation-value-annotation-element :name name :value (parse-annotation annotation)))
         ((listp value)
          (make-array-annotation-element :name name :values (mapcar #'parse-annotation-element value)))
         (t (make-primitive-or-string-annotation-element :name name :value value)))))))

;;TODO:
;; - Returning nil as null is broken
;; - Function calls with 8+ args
;; - super method invocation. Idea: generate companion methods super_... to use with plain jcall. Add a flag per method to optionally disable this when not needed.
;; - Constructors
;; - optional accessors (CLOS methods) for properties?

#+example
(java:jnew-runtime-class
 "Foo"
 :interfaces (list "java.lang.Comparable")
 :fields (list '("someField" "java.lang.String") '("anotherField" "java.lang.Object" :getter t))
 :methods (list
           (list "foo" :void '("java.lang.Object")
                 (lambda (this that) (print (list this that)))
                 :annotations (list "java.lang.Deprecated"
                                    '("java.lang.annotation.Retention"
                                      (:enum "java.lang.annotation.RetentionPolicy" :value "RUNTIME"))
                                    '("javax.xml.bind.annotation.XmlAttribute" ("required" :value t))
                                    '("com.manydesigns.portofino.system.model.users.annotations.RequiresPermissions"
                                      ("level"
                                       :enum "com.manydesigns.portofino.model.pages.AccessLevel"
                                       :value "EDIT")
                                      ("permissions" :value ("foo" "bar")))))
           (list "bar" :int '("java.lang.Object")
                 (lambda (this that) (print (list this that)) 23))))

(provide "RUNTIME-CLASS")
