(require "COMPILER-PASS2")
(require "JVM-CLASS-FILE")

;;The package is set to :jvm for convenience, since most of the symbols used
;;here come from that package. However, the functions we're definining belong
;;to the :java package.
(in-package :jvm)

(defconstant +abcl-java-object+ (make-jvm-class-name "org.armedbear.lisp.JavaObject"))

(defun java:jnew-runtime-class
    (class-name &key (superclass (make-jvm-class-name "java.lang.Object"))
     interfaces constructors methods fields (access-flags '(:public)))
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
   (method-name return-type argument-types function &key modifiers annotations)
   where method-name is a string, return-type and argument-types are strings or keywords for
   primitive types (:void, :int, etc.), and function is a Lisp function of minimum arity
   (1+ (length argument-types)); the instance (`this') is passed in as the first argument.

   Field definitions are lists of the form
   (field-name type modifier*)

   If FILE-NAME is given, a .class file will be written; this is useful for debugging only."
  (declare (ignorable constructors fields))
  (let* ((jvm-class-name (make-jvm-class-name class-name))
         (class-file (make-class-file jvm-class-name superclass access-flags))
         (stream (sys::%make-byte-array-output-stream))
         ;;TODO provide constructor in MemoryClassLoader
         (memory-class-loader (java:jnew "org.armedbear.lisp.MemoryClassLoader" ""))
         method-implementation-fields)
    (setf (class-file-interfaces class-file)
          (mapcar #'make-jvm-class-name interfaces))
    (dolist (m methods)
      (destructuring-bind (name return-type argument-types function &key (modifiers '(:public)) annotations) m
        (let* ((argument-types (mapcar #'make-jvm-class-name argument-types))
               (argc (length argument-types))
               (return-type (if (keywordp return-type)
                                return-type
                                (make-jvm-class-name return-type)))
               (jmethod (make-jvm-method name return-type argument-types :flags modifiers))
               (field-name (string (gensym name))))
          (class-add-method class-file jmethod)
          (let ((field (make-field field-name +lisp-object+ :flags '(:public :static))))
            (class-add-field class-file field)
            (push (cons field-name function) method-implementation-fields))
          (when annotations
            (method-add-attribute jmethod (make-runtime-visible-annotations-attribute
                                           :list (mapcar #'parse-annotation annotations))))
          (with-code-to-method (class-file jmethod)
            ;;Allocate registers (2 * argc to load and store arguments + 2 to box "this")
            (dotimes (i (* 2 (1+ argc)))
              (allocate-register nil))
            ;;Box "this" (to be passed as the first argument to the Lisp function)
            (aload 0)
            (emit 'iconst_1) ;;true
            (emit-invokestatic +abcl-java-object+ "getInstance"
                               (list +java-object+ :boolean) +lisp-object+)
            (astore (1+ argc))
            ;;Box each argument
            (loop
               :for arg-type :in argument-types
               :for i :from 1
               :do (progn
                     (cond
                       ((keywordp arg-type)
                        (error "Unsupported arg-type: ~A" arg-type))
                       ((eq arg-type :int) :todo)
                       (t (aload i)
                          (emit 'iconst_1) ;;true
                          (emit-invokestatic +abcl-java-object+ "getInstance"
                                             (list +java-object+ :boolean) +lisp-object+)))
                     (astore (+ i (1+ argc)))))
            ;;Load the Lisp function from its static field
            (emit-getstatic jvm-class-name field-name +lisp-object+)
            (if (<= (1+ argc) call-registers-limit)
                (progn
                  ;;Load the boxed this
                  (aload (1+ argc))
                  ;;Load each boxed argument
                  (dotimes (i argc)
                    (aload (+ argc 2 i))))
                (error "execute(LispObject[]) is currently not supported"))
            (emit-call-execute (1+ (length argument-types)))
            (cond
              ((eq return-type :void)
               (emit 'pop)
               (emit 'return))
              ((eq return-type :int)
               (emit-invokevirtual +lisp-object+ "intValue" nil :int)
               (emit 'ireturn))
              ((jvm-class-name-p return-type)
               (emit-invokevirtual +lisp-object+ "javaInstance" nil +java-object+)
               (emit-checkcast return-type)
               (emit 'areturn))
              (t
               (error "Unsupported return type: ~A" return-type)))))))
    (when (null constructors)
      (let ((ctor (make-jvm-method :constructor :void nil :flags '(:public))))
        (class-add-method class-file ctor)
        (with-code-to-method (class-file ctor)
          (aload 0)
          (emit-invokespecial-init (class-file-superclass class-file) nil)
          (emit 'return))))
    (finalize-class-file class-file)
    (write-class-file class-file stream)
    (finish-output stream)
    #+test-record-generated-class-file
    (with-open-file (f (format nil "~A.class" class-name) :direction :output :element-type '(signed-byte 8))
      (write-sequence (java::list-from-jarray (sys::%get-output-stream-bytes stream)) f))
    (sys::put-memory-function memory-class-loader
                              class-name (sys::%get-output-stream-bytes stream))
    (let ((jclass (java:jcall "loadClass" memory-class-loader class-name)))
      (dolist (method method-implementation-fields)
        (setf (java:jfield jclass (car method)) (cdr method)))
      jclass)))

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

#+example
(java:jnew-runtime-class
 "Foo"
 :interfaces (list "java.lang.Comparable")
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