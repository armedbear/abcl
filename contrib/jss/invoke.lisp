;; Copyright (C) 2005 Alan Ruttenberg
;; Copyright (C) 2011-2 Mark Evenson
;;
;; Since JSS 1.0 was largely derivative of the Jscheme System, the
;; current system is licensed under the same terms, namely:

;; This software is provided 'as-is', without any express or
;; implied warranty.

;; In no event will the author be held liable for any damages
;; arising from the use of this software.

;; Permission is granted to anyone to use this software for any
;; purpose, including commercial applications, and to alter it
;; and redistribute it freely, subject to the following
;; restrictions:

;; 1. The origin of this software must not be misrepresented; you
;;    must not claim that you wrote the original software. If you
;;    use this software in a product, an acknowledgment in the
;;    product documentation would be appreciated but is not
;;    required.

;; 2. Altered source versions must be plainly marked as such, and
;;    must not be misrepresented as being the original software.

;; 3. This notice may not be removed or altered from any source
;;    distribution.


;; The dynamic dispatch of the java.lang.reflect package is used to
;; make it real easy, if perhaps less efficient, to write Java code
;; since you don't need to be bothered with imports, or with figuring
;; out which method to call.  The only time that you need to know a
;; class name is when you want to call a static method, or a
;; constructor, and in those cases, you only need to know enough of
;; the class name that is unique wrt to the classes on your classpath.
;;
;; Java methods look like this: #"toString". Java classes are
;; represented as symbols, which are resolved to the appropriate java
;; class name. When ambiguous, you need to be more specific. A simple example:

;; (let ((sw (new 'StringWriter)))
;;   (#"write" sw "Hello ")
;;   (#"write" sw "World")
;;   (print (#"toString" sw)))

;; What's happened here? First, all the classes in all the jars in the
;; classpath have been collected.  For each class a.b.C.d, we have
;; recorded that b.c.d, b.C.d, C.d, c.d, and d potentially refer to
;; this class. In your call to new, as long as the symbol can refer to
;; only one class, we use that class. In this case, it is
;; java.io.StringWriter. You could also have written (new
;; 'io.stringwriter), (new '|io.StringWriter|), (new
;; 'java.io.StringWriter)...

;; the call (#"write" sw "Hello "), uses the code in invoke.java to
;; call the method named "write" with the arguments sw and "Hello ". 
;; JSS figures out the right java method to call, and calls it.

;; If you want to do a raw java call, use #0"toString". Raw calls
;; return their results as Java objects, avoiding doing the usual Java
;; object to Lisp object conversions that ABCL does.

;; (with-constant-signature ((name jname raw?)*) &body body)
;; binds a macro which expands to a jcall, promising that the same method 
;; will be called every time. Use this if you are making a lot of calls and 
;; want to avoid the overhead of a the dynamic dispatch. 
;; e.g. (with-constant-signature ((tostring "toString")) 
;;        (time (dotimes (i 10000) (tostring "foo"))))
;; runs about 3x faster than (time (dotimes (i 10000) (#"toString" "foo")))
;;
;; (with-constant-signature ((tostring "toString" t)) ...) will cause the 
;; toString to be a raw java call. see get-all-jar-classnames below for an example.
;; 
;; Implementation is that the first time the function is called, the
;; method is looked up based on the arguments passed, and thereafter
;; that method is called directly.  Doesn't work for static methods at
;; the moment (lazy)
;;
;; (japropos string) finds all class names matching string
;; (jcmn class-name) lists the names of all methods for the class
;;
;; TODO
;;   - Make with-constant-signature work for static methods too.
;;   - #2"toString" to work like function scoped (with-constant-signature ((tostring "toString")) ...)
;;   - #3"toString" to work like runtime scoped (with-constant-signature ((tostring "toString")) ...)
;;      (both probably need compiler support to work)
;;   - Maybe get rid of second " in reader macro. #"toString looks nicer, but might 
;;     confuse lisp mode.
;;   - write jmap, analogous to map, but can take java collections, java arrays etc.
;;   - write loop clauses for java collections. 
;;   - Register classes in .class files below classpath directories (when :wild-inferiors works)
;;   - Make documentation like Edi Weitz
;;
;; Thanks: Peter Graves, Jscheme developers, Mike Travers for skij,  
;; Andras Simon for jfli-abcl which bootstrapped me and taught me how to do 
;; get-all-jar-classnames
;; 

;; changelog 

;; Sat January 28, 2006, alanr: 

;; Change imports strategy. Only index by last part of class name,
;; case insensitive. Make the lookup-class-name logic be a bit more
;; complicated. This substantially reduces the time it takes to do the
;; auto imports and since class name lookup is relatively infrequent,
;; and in any case cached, this doesn't effect run time speed.  (did
;; try caching, but didn't pay - more time was spent reading and
;; populating large hash table)
;; 
;; Split class path by ";" in addition to ":" for windows.
;;
;; Tested on windows, linux.

;; 2011-05-21 Mark Evenson
;;   "ported" to native ABCL without needing the jscheme.jar or bsh-2.0b4.jar

(in-package :jss)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *do-auto-imports* t 
    "Whether to automatically introspect all Java classes on the classpath when JSS is loaded."))

(defvar *muffle-warnings* t)

(defvar *imports-resolved-classes* (make-hash-table :test 'equalp))

(defun find-java-class (name)
  "Returns the java.lang.Class representation of NAME.

NAME can either string or a symbol according to the usual JSS conventions."
  (jclass (maybe-resolve-class-against-imports name)))

(defmacro invoke-add-imports (&rest imports)
  "Push these imports onto the search path. If multiple, earlier in list take precedence"
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (clrhash *imports-resolved-classes*)
     (dolist (i (reverse ',imports))
       (setq *imports-resolved-classes* (delete i *imports-resolved-classes* :test 'equal))
       )))

(defun clear-invoke-imports ()
  (clrhash *imports-resolved-classes*))

(defun maybe-resolve-class-against-imports (classname)
  (or (gethash (string classname) *imports-resolved-classes*)
      (let ((found (lookup-class-name classname)))
        (if found
            (progn 
              (setf (gethash classname *imports-resolved-classes*) found)
              found)
            (string classname)))))

(defvar *class-name-to-full-case-insensitive* (make-hash-table :test 'equalp))

;; This is the function that calls invoke to call your java
;; method. The first argument is the method name or 'new. The second
;; is the object you are calling it on, followed by the rest of the
;; arguments. If the "object" is a symbol, then that symbol is assumed
;; to be a java class, and a static method on the class is called,
;; otherwise a regular method is called.

(defun invoke (method object &rest args)
  (invoke-restargs method object args))

(defun invoke-restargs (method object args &optional (raw? nil))
  (let* ((object-as-class-name 
          (if (symbolp object) (maybe-resolve-class-against-imports object)))
         (object-as-class 
          (if object-as-class-name (find-java-class object-as-class-name))))
    (if (eq method 'new)
        (apply #'jnew (or object-as-class-name object) args)
        (if raw?
            (if (symbolp object)
                (apply #'jstatic-raw method object-as-class  args)
                (apply #'jcall-raw method object  args))
            (if (symbolp object)
                (apply #'jstatic method object-as-class args)
                (apply #'jcall method object args))))))

(defconstant +set-accessible+ 
  (jmethod "java.lang.reflect.AccessibleObject" "setAccessible" "boolean"))

(defun invoke-find-method (method object args)
  (let ((result 
         (if (symbolp object)
                ;;; static method
             (apply #'jmethod (lookup-class-name object) 
                    method (mapcar #'jobject-class args))
                  ;;; instance method
             (apply #'jresolve-method 
                    method object args))))
    (jcall +set-accessible+ result +true+)
    result))

;; This is the reader macro for java methods. it translates the method
;; into a lambda form that calls invoke. Which is nice because you
;; can, e.g. do this: (mapcar #"toString" list-of-java-objects). The reader
;; macro takes one arg. If 0, then jstatic-raw is called, so that abcl doesn't
;; automagically convert the returned java object into a lisp object. So
;; #0"toString" returns a java.lang.String object, where as #"toString" returns
;; a regular Lisp string as ABCL converts the Java string to a Lisp string.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun read-invoke (stream char arg) 
    (if (eql arg 1)
	(progn (require 'javaparser)
	       (read-sharp-java-expression stream))
	(progn
	  (unread-char char stream)
	  (let ((name (read stream)))
	    (if (or (find #\. name) (find #\{ name))
		(jss-transform-to-field name)
		(let ((object-var (gensym))
		      (args-var (gensym)))
		  `(lambda (,object-var &rest ,args-var) 
		     (invoke-restargs ,name  ,object-var ,args-var ,(eql arg 0)))))))))
  (set-dispatch-macro-character #\# #\" 'read-invoke))

(defmacro with-constant-signature (fname-jname-pairs &body body)
  "Expand all references to FNAME-JNAME-PAIRS in BODY into static function calls promising that the same function bound in the FNAME-JNAME-PAIRS will be invoked with the same argument signature.

FNAME-JNAME-PAIRS is a list of (symbol function &optional raw)
elements where symbol will be the symbol bound to the method named by
the string function.  If the optional parameter raw is non-nil, the
result will be the raw JVM object, uncoerced by the usual conventions.

Use this macro if you are making a lot of calls and 
want to avoid the overhead of the dynamic dispatch."

  (if (null fname-jname-pairs)
      `(progn ,@body)
      (destructuring-bind ((fname jname &optional raw) &rest ignore) fname-jname-pairs
        (declare (ignore ignore))
        (let ((varname (gensym)))
          `(let ((,varname nil))
             (macrolet ((,fname (&rest args)
                          `(if ,',varname
                               (if ,',raw
                                   (jcall-raw ,',varname ,@args)
                                   (jcall ,',varname ,@args))
                               (progn
                                 (setq ,',varname (invoke-find-method ,',jname ,(car args) (list ,@(rest args))))
                                 (if ,',raw
                                     (jcall-raw ,',varname ,@args)
                                     (jcall ,',varname ,@args))))))
               (with-constant-signature ,(cdr fname-jname-pairs)
                 ,@body)))))))

(defun lookup-class-name (name
                          &key
                            (table *class-name-to-full-case-insensitive*)
                            (muffle-warning nil)
                            (return-ambiguous nil))
  (setq name (string name))
  (let* (;; cant (last-name-pattern (#"compile" '|java.util.regex.Pattern| ".*?([^.]*)$"))
         ;; reason: bootstrap - the class name would have to be looked up...
         (last-name-pattern (load-time-value (jstatic (jmethod "java.util.regex.Pattern" "compile"
                                                               (jclass "java.lang.String"))
                                                      (jclass "java.util.regex.Pattern") 
                                                      ".*?([^.]*)$")))
         (last-name 
          (let ((matcher (#0"matcher" last-name-pattern name)))
            (#"matches" matcher)
            (#"group" matcher 1))))
    (let* ((bucket (gethash last-name *class-name-to-full-case-insensitive*))
           (bucket-length (length bucket)))
      (or (find name bucket :test 'equalp)
          (flet ((matches-end (end full test)
                   (= (+ (or (search end full :from-end t :test test) -10)
                         (length end))
                      (length full)))
                 (ambiguous (choices)
		   (if return-ambiguous 
		       (return-from lookup-class-name choices)
		       (error "Ambiguous class name: ~a can be ~{~a~^, ~}" name choices))))
            (if (zerop bucket-length)
		(progn
                  (unless muffle-warning (warn "can't find class named ~a" name)) nil)
                (let ((matches (loop for el in bucket when (matches-end name el 'char=) collect el)))
                  (if (= (length matches) 1)
                      (car matches)
                      (if (= (length matches) 0)
                          (let ((matches (loop for el in bucket when (matches-end name el 'char-equal) collect el)))
                            (if (= (length matches) 1)
                                (car matches)
                                (if (= (length matches) 0)
				    (progn
                                      (unless muffle-warning (warn "can't find class named ~a" name)) nil)
                                    (ambiguous matches))))
                          (ambiguous matches))))))))))

(defun get-all-jar-classnames (jar-file-name)
  (let* ((jar (jnew (jconstructor "java.util.jar.JarFile" (jclass "java.lang.String")) (namestring (truename jar-file-name))))
         (entries (#"entries" jar)))
    (with-constant-signature ((matcher "matcher" t) (substring "substring")
                              (jreplace "replace" t) (jlength "length")
                              (matches "matches") (getname "getName" t)
                              (next "nextElement" t) (hasmore "hasMoreElements")
                              (group "group"))
      (loop while (hasmore entries)
         for name =  (getname (next entries))
         with class-pattern = (jstatic "compile" "java.util.regex.Pattern" ".*\\.class$")
         with name-pattern = (jstatic "compile" "java.util.regex.Pattern" ".*?([^.]*)$")
         when (matches (matcher class-pattern name))
         collect
           (let* ((fullname (substring (jreplace name #\/ #\.) 0 (- (jlength name) 6)))
                  (matcher (matcher name-pattern fullname))
                  (name (progn (matches matcher) (group matcher 1))))
             (cons name fullname))
           ))))

(defun jar-import (file)
  "Import all the Java classes contained in the pathname FILE into the JSS dynamic lookup cache."
  (when (probe-file file)
    (loop for (name . full-class-name) in (get-all-jar-classnames file)
       do 
         (pushnew full-class-name (gethash name *class-name-to-full-case-insensitive*) 
                  :test 'equal))))

(defun new (class-name &rest args)
  "Invoke the Java constructor for CLASS-NAME with ARGS.

CLASS-NAME may either be a symbol or a string according to the usual JSS conventions."
  (invoke-restargs 'new class-name args))

(defvar *running-in-osgi* (ignore-errors (jclass "org.osgi.framework.BundleActivator")))

(define-condition no-such-java-field (error)
  ((field-name
    :initarg :field-name
    :reader field-name
    )
   (object
    :initarg :object
    :reader object
    ))
  (:report (lambda (c stream)
             (format stream "Unable to find a FIELD named ~a for ~a"
                     (field-name c) (object c))))
  )

(defun get-java-field (object field &optional (try-harder *running-in-osgi*))
  "Get the value of the FIELD contained in OBJECT.
If OBJECT is a symbol it names a dot qualified static FIELD."
  (if try-harder
      (let* ((class (if (symbolp object)
                        (setq object (find-java-class object))
                        (if (equal "java.lang.Class" (jclass-name (jobject-class object)))
                            object
                            (jobject-class object))))
             (jfield (if (java-object-p field)
                         field
                         (or (find-declared-field field class)
                             (error 'no-such-java-field :field-name field :object object)))))
        (#"setAccessible" jfield +true+)
        (values (#"get" jfield object) jfield))
      (if (symbolp object)
          (let ((class (find-java-class object)))
            (jfield class field))
          (jfield field object))))

(defun find-declared-field (field class)
  "Return a FIELD object corresponding to the definition of FIELD
\(a string\) visible at CLASS. *Not* restricted to public classes, and checks
all superclasses of CLASS.
   Returns NIL if no field object is found."
  (loop while class
     for field-obj = (get-declared-field class field)
     if field-obj
     do (return-from find-declared-field field-obj)
     else
     do (setf class (jclass-superclass class)))
  nil)

(defun get-declared-field (class fieldname)
  (find fieldname (#"getDeclaredFields" class)
        :key 'jfield-name :test 'equal))

;; TODO use #"getSuperclass" and #"getInterfaces" to see whether there
;; are fields in superclasses that we might set
(defun set-java-field (object field value &optional (try-harder *running-in-osgi*))
  "Set the FIELD of OBJECT to VALUE.
If OBJECT is a symbol, it names a dot qualified Java class to look for
a static FIELD.  If OBJECT is an instance of java:java-object, the
associated is used to look up the static FIELD."
  (if try-harder
      (let* ((class (if (symbolp object)
                        (setq object (find-java-class object))
                        (if (equal "java.lang.Class" (jclass-name (jobject-class object)) )
                            object
                            (jobject-class object))))
             (jfield (if (java-object-p field)
                         field
                         (or (find-declared-field field class)
                             (error 'no-such-java-field :field-name field :object object)))))
        (#"setAccessible" jfield +true+)
        (values (#"set" jfield object value) jfield))
      (if (symbolp object)
          (let ((class (find-java-class object)))
            (setf (jfield (#"getName" class) field) value))
          (if (typep object 'java-object)
              (setf (jfield (jclass-of object) field) value)
              (setf (jfield object field) value)))))

(defun (setf get-java-field) (value object field &optional (try-harder *running-in-osgi*))
  (set-java-field object field value try-harder))


(defconstant +for-name+ 
  (jmethod "java.lang.Class" "forName" "java.lang.String" "boolean" "java.lang.ClassLoader"))

(defun find-java-class (name)
  (or (jstatic +for-name+ "java.lang.Class" 
               (maybe-resolve-class-against-imports name) +true+ java::*classloader*)
      (ignore-errors (jclass (maybe-resolve-class-against-imports name)))))

(defmethod print-object ((obj (jclass "java.lang.Class")) stream) 
  (print-unreadable-object (obj stream :identity nil)
    (format stream "java class ~a" (jclass-name obj))))

(defmethod print-object ((obj (jclass "java.lang.reflect.Method")) stream) 
  (print-unreadable-object (obj stream :identity nil)
    (format stream "method ~a" (#"toString" obj))))

(defun do-auto-imports ()
  (labels ((expand-paths (cp)
             (loop :for s :in cp
                :appending (loop :for entry 
                              :in (let ((p (pathname s)))
                                    (if (wild-pathname-p p)
                                        (directory p)
                                        (list p)))
                              :collecting entry)))
           (import-classpath (cp)
             (mapcar 
              (lambda (p) 
                (when *load-verbose*
                  (format t ";; Importing ~A~%" p))
                (cond 
                  ((file-directory-p p) )
                  ((equal (pathname-type p) "jar")
                   (jar-import (merge-pathnames p
                                                (format nil "~a/" (jstatic "getProperty" "java.lang.System" "user.dir")))))))
              cp))
           (split-classpath (cp)
             (coerce 
              (jcall "split" cp 
                     (string (jfield (jclass "java.io.File") "pathSeparatorChar")))
              'cons))
           (do-imports (cp)
             (import-classpath (expand-paths (split-classpath cp)))))
    (do-imports (jcall "getClassPath" (jstatic "getRuntimeMXBean" '|java.lang.management.ManagementFactory|)))
    (do-imports (jcall "getBootClassPath" (jstatic "getRuntimeMXBean" '|java.lang.management.ManagementFactory|)))))

(eval-when (:load-toplevel :execute)
  (when *do-auto-imports* 
    (do-auto-imports)))

(defun japropos (string)
  "Output the names of all Java class names loaded in the current process which match STRING.."
  (setq string (string string))
  (let ((matches nil))
    (maphash (lambda(key value) 
               (declare (ignore key))
               (loop for class in value
                  when (search string class :test 'string-equal)
                  do (pushnew (list class "Java Class") matches :test 'equal)))
             *class-name-to-full-case-insensitive*)
    (loop for (match type) in (sort matches 'string-lessp :key 'car)
       do (format t "~a: ~a~%" match type))
    ))

(defun jclass-method-names (class &optional full)
  (if (java-object-p class)
      (if (equal (jclass-name (jobject-class class)) "java.lang.Class")
          (setq class (jclass-name class))
          (setq class (jclass-name (jobject-class class)))))
  (union
   (remove-duplicates (map 'list (if full #"toString" 'jmethod-name) (#"getMethods" (find-java-class class))) :test 'equal)
   (ignore-errors (remove-duplicates (map 'list (if full #"toString" 'jmethod-name) (#"getConstructors" (find-java-class class))) :test 'equal))))

(defun java-class-method-names (class &optional stream)
  "Return a list of the public methods encapsulated by the JVM CLASS.

If STREAM non-nil, output a verbose description to the named output stream.

CLASS may either be a string naming a fully qualified JVM class in dot
notation, or a symbol resolved against all class entries in the
current classpath."
  (if stream
      (dolist (method (jclass-method-names class t))
        (format stream "~a~%" method))
      (jclass-method-names class)))

(setf (symbol-function 'jcmn) #'java-class-method-names)

(defun path-to-class (classname)
  (let ((full (lookup-class-name classname)))
    (#"toString" 
     (#"getResource" 
      (find-java-class full)
      (concatenate 'string "/" (substitute #\/ #\. full) ".class")))))

;; http://www.javaworld.com/javaworld/javaqa/2003-07/02-qa-0725-classsrc2.html

(defun all-loaded-classes ()
  (let ((classes-field 
         (find "classes" (#"getDeclaredFields" (jclass "java.lang.ClassLoader"))
               :key #"getName" :test 'equal)))
    (#"setAccessible" classes-field +true+)
    (loop for classloader in (mapcar #'first (dump-classpath))
       append
         (loop with classesv = (#"get" classes-field classloader)
            for i below (#"size" classesv)
            collect (#"getName" (#"elementAt" classesv i)))
       append
         (loop with classesv = (#"get" classes-field (#"getParent" classloader))
            for i below (#"size" classesv)
            collect (#"getName" (#"elementAt" classesv i))))))

(defun get-dynamic-class-path ()
  (rest 
   (find-if (lambda (loader) 
              (string= "org.armedbear.lisp.JavaClassLoader"
                       (jclass-name (jobject-class loader))))
            (dump-classpath)
            :key #'car)))

(defun java-gc ()
  (#"gc" (#"getRuntime" 'java.lang.runtime))
  (#"runFinalization" (#"getRuntime" 'java.lang.runtime))
  (#"gc" (#"getRuntime" 'java.lang.runtime))
  (java-room))

(defun java-room ()
  (let ((rt (#"getRuntime" 'java.lang.runtime)))
    (values (- (#"totalMemory" rt) (#"freeMemory" rt))
            (#"totalMemory" rt)
            (#"freeMemory" rt)
            (list :used :total :free))))

(defun verbose-gc (&optional (new-value nil new-value-supplied))
  (if new-value-supplied
      (progn (#"setVerbose" (#"getMemoryMXBean"  'java.lang.management.ManagementFactory) new-value) new-value)
      (#"isVerbose" (#"getMemoryMXBean"  'java.lang.management.ManagementFactory))))

(defun all-jars-below (directory) 
  (loop with q = (system:list-directory directory) 
     while q for top = (pop q)
     if (null (pathname-name top)) do (setq q (append q (all-jars-below top))) 
     if (equal (pathname-type top) "jar") collect top))

(defun all-classfiles-below (directory) 
  (loop with q = (system:list-directory directory) 
     while q for top = (pop q)
     if (null (pathname-name top)) do (setq q (append q (all-classfiles-below top ))) 
     if (equal (pathname-type top) "class")
     collect top
       ))

(defun all-classes-below-directory (directory)
  (loop for file in (all-classfiles-below directory) collect
       (format nil "~{~a.~}~a"
               (subseq (pathname-directory file) (length (pathname-directory directory)))
               (pathname-name file))
       ))

(defun classfiles-import (directory)
  "Load all Java classes recursively contained under DIRECTORY in the current process."
  (setq directory (truename directory))
  (loop for full-class-name in (all-classes-below-directory directory)
     for name = (#"replaceAll" full-class-name "^.*\\." "")
     do
       (pushnew full-class-name (gethash name *class-name-to-full-case-insensitive*) 
                :test 'equal)))

(defun jclass-all-interfaces (class)
  "Return a list of interfaces the class implements"
  (unless (java-object-p class)
    (setq class (find-java-class class)))
  (loop for aclass = class then (#"getSuperclass" aclass)
     while aclass
     append (coerce (#"getInterfaces" aclass) 'list)))

(defun safely (f name)
  (let ((fname (gensym)))
    (compile fname
             `(lambda(&rest args)
                (with-simple-restart (top-level
                                      "Return from lisp method implementation for ~a." ,name)
                  (apply ,f args))))
    (symbol-function fname)))

(defun jdelegating-interface-implementation (interface dispatch-to &rest method-names-and-defs)
  "Creates and returns an implementation of a Java interface with
   methods calling Lisp closures as given in METHOD-NAMES-AND-DEFS.

   INTERFACE is an interface 

   DISPATCH-TO is an existing Java object

   METHOD-NAMES-AND-DEFS is an alternating list of method names
   (strings) and method definitions (closures).

   For missing methods, a dummy implementation is provided that
   calls the method on DISPATCH-TO."
  (let ((implemented-methods
         (loop for m in method-names-and-defs
            for i from 0
            if (evenp i) 
            do (assert (stringp m) (m) "Method names must be strings: ~s" m) and collect m
            else
            do (assert (or (symbolp m) (functionp m)) (m) "Methods must be function designators: ~s" m))))
    (let ((safe-method-names-and-defs 
           (loop for (name function) on method-names-and-defs by #'cddr
              collect name collect (safely function name))))
      (loop for method across
           (jclass-methods interface :declared nil :public t)
         for method-name = (jmethod-name method)
         when (not (member method-name implemented-methods :test #'string=))
         do
           (let* ((def  `(lambda
                             (&rest args)
                           (invoke-restargs ,(jmethod-name method) ,dispatch-to args t)
                           )))
             (push (coerce def 'function) safe-method-names-and-defs)
             (push method-name safe-method-names-and-defs)))
      (apply #'java::%jnew-proxy  interface safe-method-names-and-defs))))


