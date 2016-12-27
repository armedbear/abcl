(in-package :system)

;; Author: Alan Ruttenberg December 2016

;; This code is released under Creative Common CC0 declaration
;; (https://wiki.creativecommons.org/wiki/CC0) and as such is intended
;; to be in the public domain.


;; A compiled function is an instance of a class - This class has
;; multiple instances if it represents a closure, or a single instance if
;; it represents a non-closed-over function.

;; The ABCL compiler stores constants that are used in function execution
;; as private java fields. This includes symbols used to invoke function,
;; locally-defined functions (such as via labels or flet) and string and
;; other literal constants.

;; This file provides access to those internal values, and uses them in
;; at least two ways. First, to annotate locally defined functions with
;; the top-level function they are defined within, and second to search
;; for callers of a give function(*). This may yield some false
;; positives, such as when a symbol that names a function is also used
;; for some other purpose. It can also have false negatives, as when a
;; function is inlined. Still, it's pretty useful. The second use to to
;; find source locations for frames in the debugger. If the source
;; location for a local function is asked for the location of its 'owner'
;; is instead returns.

;; (*) Since java functions are strings, local fields also have these
;; strings. In the context of looking for callers of a function you can
;; also give a string that names a java method. Same caveat re: false
;; positives.

;; In order to record information about local functions, ABCL defines a
;; function-plist, which is for the most part unused, but is used here
;; with set of keys indicating where the local function was defined and
;; in what manner, i.e. as normal local function, as a method function,
;; or as an initarg function. There may be other places functions are
;; stashed away (defstructs come to mind) and this file should be added
;; to to take them into account as they are discovered.

;; This file does not depend on jss, but provides a bit of
;; jss-specific functionality if jss *is* loaded.

(defun function-internal-fields (f)
  "return a list of values of fields declared in the class implementing the function"
  (if (symbolp f) 
      (setq f (symbol-function f)))
  ;; think about other fields
  (let ((fields (java::jcall "getDeclaredFields" (java::jcall "getClass" f))))
    (loop for field across fields
	  do (java::jcall "setAccessible" field t)
	  collect
	  (java::jcall "get" field f))))

(defun function-internals (f)
  "internal fields + closed-over values"
  (append (function-internal-fields f)
	  (and (java::jcall "isInstance" (java::jclass "org.armedbear.lisp.CompiledClosure") f)
	       (compiled-closure-context f))))

(defun compiled-closure-context (f)
  "For compiled closures, the values closed over"
  (let ((context (java::jcall "get" (load-time-value (java::jclass-field "org.armedbear.lisp.CompiledClosure" "ctx")) f)))
    (loop for binding across context
	  collect 
	  (java::jcall "get" (load-time-value (java::jclass-field "org.armedbear.lisp.ClosureBinding" "value")) binding))))
   
(defun foreach-internal-field (fn-fn not-fn-fn &optional (fns :all) (definer nil))
  "fn-n gets called with top, internal function, not-fn-fn gets called with top anything-but"
  (declare (optimize (speed 3) (safety 0)))
  (macrolet ((fields (c) `(java::jcall ,(java::jmethod "java.lang.Class" "getDeclaredFields") ,c))
	     (get (f i) `(java::jcall ,(java::jmethod "java.lang.reflect.Field" "get" "java.lang.Object") ,f ,i))
	     (access (f b) `(java::jcall ,(java::jmethod "java.lang.reflect.AccessibleObject" "setAccessible" "boolean") ,f ,b))
	     (getclass (o) `(java::jcall ,(java::jmethod "java.lang.Object" "getClass") ,o)))
    (labels ((function-internal-fields (f)
	       (if (symbolp f) 
		   (setq f (symbol-function f)))
	       (let ((fields (fields (getclass f))))
		 (loop for field across fields
		       do (access field t)
		       collect
		       (get field f))))
	     (check (f top seen)
	       (declare (optimize (speed 3) (safety 0)))
	       (dolist (el (function-internal-fields f))
		 (if (functionp el)
		     (let ((name? (third (multiple-value-list (function-lambda-expression el)))))
		       (if (or (consp name?) (and name? (fboundp name?) (eq el (symbol-function name?))) )
			   (progn
			     (when not-fn-fn (funcall not-fn-fn top name?))
			     (when (not (member el seen :test #'eq))
			       (push el seen)
			       (check el top seen)))
			   (when (not (member el seen :test #'eq))
			     (when fn-fn (funcall fn-fn top el))
			     (push el seen)
			     (check el top seen))))
		     (when not-fn-fn 
		       (funcall not-fn-fn top el)
		       )))))
      (if (eq fns :all)
	  (progn
	    (dolist (p (list-all-packages))
	    (do-symbols (s p)
	      (when (fboundp s)
		(check (symbol-function s) s nil))))
	    (each-non-symbol-compiled-function (lambda (definer f) (check f definer nil))))
	  (dolist (f fns) 
	    (check (if (not (symbolp f)) f  (symbol-function f)) (or definer f) nil))
	  ))))

(defun callers (thing &aux them)
  (foreach-internal-field
   nil
   (lambda(top el)
     (when (equal el thing)
       (pushnew top them)
       )))
  them)

(defun annotate-internal-functions (&optional (fns :all) definer)
  "Iterate over functions reachable from arg fns (all functions
   if :all). When not a top-level function add
   key: :internal-to-function value top-level thing in which the
   function is defined. definers are the top-level functions, This
   gets called after fset"
  (foreach-internal-field
   (lambda(top internal)
     (unless (eq (if (symbolp top) (symbol-function top) top) internal)
       (setf (getf (function-plist internal) :internal-to-function) (or definer top))
       ))
   nil
   fns
   definer))

(defun annotate-clos-methods (&optional (which :all))
  "Iterate over all clos methods, marking method-functions and
method-fast-functions with the function plist
indicator :method-function or :method-fast-function, value the method
object. This gets called once."
  (flet ((annotate (method)
	   (let ((method-function (mop::std-method-function method))
		 (fast-function  (mop::std-method-fast-function method)))
	     (when (and method-function (compiled-function-p method-function)) 
	       (setf (getf (function-plist method-function) :method-function) method)
	       (annotate-internal-functions (list method-function) method))
	     (when (and fast-function (compiled-function-p fast-function))
	       (setf (getf (function-plist fast-function) :method-fast-function) method)
	       (annotate-internal-functions (list fast-function) method)))))
      (if (eq which :all)
	  (loop for q = (list (find-class t)) then q
		for focus = (pop q)
		while focus
		do (setq q (append q (mop::class-direct-subclasses  focus)))
		   (loop for method in (mop::class-direct-methods focus)
			 do (annotate method)))

	  (dolist (f which)
	    (annotate f)
	    ))))

(defun annotate-clos-slots (&optional (which :all))
  "Iterate over all clos slots, marking compile-initarg functions as :initfunction value slot"
  (flet ((annotate (slot)
	   (let ((initfunction (and (slot-boundp slot 'initfunction)
				    (slot-value slot 'initfunction))))
	     (when initfunction
	       (setf (getf (function-plist initfunction) :initfunction) slot)
	       (annotate-internal-functions (list initfunction) slot)))))
    (if (eq which :all)
	(loop for q = (list (find-class t)) then q
	      for focus = (pop q)
	      while focus
	      do (setq q (append q (mop::class-direct-subclasses  focus)))
		 (loop for slot in (mop::class-direct-slots focus)
		       do (annotate slot)))
	(dolist (f which)
	  (annotate f)
	  ))))

(defun method-spec-list (method)
  "Given a method object, translate it into specification (name qualifiers specializers)"
  `(,(mop::generic-function-name  (mop::method-generic-function method))
    ,(mop::method-qualifiers method) 
    ,(mapcar #'(lambda (c)
		 (if (typep c 'mop:eql-specializer)
		     `(eql ,(mop:eql-specializer-object c))
		     (class-name c)))
	     (mop:method-specializers method))))

;; function names for printing, inspection and in the debugger

(defun any-function-name (function &aux it)
  "Compute function name based on the actual function name, if it is a
named function or the values on the function-plist that functions
above have used annotate local functions"
  (maybe-jss-function function)
  (let ((plist (sys::function-plist function)))
    (cond ((setq it (getf plist :internal-to-function))
	   `(:local-function ,@(if (java::jcall "getLambdaName" function) 
				   (list (java::jcall "getLambdaName" function))
				   (if (getf plist :jss-function)
				       (list (concatenate 'string "#\"" (getf plist :jss-function) "\"")))
				   )
			     :in ,@(if (typep it 'mop::standard-method)
				       (cons :method (method-spec-list it))
				       (list it))))
	  ((setq it (getf plist :method-function))
	   (cons :method-function (sys::method-spec-list it)))	   
	  ((setq it (getf plist :method-fast-function))
	   (cons :method-fast-function (sys::method-spec-list it)))
	  ((setq it (getf plist :initfunction))
	   (let ((class (and (slot-boundp it 'allocation-class) (slot-value it 'allocation-class))))
	     (list :slot-initfunction (slot-value it 'name ) :for (if class (class-name class) '??))))
	  (t (or (nth-value 2 (function-lambda-expression function))
		 (and (not (compiled-function-p function))
		      `(:anonymous-interpreted-function))
		 (function-name-by-where-loaded-from function))))))

(defun function-name-by-where-loaded-from (function)
  "name of last resource - used the loaded-from field from the function to construct the name"
  (let* ((class (java::jcall "getClass" function))
	 (loaded-from (sys::get-loaded-from function))
	 (name (java::jcall "replace" (java::jcall "getName" class) "org.armedbear.lisp." ""))
	 (where (and loaded-from (concatenate 'string (pathname-name loaded-from) "." (pathname-type loaded-from)))))
    `(:anonymous-function ,name ,@(if (sys::arglist function)  (sys::arglist function)) 
			  ,@(if where (list (list :from where))))))
  
(defun maybe-jss-function (f)
  "Determing if function is something list #"foo" called as a
  function. If so add to function internal plist :jss-function and the
  name of the java methods"
  (and (find-package :jss)
       (or (getf (sys::function-plist f) :jss-function)
	   (let ((internals (function-internal-fields f)))
	     (and (= (length internals) 2)
		  (eq (second internals) (intern "INVOKE-RESTARGS" :jss))
		  (stringp (first internals))
		  (setf (getf (sys::function-plist f) :jss-function) (first internals)))))))
	 
(defun local-function-p (function)
  "Helper function. Tests whether a function wasn't defined at top
  level based on function-plist annotations"
  (and (functionp function)
       (let ((plist  (sys::function-plist function)))
	 (or (getf plist :internal-to-function)
	     (getf plist :method-function)
	     (getf plist :method-fast-function)
	     (getf plist :slot-initfunction)))))

(defun local-function-owner (function)
  "For local function, return the 'owner' typically the top-level function or clos method"
  (local-function-p function))

(defmethod print-object ((f function) stream)
  "Print a function using any-function-name. Requires a patch to
  system::output-ugly-object in order to prevent the function being
  printed by a java primitive"
  (print-unreadable-object (f stream :identity t)
    (let ((name (any-function-name  f)))
       (if (consp name)
           (format stream "~{~a~^ ~}" name)
           (princ name stream)))))

(defun each-non-symbol-compiled-function (f)
  (loop for q = (list (find-class t)) then q
	for focus = (pop q)
	while focus
	do (setq q (append q (mop::class-direct-subclasses  focus)))
	   (loop for method in (mop::class-direct-methods focus)
		 do (when (compiled-function-p (mop::method-function method)) (funcall f method (mop::method-function method))))
	   (loop for slot in (mop::class-direct-slots focus)
		 for initfunction = (and (slot-boundp slot 'initfunction) (slot-value slot 'initfunction))
		 do (and initfunction (compiled-function-p initfunction) (funcall f slot initfunction)))))
                                
;; hooks into defining 
 
(defvar *fset-hooks* nil "functions on this list get called with name and function *after* the symbol-function is set")

(defvar *annotate-function-backlog?* t "true before this file has been loaded and function annotations are placed")

(defun fset-hook-annotate-internal-function (name function)
  "Called at the end of fset. If function annotations have not yet
  been added, add local function annotations to all functions. If not,
  just add annotations to function specified in the arglist"
  (declare (ignore function))
  (when *annotate-function-backlog?* 
    (setq *annotate-function-backlog?* nil)
    (annotate-internal-functions)
    (annotate-clos-methods)
    (annotate-clos-slots)
    )
  (annotate-internal-functions (list name)))

;; Here we hook into clos in order to have method and slot functions
;; annotated when they are defined.

(defmethod mop::add-direct-method :after (class method)
  (annotate-clos-methods (list method)))

(defmethod mop::ensure-class-using-class :after (class name  &key direct-slots
                                             direct-default-initargs 
                                             &allow-other-keys)
  (annotate-clos-slots (mop::class-direct-slots (find-class name))))

;; needs to be the last thing. Some interaction with the fasl loader
(pushnew 'fset-hook-annotate-internal-function sys::*fset-hooks*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		 
(defun get-pid ()
  "Get the process identifier of this lisp process. Used to be in
  slime but generally useful, so now back in abcl proper."
  (handler-case
      (let* ((runtime
              (java::jstatic "getRuntime" "java.lang.Runtime"))
             (command
              (java::jnew-array-from-array
               "java.lang.String" #("sh" "-c" "echo $PPID")))
             (runtime-exec-jmethod
              ;; Complicated because java.lang.Runtime.exec() is
              ;; overloaded on a non-primitive type (array of
              ;; java.lang.String), so we have to use the actual
              ;; parameter instance to get java.lang.Class
              (java::jmethod "java.lang.Runtime" "exec"
                            (java::jcall
                             (java::jmethod "java.lang.Object" "getClass")
                             command)))
             (process
              (java::jcall runtime-exec-jmethod runtime command))
             (output
              (java::jcall (java::jmethod "java.lang.Process" "getInputStream")
                          process)))
         (java::jcall (java::jmethod "java.lang.Process" "waitFor")
                     process)
	 (loop :with b :do
	    (setq b
		  (java::jcall (java::jmethod "java.io.InputStream" "read")
			      output))
	    :until (member b '(-1 #x0a))	; Either EOF or LF
	    :collecting (code-char b) :into result
	    :finally (return
		       (parse-integer (coerce result 'string)))))
    (t () 0)))

(provide :abcl-introspect)
