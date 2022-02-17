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
  (let ((fields (java:jcall "getDeclaredFields" (java:jcall "getClass" f))))
    (loop for field across fields
          do (java:jcall "setAccessible" field t)
          collect
          (java:jcall "get" field f))))

(defun function-internals (f)
  "internal fields + closed-over values"
  (append (function-internal-fields f)
          (and (java:jcall "isInstance" (java::jclass "org.armedbear.lisp.CompiledClosure") f)
               (compiled-closure-context f))))

(defun compiled-closure-context (f)
  "For compiled closures, the values closed over"
  (let ((context (java:jcall "get" (load-time-value (java::jclass-field "org.armedbear.lisp.CompiledClosure" "ctx")) f)))
    (loop for binding across context
          collect 
          (java:jcall "get" (load-time-value (java::jclass-field "org.armedbear.lisp.ClosureBinding" "value")) binding))))
   
(defun foreach-internal-field (fn-fn not-fn-fn &optional (fns :all) (definer nil))
  "fn-n gets called with top, internal function, not-fn-fn gets called with top anything-but"
  (declare (optimize (speed 3) (safety 0)))
  (macrolet ((fields (c) `(java:jcall ,(java::jmethod "java.lang.Class" "getDeclaredFields") ,c))
             (get (f i) `(java:jcall ,(java::jmethod "java.lang.reflect.Field" "get" "java.lang.Object") ,f ,i))
             (access (f b) `(java:jcall ,(java::jmethod "java.lang.reflect.AccessibleObject" "setAccessible" "boolean") ,f ,b))
             (getclass (o) `(java:jcall ,(java::jmethod "java.lang.Object" "getClass") ,o)))
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

(defvar *function-class-names* (make-hash-table :test 'equalp :weakness :value)
  "Table mapping java class names of function classes to their function. Value is either symbol or (:in symbol) if an internal function")

(defun index-function-class-names (&optional (fns :all))
  "Create a table mapping class names to function, for cases where the class name appears in backtrace (although perhaps that's a bug?)"
  (if (eq fns :all)
      (dolist (p (list-all-packages))
        (do-symbols (s p)
          (when (and (eq (symbol-package s) p) (fboundp s)
                     ;; system is touchy about #'autoload 
                     (not (eq (symbol-function s) #'autoload)))
            (unless (#"matches" (#"getName" (#"getClass" (symbol-function s))) ".*Closure$")
                (setf (gethash (#"getName" (#"getClass" (symbol-function s))) *function-class-names*) (symbol-function s))))))
      (dolist (s fns)
        (setf (gethash (#"getName" (#"getClass" (if (symbolp s) (symbol-function s) s))) *function-class-names*) s)))
  (foreach-internal-field 
   (lambda(top internal)
     (let ((fn (if (symbolp top) (symbol-function top) top)))
           (unless (or (eq fn internal) (#"matches" (#"getName" (#"getClass" fn)) ".*Closure$"))
             (setf (gethash (#"getName" (#"getClass" internal)) *function-class-names*)
                   internal))))
   nil
   fns
   nil))

(defun java-class-lisp-function (class-name)
  "Return either function-name or (:in function-name) or nil if class isn't that of lisp function"
  (gethash class-name *function-class-names* ))

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
               (annotate-internal-functions (list method-function) method)
               (index-function-class-names (list method-function)))
             (when (and fast-function (compiled-function-p fast-function))
               (setf (getf (function-plist fast-function) :method-fast-function) method)
               (annotate-internal-functions (list fast-function) method)
               (index-function-class-names (list method-function))))))
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
  (cond ((typep function 'generic-function)
         (mop::generic-function-name function))
        ((typep function 'mop::method)
         (mop::generic-function-name (mop::method-generic-function function)))
        (t
         (maybe-jss-function function)
         (let ((interpreted (not (compiled-function-p function))))
           (let ((plist (sys::function-plist function)))
             (cond ((setq it (getf plist :internal-to-function))
                    `(:local-function ,@(if (java:jcall "getLambdaName" function) 
                                            (list (java:jcall "getLambdaName" function))
                                            (if (getf plist :jss-function)
                                                (list (concatenate 'string "#\"" (getf plist :jss-function) "\"")))
                                            )
                                      ,@(if interpreted '((interpreted)))
                                      :in ,@(if (typep it 'mop::standard-method)
                                                (cons :method (method-spec-list it))
                                                (list it))))
                   ((setq it (getf plist :method-function))
                    `(:method-function ,@(if interpreted '((interpreted))) ,@(sys::method-spec-list it)))          
                   ((setq it (getf plist :method-fast-function))
                    `(:method-fast-function ,@(if interpreted '("(interpreted)")) ,@(sys::method-spec-list it)))
                   ((setq it (getf plist :initfunction))
                    (let ((class (and (slot-boundp it 'allocation-class) (slot-value it 'allocation-class))))
                      `(:slot-initfunction ,(slot-value it 'name ) ,@(if interpreted '((interpreted))) :for ,(if class (class-name class) '??))))
                   ((#"equals" function (symbol-function 'lambda))
                    '(:macro-function lambda))
                   ((equal (#"getName" (#"getClass" function)) "org.armedbear.lisp.MacroObject")
                    `(:macro-object ,@(any-function-name #"{function}.expander")))
                   (t (or (and (nth-value 2 (function-lambda-expression function))
                               (if interpreted
                                   `(,(nth-value 2 (function-lambda-expression function)) ,'(interpreted))
                                   (let ((name (nth-value 2 (function-lambda-expression function))))
                                     (if (macro-function-p function)
                                         `(:macro ,name)
                                         name))))
                          (and (not (compiled-function-p function))
                               (let ((body (#"getBody" function)))
                                 (if (and (consp body) (consp (car body)) (eq (caar body) 'jss::invoke-restargs))
                                     `(:interpreted-function ,(concatenate 'string "#\"" (cadar body) "\""))
                                     `(:anonymous-interpreted-function))))
                          (function-name-by-where-loaded-from function)))))))))

(defun function-name-by-where-loaded-from (function)
  "name of last resource - used the loaded-from field from the function to construct the name"
  (let* ((class (java:jcall "getClass" function))
         (loaded-from (sys::get-loaded-from function))
         (name (java:jcall "replace" (java:jcall "getName" class) "org.armedbear.lisp." ""))
         (where (and loaded-from (concatenate 'string (pathname-name loaded-from) "." (pathname-type loaded-from)))))
    `(:anonymous-function ,name ,@(if (sys::arglist function)  (sys::arglist function)) 
                          ,@(if where (list (list :from where))))))
  
(defun maybe-jss-function (f)
  "Determing if function is something list #\"foo\" called as a
  function. If so add to function internal plist :jss-function and the
  name of the java methods"
  (and (find-package :jss)
       (compiled-function-p f)
       (or (getf (sys::function-plist f) :jss-function)
           (let ((internals (function-internal-fields f)))
             (and (= (length internals) 2)
                  (eq (second internals) (intern "INVOKE-RESTARGS" :jss))
                  (stringp (first internals))
                  (setf (getf (sys::function-plist f) :jss-function) (first internals)))))))
         
(defun local-function-p (function)
  "Helper function. Tests whether a function wasn't defined at top
  level based on function-plist annotations"
  (and (and (functionp function) (not (typep function 'generic-function)))
       (let ((plist  (sys::function-plist function)))
         (or (getf plist :internal-to-function)
             (getf plist :method-function)
             (getf plist :method-fast-function)
             (getf plist :slot-initfunction)))))

(defun local-function-owner (function)
  "For local function, return the 'owner' typically the top-level function or clos method"
  (local-function-p function))

(defvar *function-print-object-prefix* "function ")

(defmethod print-object ((f function) stream)
  "Print a function using any-function-name. Requires a patch to
  system::output-ugly-object in order to prevent the function being
  printed by a java primitive"
  (if (or (typep f 'mop::generic-function)
          (typep f 'mop::method))
      (call-next-method)
      (print-unreadable-object (f stream :identity t)
        (let ((name (any-function-name  f)))
          (if (consp name)
              (format stream "~{~a~^ ~}" name)
              (format stream "~a~a" *function-print-object-prefix* name))))))

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
  (when *annotate-function-backlog?* 
    (setq *annotate-function-backlog?* nil)
    (annotate-internal-functions)
    (annotate-clos-methods)
    (annotate-clos-slots)
    (index-function-class-names) ;; still missing some cases e.g. generic functions and method functions
    )
  (index-function-class-names (list function))
  (annotate-internal-functions (list name)))

;; Here we hook into clos in order to have method and slot functions
;; annotated when they are defined.

(defmethod mop::add-direct-method :after (class method)
  (annotate-clos-methods (list method))
)

(defmethod mop::ensure-class-using-class :after (class name  &key direct-slots
                                             direct-default-initargs 
                                             &allow-other-keys)
  (annotate-clos-slots (mop::class-direct-slots (find-class name))))

;; Environments

;; Return a list of the variables and functions in an environment. The form of the list is
;; (kind name value)
;; where kind is either :lexical-variable or :lexical-function :special-variable

(defun environment-parts(env)
  (append
   (loop for binding =  (jss:get-java-field env "vars" t) then (jss:get-java-field binding "next" t)
         while binding
         for symbol = (jss:get-java-field binding "symbol" t)
         for value = (jss:get-java-field binding "value" t)
         for special = (jss:get-java-field binding "specialp" t)
         unless (find symbol them :key 'second)
           collect (list (if special
                             :special-variable
                             :lexical-variable)
                         symbol
                         (if special
                             (symbol-value symbol)
                             value))
             into them
         finally (return them))
   (loop for binding =  (jss:get-java-field env "lastFunctionBinding" t)
           then (jss:get-java-field binding "next" t)
         while binding
         for name = (jss:get-java-field binding "name" t)
         for value = (jss:get-java-field  binding "value" t)
         unless (find name them :key 'second)
           collect (list :lexical-function name value) into them
         finally (return them))))

;; Locals

;; Locals are retrived from envStack, a stack of environments and
;; function call markers distinct from the call stack, one per
;; thread. Locals are only available for interpreted functions.  The
;; envStack is distinct from the call stance because there are function
;; calls which create environments, for instance to special operators
;; like sf_let, that are not in the lisp call stack.

;; A function call marker in this context is an environment with a variable binding
;; whose symbol is nil. Implementing the markers this way means we don't have
;; to deal with different sorts of things on the envStack, which makes the
;; java side of things easier.

;; Environments are chained. So a binding of a new local, by e.g. let, will
;; have a new environment created which has the new binding and a pointer
;; to the environment with the previous binding.

;; Since all environments created are on the envStack, we have to figure
;; out which environment is the one that is the most current for a given
;; function being executed when we land in the debugger.

;; collapse-locals is responsible for filtering out the environments
;; that aren't the most current for each function being executed. It
;; returns a list whose head is the function being executed and whose
;; tail is a list of bindings from environment-parts.

;; have to get the stack contents using this instead of j2list as in
;; that case we get a concurrent modification exception as we iterate
;; through the iterator, when some other function call is made.

(defun stack-to-list (stack)
  (coerce (#"toArray" stack) 'list))

(defun collapse-locals (thread)
  (loop for bindings in (mapcar 'sys::environment-parts
                                (stack-to-list (jss:get-java-field thread "envStack" t)))
        with last-locals
        with last-function
        for binding = (car bindings)
        if (eq (second binding) nil)
          collect (prog1
                      (list last-function  last-locals)
                    (setq last-locals nil)
                    (setq last-function (third binding)))
        else
          do (setq last-locals bindings)))

;; Now that we have the pairings of function-executing and lexicals we need
;; to associate each such function with the stack frame for it being
;; called.  To do that, for each function and locals we find and record the
;; first occurrence of the function in the backtrace.  Functions may appear
;; more than once in the envStack because they have been called more than
;; once.  In addition the envStack will have more functions than there are
;; frames.

;; In order for our envstack association to be an alignment with the stack,
;; the associations must be in ascending order.  That is, if we start at
;; the top of the collapsed envstack, then the frame number each function
;; is associated with must be in ascending order.

;; So, first walk through the associations and remove any frame numbers
;; above that are greater than the index of this association. e.g.  if we
;; have

;; (f1 frame#3 locals)
;; (f2 frame#2 locals)

;; then frame#3 must be a wrong pairing since it is out of order. So we
;; erase those to get

;; (f1 nil locals)
;; (f2 frame#2 locals)

;; Also, since there may be more than one call to a function we might have
;; something like

;; (f1 frame#2 locals)
;; (f2 frame#3 locals)
;; (f1 frame#2 locals)

;; Only the first one is right, so we erases subsequent ones, yielding

;; (f1 frame#2 locals)
;; (f2 frame#3 locals)
;; (f1 nil locals)

;; At this point we now have a some-to-one mapping of functions to frames
;; find-locals takes a backtrace and an index of a frame in that backtrace
;; and returns the locals for the frame. To get it we just search for the
;; first entry that has the required frame number.

;; find-locals still has debugging code in it which will be removed after
;; there has been sufficient testing.

(defvar *debug-locals* nil)

(defun find-locals (index backtrace)
  (let ((thread (jss:get-java-field (nth index backtrace) "thread" t)))
    (and *debug-locals* (print `(:collapse ,thread ,index)))
    (let((collapsed (collapse-locals thread)))
      (and *debug-locals* (map nil 'print collapsed))
      (let ((alignment 
              (loop for function-local-association in (reverse collapsed)
                    with backtrace = (map 'list (if *debug-locals* 'print 'identity) backtrace)
                    for pos = (position (car function-local-association) backtrace
                                        :key (lambda(frame)
                                               (if (typep frame 'sys::lisp-stack-frame)
                                                   (#"getOperator" frame)
                                                   (jss:get-java-field frame "METHOD" t))))
                    collect (list (car function-local-association)
                                  pos
                                  (cdr function-local-association)))))
        (and *debug-locals* (print :erase) (map nil 'print alignment))
        ;; first erasure of out of order frames
        (loop for (nil pos) in alignment
              for i from 0
              when pos do
                (loop for pair in (subseq alignment 0 i)
                      for (nil pos2) = pair
                      unless (null pos2)
                        if (> pos2 pos)
                          do  (setf (second pair) nil)))
        (and *debug-locals* (print :align) (map nil 'print alignment))
        ;; second erasure of duplicate frame numbers
        (loop for (nil pos) in alignment
              for i from 0
              do
                 (loop for pair in (subseq alignment (1+ i))
                       for (nil pos2) = pair
                       unless (null pos2)
                         if (eql pos2 pos)
                           do  (setf (second pair) nil)))
        (and *debug-locals* (map nil 'print alignment))
        (if *debug-locals*
            (print `(:find ,(cddr (find index alignment :key 'second :test 'eql)))))
        ;; finally, look up the locals for the given frame index
        (cddr (find index alignment :key 'second :test 'eql))))))


;; needs to be the last thing. Some interaction with the fasl loader
(pushnew 'fset-hook-annotate-internal-function sys::*fset-hooks*)

(provide :abcl-introspect)

