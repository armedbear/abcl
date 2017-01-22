(in-package :system)

(require :jss) ;; for now

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; I don't understand the algorithm that sys:backtrace uses, which seems
;; broken, so here's an alternative.

;; The lisp portion of the stack backtrace is computed as it is now. It
;; will have invoke-debugger at the top then some java stack frames that
;; abcl pushes (the "i don't understand") and then the rest of the
;; backtrace. We trim that by popping off the invoke-debugger and java
;; stack frames, leaving just lisp frames.

;; If there's a java exception. In that case we compare the stacktrace of
;; the exception to the java stack trace and grab the top part of it
;; that's unique to the exception. We prepend this to the lisp stack
;; trace.

;; The result will be that we will *not* see the call to invoke debugger,
;; or any of the swank handling, just what (I think) is relative.

;; What still needs to be investigated is how this plays in cases where
;; there are callbacks to lisp from java.

;; A good test to see the difference would be 

;; (#"replaceAll" "" "(?o" "")

;; which should now show the calls within the regex code leading to
;; the exception.

(defvar *use-old-backtrace* nil "set to t to fall back to the standard backtrace")

(defvar *hide-swank-frames* t "set to nil if you want to see debugger internal frames")

(defvar *unwelcome-java-frames*
  '("sun.reflect.Native.*AccessorImpl\\..*"
    "sun.reflect.Delegating.*AccessorImpl\\..*"
    "sun.reflect.Generated.*Accessor\\d+\\.invoke")
  "if a java frame matches any of these patterns, don't show it"
  )

(defvar *caught-frames* nil "When backtrace is called, it sets this to
  the java stack frames that are unique to the java exception, which is
  then subsequently used by slime to mark them")

(defun swankish-frame (frame)
  "hackish test for whether a frame is some internal function from swank"
  (let ((el (car (sys::frame-to-list frame))))
    (let ((package
	    (cond ((symbolp el) 
		   (symbol-package el)
		   (package-name (symbol-package el)))
		  ;; hack! really I mean anything with a function plist
		  ((eq (type-of el) 'compiled-function)
		   (let ((owner (getf (function-plist  el) :internal-to-function)))
		     (if (and (symbolp owner)
			      (symbol-package owner))
			 (package-name 
			  (symbol-package owner))
			 "")))
		  (t ""))))
      (and package (#"matches" package "SWANK.*")))))

(defun javaframe (java-stack-frame)
  "Return the java StackFrame instance"
  (if (java::java-object-p  java-stack-frame)
      java-stack-frame
      (#"get" (load-time-value (java::jclass-field  "org.armedbear.lisp.JavaStackFrame" "javaFrame")) java-stack-frame)))
    
(defun stackframe-head (frame &optional with-method)
  "If a lisp frame, the function (symbol or function). In a java frame the class name, with method if with-method is t"
  (if (typep frame 'lisp-stack-frame)
      (#"getOperator" frame)
      (let ((frame (if (typep frame 'java-stack-frame) (javaframe frame) frame)))
	(if with-method 
	    (concatenate 'string (#"getClassName" frame) "." (#"getMethodName" frame))
	    (#"getClassName" frame)))))

(defun backtrace-invoke-debugger-position (stacktrace)
  "Position of the call to invoke-debugger"
  (let ((looking-for `(invoke-debugger ,#'invoke-debugger)))
    (position-if (lambda(e) (memq (#"getOperator" e) looking-for)) stacktrace)))

(defun swank-p ()
  "are we running with slime/swank? This should work without swank too"
  (find-package 'swank))

(defun repl-loop-position (stacktrace start)
  "Position of the frame starting the repl at this level"
  (if (swank-p)
      (position-if (lambda(e) (eq (stackframe-head e) (intern "SLDB-LOOP" 'swank))) stacktrace :start start)
      (position-if (lambda(e) (eq (stackframe-head e) 'debug-loop)) stacktrace :start start)
      ))

(defun last-internal-calls-position (stacktrace)
  "Some java frames are replicates of the lisp stack frame. This gets
  the position of the closest to top non-user lisp call. It should leave
  intact frames corresponding to cases where a piece of lisp implemented
  in java calls another lisp function"
  (let ((pos (position-if (lambda(e)
			    (and (not (typep e 'lisp-stack-frame))
				 (not (member  (#"getMethodName" (javaframe e)) '("execute" "evalCall" "eval" "funcall" "apply") :test 'equal))))
			  stacktrace :from-end t)))
    pos))

(defun java-frame-segment (stacktrace)
  "Returns the bounds of the section of the backtrace that have been added with pushJavaStackFrame"
  (let ((start (position-if (lambda(e) (typep e 'java-stack-frame)) stacktrace)))
    (and start (list start (position-if (lambda(e) (typep e 'lisp-stack-frame)) stacktrace :start start)))))

(defun splice-out (sequence from to)
  "remove elements from->to from sequence"
  (append (subseq sequence 0 from) (subseq sequence to)))

(defun splice-out-java-stack-duplicating-lisp-stack (stacktrace)
  "cut out a section of java frames, maximally ending at the first lisp stack frame hit"
  (let ((extra-java-frames-pos (last-internal-calls-position stacktrace)))
    (let ((spliced
	    (if extra-java-frames-pos
		(append (subseq stacktrace 0 extra-java-frames-pos)
			(let ((lisp-frame-pos (position 'lisp-stack-frame stacktrace :key 'type-of :start extra-java-frames-pos)))
			  (and lisp-frame-pos
			      (subseq stacktrace 
				      (position 'lisp-stack-frame stacktrace :key 'type-of :start extra-java-frames-pos)))))
		stacktrace)))
      spliced)))

(defun difference-between-exception-stacktrace-and-after-caught-stacktrace (condition)
  "When there's a java exception, the condition has the stack trace as
   it was when the exception was thrown. Our backtrace is after it is
   caught. This function gets the difference - the frames unique to the
   exception"
  (let* ((exception-stack-trace (coerce (#"getStackTrace" (java::java-exception-cause condition)) 'list))
	 (debugger-stack-trace 
	   (coerce (subseq exception-stack-trace
			   (position (#"getName" (#"getClass" #'invoke-debugger))
				     (#"getStackTrace" (#"currentThread" 'Thread))
				     :key #"getClassName"
				     :test 'string-equal))
		   'list)))
    (subseq exception-stack-trace
	    0 (position-if (lambda(frame) (find frame debugger-stack-trace :test (lambda(a b ) (eql (#"hashCode" a) (#"hashCode" b)))))
			 exception-stack-trace))))

(defun remove-unsightly-java-frames (stacktrace)
  "Remove uninformative java frames, typically bits of the internals of the java implementation"
  (remove-if (lambda(frame) 
	       (member (stackframe-head frame t) *unwelcome-java-frames* :test #"matches"))
	     stacktrace))

  ;; 3: (invoke-debugger #<java-exception org.semanticweb.owlapi.reasoner.InconsistentOntologyException: Inconsistent ontology {8F97F7A}>)
  ;; 4: org.armedbear.lisp.Lisp.error(Lisp.java:385)

  ;; 5: (invoke-debugger #<reader-error {2FE2E7E6}>)
  ;; 6: (error #<reader-error {2FE2E7E6}>)
  ;; 7: (#<local-function in eval-region {D6D0A1B}> #<reader-error {2FE2E7E6}>)
  ;; 8: (signal #<reader-error {2FE2E7E6}>)
  ;; 9: org.armedbear.lisp.Lisp.error(Lisp.java:385)

(defun lisp-stack-exception-catching-frames (stacktrace)
  "The frames corresponding to ABCL's internal handling of an exception"
  (and (eq (stackframe-head (car stacktrace)) 'invoke-debugger)
       (let ((error-position (position "org.armedbear.lisp.Lisp.error" stacktrace 
			     :key (lambda(e) (stackframe-head e t))
			     :test 'equal)))
	 (if error-position
	     (subseq stacktrace 0 (1+ error-position))
	     (list (car stacktrace))
	     ))))

(defun splice-out-spurious-error-frames (stacktrace)
  "if there are nested exceptions sometimes there are extra (error),
   <function>, (signal) frames.  we only want the first error. Remove
   repeated ones.  Illiustrated by first getting an errors with an
   inconsistent ontology and then calling (read-from-string \"#<\") to
   generate a reader error. Get rid of these. Finally, if the next
   next frame after error is signal of the same condition, those two
   frames are also removed"
  (let ((error-position (position 'error stacktrace :key 'stackframe-head)))
    (if (and error-position (> (length stacktrace) (+ error-position 3)))
	(loop with trash = 0 
	      for pos = error-position then next
	      for next = (+ pos 3)
	      until  (not (eq (stackframe-head (nth next stacktrace)) 'error))
	      do (incf trash 3)
	      finally (return 
			(let ((spliced (if (> trash 1)
					   (splice-out  stacktrace (1+ error-position) (+ error-position trash 1))
					   stacktrace)))
			  (if (and (eq (stackframe-head (nth (+ error-position 2) spliced))  'signal)
				   (eq (second (frame-to-list (nth error-position spliced)))
				       (second (frame-to-list (nth (+ error-position 2) spliced)))))
			      (splice-out  spliced (1+ error-position) (+ error-position 3))
			      stacktrace))))
	stacktrace)))
  
(defun new-backtrace (condition)
  "New implementation of backtrace that tries to clean up the stack
  trace shown when an error occurs. There are a bunch of
  idiosyncrasies of what sys:backtrace generates which land up
  obscuring what the problem is, or at least making it more of a hunt
  than one would want. This backtrace tries to show only stuff I think
  matters - user function calls and, when there's an exception, calls
  inside the lisp implementation leading to the error"
  (if *use-old-backtrace*
      (backtrace) 
      (let* ((lisp-stacktrace (#"backtrace" (threads::current-thread) 0))
	     (invoke-pos (backtrace-invoke-debugger-position lisp-stacktrace))
	     (repl-loop-pos (repl-loop-position lisp-stacktrace invoke-pos)))
	(let ((narrowed-lisp-stacktrace 
		(splice-out-java-stack-duplicating-lisp-stack (subseq lisp-stacktrace invoke-pos (and repl-loop-pos (1+ repl-loop-pos))))))
	  (when *hide-swank-frames*
	    (let ((swank-start (position-if 'swankish-frame narrowed-lisp-stacktrace)))
	      (and swank-start
		   (setq narrowed-lisp-stacktrace
			 (append 
			  (subseq narrowed-lisp-stacktrace 0 swank-start)
			  (if repl-loop-pos (last narrowed-lisp-stacktrace) nil))))))
	  (setq narrowed-lisp-stacktrace (splice-out-spurious-error-frames narrowed-lisp-stacktrace))
	  (if (typep condition 'java::java-exception)
	      (progn
		(let* ((delta (difference-between-exception-stacktrace-and-after-caught-stacktrace condition))
		       (cleaned (splice-out-java-stack-duplicating-lisp-stack (remove-unsightly-java-frames delta)))
		       (exception-frames (lisp-stack-exception-catching-frames narrowed-lisp-stacktrace)))
		  (setq *caught-frames* delta)
		  (let ((result (append exception-frames 
					(mapcar (lambda(frame) (jss::new 'javastackframe frame)) cleaned)
					(subseq narrowed-lisp-stacktrace (length exception-frames)))))
		    result
		    )))
	      narrowed-lisp-stacktrace)))))

#|
(defmethod ho ((a t))  (read-from-string "(#\"setLambdaName\" #<g466140 {168C36ED}> '(flet a))"))
(defmethod no ((a t))  (read-from-string "(#\"setLambdaName\" #<g466140 {168C36ED}> '(flet a))"))
(defmethod fo ()  (ho 1) (no 1))
(defun bar () (fo))
(defun foo () (funcall #'bar))
(defun baz () (foo))


caused by reader-error

Checking for execute isn't enough.
Symbol.execute might be good

So maybe modify:
Find invoke-debugger position
go down stack until you reach a symbol.execute, then skip rest of string of java frames.

Right now I skip from invoke-debugger to next list but because signal is there it gets stuck.

  5: (invoke-debugger #<reader-error {4BFF7154}>)
below here ok
  6: (error #<reader-error {4BFF7154}>)
  7: (#<local-function in eval-region {AC27B6F}> #<reader-error {4BFF7154}>)
  8: (signal #<reader-error {4BFF7154}>)
  9: org.armedbear.lisp.Lisp.error(Lisp.java:385)
 10: org.armedbear.lisp.LispReader$22.execute(LispReader.java:350)
 11: org.armedbear.lisp.Stream.readDispatchChar(Stream.java:813)
 12: org.armedbear.lisp.LispReader$6.execute(LispReader.java:130)
 13: org.armedbear.lisp.Stream.processChar(Stream.java:588)
 14: org.armedbear.lisp.Stream.readList(Stream.java:755)
 15: org.armedbear.lisp.LispReader$3.execute(LispReader.java:88)
 16: org.armedbear.lisp.Stream.processChar(Stream.java:588)
 17: org.armedbear.lisp.Stream.readPreservingWhitespace(Stream.java:557)
 18: org.armedbear.lisp.Stream.readPreservingWhitespace(Stream.java:566)
 19: org.armedbear.lisp.Stream.read(Stream.java:501)
above here is ok

below here junk
 20: org.armedbear.lisp.Stream$16.execute(Stream.java:2436)
 21: org.armedbear.lisp.Symbol.execute(Symbol.java:826)
 22: org.armedbear.lisp.LispThread.execute(LispThread.java:851)
 23: org.armedbear.lisp.swank_528.execute(swank.lisp:1732)
 24: org.armedbear.lisp.Symbol.execute(Symbol.java:803)
 25: org.armedbear.lisp.LispThread.execute(LispThread.java:814)
 26: org.armedbear.lisp.swank_repl_47.execute(swank-repl.lisp:270)
 27: org.armedbear.lisp.LispThread.execute(LispThread.java:798)
 28: org.armedbear.lisp.swank_repl_48.execute(swank-repl.lisp:283)
 29: org.armedbear.lisp.Symbol.execute(Symbol.java:803)
 30: org.armedbear.lisp.LispThread.execute(LispThread.java:814)
 31: org.armedbear.lisp.swank_repl_46.execute(swank-repl.lisp:270)
 32: org.armedbear.lisp.LispThread.execute(LispThread.java:798)
 33: org.armedbear.lisp.swank_272.execute(swank.lisp:490)
 34: org.armedbear.lisp.Symbol.execute(Symbol.java:814)
 35: org.armedbear.lisp.LispThread.execute(LispThread.java:832)
 36: org.armedbear.lisp.swank_repl_45.execute(swank-repl.lisp:270)
 37: org.armedbear.lisp.LispThread.execute(LispThread.java:798)
 38: abcl_fcbf3596_211f_4d83_bc8b_e11e207b8d21.execute(Unknown Source)
 39: org.armedbear.lisp.LispThread.execute(LispThread.java:814)
 40: org.armedbear.lisp.Lisp.funcall(Lisp.java:172)
 41: org.armedbear.lisp.Primitives$pf_apply.execute(Primitives.java:2827)
end junk

 42: (read #S(system::string-input-stream) nil #S(system::string-input-stream))
 43: (swank::eval-region "(#\"setLambdaName\" #<g466140 {168C36ED}> '(flet a))\n")
 44: (#<local-function in repl-eval {B47713B}>)



From a compiled function looks different
  0: (error #<reader-error {7ED23D2A}>)
  1: (#<local-function in eval-region {3FBB9CBD}> #<reader-error {7ED23D2A}>)
  2: (signal #<reader-error {7ED23D2A}>)
  3: org.armedbear.lisp.Lisp.error(Lisp.java:385)
  4: org.armedbear.lisp.LispReader$22.execute(LispReader.java:350)
  5: org.armedbear.lisp.Stream.readDispatchChar(Stream.java:813)
  6: org.armedbear.lisp.LispReader$6.execute(LispReader.java:130)
  7: org.armedbear.lisp.Stream.processChar(Stream.java:588)
  8: org.armedbear.lisp.Stream.readList(Stream.java:755)
  9: org.armedbear.lisp.LispReader$3.execute(LispReader.java:88)
 10: org.armedbear.lisp.Stream.processChar(Stream.java:588)
 11: org.armedbear.lisp.Stream.readPreservingWhitespace(Stream.java:557)
 12: org.armedbear.lisp.Stream.readPreservingWhitespace(Stream.java:566)
 13: org.armedbear.lisp.Stream.read(Stream.java:501) <- this is probably where we want the stack to stop.

Looks like symbol.execute 
 14: org.armedbear.lisp.Stream$15.execute(Stream.java:2387) <= %read from string
 15: org.armedbear.lisp.Symbol.execute(Symbol.java:867)
 16: org.armedbear.lisp.LispThread.execute(LispThread.java:918)
 17: org.armedbear.lisp.read_from_string_1.execute(read-from-string.lisp:33)
 18: org.armedbear.lisp.CompiledClosure.execute(CompiledClosure.java:98)
 19: org.armedbear.lisp.Symbol.execute(Symbol.java:803)
 20: org.armedbear.lisp.LispThread.execute(LispThread.java:814)
 21: abcl_2ad63c53_52f1_460b_91c2_1b153251d9f3.execute(Unknown Source)
 22: org.armedbear.lisp.LispThread.execute(LispThread.java:798)
 23: org.armedbear.lisp.Lisp.evalCall(Lisp.java:572)
 24: org.armedbear.lisp.Lisp.eval(Lisp.java:543)
 25: org.armedbear.lisp.Primitives$pf__eval.execute(Primitives.java:345)
 26: (system::%read-from-string "(#\"setLambdaName\" #<g466140 {168C36ED}> '(flet a))" t nil 0 nil nil)
 27: (read-from-string "(#\"setLambdaName\" #<g466140 {168C36ED}> '(flet a))")
 28: (system::bar)

|#

  
#|
Don't really want 456. Ban them outright? No - make a list
  4: sun.reflect.NativeMethodAccessorImpl.invoke0(Native Method)
  5: sun.reflect.NativeMethodAccessorImpl.invoke(NativeMethodAccessorImpl.java:62)
  6: sun.reflect.DelegatingMethodAccessorImpl.invoke(DelegatingMethodAccessorImpl.java:43)
  7: java.lang.reflect.Method.invoke(Method.java:497)
|#

;; (#"setLambdaName" #<g466140 {168C36ED}> '(flet a))
;; reader error is still ugly. Maybe anything that calls signal.

(provide :stacktrace)
