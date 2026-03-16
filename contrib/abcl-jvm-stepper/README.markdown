ABCL-JVM-STEPPER
================

ABCL-JVM-STEPPER provides a prototype implementation of a stepper as a replacement for the empty cl:step.

It can work on both the plain REPL and Slime

This stepper is designed to work on compiled code unlike ABCL-STEPPER, which works on interpreted code.
You need to instrument first the code before calling the step macro.

It shares all features of ABCL-STEPPER with the exception that it cannot show values of lexicals variables because they
are eliminated after the compilation and it can only show global variable values, this also affects to the features :inspect
and :watch.The feature :globals allow to show the values of the bound symbols linked to special variables in the current package.


Comparison with ABCL-STEPPER:
-----------------------------

Advantages:

- Significantly faster

- Completely isolated at thread level which means you can run multiple stepping sessions in different connections without any interference
between them, with the main thread running as well. This is useful for production environments.

- Can see the values of the arguments to the called functions out of the box

- Works also on Sly


Limitations:

- The stepper allows to jump between function calls after instrumenting the related functions but it can't yet step into their non function calls subforms. A workaround to solve this issue is the use of the utility macro 'stackify' (or something similar) wrapping the selected form to analyze before the instrumentation and the stepper will stop in the form and the user will be able to see the output of the form.
See how to do it in the example session below.

```
(defmacro stackify (form)
  (let ((inner-stack-name
          (gensym
           (format nil "~a -> form#"
                   (with-output-to-string (s) (pprint form s))))))
    `(funcall (sys:fset ',inner-stack-name (lambda () ,form)))))
```

- It can't yet print intermediate multiple values, the first value will be printed instead


Usage:
-------

The contrib exports the following symbols:

jstepper:instrument-compile-function, jstepper:instrument-compile-call, jstepper:instrument-compile-form & jstepper:step

The firsts 3 ones are used, as their names indicates for instrument the code when compiling to be ready for the stepper

The last one is the implementation of cl:step macro.

Attaching a sample session to illustrate the use of the stepper

```
CL-USER(1): (require :asdf)
("uiop" "UIOP" "asdf" "ASDF")
CL-USER(2): (require :abcl-contrib)
("JAVA" "ABCL-CONTRIB")
CL-USER(3): (asdf:load-system :abcl-jvm-stepper)
; Compiling jar:file:///home/alejandrozf/projects/abcl/dist/abcl-contrib.jar!/abcl-jvm-stepper/abcl-jvm-stepper.lisp ...
; (DEFPACKAGE #:ABCL-JVM-STEPPER ...)
; (IN-PACKAGE :ABCL-JVM-STEPPER)
; (DEFPARAMETER *GLOBAL-STOP* ...)
; (DEFPARAMETER *DELIMITED-STOP* ...)
; (DEFPARAMETER *SEEN-ENV-PAIRS* ...)
; (DEFPARAMETER *STEP-COUNTER* ...)
; (DEFPARAMETER *STEP-COUNTER-STACKTRACE* ...)
; (DEFPARAMETER *STEPPER-STOP-PACKAGES* ...)
; (DEFPARAMETER *STEPPER-STOP-SYMBOLS* ...)
; (DEFPARAMETER *STEPPER-WATCH-SYMBOLS* ...)
; (DEFPARAMETER *STEP-NEXT-ACTIVE* ...)
; (DEFPARAMETER *ENV* ...)
; (DEFUN PRINT-HELP ...)
; (DEFUN PRINT-GLOBAL-VARS ...)
; (DEFMACRO WITH-USER-SYMBOL ...)
; (DEFUN STEP-IN-SYMBOL-P ...)
; (DEFUN COMPILER-BACKEND::STEP-HANDLER ...)
; (DEFUN COMPILER-BACKEND::GET-STACK-TRACE-DATA ...)
; (DEFUN COMPILER-BACKEND::AFTER-STEP-HANDLER-1 ...)
; (DEFUN COMPILER-BACKEND::AFTER-STEP-HANDLER-2 ...)
; (DEFUN COMPILER-BACKEND::AFTER-STEP-HANDLER-3 ...)
; (DEFUN GET-CURRENT-ENV ...)
; (DEFMACRO INSTRUMENT-COMPILE-FORM ...)
; (DEFMACRO INSTRUMENT-COMPILE-FUNCTION ...)
; (DEFMACRO INSTRUMENT-COMPILE-CALL ...)
; (DEFMACRO STEP ...)
; Wrote /home/alejandrozf/.cache/common-lisp/abcl-1.9.3-dev-fasl43-linux-x64/home/alejandrozf/projects/abcl/dist/abcl-contrib.jar/abcl-jvm-stepper/abcl-jvm-stepper-tmpE63JKQ1Y.abcl (0.662 seconds)
("LOOP" "COMPILER-TYPES" "KNOWN-FUNCTIONS" "JVM-CLASS-FILE" "KNOWN-SYMBOLS" "DUMP-FORM" "COMPILER-ERROR" "JVM-INSTRUCTIONS" "JVM" "COMPILER-BACKEND" "COMPILER-PASS1" "COMPILER-PASS2" "COMPILE-FILE")
CL-USER(4): (progn (defun loop-1 (a b)
         (loop :for i :below a
               :collect (list a b)))

       (defun loop-2 (a)
         (loop :for i :below a
               :collect i))

       (defun loop-3 (n &optional (times 1))
         (loop :for i :below times
               :collect times)
         (loop-4 (+ n times)))

       (defun loop-4 (k)
         (loop :for i :below k
               :collect (cons (/ i k) (+ i k))))

       (defun test-next (n)
         (loop-1 (1+ n) n)
         (loop-2 (1- n))
         (loop-3 n 3)
         ;; quit (q) here
         (defparameter *test-next-var* (loop :for i :below 7
                                             :collect i))))
TEST-NEXT
CL-USER(5): (progn (jstepper:instrument-compile-function 'loop-1)
                (jstepper:instrument-compile-function 'loop-2)
                (jstepper:instrument-compile-function 'loop-3)
                (jstepper:instrument-compile-function 'loop-4)
                (jstepper:instrument-compile-function 'test-next))
TEST-NEXT
NIL
NIL
CL-USER(6): (jstepper:step (test-next 7))

We are in the stepper mode
Evaluating step 1 -->
(TEST-NEXT 7)
Type ':?' for a list of options
:s

We are in the stepper mode
Evaluating step 2 -->
(LOOP-1 8 7)
Type ':?' for a list of options
:s
step 2 ==> value: ((8 7) (8 7) (8 7) (8 7) (8 7) (8 7) (8 7) (8 7))

We are in the stepper mode
Evaluating step 3 -->
(LOOP-2 6)
Type ':?' for a list of options
:s
step 3 ==> value: (0 1 2 3 4 5)

We are in the stepper mode
Evaluating step 4 -->
(LOOP-3 7 3)
Type ':?' for a list of options
:s

We are in the stepper mode
Evaluating step 5 -->
(LOOP-4 10)
Type ':?' for a list of options
:s
step 5 ==> value: ((0 . 10) (1/10 . 11) (1/5 . 12) (3/10 . 13) (2/5 . 14) (1/2 . 15) (3/5 . 16) (7/10 . 17) (4/5 . 18) (9/10 . 19))
step 4 ==> value: ((0 . 10) (1/10 . 11) (1/5 . 12) (3/10 . 13) (2/5 . 14) (1/2 . 15) (3/5 . 16) (7/10 . 17) (4/5 . 18) (9/10 . 19))
step 1 ==> value: *TEST-NEXT-VAR*
*TEST-NEXT-VAR*
CL-USER(7): (jstepper:step (test-next 7))

We are in the stepper mode
Evaluating step 1 -->
(TEST-NEXT 7)
Type ':?' for a list of options
:c
step 1 ==> value: *TEST-NEXT-VAR*
*TEST-NEXT-VAR*
CL-USER(8): (jstepper:step (test-next 7))

We are in the stepper mode
Evaluating step 1 -->
(TEST-NEXT 7)
Type ':?' for a list of options
:s

We are in the stepper mode
Evaluating step 2 -->
(LOOP-1 8 7)
Type ':?' for a list of options
:g
*TEST-NEXT-VAR*=(0 1 2 3 4 5 6)
Type ':?' for a list of options
:q
NIL
CL-USER(9): (jstepper:step (test-next 7))

We are in the stepper mode
Evaluating step 1 -->
(TEST-NEXT 7)
Type ':?' for a list of options
:?
Type ':g' to see the values of symbols of the current package
Type ':c' to resume the evaluation until the end without the stepper
Type ':n' to resume the evaluation until the next form previously selected to step in
Type ':s' to step into the form
Type ':sn' to step to the next form
Type ':i' to inspect the current value of a global variable or symbol
Type ':b' to add a symbol as a breakpoint to use with next (n)
Type ':r' to remove a symbol used as a breakpoint with next (n)
Type ':d' to remove all breakpoints used with next (n)
Type ':w' to print the value of a global binding in all the steps (watch)
Type ':u' to remove a watched binding (unwatch)
Type ':bt' to show the backtrace
Type ':q' to quit the evaluation and return NIL
Type ':?' for a list of options
:s

We are in the stepper mode
Evaluating step 2 -->
(LOOP-1 8 7)
Type ':?' for a list of options
:s
step 2 ==> value: ((8 7) (8 7) (8 7) (8 7) (8 7) (8 7) (8 7) (8 7))

We are in the stepper mode
Evaluating step 3 -->
(LOOP-2 6)
Type ':?' for a list of options
:s
step 3 ==> value: (0 1 2 3 4 5)

We are in the stepper mode
Evaluating step 4 -->
(LOOP-3 7 3)
Type ':?' for a list of options
:sn
step 4 ==> value: ((0 . 10) (1/10 . 11) (1/5 . 12) (3/10 . 13) (2/5 . 14) (1/2 . 15) (3/5 . 16) (7/10 . 17) (4/5 . 18) (9/10 . 19))
step 1 ==> value: *TEST-NEXT-VAR*
*TEST-NEXT-VAR*
CL-USER(10): (jstepper:step (test-next 7))

We are in the stepper mode
Evaluating step 1 -->
(TEST-NEXT 7)
Type ':?' for a list of options
:b
Type the name of the symbol to use as a breakpoint with next (n): loop-3
Type ':?' for a list of options
:n

We are in the stepper mode
Evaluating step 2 -->
(LOOP-3 7 3)
Type ':?' for a list of options
:s

We are in the stepper mode
Evaluating step 3 -->
(LOOP-4 10)
Type ':?' for a list of options
:s
step 3 ==> value: ((0 . 10) (1/10 . 11) (1/5 . 12) (3/10 . 13) (2/5 . 14) (1/2 . 15) (3/5 . 16) (7/10 . 17) (4/5 . 18) (9/10 . 19))
step 2 ==> value: ((0 . 10) (1/10 . 11) (1/5 . 12) (3/10 . 13) (2/5 . 14) (1/2 . 15) (3/5 . 16) (7/10 . 17) (4/5 . 18) (9/10 . 19))
step 1 ==> value: *TEST-NEXT-VAR*
*TEST-NEXT-VAR*
CL-USER(11): (jstepper:step (test-next 7))

We are in the stepper mode
Evaluating step 1 -->
(TEST-NEXT 7)
Type ':?' for a list of options
:i
Enter the symbol to inspect: *read-base*

10
Type ':?' for a list of options
:bt

(#<LISP-STACK-FRAME (TEST-NEXT 7) {50A8BAEE}>
 #<LISP-STACK-FRAME (SYSTEM::%EVAL (ABCL-JVM-STEPPER:STEP (TEST-NEXT 7))) {55F5A994}>
 #<LISP-STACK-FRAME (EVAL (ABCL-JVM-STEPPER:STEP (TEST-NEXT 7))) {2BA39118}>
 #<LISP-STACK-FRAME (SYSTEM:INTERACTIVE-EVAL (ABCL-JVM-STEPPER:STEP (TEST-NEXT 7))) {4A5AB591}>
 #<LISP-STACK-FRAME (TOP-LEVEL::REPL) {5BE5B757}>
 #<LISP-STACK-FRAME (TOP-LEVEL::TOP-LEVEL-LOOP) {F53B57E}>)
 Type ':?' for a list of options
:d
Removed all symbol breakpoints
Type ':?' for a list of options
:n
step 1 ==> value: *TEST-NEXT-VAR*
*TEST-NEXT-VAR*
CL-USER(12): (jstepper:step (test-next 7))

We are in the stepper mode
Evaluating step 1 -->
(TEST-NEXT 7)
Type ':?' for a list of options
:b
Type the name of the symbol to use as a breakpoint with next (n): loop-2
Type ':?' for a list of options
:r
Type the name of the breakpoint symbol to remove: loop-2
Type ':?' for a list of options
:n
step 1 ==> value: *TEST-NEXT-VAR*
*TEST-NEXT-VAR*
CL-USER(13): (jstepper:step (test-next 7))

We are in the stepper mode
Evaluating step 1 -->
(TEST-NEXT 7)
Type ':?' for a list of options
:s

We are in the stepper mode
Evaluating step 2 -->
(LOOP-1 8 7)
Type ':?' for a list of options
:w
Type the name of the symbol to watch: *read-base*
Type ':?' for a list of options
Watched bindings:
*READ-BASE* = 10
:s
step 2 ==> value: ((8 7) (8 7) (8 7) (8 7) (8 7) (8 7) (8 7) (8 7))

We are in the stepper mode
Evaluating step 3 -->
(LOOP-2 6)
Type ':?' for a list of options
Watched bindings:
*READ-BASE* = 10
:u
Type the name of the symbol to (un)watch: *read-base*
Type ':?' for a list of options
:s
step 3 ==> value: (0 1 2 3 4 5)

We are in the stepper mode
Evaluating step 4 -->
(LOOP-3 7 3)
Type ':?' for a list of options
:
Type ':?' for a list of options
:S

We are in the stepper mode
Evaluating step 5 -->
(LOOP-4 10)
Type ':?' for a list of options
:s
step 5 ==> value: ((0 . 10) (1/10 . 11) (1/5 . 12) (3/10 . 13) (2/5 . 14) (1/2 . 15) (3/5 . 16) (7/10 . 17) (4/5 . 18) (9/10 . 19))
step 4 ==> value: ((0 . 10) (1/10 . 11) (1/5 . 12) (3/10 . 13) (2/5 . 14) (1/2 . 15) (3/5 . 16) (7/10 . 17) (4/5 . 18) (9/10 . 19))
step 1 ==> value: *TEST-NEXT-VAR*
*TEST-NEXT-VAR*
CL-USER(14): ;; use of stackify
(defmacro stackify (form)
  (let ((inner-stack-name (gensym (format nil "~a -> form#" (write-to-string form)))))
    `(funcall (sys:fset ',inner-stack-name (lambda () ,form)))))
STACKIFY
CL-USER(15): (defun test (x)
               (if (zerop x) (list 0) (list 1)))
TEST
CL-USER(16): (jstepper:instrument-compile-function 'test)
TEST
NIL
NIL
CL-USER(17): (jstepper:step (test 1))

We are in the stepper mode
Evaluating step 1 -->
(TEST 1)
Type ':?' for a list of options
:s
step 1 ==> value: (1)
(1)
CL-USER(18): (defun test (x)
               (if (stackify (zerop x)) (stackify (list 0)) (stackify (list x))))
TEST
CL-USER(19): (jstepper:instrument-compile-function 'test)
TEST
NIL
NIL
CL-USER(20): (jstepper:step (test 1))

We are in the stepper mode
Evaluating step 1 -->
(TEST 1)
Type ':?' for a list of options
:s

We are in the stepper mode
Evaluating step 2 -->
((ZEROP X) -> form#2074)
Type ':?' for a list of options
:s
step 2 ==> value: NIL

We are in the stepper mode
Evaluating step 3 -->
((LIST X) -> form#2076)
Type ':?' for a list of options
:s
step 3 ==> value: (1)
step 1 ==> value: (1)
(1)
CL-USER(21): (defun test (x)
               (values x 1 2 3))
TEST
CL-USER(22): (jstepper:instrument-compile-function 'test)
TEST
NIL
NIL
CL-USER(23): (jstepper:step (test 7))

We are in the stepper mode
Evaluating step 1 -->
(TEST 7)
Type ':?' for a list of options
:s
step 1 ==> value: 7
7
1
2
3
CL-USER(24): (require :quicklisp-abcl)
; Compiling jar:file:///home/alejandrozf/projects/abcl/dist/abcl-contrib.jar!/quicklisp/quicklisp-abcl.lisp ...
; (IN-PACKAGE :CL-USER)
; (DEFPACKAGE QUICKLISP-ABCL ...)
; (IN-PACKAGE :QUICKLISP-ABCL)
; (DEFVAR *QUICKLISP-PARENT-DIR* ...)
; (DEFUN QUICKLISP/BOOT/FASLS ...)
; (DEFUN ENSURE-INSTALLATION ...)
; Wrote /home/alejandrozf/.cache/common-lisp/abcl-1.9.3-dev-fasl43-linux-x64/home/alejandrozf/projects/abcl/dist/abcl-contrib.jar/quicklisp/quicklisp-abcl-tmpM3WJ8BZJ.abcl (0.077 seconds)
; Caught RECURSIVE-OPERATE:
;   Deprecated recursive use of (ASDF/OPERATE:OPERATE 'ASDF/LISP-ACTION:LOAD-OP '("quicklisp")) while visiting (ASDF/LISP-ACTION:LOAD-OP "quicklisp-abcl") - please use proper dependencies instead

; Compiling /home/alejandrozf/quicklisp/setup.lisp ...
; (DEFPACKAGE #:QL-SETUP ...)
; (IN-PACKAGE #:QL-SETUP)
; (UNLESS *LOAD-TRUENAME* ...)
; (DEFVAR *QUICKLISP-HOME* ...)
; (DEFUN QMERGE ...)
; (DEFUN QENOUGH ...)
; (DEFVAR *REQUIRED-ASDF-VERSION* ...)
; (DEFUN IMPLEMENTATION-SIGNATURE ...)
; (DEFUN DUMB-STRING-HASH ...)
; (DEFUN ASDF-FASL-PATHNAME ...)
; (DEFUN ENSURE-ASDF-LOADED ...)
; (LET (#) ...)
; (PUSH (QMERGE "quicklisp/") ...)
; (LET (# # ...) ...)
; Wrote /home/alejandrozf/quicklisp/setup.abcl (0.076 seconds)

; Compilation unit finished
;   Caught 1 WARNING condition

NIL
CL-USER(25): (ql:quickload :alexandria)
To load "alexandria":
  Load 1 ASDF system:
    alexandria
; Loading "alexandria"
[package alexandria]..............................
[package alexandria-2]
(:ALEXANDRIA)
CL-USER(26): (jstepper:instrument-compile-call (asdf:load-system :alexandria :force t))
; Compiling /home/alejandrozf/quicklisp/dists/quicklisp/software/alexandria-20220707-git/alexandria-1/package.lisp ...
; (DEFPACKAGE :ALEXANDRIA ...)
; Wrote /home/alejandrozf/.cache/common-lisp/abcl-1.9.3-dev-fasl43-linux-x64/home/alejandrozf/quicklisp/dists/quicklisp/software/alexandria-20220707-git/alexandria-1/package-tmpIQR49OH.abcl (0.011 seconds)
; Compiling /home/alejandrozf/quicklisp/dists/quicklisp/software/alexandria-20220707-git/alexandria-1/definitions.lisp ...
; (IN-PACKAGE :ALEXANDRIA)
; (DEFUN %REEVALUATE-CONSTANT ...)
; (DEFMACRO DEFINE-CONSTANT ...)
; Wrote /home/alejandrozf/.cache/common-lisp/abcl-1.9.3-dev-fasl43-linux-x64/home/alejandrozf/quicklisp/dists/quicklisp/software/alexandria-20220707-git/alexandria-1/definitions-tmp1K1M3XPY.abcl (0.031 seconds)
; Compiling /home/alejandrozf/quicklisp/dists/quicklisp/software/alexandria-20220707-git/alexandria-1/binding.lisp ...
; (IN-PACKAGE :ALEXANDRIA)
; (DEFMACRO IF-LET ...)
; (DEFMACRO WHEN-LET ...)
; (DEFMACRO WHEN-LET* ...)
; Wrote /home/alejandrozf/.cache/common-lisp/abcl-1.9.3-dev-fasl43-linux-x64/home/alejandrozf/quicklisp/dists/quicklisp/software/alexandria-20220707-git/alexandria-1/binding-tmpTF28Z58Y.abcl (0.034 seconds)
; Compiling /home/alejandrozf/quicklisp/dists/quicklisp/software/alexandria-20220707-git/alexandria-1/strings.lisp ...
; (IN-PACKAGE :ALEXANDRIA)
; (DEFTYPE STRING-DESIGNATOR ...)
; Wrote /home/alejandrozf/.cache/common-lisp/abcl-1.9.3-dev-fasl43-linux-x64/home/alejandrozf/quicklisp/dists/quicklisp/software/alexandria-20220707-git/alexandria-1/strings-tmpBZ5Y27S8.abcl (0.002 seconds)
; Compiling /home/alejandrozf/quicklisp/dists/quicklisp/software/alexandria-20220707-git/alexandria-1/conditions.lisp ...
; (IN-PACKAGE :ALEXANDRIA)
; (DEFUN REQUIRED-ARGUMENT ...)
; (DEFINE-CONDITION SIMPLE-STYLE-WARNING ...)
; (DEFUN SIMPLE-STYLE-WARNING ...)
; (DEFINE-CONDITION SIMPLE-READER-ERROR ...)
; (DEFUN SIMPLE-READER-ERROR ...)
; (DEFINE-CONDITION SIMPLE-PARSE-ERROR ...)
; (DEFUN SIMPLE-PARSE-ERROR ...)
; (DEFINE-CONDITION SIMPLE-PROGRAM-ERROR ...)
; (DEFUN SIMPLE-PROGRAM-ERROR ...)
; (DEFMACRO IGNORE-SOME-CONDITIONS ...)
; (DEFMACRO UNWIND-PROTECT-CASE ...)
; Wrote /home/alejandrozf/.cache/common-lisp/abcl-1.9.3-dev-fasl43-linux-x64/home/alejandrozf/quicklisp/dists/quicklisp/software/alexandria-20220707-git/alexandria-1/conditions-tmpSM15NSP1.abcl (0.045 seconds)
; Compiling /home/alejandrozf/quicklisp/dists/quicklisp/software/alexandria-20220707-git/alexandria-1/symbols.lisp ...
; (IN-PACKAGE :ALEXANDRIA)
; (DECLAIM (INLINE ENSURE-SYMBOL))
; (DEFUN ENSURE-SYMBOL ...)
; (DEFUN MAYBE-INTERN ...)
; (DECLAIM (INLINE FORMAT-SYMBOL))
; (DEFUN FORMAT-SYMBOL ...)
; (DEFUN MAKE-KEYWORD ...)
; (DEFUN MAKE-GENSYM ...)
; (DEFUN MAKE-GENSYM-LIST ...)
; (DEFUN SYMBOLICATE ...)
; Wrote /home/alejandrozf/.cache/common-lisp/abcl-1.9.3-dev-fasl43-linux-x64/home/alejandrozf/quicklisp/dists/quicklisp/software/alexandria-20220707-git/alexandria-1/symbols-tmpYUTLOGU5.abcl (0.035 seconds)
; Compiling /home/alejandrozf/quicklisp/dists/quicklisp/software/alexandria-20220707-git/alexandria-1/macros.lisp ...
; (IN-PACKAGE :ALEXANDRIA)
; (DEFMACRO WITH-GENSYMS ...)
; (DEFMACRO WITH-UNIQUE-NAMES ...)
; (DEFMACRO ONCE-ONLY ...)
; (DEFUN PARSE-BODY ...)
; (DEFUN PARSE-ORDINARY-LAMBDA-LIST ...)
; (DEFUN EXPAND-DESTRUCTURING-CASE ...)
; (DEFMACRO DESTRUCTURING-CASE ...)
; (DEFMACRO DESTRUCTURING-CCASE ...)
; (DEFMACRO DESTRUCTURING-ECASE ...)
; (DOLIST (NAME #) ...)
; Wrote /home/alejandrozf/.cache/common-lisp/abcl-1.9.3-dev-fasl43-linux-x64/home/alejandrozf/quicklisp/dists/quicklisp/software/alexandria-20220707-git/alexandria-1/macros-tmpP663YFNK.abcl (0.1 seconds)
; Compiling /home/alejandrozf/quicklisp/dists/quicklisp/software/alexandria-20220707-git/alexandria-1/functions.lisp ...
; (IN-PACKAGE :ALEXANDRIA)
; (DECLAIM (INLINE ENSURE-FUNCTION))
; (DECLAIM (FTYPE # ...))
; (DEFUN ENSURE-FUNCTION ...)
; (DEFINE-MODIFY-MACRO ENSURE-FUNCTIONF/1 ...)
; (DEFMACRO ENSURE-FUNCTIONF ...)
; (DEFUN DISJOIN ...)
; (DEFUN CONJOIN ...)
; (DEFUN COMPOSE ...)
; (DEFINE-COMPILER-MACRO COMPOSE ...)
; (DEFUN MULTIPLE-VALUE-COMPOSE ...)
; (DEFINE-COMPILER-MACRO MULTIPLE-VALUE-COMPOSE ...)
; (DECLAIM (INLINE CURRY ...))
; (DEFUN CURRY ...)
; (DEFINE-COMPILER-MACRO CURRY ...)
; (DEFUN RCURRY ...)
; (DEFINE-COMPILER-MACRO RCURRY ...)
; (DECLAIM (NOTINLINE CURRY ...))
; (DEFMACRO NAMED-LAMBDA ...)
; Wrote /home/alejandrozf/.cache/common-lisp/abcl-1.9.3-dev-fasl43-linux-x64/home/alejandrozf/quicklisp/dists/quicklisp/software/alexandria-20220707-git/alexandria-1/functions-tmpZC83RYKP.abcl (0.107 seconds)
; Compiling /home/alejandrozf/quicklisp/dists/quicklisp/software/alexandria-20220707-git/alexandria-1/lists.lisp ...
; (IN-PACKAGE :ALEXANDRIA)
; (DECLAIM (INLINE SAFE-ENDP))
; (DEFUN SAFE-ENDP ...)
; (DEFUN ALIST-PLIST ...)
; (DEFUN PLIST-ALIST ...)
; (DECLAIM (INLINE RACONS))
; (DEFUN RACONS ...)
; (DEFINE-ALIST-GET ASSOC-VALUE ...)
; (DEFINE-ALIST-GET RASSOC-VALUE ...)
; (DEFUN MALFORMED-PLIST ...)
; (DEFMACRO DOPLIST ...)
; (DEFINE-MODIFY-MACRO APPENDF ...)
; (DEFINE-MODIFY-MACRO NCONCF ...)
; (DEFINE-MODIFY-MACRO UNIONF ...)
; (DEFINE-MODIFY-MACRO NUNIONF ...)
; (DEFINE-MODIFY-MACRO REVERSEF ...)
; (DEFINE-MODIFY-MACRO NREVERSEF ...)
; (DEFUN CIRCULAR-LIST ...)
; (DEFUN CIRCULAR-LIST-P ...)
; (DEFUN CIRCULAR-TREE-P ...)
; (DEFUN PROPER-LIST-P ...)
; (DEFTYPE PROPER-LIST ...)
; (DEFUN CIRCULAR-LIST-ERROR ...)
; (DEF PROPER-LIST-LENGTH ...)
; (DEF LASTCAR ...)
; (DEF (SETF LASTCAR) ...)
; (DEFUN MAKE-CIRCULAR-LIST ...)
; (DEFTYPE CIRCULAR-LIST ...)
; (DEFUN ENSURE-CAR ...)
; (DEFUN ENSURE-CONS ...)
; (DEFUN ENSURE-LIST ...)
; (DEFUN REMOVE-FROM-PLIST ...)
; (DEFUN DELETE-FROM-PLIST ...)
; (DEFINE-MODIFY-MACRO REMOVE-FROM-PLISTF ...)
; (DEFINE-MODIFY-MACRO DELETE-FROM-PLISTF ...)
; (DECLAIM (INLINE SANS))
; (DEFUN SANS ...)
; (DEFUN MAPPEND ...)
; (DEFUN SETP ...)
; (DEFUN SET-EQUAL ...)
; (DEFUN MAP-PRODUCT ...)
; (DEFUN FLATTEN ...)
; Wrote /home/alejandrozf/.cache/common-lisp/abcl-1.9.3-dev-fasl43-linux-x64/home/alejandrozf/quicklisp/dists/quicklisp/software/alexandria-20220707-git/alexandria-1/lists-tmpD5W3WD61.abcl (0.178 seconds)
; Compiling /home/alejandrozf/quicklisp/dists/quicklisp/software/alexandria-20220707-git/alexandria-1/types.lisp ...
; (IN-PACKAGE :ALEXANDRIA)
; (DEFTYPE ARRAY-INDEX ...)
; (DEFTYPE ARRAY-LENGTH ...)
; (FROB FIXNUM ...)
; (FROB INTEGER)
; (FROB RATIONAL)
; (FROB REAL)
; (FROB FLOAT)
; (FROB SHORT-FLOAT)
; (FROB SINGLE-FLOAT)
; (FROB DOUBLE-FLOAT)
; (FROB LONG-FLOAT)
; (DEFUN OF-TYPE ...)
; (DEFINE-COMPILER-MACRO OF-TYPE ...)
; (DECLAIM (INLINE TYPE=))
; (DEFUN TYPE= ...)
; (DEFINE-MODIFY-MACRO COERCEF ...)
; Wrote /home/alejandrozf/.cache/common-lisp/abcl-1.9.3-dev-fasl43-linux-x64/home/alejandrozf/quicklisp/dists/quicklisp/software/alexandria-20220707-git/alexandria-1/types-tmpN9QY3PMS.abcl (0.154 seconds)
; Compiling /home/alejandrozf/quicklisp/dists/quicklisp/software/alexandria-20220707-git/alexandria-1/io.lisp ...
; (IN-PACKAGE :ALEXANDRIA)
; (DEFMACRO WITH-OPEN-FILE* ...)
; (DEFUN DEFAULT-ELEMENT-TYPE ...)
; (DEFMACRO WITH-INPUT-FROM-FILE ...)
; (DEFMACRO WITH-OUTPUT-TO-FILE ...)
; (DEFUN READ-STREAM-CONTENT-INTO-STRING ...)
; (DEFUN READ-FILE-INTO-STRING ...)
; (DEFUN WRITE-STRING-INTO-FILE ...)
; (DEFUN READ-STREAM-CONTENT-INTO-BYTE-VECTOR ...)
; (DEFUN READ-FILE-INTO-BYTE-VECTOR ...)
; (DEFUN WRITE-BYTE-VECTOR-INTO-FILE ...)
; (DEFUN COPY-FILE ...)
; (DEFUN COPY-STREAM ...)
; Wrote /home/alejandrozf/.cache/common-lisp/abcl-1.9.3-dev-fasl43-linux-x64/home/alejandrozf/quicklisp/dists/quicklisp/software/alexandria-20220707-git/alexandria-1/io-tmpMLIGK48E.abcl (0.087 seconds)
; Compiling /home/alejandrozf/quicklisp/dists/quicklisp/software/alexandria-20220707-git/alexandria-1/hash-tables.lisp ...
; (IN-PACKAGE :ALEXANDRIA)
; (DEFMACRO ENSURE-GETHASH ...)
; (DEFUN COPY-HASH-TABLE ...)
; (DECLAIM (INLINE MAPHASH-KEYS))
; (DEFUN MAPHASH-KEYS ...)
; (DECLAIM (INLINE MAPHASH-VALUES))
; (DEFUN MAPHASH-VALUES ...)
; (DEFUN HASH-TABLE-KEYS ...)
; (DEFUN HASH-TABLE-VALUES ...)
; (DEFUN HASH-TABLE-ALIST ...)
; (DEFUN HASH-TABLE-PLIST ...)
; (DEFUN ALIST-HASH-TABLE ...)
; (DEFUN PLIST-HASH-TABLE ...)
; Wrote /home/alejandrozf/.cache/common-lisp/abcl-1.9.3-dev-fasl43-linux-x64/home/alejandrozf/quicklisp/dists/quicklisp/software/alexandria-20220707-git/alexandria-1/hash-tables-tmp4MJGTR1L.abcl (0.062 seconds)
; Compiling /home/alejandrozf/quicklisp/dists/quicklisp/software/alexandria-20220707-git/alexandria-1/control-flow.lisp ...
; (IN-PACKAGE :ALEXANDRIA)
; (DEFUN EXTRACT-FUNCTION-NAME ...)
; (DEFUN GENERATE-SWITCH-BODY ...)
; (DEFMACRO SWITCH ...)
; (DEFMACRO ESWITCH ...)
; (DEFMACRO CSWITCH ...)
; (DEFMACRO WHICHEVER ...)
; (DEFMACRO XOR ...)
; (DEFMACRO NTH-VALUE-OR ...)
; (DEFMACRO MULTIPLE-VALUE-PROG2 ...)
; Wrote /home/alejandrozf/.cache/common-lisp/abcl-1.9.3-dev-fasl43-linux-x64/home/alejandrozf/quicklisp/dists/quicklisp/software/alexandria-20220707-git/alexandria-1/control-flow-tmpNBJUAK4K.abcl (0.059 seconds)
; Compiling /home/alejandrozf/quicklisp/dists/quicklisp/software/alexandria-20220707-git/alexandria-1/arrays.lisp ...
; (IN-PACKAGE :ALEXANDRIA)
; (DEFUN COPY-ARRAY ...)
; Wrote /home/alejandrozf/.cache/common-lisp/abcl-1.9.3-dev-fasl43-linux-x64/home/alejandrozf/quicklisp/dists/quicklisp/software/alexandria-20220707-git/alexandria-1/arrays-tmp53U2EVSK.abcl (0.007 seconds)
; Compiling /home/alejandrozf/quicklisp/dists/quicklisp/software/alexandria-20220707-git/alexandria-1/sequences.lisp ...
; (IN-PACKAGE :ALEXANDRIA)
; (DECLAIM (INLINE COPY-SEQUENCE ...))
; (DEFUN SEQUENCE-OF-LENGTH-P ...)
; (DEFUN ROTATE-TAIL-TO-HEAD ...)
; (DEFUN ROTATE-HEAD-TO-TAIL ...)
; (DEFUN ROTATE ...)
; (DEFUN SHUFFLE ...)
; (DEFUN RANDOM-ELT ...)
; (DECLAIM (INLINE REMOVE/SWAPPED-ARGUMENTS))
; (DEFUN REMOVE/SWAPPED-ARGUMENTS ...)
; (DEFINE-MODIFY-MACRO REMOVEF ...)
; (DECLAIM (INLINE DELETE/SWAPPED-ARGUMENTS))
; (DEFUN DELETE/SWAPPED-ARGUMENTS ...)
; (DEFINE-MODIFY-MACRO DELETEF ...)
; (DEFTYPE PROPER-SEQUENCE ...)
; (WHEN (AND # ...) ...)
; (DEFUN EMPTYP ...)
; (DEFUN LENGTH= ...)
; (DEFINE-COMPILER-MACRO LENGTH= ...)
; (DEFUN COPY-SEQUENCE ...)
; (DEFUN FIRST-ELT ...)
; (DEFUN (SETF FIRST-ELT) ...)
; (DEFUN LAST-ELT ...)
; (DEFUN (SETF LAST-ELT) ...)
; (DEFUN STARTS-WITH-SUBSEQ ...)
; (DEFUN ENDS-WITH-SUBSEQ ...)
; (DEFUN STARTS-WITH ...)
; (DEFUN ENDS-WITH ...)
; (DEFUN MAP-COMBINATIONS ...)
; (DEFUN MAP-PERMUTATIONS ...)
; (DEFUN MAP-DERANGEMENTS ...)
; (DECLAIM (NOTINLINE SEQUENCE-OF-LENGTH-P))
; (DEFUN EXTREMUM ...)
; Wrote /home/alejandrozf/.cache/common-lisp/abcl-1.9.3-dev-fasl43-linux-x64/home/alejandrozf/quicklisp/dists/quicklisp/software/alexandria-20220707-git/alexandria-1/sequences-tmpEJABE4XO.abcl (0.176 seconds)
; Compiling /home/alejandrozf/quicklisp/dists/quicklisp/software/alexandria-20220707-git/alexandria-1/numbers.lisp ...
; (IN-PACKAGE :ALEXANDRIA)
; (DECLAIM (INLINE CLAMP))
; (DEFUN CLAMP ...)
; (DEFUN GAUSSIAN-RANDOM ...)
; (DECLAIM (INLINE IOTA))
; (DEFUN IOTA ...)
; (DECLAIM (INLINE MAP-IOTA))
; (DEFUN MAP-IOTA ...)
; (DECLAIM (INLINE LERP))
; (DEFUN LERP ...)
; (DECLAIM (INLINE MEAN))
; (DEFUN MEAN ...)
; (DEFUN MEDIAN ...)
; (DECLAIM (INLINE VARIANCE))
; (DEFUN VARIANCE ...)
; (DECLAIM (INLINE STANDARD-DEVIATION))
; (DEFUN STANDARD-DEVIATION ...)
; (DEFINE-MODIFY-MACRO MAXF ...)
; (DEFINE-MODIFY-MACRO MINF ...)
; (DEFCONSTANT +FACTORIAL-BISECTION-RANGE-LIMIT+ ...)
; (DEFCONSTANT +FACTORIAL-DIRECT-MULTIPLICATION-LIMIT+ ...)
; (DEFUN %MULTIPLY-RANGE ...)
; (DECLAIM (INLINE FACTORIAL))
; (DEFUN %FACTORIAL ...)
; (DEFUN FACTORIAL ...)
; (DEFUN BINOMIAL-COEFFICIENT ...)
; (DEFUN SUBFACTORIAL ...)
; (DEFUN COUNT-PERMUTATIONS ...)
; Wrote /home/alejandrozf/.cache/common-lisp/abcl-1.9.3-dev-fasl43-linux-x64/home/alejandrozf/quicklisp/dists/quicklisp/software/alexandria-20220707-git/alexandria-1/numbers-tmpKJ4ECJ5I.abcl (0.114 seconds)
; Compiling /home/alejandrozf/quicklisp/dists/quicklisp/software/alexandria-20220707-git/alexandria-1/features.lisp ...
; (IN-PACKAGE :ALEXANDRIA)
; (DEFUN FEATUREP ...)
; Wrote /home/alejandrozf/.cache/common-lisp/abcl-1.9.3-dev-fasl43-linux-x64/home/alejandrozf/quicklisp/dists/quicklisp/software/alexandria-20220707-git/alexandria-1/features-tmpZB81WZD2.abcl (0.009 seconds)
; Compiling /home/alejandrozf/quicklisp/dists/quicklisp/software/alexandria-20220707-git/alexandria-2/package.lisp ...
; (IN-PACKAGE :CL-USER)
; (DEFPACKAGE :ALEXANDRIA-2 ...)
; Wrote /home/alejandrozf/.cache/common-lisp/abcl-1.9.3-dev-fasl43-linux-x64/home/alejandrozf/quicklisp/dists/quicklisp/software/alexandria-20220707-git/alexandria-2/package-tmp7KOQDEE1.abcl (0.006 seconds)
; Compiling /home/alejandrozf/quicklisp/dists/quicklisp/software/alexandria-20220707-git/alexandria-2/arrays.lisp ...
; (IN-PACKAGE :ALEXANDRIA-2)
; (DEFUN DIM-IN-BOUNDS-P ...)
; (DEFUN ROW-MAJOR-INDEX ...)
; (DEFUN RMAJOR-TO-INDICES ...)
; Wrote /home/alejandrozf/.cache/common-lisp/abcl-1.9.3-dev-fasl43-linux-x64/home/alejandrozf/quicklisp/dists/quicklisp/software/alexandria-20220707-git/alexandria-2/arrays-tmp180J21SX.abcl (0.029 seconds)
; Compiling /home/alejandrozf/quicklisp/dists/quicklisp/software/alexandria-20220707-git/alexandria-2/control-flow.lisp ...
; (IN-PACKAGE :ALEXANDRIA-2)
; (DEFUN LINE-UP-ITER ...)
; (DEFMACRO LINE-UP-FIRST ...)
; (DEFMACRO LINE-UP-LAST ...)
; Wrote /home/alejandrozf/.cache/common-lisp/abcl-1.9.3-dev-fasl43-linux-x64/home/alejandrozf/quicklisp/dists/quicklisp/software/alexandria-20220707-git/alexandria-2/control-flow-tmpHYB1C0U0.abcl (0.012 seconds)
; Compiling /home/alejandrozf/quicklisp/dists/quicklisp/software/alexandria-20220707-git/alexandria-2/sequences.lisp ...
; (IN-PACKAGE :ALEXANDRIA-2)
; (DEFUN SUBSEQ* ...)
; Wrote /home/alejandrozf/.cache/common-lisp/abcl-1.9.3-dev-fasl43-linux-x64/home/alejandrozf/quicklisp/dists/quicklisp/software/alexandria-20220707-git/alexandria-2/sequences-tmpYQ2EMB3S.abcl (0.005 seconds)
; Compiling /home/alejandrozf/quicklisp/dists/quicklisp/software/alexandria-20220707-git/alexandria-2/lists.lisp ...
; (IN-PACKAGE :ALEXANDRIA-2)
; (DEFUN DELETE-FROM-PLIST* ...)
; Wrote /home/alejandrozf/.cache/common-lisp/abcl-1.9.3-dev-fasl43-linux-x64/home/alejandrozf/quicklisp/dists/quicklisp/software/alexandria-20220707-git/alexandria-2/lists-tmp1UO5B306.abcl (0.006 seconds)
T
CL-USER(27): (jstepper:step (alexandria:standard-deviation '(1 3 5 7 11)))

We are in the stepper mode
Evaluating step 1 -->
(STANDARD-DEVIATION (1 3 5 7 11))
Type ':?' for a list of options
:s

We are in the stepper mode
Evaluating step 2 -->
(VARIANCE (1 3 5 7 11) BIASED T)
Type ':?' for a list of options
:sn
step 2 ==> value: 296/25
step 1 ==> value: 3.4409301
3.4409301
CL-USER(28):
```

Have fun!
