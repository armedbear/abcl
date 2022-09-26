ABCL-STEPPER
============

ABCL-STEPPER provides a working implementation of an stepper as a replacement for the empty cl:step

Some characteristics:

- For intepreted code, it won't step into compiled functions

- For plain REPL only, it is still not working with Sly/Slime, although it can be used as well in the inferior lisp buffer of those systems
When called inside Sly/Slime main REPL it will only show print a message and return the form without any stepping

'?' will print a minimal help

'i' can inspect variables and symbols (case-insensitive when inspecting)

'c' will resume the evaluation until the end without the stepper

's' will resume the evaluation until the next form to be analyzed

'l' will show the local bindings for variables and function
in the current environment passed to the current form to evaluate

'q': The quit q feature will abort the evaluation in the stepper
and return NIL. This is useful to avoid running the remaining
forms in the code when the user wants to leave the
stepper, specially if the rest of the program is doing costly
operations.

'n' allows to jump the next (n) symbol:
The next n feature allow to stop the stepper only when the
interpreter is analyzing one of the symbols specified in the
list of sys::stepper-stop-symbols or any of the exported
symbols presented in any of the list of packages specified in
sys::stepper-stop-packages. These variables will have
initially the value NIL and if they are not modified, next will
behave exactly as continue. It is useful when we want to
step large or complex code and avoid stepping every form in
order to jump only to the interested ones.

Usage:

Attaching a sample session to illustrate the use of the stepper

```
CL-USER(1): (require :abcl-contrib)
("uiop" "UIOP" "asdf" "ASDF" "JAVA" "ABCL-CONTRIB")
CL-USER(2): (require :abcl-stepper)
("LOOP" "COMPILER-TYPES" "KNOWN-FUNCTIONS" "JVM-CLASS-FILE" "KNOWN-SYMBOLS" "DUMP-FORM" "COMPILER-ERROR" "JVM-INSTRUCTIONS" "JVM" "COMPILER-PASS1" "COMPILER-PASS2" "ABCL-STEPPER")
CL-USER(3): (defparameter *some-var* 1)
*SOME-VAR*
CL-USER(4): (defun test ()
  (let ((*some-var* nil)
        (x 3))
    (list *some-var* 3)))
TEST
CL-USER(5): (stepper:step (test))
We are in the stepper mode
Evaluating :
TEST
With args:
NIL
Type '?' for a list of options
i
Type the name of the symbol: *some-var*
Type the name of the package ('-' for current package): -
1
Type '?' for a list of options
s
We are in the stepper mode
Evaluating :
BLOCK
With args:
(TEST (LET ((*SOME-VAR* NIL) (X 3)) (LIST *SOME-VAR* 3)))
Type '?' for a list of options
s
We are in the stepper mode
Evaluating :
LET
With args:
(((*SOME-VAR* NIL) (X 3)) (LIST *SOME-VAR* 3))
Type '?' for a list of options
s
We are in the stepper mode
Evaluating :
LIST
With args:
(*SOME-VAR* 3)
Type '?' for a list of options
i
Type the name of the symbol: x
Type the name of the package ('-' for current package): -
3
Type '?' for a list of options
i
Type the name of the symbol: *some-var*
Type the name of the package ('-' for current package): -
NIL
Type '?' for a list of options
s
We are in the stepper mode
Evaluating :
SYSTEM:%SET-STEPPER-OFF
With args:
NIL
Type '?' for a list of options
i
Type the name of the symbol: *some-var*
Type the name of the package ('-' for current package): -
1
Type '?' for a list of options
c
(NIL 3)
CL-USER(6): (stepper:step (flet ((flet1 (n) (+ n n)))
        (flet ((flet2 (n) (+ 2 (flet1 n))))
          (flet2 2))))
We are in the stepper mode
Evaluating :
FLET
With args:
(((FLET1 (N) (+ N N))) (FLET ((FLET2 (N) (+ 2 (FLET1 N)))) (FLET2 2)))
Type '?' for a list of options
s
We are in the stepper mode
Evaluating :
FLET
With args:
(((FLET2 (N) (+ 2 (FLET1 N)))) (FLET2 2))
Type '?' for a list of options
l
Showing the values of variable bindings.
From inner to outer scopes:
Showing the values of functions bindings.
From inner to outer scopes:
FLET1=#<FUNCTION #<(FLET FLET1) {330B05F3}> {330B05F3}>
Type '?' for a list of options
s
We are in the stepper mode
Evaluating :
(FLET FLET2)
With args:
(2)
Type '?' for a list of options
l
Showing the values of variable bindings.
From inner to outer scopes:
Showing the values of functions bindings.
From inner to outer scopes:
FLET2=#<FUNCTION #<(FLET FLET2) {857E3F2}> {857E3F2}>
FLET1=#<FUNCTION #<(FLET FLET1) {330B05F3}> {330B05F3}>
Type '?' for a list of options
s
We are in the stepper mode
Evaluating :
BLOCK
With args:
(FLET2 (+ 2 (FLET1 N)))
Type '?' for a list of options
l
Showing the values of variable bindings.
From inner to outer scopes:
N=2
Showing the values of functions bindings.
From inner to outer scopes:
FLET1=#<FUNCTION #<(FLET FLET1) {330B05F3}> {330B05F3}>
Type '?' for a list of options
n
6
CL-USER(7): (stepper:step (progn
        ((lambda (c d) (list c d)) 3 7)))
We are in the stepper mode
Evaluating :
PROGN
With args:
(((LAMBDA (C D) (LIST C D)) 3 7))
Type '?' for a list of options
s
We are in the stepper mode
Evaluating :
#<FUNCTION (LAMBDA (C D)) {74D34EF}>
With args:
(3 7)
Type '?' for a list of options
l
Showing the values of variable bindings.
From inner to outer scopes:
Showing the values of functions bindings.
From inner to outer scopes:
Type '?' for a list of options
s
We are in the stepper mode
Evaluating :
LIST
With args:
(C D)
Type '?' for a list of options
l
Showing the values of variable bindings.
From inner to outer scopes:
D=7
C=3
Showing the values of functions bindings.
From inner to outer scopes:
Type '?' for a list of options
n
(3 7)
CL-USER(8): (stepper:step (let ((a 1))
        (block whatever (list 1 2))
        a))
We are in the stepper mode
Evaluating :
LET
With args:
(((A 1)) (BLOCK WHATEVER (LIST 1 2)) A)
Type '?' for a list of options
s
We are in the stepper mode
Evaluating :
BLOCK
With args:
(WHATEVER (LIST 1 2))
Type '?' for a list of options
l
Showing the values of variable bindings.
From inner to outer scopes:
A=1
Showing the values of functions bindings.
From inner to outer scopes:
Type '?' for a list of options
s
We are in the stepper mode
Evaluating :
LIST
With args:
(1 2)
Type '?' for a list of options
q
NIL
CL-USER(9): (stepper:step (let ((a 1))
        (block whatever (list 1 2))
        a))
We are in the stepper mode
Evaluating :
LET
With args:
(((A 1)) (BLOCK WHATEVER (LIST 1 2)) A)
Type '?' for a list of options
c
1
CL-USER(10): (stepper:step (let ((a 1))
        (let ((a 2) (b 1))
          (- a b))
        (+ a 3 7)))
We are in the stepper mode
Evaluating :
LET
With args:
(((A 1)) (LET ((A 2) (B 1)) (- A B)) (+ A 3 7))
Type '?' for a list of options
s
We are in the stepper mode
Evaluating :
LET
With args:
(((A 2) (B 1)) (- A B))
Type '?' for a list of options
l
Showing the values of variable bindings.
From inner to outer scopes:
A=1
Showing the values of functions bindings.
From inner to outer scopes:
Type '?' for a list of options
s
We are in the stepper mode
Evaluating :
-
With args:
(A B)
Type '?' for a list of options
l
Showing the values of variable bindings.
From inner to outer scopes:
B=1
A=2
A=1
Showing the values of functions bindings.
From inner to outer scopes:
Type '?' for a list of options
s
We are in the stepper mode
Evaluating :
+
With args:
(A 3 7)
Type '?' for a list of options
l
Showing the values of variable bindings.
From inner to outer scopes:
A=1
Showing the values of functions bindings.
From inner to outer scopes:
Type '?' for a list of options
c
11
CL-USER(11): (stepper:step (progn (defparameter *azf* 1)))
We are in the stepper mode
Evaluating :
PROGN
With args:
((DEFPARAMETER *AZF* 1))
Type '?' for a list of options
c
*AZF*
CL-USER(12): *azf*
1
CL-USER(13): (defpackage step-next (:use :cl))
#<PACKAGE STEP-NEXT>
CL-USER(14): (in-package :step-next)
#<PACKAGE STEP-NEXT>
STEP-NEXT(15): (defun loop-1 (a b)
  (loop :for i :below a
        :collect (list a b)))
LOOP-1
STEP-NEXT(16): (defun loop-2 (a)
  (loop :for i :below a
        :collect i))
LOOP-2
STEP-NEXT(17): (defun loop-3 (n &optional (times 1))
  (loop :for i :below times
        :collect times))
LOOP-3
STEP-NEXT(18): (defun test-next (n)
  (loop-1 (1+ n) n)
  (loop-2 (1- n))
  (loop-3 n 3)
  ;; quit (q) here
  (defparameter *test-next-var* (loop :for i :below (expt 10 6)
                                      :collect i)))
TEST-NEXT
STEP-NEXT(19): (push 'loop-1 stepper::*stepper-stop-symbols*)
(LOOP-1)
STEP-NEXT(20): (export 'loop-3)
T
STEP-NEXT(21): (push 'step-next stepper::*stepper-stop-packages*)
(STEP-NEXT)
STEP-NEXT(22):
(stepper:step (test-next 7))
We are in the stepper mode
Evaluating :
TEST-NEXT
With args:
(7)
Type '?' for a list of options
n
We are in the stepper mode
Evaluating :
LOOP-1
With args:
((1+ N) N)
Type '?' for a list of options
n
We are in the stepper mode
Evaluating :
LOOP-3
With args:
(N 3)
Type '?' for a list of options
q
NIL
STEP-NEXT(23): (assert (not (boundp '*test-next-var*)))
NIL
STEP-NEXT(24):
```

For use it with ASDF systems we can use asdf:load-source-op:

```
CL-USER(1): (asdf:operate 'asdf:load-source-op :alexandria)
#<ASDF/LISP-ACTION:LOAD-SOURCE-OP >
#<ASDF/PLAN:SEQUENTIAL-PLAN {245AD6EA}>
CL-USER(6): (step (alexandria:plist-hash-table '(:a 1 :b 2 :c 3)))
We are in the stepper mode
Evaluating:
ALEXANDRIA:PLIST-HASH-TABLE
With args:
('(:A 1 :B 2 :C 3))
Type '?' for a list of options
s
We are in the stepper mode
Evaluating:
QUOTE
With args:
((:A 1 :B 2 :C 3))
Type '?' for a list of options
s
We are in the stepper mode
Evaluating:
BLOCK
With args:
(ALEXANDRIA:PLIST-HASH-TABLE (LET ((ALEXANDRIA::TABLE (APPLY #'MAKE-HASH-TABLE ALEXANDRIA::HASH-TABLE-INITARGS))) (DO ((ALEXANDRIA::TAIL ALEXANDRIA::PLIST (CDDR ALEXANDRIA::TAIL))) ((NOT ALEXANDRIA::TAIL)) (LET ((#:KEY14153 (CAR ALEXANDRIA::TAIL)) (#:HASH-TABLE14154 ALEXANDRIA::TABLE)) (MULTIPLE-VALUE-BIND (#:VALUE14155 #:PRESENTP14156) (GETHASH #:KEY14153 #:HASH-TABLE14154) (IF #:PRESENTP14156 (VALUES #:VALUE14155 #:PRESENTP14156) (VALUES (SYSTEM:PUTHASH #:KEY14153 #:HASH-TABLE14154 (CADR ALEXANDRIA::TAIL)) NIL))))) ALEXANDRIA::TABLE))
Type '?' for a list of options
s
We are in the stepper mode
Evaluating:
LET
With args:
(((ALEXANDRIA::TABLE (APPLY #'MAKE-HASH-TABLE ALEXANDRIA::HASH-TABLE-INITARGS))) (DO ((ALEXANDRIA::TAIL ALEXANDRIA::PLIST (CDDR ALEXANDRIA::TAIL))) ((NOT ALEXANDRIA::TAIL)) (LET ((#:KEY14153 (CAR ALEXANDRIA::TAIL)) (#:HASH-TABLE14154 ALEXANDRIA::TABLE)) (MULTIPLE-VALUE-BIND (#:VALUE14155 #:PRESENTP14156) (GETHASH #:KEY14153 #:HASH-TABLE14154) (IF #:PRESENTP14156 (VALUES #:VALUE14155 #:PRESENTP14156) (VALUES (SYSTEM:PUTHASH #:KEY14153 #:HASH-TABLE14154 (CADR ALEXANDRIA::TAIL)) NIL))))) ALEXANDRIA::TABLE)
Type '?' for a list of options
s
We are in the stepper mode
Evaluating:
APPLY
With args:
(#'MAKE-HASH-TABLE ALEXANDRIA::HASH-TABLE-INITARGS)
Type '?' for a list of options
s
We are in the stepper mode
Evaluating:
FUNCTION
With args:
(MAKE-HASH-TABLE)
Type '?' for a list of options
c
#<EQL HASH-TABLE 3 entries, 11 buckets {33172E22}>
CL-USER(2):
```
