Lisp FFI
========

    Mark Evenson
    Created:  15-FEB-2010
    Modified: 18-MAR-2010

FFI stands for "Foreign Function Interface", which is the way the
contemporary Lisp world refers to methods of "calling out" from Lisp
into "foreign" langauges and envrionments.  This document describes
the various ways that one interacts with Lisp world of Abcl from Java,
considering the hosted Lisp as the "Foreign Function" that needs to be
"Interfaced".

# Lisp FFI

## Calling Lisp from Java

Note: As the entire ABCL Lisp system resides in the org.armedbear.lisp
package the following code snippets do not show the relevant import
statements in the interest of brevity.

Per JVM, there can only ever be a single Lisp interpreter.  This is
started by calling the static method `Interpreter.createInstance()`.

    Interpreter interpreter = Interpreter.createInstance();

If this method has already been invoked in the lifetime of the current
Java process it will return null, so if you are writing Java whose
lifecycle is a bit out of your control (like in a Java servlet), a
safer invocation pattern might be:

    Interpreter interpreter = Interpreter.getInstance();
    if (interpreter == null) {
      interpreter = Interpreter.createInstance();
    }

The Lisp `EVAL` primitive may be simply passed strings for evaluation,
as follows
   
    String line = "(load \"file.lisp\")";
    LispObject result = interpreter.eval(line);

Notice that all possible return values from an arbitrary Lisp
computation are collapsed into a single return value.  Doing useful
further computation on the `LispObject` depends on knowing what the
result of the computation might be, usually involves some amount
of instanceof introspection, and forms a whole topic to itself
(c.f. [Introspecting a LispObject](#introspecting)).  

Using `EVAL` involves the Lisp interpreter.  Lisp functions may be
directly invoked by Java method calls as follows.  One simply locates
the package containing the symbol, then obtains a reference to the
symbol, and then invokes the `execute()` method with the desired
parameters.

    interpreter.eval("(defun foo (msg) (format nil \"You told me '~A'~%\" msg))");
    Package pkg = Packages.findPackage("CL-USER");
    Symbol foo = pkg.findAccessibleSymbol("FOO"); 
    Function fooFunction = (Function)foo.getSymbolFunction();
    JavaObject parameter = new JavaObject("Lisp is fun!");
    LispObject result = fooFunction.execute(parameter);
    // How to get the "naked string value"?
    System.out.prinln("The result was " + result.writeToString()); 

If one is calling an primitive function in the CL package the syntax
becomes considerably simpler if we can locate the instance of
definition in the ABCL source, we can invoke the symbol directly.  To
tell if a `LispObject` contains a reference to a symbol.

    boolean nullp(LispObject object) {
      LispObject result = Primitives.NULL.execute(object);
      if (result == NIL) {
        return false;
      }
      return true;
   }

<a name="interpreting"/>
## Introspecting a LispObject 

We present various patterns for introspecting an an arbitrary
`LispObject` which can represent the result of every Lisp evaluation
into semantics that Java can meaniningfully deal with.

### LispObject as boolean

If the LispObject a generalized boolean values, one can use
`getBooleanValue()` to convert to Java:

     LispObject object = Symbol.NIL;
     boolean javaValue = object.getBooleanValue();

Although since in Lisp, any value other than NIL means "true", the
use of Java equality it quite a bit easier and more optimal:

    boolean javaValue = (object != Symbol.NIL);

### LispObject is a list

If LispObject is a list, it will have the type `Cons`.  One can then use
the `copyToArray[]` to make things a bit more suitable for Java
iteration.

    LispObject result = interpreter.eval("'(1 2 4 5)");
    if (result instanceof Cons) {
      LispObject array[] = ((Cons)result.copyToArray());
      ...
    }
    
A more Lispy way to iterated down a list is to use the `cdr()` access
function just as like one would traverse a list in Lisp:;

    LispObject result = interpreter.eval("'(1 2 4 5)");
    while (result != Symbol.NIL) {
      doSomething(result.car());
      result = result.cdr();
    }

