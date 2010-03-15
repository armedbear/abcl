Misc
====

Miscellaneous fragments of topics on aspects of ABCL that should be
collected into a more systematic documentation someday.

# Java FFI

## Calling Lisp from Java

Note: If you are wondering where the symbols are from in the following
text, the entire ABCL Lisp system resides in the org.armedbear.lisp
package, so they are in this package.

Per JVM, there can only ever be a single Lisp interpreter.  This is
started by calling the static method Interpreter.createInstance().

    Interpreter interpreter = Interpreter.createInstance();

If this method has already been invoked in the lifetime of the current
Java process it will return null, so if you are writing Java whose
lifecycle is a bit out of your control (like in a Java servlet), a
safer invocation pattern might be:
      
    Interpreter interpreter = Interpreter.getInstance();
    if (interpreter == null) {
      interpreter = Interpreter.createInstance();
    }

The Lisp EVAL primitive may be simply passed strings for evaluation,
as follows
   
   String line = "(load \"file.lisp\")";
   LispObject result = interpreter.eval(line);

Notice that all possible return values from an arbitrary Lisp
computation are collapsed into a single return value.  Doing useful
further computation on the LispObject depends on knowing what the
result of the computation might be, usually involves some amount
of instanceof introspection, and forms a whole topic to itself
(c.f. "Interpreting a LispObject in Java").  

Using EVAL involves the Lisp interpreter.  Lisp functions may be
directly invoked by Java method calls as follows.  One simply locates
the pacakge containing the symbol, then obtains a reference to the
symbol, and then invokes the execute() method with the desired
parameters.

   interpreter.eval("(defun foo (msg) (format nil \"You told me '~A'~%\" msg))");
   Package pkg = Packages.findPackage("CL-USER");
   Symbol foo = pkg.findAccessibleSymbol("FOO"); 
   Function fooFunction = (Function)foo.getSymbolFunction();
   JavaObject parameter = new JavaObject("Lisp is fun!");
   LispObject result = fooFunction.execute(parameter);
// How to get the "naked string value"?
   System.out.prinln("The result was " + result.writeToString()); 


If one 


## Interpreting a LispObject in Java

### LispObject as boolean

If the LispObject a generalized boolean values, one can use
getBooleanValue() to convert to Java:

    


### LispObject is a list

If LispObject is a list, it will have the type Cons.  One can then use
the copyToArray[] to make things a bit more suitable for Java
iteration.

   LispObject result = interpreter.eval("'(1 2 4 5)");
   if (result instanceof Cons) {
     LispObject array[] = ((Cons)result.copyToArray());
     ...
   }
