JSS
===

Created by Alan Ruttenburg


JSS stands for either "Java Simple Syntax" or "Java Syntax Sucks",
depending on your mood.

The dynamic dispatch of the java.lang.reflect package is used to make
it real easy, if perhaps less efficient, to write Java code since you
don't need to be bothered with imports, or with figuring out which
method to call.  The only time that you need to know a class name is
when you want to call a static method, or a constructor, and in those
cases, you only need to know enough of the class name that is unique
wrt to the classes on your classpath.

Java methods look like this: #"toString". Java classes are represented
as symbols, which are resolved to the appropriate java class
name. When ambiguous, you need to be more specific. A simple example:

    (let ((sw (new 'StringWriter)))
       (#"write" sw "Hello ")
       (#"write" sw "World")
       (print (#"toString" sw)))

What's happened here? First, all the classes in all the jars in the
classpath have been collected.  For each class a.b.C.d, we have
recorded that b.c.d, b.C.d, C.d, c.d, and d potentially refer to this
class. In your call to new, as long as the symbol can refer to only
one class, we use that class. In this case, it is
java.io.StringWriter. You could also have written (new
'io.stringwriter), (new '|io.StringWriter|), (new
'java.io.StringWriter)...

the call (#"write" sw "Hello "), uses the code in invoke.java to
call the method named "write" with the arguments sw and "Hello ". 
JSS figures out the right java method to call, and calls it.

If you want to do a raw java call, use #0"toString". Raw calls
return their results as Java objects, avoiding doing the usual Java
object to Lisp object conversions that ABCL does.

(with-constant-signature ((name jname raw?)*) &body body)
binds a macro which expands to a jcall, promising that the same method 
will be called every time. Use this if you are making a lot of calls and 
want to avoid the overhead of a the dynamic dispatch. 
e.g. (with-constant-signature ((tostring "toString")) 
        (time (dotimes (i 10000) (tostring "foo"))))
runs about 3x faster than (time (dotimes (i 10000) (#"toString" "foo")))

(with-constant-signature ((tostring "toString" t)) ...) will cause the
toString to be a raw java call. see get-all-jar-classnames below for
an example.
 
Implementation is that the first time the function is called, the
method is looked up based on the arguments passed, and thereafter
that method is called directly.  Doesn't work for static methods at
the moment (lazy)

(japropos string) finds all class names matching string

(jcmn class-name) lists the names of all methods for the class
