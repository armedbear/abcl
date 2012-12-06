JSS
===

Created by Alan Ruttenberg


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
name. When ambiguous, you need to be more specific. A simple example
from CL-USER:

    (require :jss)
    (in-package :jss)
    (let ((sw (new 'StringWriter)))
       (#"write" sw "Hello ")
       (#"write" sw "World")
       (print (#"toString" sw)))

What's happened here? First, all the classes in all the jars in the
classpath have been collected.  For each class a.b.C.d, we have
recorded that b.c.d, b.C.d, C.d, c.d, and d potentially refer to this
class. In your call to new, as long as the symbol can refer to only
one class, we use that class. In this case, it is
java.io.StringWriter. You could also have written 

     (new 'io.stringwriter)

or      
     (new '|io.StringWriter|)

or     
     (new 'java.io.StringWriter)

The call 

     (#"write" sw "Hello ")
     
uses the code in invoke.java to call the method named "write" with
the arguments sw and "Hello ".  JSS figures out the right java method
to call, and calls it.

An interactive restart is available to resolve class ambiguity.

Static calls are possible as well with the #" macro, but the
first argument MUST BE A SYMBOL to distinguish 

     (#"getProperties" "java.lang.System")
     
from 

     (#"getProperties" 'java.lang.System)     
     
The first attempts to call a method on the java.lang.String object
with the contents "java.lang.System", which results in an error, while
the second invokes the static java.lang.System.getProperties() method.     

If you want to do a raw java call, use #0"toString". Raw calls
return their results as Java objects, avoiding doing the usual Java
object to Lisp object conversions that ABCL does.


    (with-constant-signature ((name jname raw?)*) &body body)
    
binds a macro which expands to a jcall, promising that the same method 
will be called every time. Use this if you are making a lot of calls and 
want to avoid the overhead of a the dynamic dispatch. 
e.g.
 
    (with-constant-signature ((tostring "toString")) 
        (time (dotimes (i 10000) (tostring "foo"))))

runs about three times faster than 
 
    (time (dotimes (i 10000) (#"toString" "foo")))

So, something like

    (with-constant-signature ((tostring "toString" t)) ...) 
    
will cause the toString to be a raw java call. See
JSS::GET-ALL-JAR-CLASSNAMES for an example.
 
Implementation is that the first time the function is called, the
method is looked up based on the arguments passed, and thereafter
that method is called directly.  Doesn't work for static methods at
the moment (lazy)

(japropos string) finds all class names matching string

(jcmn class-name) lists the names of all methods for the class

Compatibility
-------------

The function ENSURE-COMPATIBILITY attempts to provide a compatibility
mode to existing users of JSS by importing the necessary symbols into
CL-USER.

Some notes on other compatibility issues:

*classpath-manager* 

   Since we are no longer using Beanshell, this is no longer present.
   For obtaining the current classloader use JAVA:*CLASSLOADER*.
   
# API

  1.0 
    Equivalent to Alan Ruttenberg's version included with the original
    [lsw]().  
    
[lsw]: http://mumble.net:8080/svn/lsw/trunk/
[lsw2]: let-me-google-that-for-you    
    

  3.0 
     In the JSS package loaded from [abcl-contrib]() 
     
abcl-contrib: http://svn.common-lisp.net/armedbear/trunk/abcl/contrib/     
   
# Colophon

<> dc:created "2005" ;
   dc:author "Mark <evenson.not.org@gmail.com>";
   dc:revised "06-DEC-2012" ;
   <> abcl:documents <urn:abcl.org/release/1.1.0/contrib/jss#3.0.5" .

   
