/*
 * Java.java
 *
 * Copyright (C) 2002-2006 Peter Graves, Andras Simon
 * $Id$
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
 *
 * As a special exception, the copyright holders of this library give you
 * permission to link this library with independent modules to produce an
 * executable, regardless of the license terms of these independent
 * modules, and to copy and distribute the resulting executable under
 * terms of your choice, provided that you also meet, for each linked
 * independent module, the terms and conditions of the license of that
 * module.  An independent module is a module which is not derived from
 * or based on this library.  If you modify this library, you may extend
 * this exception to your version of the library, but you are not
 * obligated to do so.  If you do not wish to do so, delete this
 * exception statement from your version.
 */

package org.armedbear.lisp;

import static org.armedbear.lisp.Lisp.*;

import java.lang.reflect.Array;
import java.lang.reflect.Constructor;
import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.text.MessageFormat;
import java.util.*;

public final class Java
{
    static final Map<Class,Symbol> registeredExceptions =
       new HashMap<Class,Symbol>();

    private static final LispClass java_exception = LispClass.findClass(Symbol.JAVA_EXCEPTION);

    static boolean isJavaException(LispClass lc)
    {
        return lc.subclassp(java_exception);
    }

    private static final Primitive ENSURE_JAVA_OBJECT = new pf_ensure_java_object();
    @DocString(name="ensure-java-object", args="obj",
    doc="Ensures OBJ is wrapped in a JAVA-OBJECT, wrapping it if necessary.")
    private static final class pf_ensure_java_object extends Primitive
    {
        pf_ensure_java_object()
        {
            super("ensure-java-object", PACKAGE_JAVA, true);
        }

        @Override
        public LispObject execute(LispObject obj) {
            return obj instanceof JavaObject ? obj : new JavaObject(obj);
        }
    };

    private static final Primitive REGISTER_JAVA_EXCEPTION = new pf_register_java_exception();
    @DocString(name="register-java-exception", // => T
    args="exception-name condition-symbol",
    doc="Registers the Java Throwable named by the symbol EXCEPTION-NAME as the condition " +
        "designated by CONDITION-SYMBOL.  Returns T if successful, NIL if not.")
    private static final class pf_register_java_exception extends Primitive
    {
        pf_register_java_exception()
        {
            super("register-java-exception", PACKAGE_JAVA, true);
        }

        @Override
        public LispObject execute(LispObject className, LispObject symbol)

        {
            LispClass lispClass = (LispClass) LispClass.findClass(symbol, true);
            // FIXME Signal a continuable error if the exception is already registered.
            if (isJavaException(lispClass)) {
                registeredExceptions.put(classForName(className.getStringValue()),
                                         (Symbol)symbol);
                return T;
            }
            return NIL;
        }
    };

    private static final Primitive UNREGISTER_JAVA_EXCEPTION = new pf_unregister_java_exception();
    @DocString(name="unregister-java-exception", args="exception-name",
    doc="Unregisters the Java Throwable EXCEPTION-NAME previously registered" +
        " by REGISTER-JAVA-EXCEPTION.")
    private static final class pf_unregister_java_exception extends Primitive
    {
        pf_unregister_java_exception()
        {
            super("unregister-java-exception", PACKAGE_JAVA, true);
        }

        @Override
        public LispObject execute(LispObject className)

        {
            // FIXME Verify that EXCEPTION-NAME designates a subclass of Throwable.
            return registeredExceptions.remove(classForName(className.getStringValue())) == null ? NIL : T;
        }
    };

    static Symbol getCondition(Class cl) {
        Class o = classForName("java.lang.Object");
        for (Class c = cl ; c != o ; c = c.getSuperclass()) {
            Object object = registeredExceptions.get(c);
            if (object instanceof Symbol) {
                LispClass lispClass = (LispClass) LispClass.findClass((Symbol) object, true);
                if(isJavaException(lispClass)) {
                    return (Symbol) object;
                }
            }
        }
        return null;
    }

    private static final Primitive JCLASS = new pf_jclass();
    @DocString(name="jclass", args="name-or-class-ref &optional class-loader",
    doc="Returns a reference to the Java class designated by" +
        " NAME-OR-CLASS-REF. If the CLASS-LOADER parameter is passed, the" +
        " class is resolved with respect to the given ClassLoader.")
    private static final class pf_jclass extends Primitive 
    {

        pf_jclass() 
        {
            super(Symbol.JCLASS);
        }

        @Override
        public LispObject execute(LispObject arg)
        {
	    return JavaObject.getInstance(javaClass(arg, JavaClassLoader.getCurrentClassLoader()));
        }

        @Override
        public LispObject execute(LispObject className, LispObject classLoader)
        {
	    ClassLoader loader = (ClassLoader) classLoader.javaInstance(ClassLoader.class);
	    return JavaObject.getInstance(javaClass(className, loader));
        }
    };

    static final LispObject jfield(Primitive fun, LispObject[] args, boolean translate)

    {
        if (args.length < 2 || args.length > 4)
            error(new WrongNumberOfArgumentsException(fun, 2, 4));
        String fieldName = null;
        Class c;
        Field f;
        Class fieldType;
        Object instance = null;
        try {
            if (args[1] instanceof AbstractString) {
                // Cases 1-5.
                fieldName = args[1].getStringValue();
                c = javaClass(args[0]);
            } else {
                // Cases 6 and 7.
                fieldName = args[0].getStringValue();
                instance = JavaObject.getObject(args[1]);
                c = instance.getClass();
            }
            f = c.getField(fieldName);
            fieldType = f.getType();
            switch (args.length) {
                case 2:
                    // Cases 1 and 6.
                    break;
                case 3:
                    // Cases 2,3, and 7.
                    if (instance == null) {
                        // Cases 2 and 3.
                        if (args[2] instanceof JavaObject) {
                            // Case 2.
                            instance = JavaObject.getObject(args[2]);
                            break;
                        } else {
                            // Case 3.
                            f.set(null,args[2].javaInstance(fieldType));
                            return args[2];
                        }
                    } else {
                        // Case 7.
                        f.set(instance,args[2].javaInstance(fieldType));
                        return args[2];
                    }
                case 4:
                    // Cases 4 and 5.
                    if (args[2] != NIL) {
                        // Case 4.
                        instance = JavaObject.getObject(args[2]);
                    }
                    f.set(instance,args[3].javaInstance(fieldType));
                    return args[3];
            }
            return JavaObject.getInstance(f.get(instance), translate, f.getType());
        }
        catch (NoSuchFieldException e) {
            error(new LispError("no such field"));
        }
        catch (SecurityException e) {
            error(new LispError("inaccessible field"));
        }
        catch (IllegalAccessException e) {
            error(new LispError("illegal access"));
        }
        catch (IllegalArgumentException e) {
            error(new LispError("illegal argument"));
        }
        catch (Throwable t) { // no code -> no ControlTransfer
            error(new LispError(getMessage(t)));
        }
        // Not reached.
        return NIL;
    }


    private static final Primitive JFIELD = new pf_jfield();
    @DocString(name="jfield",
    args="class-ref-or-field field-or-instance &optional instance value",
    doc="Retrieves or modifies a field in a Java class or instance.\n\n"+
        "Supported argument patterns:\n\n"+
        "   Case 1: class-ref  field-name:\n"+
        "      Retrieves the value of a static field.\n\n"+
        "   Case 2: class-ref  field-name  instance-ref:\n"+
        "      Retrieves the value of a class field of the instance.\n\n"+
        "   Case 3: class-ref  field-name  primitive-value:\n"+
        "      Stores a primitive-value in a static field.\n\n"+
        "   Case 4: class-ref  field-name  instance-ref  value:\n"+
        "      Stores value in a class field of the instance.\n\n"+
        "   Case 5: class-ref  field-name  nil  value:\n"+
        "      Stores value in a static field (when value may be\n"+
        "      confused with an instance-ref).\n\n"+
        "   Case 6: field-name  instance:\n"+
        "      Retrieves the value of a field of the instance. The\n"+
        "      class is derived from the instance.\n\n"+
        "   Case 7: field-name  instance  value:\n"+
        "      Stores value in a field of the instance. The class is\n"+
        "      derived from the instance.\n\n"
        )
    private static final class pf_jfield extends Primitive 
    {
        pf_jfield() 
        {
            super("jfield", PACKAGE_JAVA, true);
        }

        @Override
        public LispObject execute(LispObject[] args)
        {
            return jfield(this, args, true);
        }
    };

    private static final Primitive JFIELD_RAW = new pf_jfield_raw();
    @DocString(name="jfield",
    args="class-ref-or-field field-or-instance &optional instance value",
    doc="Retrieves or modifies a field in a Java class or instance. Does not\n"+
        "attempt to coerce its value or the result into a Lisp object.\n\n"+
        "Supported argument patterns:\n\n"+
        "   Case 1: class-ref  field-name:\n"+
        "      Retrieves the value of a static field.\n\n"+
        "   Case 2: class-ref  field-name  instance-ref:\n"+
        "      Retrieves the value of a class field of the instance.\n\n"+
        "   Case 3: class-ref  field-name  primitive-value:\n"+
        "      Stores a primitive-value in a static field.\n\n"+
        "   Case 4: class-ref  field-name  instance-ref  value:\n"+
        "      Stores value in a class field of the instance.\n\n"+
        "   Case 5: class-ref  field-name  nil  value:\n"+
        "      Stores value in a static field (when value may be\n"+
        "      confused with an instance-ref).\n\n"+
        "   Case 6: field-name  instance:\n"+
        "      Retrieves the value of a field of the instance. The\n"+
        "      class is derived from the instance.\n\n"+
        "   Case 7: field-name  instance  value:\n"+
        "      Stores value in a field of the instance. The class is\n"+
        "      derived from the instance.\n\n"
        )
    private static final class pf_jfield_raw extends Primitive
    {
        pf_jfield_raw() 
        {
            super("jfield-raw", PACKAGE_JAVA, true);
        }

        @Override
        public LispObject execute(LispObject[] args)
        {
            return jfield(this, args, false);
        }
    };

    private static final Primitive JCONSTRUCTOR = new pf_jconstructor();
    @DocString(name="jconstructor", args="class-ref &rest parameter-class-refs",
    doc="Returns a reference to the Java constructor of CLASS-REF with the" +
        " given PARAMETER-CLASS-REFS.")
    private static final class pf_jconstructor extends Primitive
    {
        pf_jconstructor() 
        {
            super("jconstructor", PACKAGE_JAVA, true);
        }

        @Override
        public LispObject execute(LispObject[] args)
        {
            if (args.length < 1)
                error(new WrongNumberOfArgumentsException(this, 1, -1));
            try {
                final Class<?> c = javaClass(args[0]);
                int argCount = 0;
                if (args.length == 2 && args[1] instanceof Fixnum) {
                    argCount = Fixnum.getValue(args[1]);
                } else {
                    Class<?>[] parameterTypes = new Class[args.length-1];
                    for (int i = 1; i < args.length; i++) {
                        parameterTypes[i-1] = javaClass(args[i]);
                    }
                    return JavaObject.getInstance(c.getConstructor(parameterTypes));
                }
                // Parameter types not explicitly specified.
                Constructor[] constructors = c.getConstructors();
                for (int i = 0; i < constructors.length; i++) {
                    Constructor constructor = constructors[i];
                    if (constructor.getParameterTypes().length == argCount)
                        return JavaObject.getInstance(constructor);
                }
                throw new NoSuchMethodException();
            }
            catch (NoSuchMethodException e) {
                error(new LispError("no such constructor"));
            }
            catch (ControlTransfer e) {
                throw e;
            }
            catch (Throwable t) { // ControlTransfer addressed above
                error(new LispError(getMessage(t)));
            }
            // Not reached.
            return NIL;
        }
    };

    private static final Primitive JMETHOD = new pf_jmethod();

    @DocString(name="jmethod", args="class-ref method-name &rest parameter-class-refs",
    doc="Returns a reference to the Java method METHOD-NAME of CLASS-REF with the" +
        " given PARAMETER-CLASS-REFS.")
    private static final class pf_jmethod extends Primitive 
    {
        pf_jmethod() 
        {
            super("jmethod", PACKAGE_JAVA, true);
        }

        @Override
        public LispObject execute(LispObject[] args)
        {
            if (args.length < 2)
                error(new WrongNumberOfArgumentsException(this, 2, -1));
            final Class<?> c = javaClass(args[0]);
            String methodName = args[1].getStringValue();
            try {
                int argCount = 0;
                if (args.length == 3 && args[2] instanceof Fixnum) {
                    argCount = ((Fixnum)args[2]).value;
                } else {
                    Class<?>[] parameterTypes = new Class[args.length-2];
                    for (int i = 2; i < args.length; i++)
                        parameterTypes[i-2] = javaClass(args[i]);
                    return JavaObject.getInstance(c.getMethod(methodName,
                                                              parameterTypes));
                }
                // Parameter types were not explicitly specified.
                Method[] methods = c.getMethods();
                for (int i = 0; i < methods.length; i++) {
                    Method method = methods[i];
                    if (method.getName().equals(methodName) &&
                        method.getParameterTypes().length == argCount)
                        return JavaObject.getInstance(method);
                }
                throw new NoSuchMethodException();
            }
            catch (NoSuchMethodException e) {
                StringBuilder sb = new StringBuilder("No such method: ");
                sb.append(c.getName());
                sb.append('.');
                sb.append(methodName);
                sb.append('(');
                for (int i = 2; i < args.length; i++) {
                    sb.append(args[i].princToString());
                    if (i < args.length - 1)
                        sb.append(',');
                }
                sb.append(')');
                error(new LispError(sb.toString()));
            }
            catch (ControlTransfer e) {
                throw e;
            }
            catch (Throwable t) { // ControlTransfer addressed above
                error(new LispError(getMessage(t)));
            }
            // Not reached.
            return NIL;
        }
    };

    static final LispObject jstatic(Primitive fun, LispObject[] args, boolean translate)

    {
        if (args.length < 2)
            error(new WrongNumberOfArgumentsException(fun, 2, -1));
        try {
            Method m = null;
            LispObject methodRef = args[0];
            if (methodRef instanceof JavaObject) {
                Object obj = ((JavaObject)methodRef).getObject();
                if (obj instanceof Method)
                    m = (Method) obj;
            } else if (methodRef instanceof AbstractString) {
                Class c = javaClass(args[1]);
                if (c != null) {
                    String methodName = methodRef.getStringValue();
                    Method[] methods = c.getMethods();
                    List<Method> staticMethods = new ArrayList<Method>();
                    int argCount = args.length - 2;
                    for(Method m1 : methods) {
                        if(Modifier.isStatic(m1.getModifiers())) {
                            staticMethods.add(m1);
                        }
                    }
                    if(staticMethods.size() > 0) {
                        m = findMethod(staticMethods.toArray(new Method[staticMethods.size()]), methodName, args, 2);
                    }
                    if (m == null)
                        error(new LispError("no such method"));
                }
            } else
              type_error(methodRef, Symbol.STRING);
            Object[] methodArgs = new Object[args.length-2];
            Class[] argTypes = m.getParameterTypes();
            for (int i = 2; i < args.length; i++) {
                LispObject arg = args[i];
                if (arg == NIL)
                    methodArgs[i-2] = null;
                else
                    methodArgs[i-2] = arg.javaInstance(argTypes[i-2]);
            }
            m.setAccessible(true);
            Object result = m.invoke(null, methodArgs);
	    return JavaObject.getInstance(result, translate, m.getReturnType());
        }
        catch (ControlTransfer c) {
            throw c;
        }
        catch (Throwable t) { // ControlTransfer handled above
            if (t instanceof InvocationTargetException)
                t = t.getCause();
            Symbol condition = getCondition(t.getClass());
            if (condition == null)
                error(new JavaException(t));
            else
                Symbol.SIGNAL.execute(
                    condition,
                    Keyword.CAUSE,
                    JavaObject.getInstance(t),
                    Keyword.FORMAT_CONTROL,
                    new SimpleString(getMessage(t)));
        }
        // Not reached.
        return NIL;
    }

    private static final Primitive JSTATIC = new pf_jstatic();
    @DocString(name="jstatic", args="method class &rest args",
    doc="Invokes the static method METHOD on class CLASS with ARGS.")
    private static final class pf_jstatic extends Primitive 
    {
        pf_jstatic() 
        {
            super("jstatic", PACKAGE_JAVA, true);
        }

        @Override
        public LispObject execute(LispObject[] args)
        {
            return jstatic(this, args, true);
        }
    };

    private static final Primitive JSTATIC_RAW = new pf_jstatic_raw();
    @DocString(name="jstatic-raw", args="method class &rest args",
    doc="Invokes the static method METHOD on class CLASS with ARGS. Does not "+
        "attempt to coerce the arguments or result into a Lisp object.")
    private static final class pf_jstatic_raw extends Primitive
    {
        pf_jstatic_raw() 
        {
            super("jstatic-raw", PACKAGE_JAVA, true);
        }

        @Override
        public LispObject execute(LispObject[] args)
        {
            return jstatic(this, args, false);
        }
    };

    private static final Primitive JNEW = new pf_jnew();
    @DocString(name="jnew", args="constructor &rest args",
    doc="Invokes the Java constructor CONSTRUCTOR with the arguments ARGS.")
    private static final class pf_jnew extends Primitive
    {
        pf_jnew()
        {
            super("jnew", PACKAGE_JAVA, true);
        }

        @Override
        public LispObject execute(LispObject[] args)
        {
            if (args.length < 1)
                error(new WrongNumberOfArgumentsException(this, 1, -1));
            LispObject classRef = args[0];
            try {
                Constructor constructor;
		if(classRef instanceof AbstractString) {
		    constructor = findConstructor(javaClass(classRef), args);
		} else {
		    Object object = JavaObject.getObject(classRef);
		    if(object instanceof Constructor) {
			constructor = (Constructor) object;
		    } else if(object instanceof Class<?>) {
			constructor = findConstructor((Class<?>) object, args);
		    } else {
			return error(new LispError(classRef.princToString() + " is neither a Constructor nor a Class"));
		    }
		}
                Class[] argTypes = constructor.getParameterTypes();
                Object[] initargs = new Object[args.length-1];
                for (int i = 1; i < args.length; i++) {
                    LispObject arg = args[i];
                    if (arg == NIL)
                        initargs[i-1] = null;
                    else {
                        initargs[i-1] = arg.javaInstance(argTypes[i-1]);
                    }
                }
                return JavaObject.getInstance(constructor.newInstance(initargs));
            }
            catch (ControlTransfer c) {
                throw c;
            }
            catch (Throwable t) { // ControlTransfer handled above
                if (t instanceof InvocationTargetException)
                    t = t.getCause();
                Symbol condition = getCondition(t.getClass());
                if (condition == null)
                    error(new JavaException(t));
                else
                    Symbol.SIGNAL.execute(
                        condition,
                        Keyword.CAUSE,
                        JavaObject.getInstance(t),
                        Keyword.FORMAT_CONTROL,
                        new SimpleString(getMessage(t)));
            }
            // Not reached.
            return NIL;
        }
    };

    private static final Primitive JNEW_ARRAY = new pf_jnew_array();
    @DocString(name="jnew-array", args="element-type &rest dimensions",
    doc="Creates a new Java array of type ELEMENT-TYPE, with the given" +
        " DIMENSIONS.")
    private static final class pf_jnew_array extends Primitive
    {
        pf_jnew_array()
        {
            super("jnew-array", PACKAGE_JAVA, true);
        }

        @Override
        public LispObject execute(LispObject[] args)
        {
            if (args.length < 2)
                error(new WrongNumberOfArgumentsException(this, 2, -1));
            try {
                Class c = javaClass(args[0]);
                int[] dimensions = new int[args.length - 1];
                for (int i = 1; i < args.length; i++)
                    dimensions[i-1] = ((Integer)args[i].javaInstance()).intValue();
                return JavaObject.getInstance(Array.newInstance(c, dimensions));
            }
            catch (Throwable t) { // no code -> no ControlTransfer
                error(new JavaException(t));
            }
            // Not reached.
            return NIL;
        }
    };

    static final LispObject jarray_ref(Primitive fun, LispObject[] args, boolean translate)

    {
        if (args.length < 2)
            error(new WrongNumberOfArgumentsException(fun, 2, -1));
        try {
            Object a = args[0].javaInstance();
            for (int i = 1; i<args.length - 1; i++)
                a = Array.get(a, ((Integer)args[i].javaInstance()).intValue());
            return JavaObject.getInstance(Array.get(a,
                    ((Integer)args[args.length - 1].javaInstance()).intValue()), translate);
        }
        catch (Throwable t) { // no code -> no ControlTransfer
            Symbol condition = getCondition(t.getClass());
            if (condition == null)
                error(new JavaException(t));
            else
                Symbol.SIGNAL.execute(
                    condition,
                    Keyword.CAUSE,
                    JavaObject.getInstance(t),
                    Keyword.FORMAT_CONTROL,
                    new SimpleString(getMessage(t)));
        }
        // Not reached.
        return NIL;
    }

    private static final Primitive JARRAY_REF = new pf_jarray_ref();
    @DocString(name="jarray-ref", args="java-array &rest indices",
    doc="Dereferences the Java array JAVA-ARRAY using the given INDICIES, " +
        "coercing the result into a Lisp object, if possible.")
    private static final class pf_jarray_ref extends Primitive
    {
        pf_jarray_ref()
        {
            super("jarray-ref", PACKAGE_JAVA, true);
        }

        @Override
        public LispObject execute(LispObject[] args)
        {
            return jarray_ref(this, args, true);
        }
    };

    private static final Primitive JARRAY_REF_RAW = new pf_jarray_ref_raw();
    @DocString(name="jarray-ref-raw", args="java-array &rest indices",
    doc="Dereference the Java array JAVA-ARRAY using the given INDICIES. " +
        "Does not attempt to coerce the result into a Lisp object.")
    private static final class pf_jarray_ref_raw extends Primitive
    {
        pf_jarray_ref_raw() 
        {
            super("jarray-ref-raw", PACKAGE_JAVA, true);
        }

        @Override
        public LispObject execute(LispObject[] args)
        {
            return jarray_ref(this, args, false);
        }
    };

    private static final Primitive JARRAY_SET = new pf_jarray_set();
    @DocString(name="jarray-set", args="java-array new-value &rest indices",
    doc="Stores NEW-VALUE at the given index in JAVA-ARRAY.")
    private static final class pf_jarray_set extends Primitive
    {
        pf_jarray_set()
        {
            super("jarray-set", PACKAGE_JAVA, true);
        }

        @Override
        public LispObject execute(LispObject[] args)
        {
            if (args.length < 3)
                error(new WrongNumberOfArgumentsException(this, 3, -1));
            try {
                Object a = args[0].javaInstance();
                LispObject v = args[1];
                for (int i = 2; i<args.length - 1; i++)
                    a = Array.get(a, ((Integer)args[i].javaInstance()).intValue());
                Object value = v.javaInstance();
                int index = ((Integer)args[args.length - 1].javaInstance()).intValue();
                if (value instanceof java.lang.Number
                    && a.getClass().getComponentType().equals(Byte.TYPE)) {
                    Array.setByte(a, index, ((java.lang.Number)value).byteValue());
                } else {
                    Array.set(a, index, value);
                }
                return v;
            }
            catch (Throwable t) { // no code -> no ControlTransfer
                Symbol condition = getCondition(t.getClass());
                if (condition == null)
                    error(new JavaException(t));
                else
                    Symbol.SIGNAL.execute(
                        condition,
                        Keyword.CAUSE,
                        JavaObject.getInstance(t),
                        Keyword.FORMAT_CONTROL,
                        new SimpleString(getMessage(t)));
            }
            // Not reached.
            return NIL;
        }
    };

    /**  Calls makeLispObject() to convert the result to an appropriate Lisp type. */
    private static final Primitive JCALL = new pf_jcall();
    @DocString(name="jcall", args="method-ref instance &rest args",
    doc="Invokes the Java method METHOD-REF on INSTANCE with arguments ARGS," +
        " coercing the result into a Lisp object, if possible.")
    private static final class pf_jcall extends Primitive
    {
        pf_jcall()
        {
            super(Symbol.JCALL);
        }

        @Override
        public LispObject execute(LispObject[] args)
        {
            return jcall(this, args, true);
        }
    };

    /**
     * Does no type conversion. The result of the call is simply wrapped in a
     *   JavaObject.
     */
    private static final Primitive JCALL_RAW = new pf_jcall_raw();
    @DocString(name="jcall-raw", args="method-ref instance &rest args",
    doc="Invokes the Java method METHOD-REF on INSTANCE with arguments ARGS." +
        " Does not attempt to coerce the result into a Lisp object.")
    private static final class pf_jcall_raw extends Primitive
    {
        pf_jcall_raw()
        {
            super(Symbol.JCALL_RAW);
        }

        @Override
        public LispObject execute(LispObject[] args)
        {
            return jcall(this, args, false);
        }
    };

    private static final Primitive JRESOLVE_METHOD = new pf_jresolve_method();
    @DocString(name="jresolve-method", args="method-name instance &rest args",
    doc="Finds the most specific Java method METHOD-NAME on INSTANCE " +
        "applicable to arguments ARGS. Returns NIL if no suitable method is " +
        "found. The algorithm used for resolution is the same used by JCALL " +
        "when it is called with a string as the first parameter (METHOD-REF).")
    private static final class pf_jresolve_method extends Primitive {
        pf_jresolve_method() {
            super(Symbol.JRESOLVE_METHOD);
        }

        @Override
        public LispObject execute(LispObject[] args) {
            if (args.length < 2) {
                error(new WrongNumberOfArgumentsException(this, 2, -1));
            }
            final LispObject methodArg = args[0];
            final LispObject instanceArg = args[1];
            final Object instance;
            Class<?> intendedClass = null;
            if (instanceArg instanceof AbstractString) {
                instance = instanceArg.getStringValue();
            } else if (instanceArg instanceof JavaObject) {
                JavaObject jobj = ((JavaObject)instanceArg);
                instance = jobj.getObject();
                intendedClass = jobj.getIntendedClass();
            } else {
                instance = instanceArg.javaInstance();
            }
            if(instance == null) {
                return program_error("JRESOLVE-METHOD: instance must not be null.");
            }
            String methodName = methodArg.getStringValue();
            Object[] methodArgs = translateMethodArguments(args, 2);
            Method method = findMethod(instance, intendedClass, methodName, methodArgs);
            if (method != null) {
                return JavaObject.getInstance(method);
            } else if (instanceArg instanceof JavaObject) {
                // Sometimes JavaObject.intendedClass has the default
                // value java.lang.Object, so we try again to resolve
                // the method using a dynamically requested value for
                // java.lang.Class.
                intendedClass = ((JavaObject)instanceArg).getObject().getClass();
                method = findMethod(instance, intendedClass, methodName, methodArgs);
            } else {
                return NIL;
            }
            if (method != null) {
                return JavaObject.getInstance(method);
            } else {
                return NIL;
            }
        }
    };

    static LispObject jcall(Primitive fun, LispObject[] args, boolean translate)

    {
        if (args.length < 2)
            error(new WrongNumberOfArgumentsException(fun, 2, -1));
        try {
            final LispObject methodArg = args[0];
            final LispObject instanceArg = args[1];
            final Object instance;
            Method method;
            Object[] methodArgs;
            Class<?> intendedClass = null;
            if (instanceArg instanceof AbstractString) {
                instance = instanceArg.getStringValue();
            } else if (instanceArg instanceof JavaObject) {
                JavaObject jobj = ((JavaObject)instanceArg);
                instance = jobj.getObject();
                intendedClass = jobj.getIntendedClass();
            } else {
                instance = instanceArg.javaInstance();
            }
            if(instance == null) {
                throw new NullPointerException(); //Handled below
            }
            if (methodArg instanceof AbstractString) {
                String methodName = methodArg.getStringValue();
                methodArgs = translateMethodArguments(args, 2);
                method = findMethod(instance, intendedClass, methodName, methodArgs);
                if (method == null) {
                    if (intendedClass == null) {
                        String msg = MessageFormat.format("No instance method named {0} found for type {1}", methodName, instance.getClass().getName());
                        throw new NoSuchMethodException(msg);
                    }
                    String classes = intendedClass.getName();
                    Class<?> actualClass = instance.getClass();
                    if(actualClass != intendedClass) {
                        classes += " or " + actualClass.getName();
                    }
                    throw new NoSuchMethodException("No applicable method named " + methodName + " found in " + classes);
                }
            } else
                method = (Method) JavaObject.getObject(methodArg);
            Class<?>[] argTypes = (Class<?>[])method.getParameterTypes();
	    if(argTypes.length != args.length - 2) {
		return error(new WrongNumberOfArgumentsException("Wrong number of arguments for " + method + ": expected " + argTypes.length + ", got " + (args.length - 2)));
	    }
            methodArgs = new Object[argTypes.length];
            for (int i = 2; i < args.length; i++) {
                LispObject arg = args[i];
                if (arg == NIL)
                    methodArgs[i-2] = null;
                else
                    methodArgs[i-2] = arg.javaInstance(argTypes[i-2]);
            }
            if (!method.isAccessible()) {
                 // Possible for static member classes: see #229
                 if (Modifier.isPublic(method.getModifiers())) { 
    	              method.setAccessible(true);
                 }
	    }
            return JavaObject.getInstance(method.invoke(instance, methodArgs),
                                          translate,
                                          method.getReturnType());
        }
        catch (ControlTransfer t) {
            throw t;
        }
        catch (Throwable t) { // ControlTransfer handled above
            if (t instanceof InvocationTargetException)
                t = t.getCause();
            Symbol condition = getCondition(t.getClass());
            if (condition == null)
                error(new JavaException(t));
            else
                Symbol.SIGNAL.execute(
                    condition,
                    Keyword.CAUSE,
                    JavaObject.getInstance(t),
                    Keyword.FORMAT_CONTROL,
                    new SimpleString(getMessage(t)));
        }
        // Not reached.
        return null;
    }

    private static Object[] translateMethodArguments(LispObject[] args) {
	return translateMethodArguments(args, 0);
    }

    private static Object[] translateMethodArguments(LispObject[] args, int offs) {
	int argCount = args.length - offs;
        Object[] javaArgs = new Object[argCount];
        for (int i = 0; i < argCount; ++i) {
            Object x = args[i + offs];
            if (x == NIL) {
                javaArgs[i] = null;
            } else {
                javaArgs[i] = ((LispObject) x).javaInstance();
            }
        }
	return javaArgs;
    }

    private static Method findMethod(Method[] methods, String methodName, Object[] javaArgs) {
        int argCount = javaArgs.length;
        Method result = null;
        for (int i = methods.length; i-- > 0;) {
            Method method = methods[i];
            if (!method.getName().equals(methodName)) {
                continue;
            }
            if (method.getParameterTypes().length != argCount) {
                continue;
            }
            Class<?>[] methodTypes = (Class<?>[]) method.getParameterTypes();
            if (!isApplicableMethod(methodTypes, javaArgs)) {
                continue;
            }
            if (result == null || isMoreSpecialized(methodTypes, result.getParameterTypes())) {
                result = method;
            }
        }
        return result;
    }

    private static Method findMethod(Object instance, Class<?> intendedClass, String methodName, Object[] methodArgs) {
        if(intendedClass == null) {
            intendedClass = instance.getClass();
        }
        Method method = findMethod(intendedClass, methodName, methodArgs);
        Class actualClass = null;
        if(method == null) {
            actualClass = instance.getClass();
            if(intendedClass != actualClass) { 
                method = findMethod(actualClass, methodName, methodArgs);
		if (method != null) {
		   if (isMethodCallableOnInstance(actualClass, method)) {
		      return method;
		   }
		}
            }
        }
        return method;
    }
    
    private static boolean isMethodCallableOnInstance(Class instance, Method method) {
       if (Modifier.isPublic(method.getModifiers())) {
	  return true;
       }
       if (instance.isMemberClass()) {
	  return isMethodCallableOnInstance(instance.getEnclosingClass(), method);
       }
       return false;
    }

    private static Method findMethod(Class<?> c, String methodName, Object[] javaArgs) {
        Method[] methods = c.getMethods();
        return findMethod(methods, methodName, javaArgs);
    }

    private static Method findMethod(Class<?> c, String methodName, LispObject[] args, int offset) {
        Object[] javaArgs = translateMethodArguments(args, offset);
        return findMethod(c, methodName, javaArgs);
    }

    private static Method findMethod(Method[] methods, String methodName, LispObject[] args, int offset) {
        Object[] javaArgs = translateMethodArguments(args, offset);
        return findMethod(methods, methodName, javaArgs);
    }

    static Constructor findConstructor(Class<?> c, LispObject[] args) throws NoSuchMethodException {
        int argCount = args.length - 1;
        Object[] javaArgs = translateMethodArguments(args, 1);
        Constructor[] ctors = c.getConstructors();
        Constructor result = null;
        for (int i = ctors.length; i-- > 0;) {
            Constructor ctor = ctors[i];
            if (ctor.getParameterTypes().length != argCount) {
                continue;
            }
            Class<?>[] methodTypes = (Class<?>[]) ctor.getParameterTypes();
            if (!isApplicableMethod(methodTypes, javaArgs)) {
                continue;
            }
            if (result == null || isMoreSpecialized(methodTypes, result.getParameterTypes())) {
                result = ctor;
            }
        }
        if (result == null) {
	    StringBuilder sb = new StringBuilder(c.getSimpleName());
	    sb.append('(');
	    boolean first = true;
	    for(Object o : javaArgs) {
		if(first) {
		    first = false;
		} else {
		    sb.append(", ");
		}
		if(o != null) {
		    sb.append(o.getClass().getName());
		} else {
		    sb.append("<null>");
		}
	    }
	    sb.append(')');
            throw new NoSuchMethodException(sb.toString());
        }
        return result;
    }

    private static boolean isAssignable(Class<?> from, Class<?> to) {
        from = maybeBoxClass(from);
        to = maybeBoxClass(to);
        if (to.isAssignableFrom(from)) {
            return true;
        }
        if (Byte.class.equals(from)) {
            return Short.class.equals(to) || Integer.class.equals(to) || Long.class.equals(to) || Float.class.equals(to) || Double.class.equals(to);
        } else if (Short.class.equals(from) || Character.class.equals(from)) {
            return Integer.class.equals(to) || Long.class.equals(to) || Float.class.equals(to) || Double.class.equals(to);
        } else if (Integer.class.equals(from)) {
            return Long.class.equals(to) || Float.class.equals(to) || Double.class.equals(to);
        } else if (Long.class.equals(from)) {
            return Float.class.equals(to) || Double.class.equals(to);
        } else if (Float.class.equals(from)) {
            return Double.class.equals(to);
        }
        return false;
    }

    private static boolean isApplicableMethod(Class<?>[] methodTypes,
            Object[] args) {
        for (int i = 0; i < methodTypes.length; ++i) {
            Class<?> methodType = methodTypes[i];
            Object arg = args[i];
            if (!isAssignable(arg.getClass(), methodType)) {
                return false;
            }
        }
        return true;
    }

    private static boolean isMoreSpecialized(Class<?>[] xtypes, Class<?>[] ytypes) {
        for (int i = 0; i < xtypes.length; ++i) {
            Class<?> xtype = maybeBoxClass(xtypes[i]);
            Class<?> ytype = maybeBoxClass(ytypes[i]);
            if (xtype.equals(ytype)) {
                continue;
            }
            if (isAssignable(xtype, ytype)) {
                return true;
            }
        }
        return false;
    }

    public static Class<?> maybeBoxClass(Class<?> clazz) {
	if(clazz.isPrimitive()) {
	    return getBoxedClass(clazz);
	} else {
	    return clazz;
	}
    }
    
    private static Class<?> getBoxedClass(Class<?> clazz) {
        if (clazz.equals(int.class)) {
            return Integer.class;
        } else if (clazz.equals(boolean.class)) {
            return Boolean.class;
        } else if (clazz.equals(byte.class)) {
            return Byte.class;
        } else if (clazz.equals(char.class)) {
            return Character.class;
        } else if (clazz.equals(long.class)) {
            return Long.class;
        } else if (clazz.equals(float.class)) {
            return Float.class;
        } else if (clazz.equals(double.class)) {
            return Double.class;
        } else if (clazz.equals(short.class)) {
            return Short.class;
        } else { // if (methodType.equals(void.class))
            return Void.class;
        }
    }

    // DEPRECATED Remove MAKE-IMMEDIATE-OBJECT in abcl-0.29
    private static final Primitive MAKE_IMMEDIATE_OBJECT = new pf_make_immediate_object();
    @DocString(name="make-immediate-object", args="object &optional type",
    doc="Attempts to coerce a given Lisp object into a java-object of the\n"
      + "given type.  If type is not provided, works as jobject-lisp-value.\n"
      + "Currently, type may be :BOOLEAN, treating the object as a truth value,\n"
      + "or :REF, which returns Java null if NIL is provided.\n"
      + "\n"
      + "Deprecated.  Please use JAVA:+NULL+, JAVA:+TRUE+, and JAVA:+FALSE+ for\n"
      + "constructing wrapped primitive types, JAVA:JOBJECT-LISP-VALUE for converting a\n"
      + "JAVA:JAVA-OBJECT to a Lisp value, or JAVA:JNULL-REF-P to distinguish a wrapped\n"
      + "null JAVA-OBJECT from NIL.")
    private static final class pf_make_immediate_object extends Primitive
    {
        pf_make_immediate_object()
        {
            super("make-immediate-object", PACKAGE_JAVA, true);
        }

        @Override
        public LispObject execute(LispObject[] args)
        {
            Symbol.WARN.getSymbolFunction()
                .execute(new SimpleString("JAVA:MAKE-IMMEDIATE-OBJECT is deprecated."));
            if (args.length < 1)
                error(new WrongNumberOfArgumentsException(this, 1, -1));
            LispObject object = args[0];
            if (args.length > 1) {
                LispObject type = args[1];
                if (type == Keyword.BOOLEAN) {
                    if (object == NIL)
                        return JavaObject.getInstance(Boolean.FALSE);
                    else
                        return JavaObject.getInstance(Boolean.TRUE);
                }
                if (type == Keyword.REF) {
                    if (object == NIL)
                        return JavaObject.getInstance(null);
                    else
                        error(new LispError("MAKE-IMMEDIATE-OBJECT: not implemented"));
                }
                // other special cases come here
            }
            return JavaObject.getInstance(object.javaInstance());
        }
    };

    private static final Primitive JNULL_REF_P = new pf_jnull_ref_p();
    @DocString(name="jnull-ref-p", args="object",
    doc="Returns a non-NIL value when the JAVA-OBJECT `object` is `null`,\n"
            + "or signals a TYPE-ERROR condition if the object isn't of\n"
            + "the right type.")
    private static final class pf_jnull_ref_p extends Primitive
    {
        pf_jnull_ref_p()
        {
            super("jnull-ref-p", PACKAGE_JAVA, true);
        }

        @Override
        public LispObject execute(LispObject ref)
        {
            if (ref instanceof JavaObject)
            {
                JavaObject jref = (JavaObject)ref;
                return (jref.javaInstance() == null) ? T : NIL;
            } else
                return Lisp.type_error(ref, Symbol.JAVA_OBJECT);
        }
    };


    private static final Primitive JAVA_OBJECT_P = new pf_java_object_p();
    @DocString(name="java-object-p", args="object",
    doc="Returns T if OBJECT is a JAVA-OBJECT.")
    private static final class pf_java_object_p extends Primitive
    {
        pf_java_object_p() 
        {
            super("java-object-p", PACKAGE_JAVA, true);
        }

        @Override
        public LispObject execute(LispObject arg)
        {
            return (arg instanceof JavaObject) ? T : NIL;
        }
    };

    private static final Primitive JOBJECT_LISP_VALUE = new pf_jobject_lisp_value();
    @DocString(name="jobject-lisp-value", args="java-object",
    doc="Attempts to coerce JAVA-OBJECT into a Lisp object.")
    private static final class pf_jobject_lisp_value extends Primitive
    {
        pf_jobject_lisp_value()
        {
            super("jobject-lisp-value", PACKAGE_JAVA, true, "java-object");
        }

        @Override
        public LispObject execute(LispObject arg)
        {
            return JavaObject.getInstance(arg.javaInstance(), true);
        }
    };

    private static final Primitive JCOERCE = new pf_jcoerce();
    @DocString(name="jcoerce", args="object intended-class",
    doc="Attempts to coerce OBJECT into a JavaObject of class INTENDED-CLASS." +
        "  Raises a TYPE-ERROR if no conversion is possible.")
    private static final class pf_jcoerce extends Primitive
    {
        pf_jcoerce()
        {
            super("jcoerce", PACKAGE_JAVA, true);
        }

        @Override
        public LispObject execute(LispObject javaObject, LispObject intendedClass)
        {
	    Object o = javaObject.javaInstance();
	    Class<?> c = javaClass(intendedClass);
	    try {
		return JavaObject.getInstance(o, c);
	    } catch(ClassCastException e) {
          return type_error(javaObject, new SimpleString(c.getName()));
	    }
        }
    };

    private static final Primitive JRUN_EXCEPTION_PROTECTED = new pf_jrun_exception_protected();
    @DocString(name="jrun-exception-protected", args="closure",
    doc="Invokes the function CLOSURE and returns the result.  "+
        "Signals an error if stack or heap exhaustion occurs.")
    private static final class pf_jrun_exception_protected extends Primitive
    {
        pf_jrun_exception_protected()
        {
            super("jrun-exception-protected", PACKAGE_JAVA, true);
        }

        @Override
        public LispObject execute(LispObject closure) {
            Function fun = checkFunction(closure);

            try {
                return LispThread.currentThread().execute(closure);
            }
            catch (OutOfMemoryError oom) {
                return error(new StorageCondition("Out of memory " + oom.getMessage()));
            }
            catch (StackOverflowError oos) {
                oos.printStackTrace();
                return error(new StorageCondition("Stack overflow."));
            }
        }
    };

    private static Class classForName(String className) {
	return classForName(className, JavaClassLoader.getPersistentInstance());
    }

    private static Class classForName(String className, ClassLoader classLoader) {
        try {
            return Class.forName(className, true, classLoader);
        }
        catch (ClassNotFoundException e) {
	    error(new LispError("Class not found: " + className));
	    // Not reached.
	    return null;
        }
    }

    private static Class javaClass(LispObject obj) {
	return javaClass(obj, JavaClassLoader.getCurrentClassLoader());
    }

    // Supports Java primitive types too.
    static Class javaClass(LispObject obj, ClassLoader classLoader)
    {
        if (obj instanceof AbstractString || obj instanceof Symbol) {
            String s = javaString(obj);
            if (s.equals("boolean"))
                return Boolean.TYPE;
            if (s.equals("byte"))
                return Byte.TYPE;
            if (s.equals("char"))
                return Character.TYPE;
            if (s.equals("short"))
                return Short.TYPE;
            if (s.equals("int"))
                return Integer.TYPE;
            if (s.equals("long"))
                return Long.TYPE;
            if (s.equals("float"))
                return Float.TYPE;
            if (s.equals("double"))
                return Double.TYPE;
            // Not a primitive Java type.
            Class c;
	    c = classForName(s, classLoader);
            if (c == null)
                error(new LispError(s + " does not designate a Java class."));

            return c;
        }
        // It's not a string, so it must be a JavaObject.
        final JavaObject javaObject;
        if (obj instanceof JavaObject) {
            javaObject = (JavaObject) obj;
        }
        else {
            type_error(obj, list(Symbol.OR, Symbol.STRING,
                                       Symbol.JAVA_OBJECT));
            // Not reached.
            return null;
        }
        final Object javaObjectgetObject = javaObject.getObject();
        if (javaObjectgetObject instanceof Class) {
            return (Class) javaObjectgetObject;
        }
            error(new LispError(obj.princToString() + " does not designate a Java class."));
            return null;
    }

    static final String getMessage(Throwable t)
    {
        String message = t.getMessage();
        if (message == null || message.length() == 0)
            message = t.getClass().getName();
        return message;
    }
}
