/*
 * JavaObject.java
 *
 * Copyright (C) 2002-2005 Peter Graves
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
import java.lang.reflect.Field;
import java.math.BigInteger;
import java.util.*;

public final class JavaObject extends LispObject {
    final Object obj;
    private final Class<?> intendedClass;

    public JavaObject(Object obj) {
        this.obj = obj;
        this.intendedClass =
            obj != null ? Java.maybeBoxClass(obj.getClass()) : null;
    }

    public static final Symbol JAVA_CLASS_JCLASS = PACKAGE_JAVA.intern("JAVA-CLASS-JCLASS");
    public static final Symbol JAVA_CLASS = PACKAGE_JAVA.intern("JAVA-CLASS");
    public static final Symbol ENSURE_JAVA_CLASS = PACKAGE_JAVA.intern("ENSURE-JAVA-CLASS");

    /**
     * Constructs a Java Object with the given intended class, used to access
     * the object reflectively. If the class represents a primitive type,
     * the corresponding wrapper type is used instead.
     * @throws ClassCastException if the object is not an instance of the
     *                            intended class.
     */
    public JavaObject(Object obj, Class<?> intendedClass) {
        if(obj != null && intendedClass == null) {
            intendedClass = obj.getClass();
        }
        if(intendedClass != null) {
            intendedClass = Java.maybeBoxClass(intendedClass);
            if(!intendedClass.isInstance(obj)) {
                throw new ClassCastException(obj + " can not be cast to " + intendedClass);
            }
        }
        this.obj = obj;
        this.intendedClass = intendedClass;
    }

    @Override
    public LispObject typeOf()
    {
        return Symbol.JAVA_OBJECT;
    }

    @Override
    public LispObject classOf()
    {
        if(obj == null) {
                return BuiltInClass.JAVA_OBJECT;
        } else {
            return ENSURE_JAVA_CLASS.execute(new JavaObject(obj.getClass()));
        }
    }

    @Override
    public LispObject typep(LispObject type) {
        if (type == Symbol.JAVA_OBJECT)
            return T;
        if (type == BuiltInClass.JAVA_OBJECT)
            return T;
        LispObject cls = NIL;
        if(type instanceof Symbol) {
            cls = LispClass.findClass(type, false);
        }
        if(cls == NIL) {
            cls = type;
        }
        if(cls.typep(LispClass.findClass(JAVA_CLASS, false)) != NIL) {
            if(obj != null) {
                Class c = (Class) JAVA_CLASS_JCLASS.execute(cls).javaInstance();
                return c.isAssignableFrom(obj.getClass()) ? T : NIL;
            } else {
                return T;
            }
        } else if(cls == BuiltInClass.SEQUENCE) {
            //This information is replicated here from java.lisp; it is a very
            //specific case, not worth implementing CPL traversal in typep
            if(java.util.List.class.isInstance(obj) ||
               java.util.Set.class.isInstance(obj)) {
                return T;
            }
        }
        return super.typep(type);
    }

    @Override
    public LispObject STRING()
    {
        return new SimpleString(obj != null? obj.toString(): "null");
    }

    public final Object getObject()
    {
        return obj;
    }

    /** Encapsulates obj, if required.
     * If obj is a {@link  LispObject}, it's returned as-is.
     * 
     * @param obj Any java object
     * @return obj or a new JavaObject encapsulating obj
     */
    public final static LispObject getInstance(Object obj) {
        if (obj == null)
            return new JavaObject(null);
        
        if (obj instanceof LispObject)
            return (LispObject)obj;

        return new JavaObject(obj);
    }

    /** Encapsulates obj, if required.
     * If obj is a {@link LispObject}, it's returned as-is.
     * If not, a java object with the specified intended class is returned.
     * 
     * @param obj Any java object
     * @param intendedClass the class that shall be used to access obj
     * @return obj or a new JavaObject encapsulating obj
     */
    public final static LispObject getInstance(Object obj, Class<?> intendedClass) {
        if (obj == null)
            return new JavaObject(null);
        
        if (obj instanceof LispObject)
            return (LispObject)obj;

        return new JavaObject(obj, intendedClass);
    }

    /** Encapsulates obj, if required.
     * If obj is a {@link LispObject}, it's returned as-is.
     * If obj is of a type which can be mapped to a lisp type,
     * an object of the mapped type is returned, if translated is true.
     *
     * @param obj
     * @param translated
     * @return a LispObject representing or encapsulating obj
     */
    public final static LispObject getInstance(Object obj, boolean translated) {
        return getInstance(obj, translated, obj != null ? obj.getClass() : null);
    }



    /** Encapsulates obj, if required.
     * If obj is a {@link LispObject}, it's returned as-is.
     * If obj is of a type which can be mapped to a lisp type,
     * an object of the mapped type is returned, if translated is true.
     *
     * @param obj
     * @param translated
     * @param intendedClass the class that shall be used to reflectively 
     *                      access obj; it is an error for obj not to be
     *                      an instance of this class. This parameter is ignored
     *                      if translated == true and the object can be
     *                      converted to a Lisp object.
     * @return a LispObject representing or encapsulating obj
     */
    public final static LispObject getInstance(Object obj, boolean translated, Class<?> intendedClass) {
        if (! translated)
            return getInstance(obj, intendedClass);

        if (obj == null) return NIL;

        if (obj instanceof LispObject)
            return (LispObject)obj;

        if (obj instanceof String)
            return new SimpleString((String)obj);

        if (obj instanceof Number) {
            // Number types ordered according to decreasing
            // estimated chances of occurrance

            if (obj instanceof Integer)
                return Fixnum.getInstance(((Integer)obj).intValue());

            if (obj instanceof Float)
                return new SingleFloat((Float)obj);

            if (obj instanceof Double)
                return new DoubleFloat((Double)obj);

            if (obj instanceof Long)
                return LispInteger.getInstance(((Long)obj).longValue());

            if (obj instanceof BigInteger)
                return Bignum.getInstance((BigInteger)obj);

            if (obj instanceof Short)
                return Fixnum.getInstance(((Short)obj).shortValue());

            if (obj instanceof Byte)
                return Fixnum.getInstance(((Byte)obj).byteValue());
            // We don't handle BigDecimal: it doesn't map to a Lisp type
        }

        if (obj instanceof Boolean)
            return ((Boolean)obj).booleanValue() ? T : NIL;

        if (obj instanceof Character)
            return LispCharacter.getInstance((Character)obj);

        if (obj instanceof Object[]) {
            Object[] array = (Object[]) obj;
            SimpleVector v = new SimpleVector(array.length);
            for (int i = array.length; i-- > 0;)
                v.aset(i, JavaObject.getInstance(array[i], translated));
            return v;
        }
        // TODO
        // We might want to handle:
        //  - streams
        //  - others?
        return new JavaObject(obj, intendedClass);
    }

    @Override
    public Object javaInstance() {
        return obj;
    }

    @Override
    public Object javaInstance(Class<?> c) {
        if(obj == null) {
            if(c.isPrimitive()) {
                throw new NullPointerException("Cannot assign null to " + c);
            }
            return obj;
        } else {
            c = Java.maybeBoxClass(c);
            if (c.isAssignableFrom(intendedClass) || c.isInstance(obj)) {
              // XXX In the case that c.isInstance(obj) should we then
              // "fix" the intendedClass field with the (presumably)
              // narrower type of 'obj'?

              // ME 20100323: I decided not to because a) we don't
              // know the "proper" class to narrow to (i.e. maybe
              // there's something "narrower" and b) I'm not sure how
              // primitive types relate to their boxed
              // representations.  
                return obj;
            } else {
                return error(new TypeError(intendedClass.getName() + " is not assignable to " + c.getName()));
            }
        }
    }

    /** Returns the encapsulated Java object for
     * interoperability with wait, notify, synchronized, etc.
     *
     * @return The encapsulated object
     */
    @Override
    public Object lockableInstance() {
        return obj;
    }

    public Class<?> getIntendedClass() {
        return intendedClass;
    }

    public static final Object getObject(LispObject o)

    {
        if (o instanceof JavaObject)
                return ((JavaObject)o).obj;        
        return             // Not reached.
        type_error(o, Symbol.JAVA_OBJECT);       
    }

    @Override
    public final boolean equal(LispObject other)
    {
        if (this == other)
            return true;
        if (other instanceof JavaObject)
            return (obj == ((JavaObject)other).obj);
        return false;
    }

    @Override
    public final boolean equalp(LispObject other)
    {
        return equal(other);
    }

    @Override
    public int sxhash()
    {
        return obj == null ? 0 : (obj.hashCode() & 0x7ffffff);
    }

    public static LispObject JAVA_OBJECT_TO_STRING_LENGTH 
        = LispInteger.getInstance(32);

    public static final Symbol _JAVA_OBJECT_TO_STRING_LENGTH 
        = exportSpecial("*JAVA-OBJECT-TO-STRING-LENGTH*", 
                        PACKAGE_JAVA, JAVA_OBJECT_TO_STRING_LENGTH);

    static {
        String doc = "Length to truncate toString() PRINT-OBJECT output for an otherwise "
                  +  "unspecialized JAVA-OBJECT.  Can be set to NIL to indicate no limit.";
        _JAVA_OBJECT_TO_STRING_LENGTH
            .setDocumentation(Symbol.VARIABLE, new SimpleString(doc));
    }

    @Override
    public String writeToString()
    {
        if (obj instanceof ControlTransfer)
            return obj.toString();
        final String s;
        if(obj != null) {
            Class<?> c = obj.getClass();
            StringBuilder sb
                = new StringBuilder(c.isArray() ? "jarray" : c.getName());
            sb.append(' ');
            String ts = obj.toString();
            int length = -1;
            LispObject stringLength = _JAVA_OBJECT_TO_STRING_LENGTH.symbolValueNoThrow();
            if (stringLength instanceof Fixnum) {
                length = Fixnum.getValue(stringLength);
            }
            if (length < 0) {
                sb.append(ts);
            }else if (ts.length() > length) { 
                // use '....' to not confuse user with PPRINT conventions
                sb.append(ts.substring(0, length)).append("...."); 
            } else {
                sb.append(ts);
            }
            s = sb.toString();
        } else {
            s = "null";
        }
        return unreadableString(s);
    }

    @Override
    public LispObject getDescription() {
        return new SimpleString(describeJavaObject(this));
    }

    @Override
    public LispObject getParts() {
        if(obj != null) {
            LispObject parts = NIL;
            parts = parts.push(new Cons("Java class",
                                        new JavaObject(obj.getClass())));
            if (intendedClass != null) {
                parts = parts.push(new Cons("intendedClass", new SimpleString(intendedClass.getCanonicalName())));
            }
            if (obj.getClass().isArray()) {
                int length = Array.getLength(obj);
                for (int i = 0; i < length; i++) {
                    parts = parts
                        .push(new Cons(new SimpleString(i), 
                                       JavaObject.getInstance(Array.get(obj, i))));
                }
            } else {
                parts = Symbol.NCONC.execute(parts, getInspectedFields());
            }
            return parts.nreverse();
        } else {
            return NIL;
        }
    }

    private LispObject getInspectedFields()
        {
        final LispObject[] acc = new LispObject[] { NIL };
        doClassHierarchy(obj.getClass(), new Function() {
                @Override
                public LispObject execute(LispObject arg)
                    {
                    //No possibility of type error - we're mapping this function
                    //over a list of classes
                    Class<?> c = (Class) arg.javaInstance();
                    for(Field f : c.getDeclaredFields()) {
                        LispObject value = NIL;
                        try {
                            if(!f.isAccessible()) {
                                f.setAccessible(true);
                            }
                            value = JavaObject.getInstance(f.get(obj));
                        } catch(Exception e) {}
                        acc[0] = acc[0].push(new Cons(f.getName(), value));
                    }
                    return acc[0];
                }
            });
        return acc[0].nreverse();
    }

    /**
     * Executes a function repeatedly over the minimal subtree of the
     * Java class hierarchy which contains every class in <classes>.
     */
    private static void doClassHierarchy(Collection<Class<?>> classes,
                                         LispObject callback,
                                         Set<Class<?>> visited)
        {
        Collection<Class<?>> newClasses = new LinkedList<Class<?>>();
        for(Class<?> clss : classes) {
            if(clss == null) {
                continue;
            }
            if(!visited.contains(clss)) {
                callback.execute(JavaObject.getInstance(clss, true));
                visited.add(clss);
            }
            if(!visited.contains(clss.getSuperclass())) {
                newClasses.add(clss.getSuperclass());
            }
            for(Class<?> iface : clss.getInterfaces()) {
                if (!visited.contains(iface)) {
                    newClasses.add(iface);
                }
            }
        }
        if(!newClasses.isEmpty()) {
            doClassHierarchy(newClasses, callback, visited);
        }
    }

    /**
     * Executes a function recursively over <clss> and its superclasses and
     * interfaces.
     */
    public static void doClassHierarchy(Class<?> clss, LispObject callback)
        {
        if (clss != null) {
            Set<Class<?>> visited = new HashSet<Class<?>>();
            Collection<Class<?>> classes = new ArrayList<Class<?>>(1);
            classes.add(clss);
            doClassHierarchy(classes, callback, visited);
        }
    }

    public static LispObject mapcarClassHierarchy(Class<?> clss,
                                                  final LispObject fn)
    {
        final LispObject[] acc = new LispObject[] { NIL };
        doClassHierarchy(clss, new Function() {
                @Override
                public LispObject execute(LispObject arg)
                    {
                    acc[0] = acc[0].push(fn.execute(arg));
                    return acc[0];
                }
            });
        return acc[0].nreverse();
    }

    public static String describeJavaObject(final JavaObject javaObject)
        {
        final Object obj = javaObject.getObject();
        final StringBuilder sb =
            new StringBuilder(javaObject.writeToString());
        sb.append(" is an object of type ");
        sb.append(Symbol.JAVA_OBJECT.writeToString());
        sb.append(".");
        sb.append(System.getProperty("line.separator"));
        sb.append("The wrapped Java object is ");
        if (obj == null) {
            sb.append("null.");
        } else {
            sb.append("an ");
            final Class c = obj.getClass();
            String className = c.getName();
            if (c.isArray()) {
                sb.append("array of ");
                if (className.startsWith("[L") && className.endsWith(";")) {
                    className = className.substring(1, className.length() - 1);
                    sb.append(className);
                    sb.append(" objects");
                } else if (className.startsWith("[") && className.length() > 1) {
                    char descriptor = className.charAt(1);
                    final String type;
                    switch (descriptor) {
                    case 'B': type = "bytes"; break;
                    case 'C': type = "chars"; break;
                    case 'D': type = "doubles"; break;
                    case 'F': type = "floats"; break;
                    case 'I': type = "ints"; break;
                    case 'J': type = "longs"; break;
                    case 'S': type = "shorts"; break;
                    case 'Z': type = "booleans"; break;
                    default:
                        type = "unknown type";
                    }
                    sb.append(type);
                }
                sb.append(" with ");
                final int length = java.lang.reflect.Array.getLength(obj);
                sb.append(length);
                sb.append(" element");
                if (length != 1)
                    sb.append('s');
                sb.append('.');
            } else {
                sb.append("instance of ");
                sb.append(className);
                sb.append(':');
                sb.append(System.getProperty("line.separator"));
                sb.append("  \"");
                sb.append(obj.toString());
                sb.append('"');
            }
        }
        return sb.toString();
    }

    // ### describe-java-object
    private static final Primitive DESCRIBE_JAVA_OBJECT =
        new Primitive("describe-java-object", PACKAGE_JAVA, true)
    {
        @Override
        public LispObject execute(LispObject first, LispObject second)

        {
            if (!(first instanceof JavaObject))
                return type_error(first, Symbol.JAVA_OBJECT);
            final Stream stream = checkStream(second);
            final JavaObject javaObject = (JavaObject) first;
            stream._writeString(describeJavaObject(javaObject));
            return LispThread.currentThread().nothing();
        }
    };

    //JAVA-CLASS support

    //There is no point for this Map to be weak since values keep a reference to the corresponding
    //key (the Java class). This should not be a problem since Java classes are limited in number - 
    //if they grew indefinitely, the JVM itself would crash.
    private static final Map<Class<?>, LispObject> javaClassMap = new HashMap<Class<?>, LispObject>();

    public static LispObject registerJavaClass(Class<?> javaClass, LispObject classMetaObject) {
        synchronized (javaClassMap) {
            javaClassMap.put(javaClass, classMetaObject);
            return classMetaObject;
        }
    }

    public static LispObject findJavaClass(Class<?> javaClass) {
        synchronized (javaClassMap) {
            LispObject c = javaClassMap.get(javaClass);
            if (c != null) {
                return c;
            } else {
                return NIL;
            }
        }
    }

    private static final Primitive _FIND_JAVA_CLASS = new Primitive("%find-java-class", PACKAGE_JAVA, false, "class-name-or-class") {
            public LispObject execute(LispObject arg) {
                try {
                    if(arg instanceof AbstractString) {
                        return findJavaClass(Class.forName((String) arg.getStringValue()));
                    } else {
                        return findJavaClass((Class<?>) arg.javaInstance());
                    }
                } catch (ClassNotFoundException e) {
                    return error(new LispError("Cannot find Java class " + arg.getStringValue()));
                }
            }
            
        };

    private static final Primitive _REGISTER_JAVA_CLASS = new Primitive("%register-java-class", PACKAGE_JAVA, false, "jclass class-metaobject") {
            public LispObject execute(LispObject jclass, LispObject classMetaObject) {
                return registerJavaClass((Class<?>) jclass.javaInstance(), classMetaObject);
            }
            
        };

}
