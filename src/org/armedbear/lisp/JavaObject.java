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

import java.lang.reflect.*;

import java.math.BigInteger;

import java.util.*;

public final class JavaObject extends LispObject
{
    private final Object obj;

    public JavaObject(Object obj)
    {
        this.obj = obj;
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
                return JavaClass.findJavaClass(obj.getClass());
        }
    }

    @Override
    public LispObject typep(LispObject type) throws ConditionThrowable
    {
        if (type == Symbol.JAVA_OBJECT)
            return T;
        if (type == BuiltInClass.JAVA_OBJECT)
            return T;
        if(type instanceof JavaClass && obj != null) {
                return ((JavaClass) type).getJavaClass().isAssignableFrom(obj.getClass()) ? T : NIL;
        }
        return super.typep(type);
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
     * If obj is of a type which can be mapped to a lisp type,
     * an object of the mapped type is returned, if translated is true.
     *
     * @param obj
     * @param translated
     * @return a LispObject representing or encapsulating obj
     */
    public final static LispObject getInstance(Object obj, boolean translated)
            throws ConditionThrowable
    {
        if (! translated)
            return getInstance(obj);

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
            return new LispCharacter((Character)obj);

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
        return new JavaObject(obj);
    }

    @Override
    public Object javaInstance()
    {
        return obj;
    }

    @Override
    public Object javaInstance(Class c) throws ConditionThrowable {
	return javaInstance();
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

    public static final Object getObject(LispObject o)
        throws ConditionThrowable
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

    @Override
    public String writeToString() throws ConditionThrowable
    {
        if (obj instanceof ConditionThrowable)
            return obj.toString();
	final String s;
	if(obj != null) {
	    Class<?> c = obj.getClass();
	    FastStringBuffer sb
		= new FastStringBuffer(c.isArray() ? "jarray" : c.getName());
	    sb.append(' ');
	    String ts = obj.toString();
	    if(ts.length() > 32) { //random value, should be chosen sensibly
		sb.append(ts.substring(0, 32) + "...");
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
    public LispObject getDescription() throws ConditionThrowable {
	return new SimpleString(describeJavaObject(this));
    }

    @Override
    public LispObject getParts() throws ConditionThrowable {
	if(obj != null) {
	    LispObject parts = NIL;
	    if(obj.getClass().isArray()) {
		SimpleString empty = new SimpleString("");
		int length = Array.getLength(obj);
		for(int i = 0; i < length; i++) {
		    parts = parts.push
			(new Cons(empty, JavaObject.getInstance(Array.get(obj, i))));
		}
		parts = parts.nreverse();
	    } else {
		parts = parts.push(new Cons("Java class",
					    new JavaObject(obj.getClass())));
		parts = Symbol.NCONC.execute(parts, getInspectedFields());
	    }
	    return parts;
	} else {
	    return NIL;
	}
    }

    private LispObject getInspectedFields()
	throws ConditionThrowable {
	final LispObject[] acc = new LispObject[] { NIL };
	doClassHierarchy(obj.getClass(), new Function() {
		@Override
		public LispObject execute(LispObject arg)
		    throws ConditionThrowable {
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
	throws ConditionThrowable {
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
	throws ConditionThrowable {
	if (clss != null) {
	    Set<Class<?>> visited = new HashSet<Class<?>>();
	    Collection<Class<?>> classes = new ArrayList<Class<?>>(1);
	    classes.add(clss);
	    doClassHierarchy(classes, callback, visited);
	}
    }

    public static LispObject mapcarClassHierarchy(Class<?> clss,
						  final LispObject fn)
    throws ConditionThrowable {
	final LispObject[] acc = new LispObject[] { NIL };
	doClassHierarchy(clss, new Function() {
		@Override
		public LispObject execute(LispObject arg)
		    throws ConditionThrowable {
		    acc[0] = acc[0].push(fn.execute(arg));
		    return acc[0];
		}
	    });
	return acc[0].nreverse();
    }

    public static String describeJavaObject(final JavaObject javaObject)
	throws ConditionThrowable {
	final Object obj = javaObject.getObject();
	final FastStringBuffer sb =
	    new FastStringBuffer(javaObject.writeToString());
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
            throws ConditionThrowable
        {
            if (!(first instanceof JavaObject))
                return type_error(first, Symbol.JAVA_OBJECT);
            final Stream stream = checkStream(second);
            final JavaObject javaObject = (JavaObject) first;
            stream._writeString(describeJavaObject(javaObject));
            return LispThread.currentThread().nothing();
        }
    };
}
