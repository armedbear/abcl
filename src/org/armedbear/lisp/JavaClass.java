/*
 * BuiltInClass.java
 *
 * Copyright (C) 2003-2007 Peter Graves
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
 */

package org.armedbear.lisp;

import static org.armedbear.lisp.Lisp.*;

import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.Map;
import java.util.Queue;
import java.util.Set;
import java.util.Stack;

public class JavaClass extends LispClass {

	private Class<?> javaClass;
	//There is no point for this Map to be weak since values keep a reference to the corresponding
	//key (the Java class). This should not be a problem since Java classes are limited in number - 
	//if they grew indefinitely, the JVM itself would crash.
	private static final Map<Class<?>, JavaClass> cache = new HashMap<Class<?>, JavaClass>();

	private JavaClass(Class<?> javaClass) {
	    super(new Symbol(javaClass.getCanonicalName()));
	    this.javaClass = javaClass;
	    setDirectSuperclass(BuiltInClass.JAVA_OBJECT);
	}

	private void initCPL() {
		LispObject cpl = Lisp.NIL;
		cpl = cpl.push(BuiltInClass.CLASS_T);
		cpl = cpl.push(BuiltInClass.JAVA_OBJECT);
		Set<Class<?>> alreadySeen = new HashSet<Class<?>>();
		Stack<JavaClass> stack = new Stack<JavaClass>();
		Class<?> theClass = javaClass;
		boolean stop = false;
		while(!stop && theClass != null) {
			stop = addClass(alreadySeen, stack, theClass);
			for(Class<?> c : theClass.getInterfaces()) {
				stop = addClass(alreadySeen, stack, c) && stop; //watch out for short-circuiting!
			}
			theClass = theClass.getSuperclass();
		}
		while(!stack.isEmpty()) {
			cpl = cpl.push(stack.pop());
		}
		setCPL(cpl);
	}

	private static boolean addClass(Set<Class<?>> alreadySeen, Stack<JavaClass> stack, Class<?> theClass) {
		if(!alreadySeen.contains(theClass)) {
			alreadySeen.add(theClass);
			stack.push(findJavaClass(theClass));
			return false;
		}
		return true;
	}

	public LispObject typeOf() {
		return Symbol.JAVA_CLASS;
	}

	public LispObject classOf() {
		return StandardClass.JAVA_CLASS;
	}

	public LispObject typep(LispObject type) {
		if (type == Symbol.JAVA_CLASS)
			return T;
		if (type == StandardClass.JAVA_CLASS)
			return T;
		return super.typep(type);
	}

	public LispObject getDescription() {
		return new SimpleString(writeToString());
	}

	public String writeToString() {
		StringBuilder sb = new StringBuilder("#<JAVA-CLASS ");
		sb.append(javaClass.getCanonicalName());
		sb.append('>');
		return sb.toString();
	}

	public static JavaClass findJavaClass(Class<?> javaClass) {
		synchronized (cache) {
			JavaClass c = cache.get(javaClass);
			if (c == null) {
				c = new JavaClass(javaClass);
				cache.put(javaClass, c);
				c.initCPL();
			}
			return c;
		}
	}

	public Class<?> getJavaClass() {
		return javaClass;
	}

	public boolean subclassp(LispObject obj) {
		if(obj == BuiltInClass.CLASS_T) {
			return true;
		}
		if(obj == BuiltInClass.JAVA_OBJECT) {
			return true;
		}
		if(obj instanceof JavaClass) {
			return ((JavaClass) obj).getJavaClass().isAssignableFrom(javaClass);
		}
		return false;
	}

	private static final Primitive _FIND_JAVA_CLASS = new Primitive(
			"%find-java-class", PACKAGE_JAVA, false, "string") {
		public LispObject execute(LispObject arg) {
		    try {
			return findJavaClass(Class.forName((String) arg.getStringValue()));
		    } catch (ClassNotFoundException e) {
			return error(new LispError("Cannot find Java class " + arg.getStringValue()));
		    }
		}

	};

}
