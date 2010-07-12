/*
 * JavaClassLoader.java
 *
 * Copyright (C) 2003-2005 Peter Graves
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

import java.util.Collections;
import java.util.HashSet;
import java.util.Set;
import java.net.URL;
import java.net.URLClassLoader;

public class JavaClassLoader extends URLClassLoader {

    private static JavaClassLoader persistentInstance;

    private static Set<String> packages = Collections.synchronizedSet(new HashSet<String>());

    public JavaClassLoader()
    {
        this(JavaClassLoader.class.getClassLoader());
    }

    public JavaClassLoader(ClassLoader parent) {
	super(new URL[] {}, parent);
    }

    public JavaClassLoader(URL[] classpath, ClassLoader parent) {
	super(classpath, parent);
    }

    public static JavaClassLoader getPersistentInstance()
    {
        return getPersistentInstance(null);
    }

    public static JavaClassLoader getPersistentInstance(String packageName)
    {
        if (persistentInstance == null)
            persistentInstance = new JavaClassLoader();
	definePackage(packageName);
        return persistentInstance;
    }

    private static void definePackage(String packageName)
    {
        if (packageName != null && !packages.contains(packageName)) {
            persistentInstance.definePackage(packageName,"","1.0","","","1.0","",null);
            packages.add(packageName);
        }
    }

    public Class<?> loadClassFromByteArray(byte[] classbytes) {
        return loadClassFromByteArray(null, classbytes);
    }

    public Class<?> loadClassFromByteArray(String className,
                                                byte[] classbytes)
    {
        try {
            long length = classbytes.length;
            if (length < Integer.MAX_VALUE) {
                Class<?> c =
                    defineClass(className, classbytes, 0, (int) length);
                if (c != null) {
                    resolveClass(c);
                    return c;
                }
            }
        }
    	catch (LinkageError e) {
                throw e;
    	}
        catch (Throwable t) {
            Debug.trace(t);
        }
        return null;
    }

    public Class<?> loadClassFromByteArray(String className, byte[] bytes,
                                                int offset, int length)
    {
        try {
            Class<?> c = defineClass(className, bytes, offset, length);
            if (c != null) {
                resolveClass(c);
                return c;
            }
        }
        catch (VerifyError e)
          {
            error(new LispError("Class verification failed: " + e.getMessage()));
          }
        catch (Throwable t) {
            Debug.trace(t);
        }
        return null;
    }

    @Override
    public void addURL(URL url) {
	super.addURL(url);
    }

    public static final Symbol CLASSLOADER = PACKAGE_JAVA.intern("*CLASSLOADER*");

    private static final Primitive GET_DEFAULT_CLASSLOADER = new pf_get_default_classloader();
    private static final class pf_get_default_classloader extends Primitive {
	
	private final LispObject defaultClassLoader = new JavaObject(new JavaClassLoader());

        pf_get_default_classloader() {
            super("get-default-classloader", PACKAGE_JAVA, true, "");
        }

        @Override
        public LispObject execute() {
	    return defaultClassLoader;
        }
    };

    // ### make-classloader &optional parent => java-class-loader
    private static final Primitive MAKE_CLASSLOADER = new pf_make_classloader();
    private static final class pf_make_classloader extends Primitive 
    {
        pf_make_classloader() 
        {
            super("make-classloader", PACKAGE_JAVA, true, "&optional parent");
        }

        @Override
        public LispObject execute() {
	    return new JavaObject(new JavaClassLoader(getCurrentClassLoader()));
        }

        @Override
        public LispObject execute(LispObject parent) {
	    return new JavaObject(new JavaClassLoader((ClassLoader) parent.javaInstance(ClassLoader.class)));
        }
    };

    // ### dump-classpath &optional classloader => list-of-pathname-lists
    private static final Primitive DUMP_CLASSPATH = new pf_dump_classpath();
    private static final class pf_dump_classpath extends Primitive 
    {
        pf_dump_classpath() 
        {
            super("dump-classpath", PACKAGE_JAVA, true, "&optional classloader");
        }

        @Override
        public LispObject execute() {
	    return execute(new JavaObject(getCurrentClassLoader()));
        }

        @Override
        public LispObject execute(LispObject classloader) {
	    LispObject list = NIL;
	    Object o = classloader.javaInstance();
	    while(o instanceof ClassLoader) {
		ClassLoader cl = (ClassLoader) o;
		list = list.push(dumpClassPath(cl));
		o = cl.getParent();
	    }
	    return list.nreverse();
        }
    };

    // ### add-to-classpath jar-or-jars &optional (classloader (get-current-classloader))
    private static final Primitive ADD_TO_CLASSPATH = new pf_add_to_classpath();
    private static final class pf_add_to_classpath extends Primitive 
    {
        pf_add_to_classpath() 
        {
            super("add-to-classpath", PACKAGE_JAVA, true, "jar-or-jars &optional (classloader (get-current-classloader))");
        }

        @Override
        public LispObject execute(LispObject jarOrJars) {
	    return execute(jarOrJars, new JavaObject(getCurrentClassLoader()));
        }

        @Override
        public LispObject execute(LispObject jarOrJars, LispObject classloader) {
	    Object o = classloader.javaInstance();
	    if(o instanceof JavaClassLoader) {
		JavaClassLoader jcl = (JavaClassLoader) o;
		if(jarOrJars instanceof Cons) {
		    while(jarOrJars != NIL) {
			addURL(jcl, jarOrJars.car());
			jarOrJars = jarOrJars.cdr();
		    }
		} else {
		    addURL(jcl, jarOrJars);
		}
		return T;
	    } else {
		return error(new TypeError(o + " must be an instance of " + JavaClassLoader.class.getName()));
	    }
        }
    };

    protected static void addURL(JavaClassLoader jcl, LispObject jar) {
	try {
	    if(jar instanceof Pathname) {
		jcl.addURL(((Pathname) jar).toURL());
	    } else if(jar instanceof AbstractString) {
		jcl.addURL(new Pathname(jar.toString()).toURL());
	    } else {
		error(new TypeError(jar + " must be a pathname designator"));
	    }
	} catch(java.net.MalformedURLException e) {
	    error(new LispError(jar + " is not a valid URL"));
	}
    }


    public static LispObject dumpClassPath(ClassLoader o) {
	if(o instanceof URLClassLoader) {
	    LispObject list = NIL;
	    for(URL u : ((URLClassLoader) o).getURLs()) {
		list = list.push(new Pathname(u));
	    }
	    return new Cons(new JavaObject(o), list.nreverse());
	} else {
	    return new JavaObject(o);
	}
    }

    public static ClassLoader getCurrentClassLoader() {
	LispObject classLoader = CLASSLOADER.symbolValueNoThrow();
	if(classLoader != null) {
	    return (ClassLoader) classLoader.javaInstance(ClassLoader.class);
	} else {
	    return Lisp.class.getClassLoader();
	}
    }



}
