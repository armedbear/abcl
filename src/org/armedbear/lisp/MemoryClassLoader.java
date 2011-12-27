/*
 * MemoryClassLoader.java
 *
 * Copyright (C) 2011 Erik Huelsmann
 * Copyright (C) 2010 Alessio Stalla
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

import java.util.*;

public class MemoryClassLoader extends JavaClassLoader {

    private final HashMap<String, JavaObject> hashtable = new HashMap<String, JavaObject>();
    private final JavaObject boxedThis = new JavaObject(this);
    private final String internalNamePrefix;

    public MemoryClassLoader() {
        this("org/armedbear/lisp/");
    }

    public MemoryClassLoader(String internalNamePrefix) {
        this.internalNamePrefix = internalNamePrefix;
    }

    @Override
    protected Class<?> loadClass(String name, boolean resolve)
            throws ClassNotFoundException {
        /* First we check if we should load the class ourselves,
         * allowing the default handlers to kick in if we don't...
         *
         * This strategy eliminates ClassNotFound exceptions inside
         * the inherited loadClass() eliminated ~80k exceptions during
         * Maxima compilation. Generally, creation of an exception object
         * is a pretty heavy operation, because it processes the call stack,
         * which - in ABCL - is pretty deep, most of the time.
         */
        if (hashtable.containsKey(name)) {
            String internalName = internalNamePrefix + name;
            Class<?> c = this.findLoadedClass(internalName);

            if (c == null) {
                c = findClass(name);
            }
            if (c != null) {
                if (resolve) {
                    resolveClass(c);
                }
                return c;
            }
        }

        // Fall through to our super's default handling
        return super.loadClass(name, resolve);
    }

    @Override
    protected Class<?> findClass(String name) throws ClassNotFoundException {
        try {
            byte[] b = getFunctionClassBytes(name);
            return defineClass(name, b, 0, b.length);
        } catch(Throwable e) { //TODO handle this better, readFunctionBytes uses Debug.assert() but should return null
            e.printStackTrace();
            if(e instanceof ControlTransfer) { throw (ControlTransfer) e; }
            throw new ClassNotFoundException("Function class not found: " + name, e);
        }
    }

    public byte[] getFunctionClassBytes(String name) {
        return (byte[])hashtable.get(name).javaInstance();
    }

    public byte[] getFunctionClassBytes(Class<?> functionClass) {
        return getFunctionClassBytes(functionClass.getName());
    }

    public byte[] getFunctionClassBytes(Function f) {
        byte[] b = getFunctionClassBytes(f.getClass());
        f.setClassBytes(b);
        return b;
    }

    public LispObject loadFunction(String name) {
        try {
            Function f = (Function) loadClass(name).newInstance();
            f.setClassBytes(getFunctionClassBytes(name));
            return f;
        } catch(Throwable e) {
            if(e instanceof ControlTransfer) { throw (ControlTransfer) e; }
            Debug.trace(e);
            return error(new LispError("Compiled function can't be loaded: " + name + " from memory"));
        }
    }

    private static final Primitive MAKE_MEMORY_CLASS_LOADER = new pf_make_memory_class_loader();
    private static final class pf_make_memory_class_loader extends Primitive {
        pf_make_memory_class_loader() {
            super("make-memory-class-loader", PACKAGE_SYS, false);
        }

        @Override
        public LispObject execute() {
            return new MemoryClassLoader().boxedThis;
        }
    };

    private static final Primitive PUT_MEMORY_FUNCTION = new pf_put_memory_function();
    private static final class pf_put_memory_function extends Primitive {
	pf_put_memory_function() {
            super("put-memory-function", PACKAGE_SYS, false, "loader class-name class-bytes");
        }

        @Override
        public LispObject execute(LispObject loader, LispObject className, LispObject classBytes) {
            MemoryClassLoader l = (MemoryClassLoader) loader.javaInstance(MemoryClassLoader.class);
	    return (LispObject)l.hashtable.put(className.getStringValue(), (JavaObject)classBytes);
        }
    };
    
    private static final Primitive GET_MEMORY_FUNCTION = new pf_get_memory_function();
    private static final class pf_get_memory_function extends Primitive {
	pf_get_memory_function() {
            super("get-memory-function", PACKAGE_SYS, false, "loader class-name");
        }

        @Override
        public LispObject execute(LispObject loader, LispObject name) {
            MemoryClassLoader l = (MemoryClassLoader) loader.javaInstance(MemoryClassLoader.class);
	    return l.loadFunction(name.getStringValue());
        }
    };


}