/*
 * JavaClassLoader.java
 *
 * Copyright (C) 2010 Alessio Stalla
 * $Id: JavaClassLoader.java 12298 2009-12-18 21:50:54Z ehuelsmann $
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

public class FaslClassLoader extends JavaClassLoader {

    private final LispObject[] functions;
    private String baseName;
    private LispObject loader; //The function used to load FASL functions by number
    private final JavaObject boxedThis = new JavaObject(this);
    
    public FaslClassLoader(int functionCount, String baseName, boolean useLoaderFunction) {
	functions = new LispObject[functionCount];
	this.baseName = baseName;
	if(useLoaderFunction) {
	    try {
		this.loader = (LispObject) loadClass(baseName + "_0").newInstance();
	    } catch(Exception e) {
		//e.printStackTrace();
		Debug.trace("useLoaderFunction = true but couldn't fully init FASL loader, will fall back to reflection!");
	    }
	}
    }

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
	Pathname pathname = new Pathname(name.substring("org/armedbear/lisp/".length()) + ".cls");
	return readFunctionBytes(pathname);
    }
    
    public byte[] getFunctionClassBytes(Class<?> functionClass) {
	return getFunctionClassBytes(functionClass.getName());
    }

    public byte[] getFunctionClassBytes(Function f) {
	byte[] b = getFunctionClassBytes(f.getClass());
	f.setClassBytes(b);
	return b;
    }

    public LispObject loadFunction(int fnNumber) {
	try {
	    //Function name is fnIndex + 1
	    LispObject o = (LispObject) loadClass(baseName + "_" + (fnNumber + 1)).newInstance();
	    functions[fnNumber] = o;
	    return o;
	} catch(Exception e) {
	    e.printStackTrace();
	    if(e instanceof ControlTransfer) { throw (ControlTransfer) e; }
	    throw new RuntimeException(e);
	}
    }
    
    public LispObject getFunction(int fnNumber) {
	if(fnNumber >= functions.length) {
	    return error(new LispError("Compiled function not found: " + baseName + "_" + (fnNumber + 1) + " " + Symbol.LOAD_TRUENAME.symbolValue()));
	}
	LispObject o = functions[fnNumber];
	if(o == null) {
	    if(loader != null) {
		loader.execute(boxedThis, Fixnum.getInstance(fnNumber));
		return functions[fnNumber];
	    } else { //Fallback to reflection
		return loadFunction(fnNumber);
	    }
	} else {
	    return o;
	}
    }

    public LispObject putFunction(int fnNumber, LispObject fn) {
	functions[fnNumber] = fn;
	return fn;
    }

    private static final Primitive MAKE_FASL_CLASS_LOADER = new pf_make_fasl_class_loader();
    private static final class pf_make_fasl_class_loader extends Primitive {
	pf_make_fasl_class_loader() {
            super("make-fasl-class-loader", PACKAGE_SYS, false, "function-count base-name");
        }

        @Override
        public LispObject execute(LispObject functionCount, LispObject baseName) {
            return execute(functionCount, baseName, T);
        }

        @Override
        public LispObject execute(LispObject functionCount, LispObject baseName, LispObject init) {
            return new FaslClassLoader(functionCount.intValue(), baseName.getStringValue(), init != NIL).boxedThis;
        }
    };

    private static final Primitive GET_FASL_FUNCTION = new pf_get_fasl_function();
    private static final class pf_get_fasl_function extends Primitive {
	pf_get_fasl_function() {
            super("get-fasl-function", PACKAGE_SYS, false, "loader function-number");
        }

        @Override
        public LispObject execute(LispObject loader, LispObject fnNumber) {
            FaslClassLoader l = (FaslClassLoader) loader.javaInstance(FaslClassLoader.class);
	    return l.getFunction(fnNumber.intValue());
        }
    };

}