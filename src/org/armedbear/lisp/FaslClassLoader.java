/*
 * JavaClassLoader.java
 *
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

import java.io.InputStream;
import static org.armedbear.lisp.Lisp.*;


public class FaslClassLoader extends JavaClassLoader {

    private final String baseName;
    private final JavaObject boxedThis = new JavaObject(this);

    public FaslClassLoader(String baseName) {
        this.baseName = baseName;
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
        if (name.startsWith(baseName + "_")) {
            String internalName = "org/armedbear/lisp/" + name;
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

    @Override
    public InputStream getResourceAsStream(String resourceName) {
      final LispThread thread = LispThread.currentThread();

      Pathname name = new Pathname(resourceName.substring("org/armedbear/lisp/".length()));
      LispObject truenameFasl = Symbol.LOAD_TRUENAME_FASL.symbolValue(thread);
      LispObject truename = Symbol.LOAD_TRUENAME.symbolValue(thread);
      
      if (truenameFasl instanceof Pathname) {
          return Pathname.mergePathnames(name, (Pathname)truenameFasl, Keyword.NEWEST)
                    .getInputStream();
      } else if (truename instanceof Pathname) {
          return Pathname.mergePathnames(name, (Pathname) truename, Keyword.NEWEST)
                  .getInputStream();
      } else if (!Pathname.truename(name).equals(NIL)) {
              return name.getInputStream();
      }

      return null;
    }

    public byte[] getFunctionClassBytes(String name) {
        Pathname pathname = new Pathname(name.substring("org/armedbear/lisp/".length()) + "." + Lisp._COMPILE_FILE_CLASS_EXTENSION_.symbolValue().getStringValue());
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
        //Function name is fnIndex + 1
        String name = baseName + "_" + (fnNumber + 1);
        try {
            Function f = (Function) loadClass(name).newInstance();
            f.setClassBytes(getFunctionClassBytes(name));
            return f;
        } catch(Throwable e) {
            if(e instanceof ControlTransfer) { throw (ControlTransfer) e; }
            Debug.trace(e);
            return error(new LispError("Compiled function can't be loaded: " + name + " from " + Symbol.LOAD_TRUENAME.symbolValue()));
        }
    }

    private static final Primitive MAKE_FASL_CLASS_LOADER = new pf_make_fasl_class_loader();
    private static final class pf_make_fasl_class_loader extends Primitive {
        pf_make_fasl_class_loader() {
            super("make-fasl-class-loader", PACKAGE_SYS, false, "base-name");
        }

        @Override
        public LispObject execute(LispObject baseName) {
            return new FaslClassLoader(baseName.getStringValue()).boxedThis;
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
	    return l.loadFunction(fnNumber.intValue());
        }
    };

}