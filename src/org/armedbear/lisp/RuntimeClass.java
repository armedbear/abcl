/*
 * RuntimeClass.java
 *
 * Copyright (C) 2004 Peter Graves
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

import java.io.File;
import java.util.Map;
import java.util.HashMap;

public class RuntimeClass
{
    static Map<String,RuntimeClass> classes = new HashMap<String,RuntimeClass>();

    private Map<String,Function> methods = new HashMap<String,Function>();

    // ### %jnew-runtime-class
    // %jnew-runtime-class class-name &rest method-names-and-defs
    private static final Primitive _JNEW_RUNTIME_CLASS =
        new Primitive("%jnew-runtime-class", PACKAGE_JAVA, false, "class-name &rest method-names-and-defs")
    {
        @Override
        public LispObject execute(LispObject[] args)
        {
            int length = args.length;
            if (length < 3 || length % 2 != 1)
                return error(new WrongNumberOfArgumentsException(this));
	      RuntimeClass rc = new RuntimeClass();
	      String className = args[0].getStringValue();
            for (int i = 1; i < length; i = i+2) {
                String methodName = args[i].getStringValue();
                rc.addLispMethod(methodName, (Function)args[i+1]);
	      }
            classes.put(className, rc);
	      return T;
        }
    };

    // ### jredefine-method
    // %jredefine-method class-name method-name method-def
    private static final Primitive _JREDEFINE_METHOD =
        new Primitive("%jredefine-method", PACKAGE_JAVA, false,
                      "class-name method-name method-def")
    {
        @Override
        public LispObject execute(LispObject className, LispObject methodName,
                                  LispObject methodDef)

        {

	    String cn = className.getStringValue();
	    String mn = methodName.getStringValue();
	    Function def = (Function) methodDef;
	    RuntimeClass rc = null;
	    if (classes.containsKey(cn)) {
                rc = (RuntimeClass) classes.get(cn);
                rc.addLispMethod(mn, def);
                return T;
	    }
	    else {
                error(new LispError("undefined Java class: " + cn));
                return NIL;
	    }
        }
    };

    // ### %load-java-class-from-byte-array
    private static final Primitive _LOAD_JAVA_CLASS_FROM_BYTE_ARRAY =
        new Primitive("%load-java-class-from-byte-array", PACKAGE_JAVA, false,
                      "classname bytearray")
    {
        @Override
        public LispObject execute(LispObject className, LispObject classBytes)

        {
            String cn = className.getStringValue();
	      String pn = cn.substring(0,cn.lastIndexOf('.'));
	      byte[] cb = (byte[]) classBytes.javaInstance();
            try {
                JavaClassLoader loader = JavaClassLoader.getPersistentInstance(pn);
                Class c = loader.loadClassFromByteArray(cn, cb);
                if (c != null) {
                    return T;
                }
            }
            catch (VerifyError e) {
                return error(new LispError("class verification failed: " +
                                            e.getMessage()));
            }
            catch (LinkageError e) {
                return error(new LispError("class could not be linked: " +
                                            e.getMessage()));
            }
            return error(
                new LispError("unable to load ".concat(cn)));
        }
    };

    public static final LispObject evalC(LispObject function,
                                         LispObject args,
                                         Environment env,
                                         LispThread thread)

    {
        return evalCall(function, args, env, thread);
    }

    public static RuntimeClass getRuntimeClass(String className) {
        return (RuntimeClass) classes.get(className);
    }

    public Function getLispMethod(String methodName) {
        return (Function) methods.get(methodName);
    }

    void addLispMethod(String methodName, Function def) {
        methods.put(methodName, def);
    }

    public static final LispObject makeLispObject(Object obj)
    {
        return new JavaObject(obj);
    }

    public static final Fixnum makeLispObject(byte i)
    {
        return Fixnum.getInstance(i);
    }

    public static final Fixnum makeLispObject(short i)
    {
        return Fixnum.getInstance(i);
    }

    public static final Fixnum makeLispObject(int i)
    {
        return Fixnum.getInstance(i);
    }

    public static final LispInteger makeLispObject(long i)
    {
        return Bignum.getInstance(i);
    }

    public static final SingleFloat makeLispObject(float i)
    {
        return new SingleFloat(i);
    }

    public static final DoubleFloat makeLispObject(double i)
    {
        return new DoubleFloat(i);
    }

    public static final LispCharacter makeLispObject(char i)
    {
        return LispCharacter.getInstance(i);
    }

    public static final LispObject makeLispObject(boolean i)
    {
        return i ? T : NIL;
    }
}
