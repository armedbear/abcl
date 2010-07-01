/*
 * JProxy.java
 *
 * Copyright (C) 2002-2005 Peter Graves, Andras Simon
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

import java.lang.reflect.InvocationHandler;
import java.lang.reflect.Method;
import java.lang.reflect.Proxy;
import java.util.HashMap;
import java.util.Map;
import java.util.WeakHashMap;

public final class JProxy
{
  static final Map<Object,Entry> table = new WeakHashMap<Object,Entry>();

  // ### %jnew-proxy interface &rest method-names-and-defs
  private static final Primitive _JNEW_PROXY =
    new Primitive("%jnew-proxy", PACKAGE_JAVA, false,
                  "interface &rest method-names-and-defs")
    {
      @Override
      public LispObject execute(LispObject[] args)
      {
        int length = args.length;
        if (length < 3 || length % 2 != 1)
          return error(new WrongNumberOfArgumentsException(this));
        Map<String,Function> lispDefinedMethods = new HashMap<String,Function>();
        for (int i = 1; i < length; i += 2)
          lispDefinedMethods.put(args[i].getStringValue(),
                                 (Function) args[i + 1]);
        Class iface = (Class) args[0].javaInstance();
        Object proxy = Proxy.newProxyInstance(iface.getClassLoader(),
                                              new Class[] { iface },
                                              new LispHandler(table));
        table.put(proxy, new Entry(iface, lispDefinedMethods));
        return new JavaObject(proxy);
      }
    };

  private static class LispHandler implements InvocationHandler
  {
    Map table;

    LispHandler (Map table)
    {
      this.table = table;
    }

    public Object invoke(Object proxy, Method method, Object[] args)
    {
      String methodName = method.getName();

      if (methodName.equals("hashCode"))
          return new Integer(System.identityHashCode(proxy));
      if (methodName.equals("equals"))
        return (proxy == args[0] ? Boolean.TRUE : Boolean.FALSE);
      if (methodName.equals("toString"))
        return proxy.getClass().getName() + '@' + Integer.toHexString(proxy.hashCode());

      if (table.containsKey(proxy))
        {
          Entry entry = (Entry) table.get(proxy);
          Function f = entry.getLispMethod(methodName);
          if (f != null)
            {
              LispObject lispArgs = NIL;
              if (args != null)
                {
                  for (int i = args.length - 1 ; 0 <= i  ; i--)
                    lispArgs = lispArgs.push(new JavaObject(args[i]));
                }
              LispObject result = evalCall(f, lispArgs, new Environment(),
                                           LispThread.currentThread());
              return (method.getReturnType() == void.class ? null : result.javaInstance());
            }
        }
      return null;
    }
  }

  private static class Entry
  {
    Class iface;
    Map lispDefinedMethods;

    public Entry (Class iface, Map lispDefinedMethods)
    {
      this.iface = iface;
      this.lispDefinedMethods = lispDefinedMethods;
    }

    public Function getLispMethod(String methodName)
    {
      if (lispDefinedMethods.containsKey(methodName))
        return (Function)lispDefinedMethods.get(methodName);
      return null;
    }
  }
  
  	//NEW IMPLEMENTATION by Alessio Stalla 
  
  	/**
  	 * A weak map associating each proxy instance with a "Lisp-this" object. 
  	 */
  	static final Map<Object, LispObject> proxyMap = new WeakHashMap<Object, LispObject>();
  
    public static class LispInvocationHandler implements InvocationHandler {
	
	private Function function;
	private static Method hashCodeMethod;
	private static Method equalsMethod;
	private static Method toStringMethod;
  	
	static {
	    try {
		hashCodeMethod = Object.class.getMethod("hashCode", new Class[] {});
		equalsMethod = Object.class.getMethod("equals", new Class[] { Object.class });
		toStringMethod = Object.class.getMethod("toString", new Class[] {});
	    } catch (Exception e) {
		throw new Error("Something got horribly wrong - can't get a method from Object.class", e);
	    }
	}
	
	public LispInvocationHandler(Function function) {
	    this.function = function;
	}
  		
	public Object invoke(Object proxy, Method method, Object[] args) throws Throwable {
	    if(hashCodeMethod.equals(method)) {
		return System.identityHashCode(proxy);
	    }
	    if(equalsMethod.equals(method)) {
		return proxy == args[0];
	    }
	    if(toStringMethod.equals(method)) {
		return proxy.getClass().getName() + '@' + Integer.toHexString(proxy.hashCode());
	    }
	    	
	    if(args == null) {
		args = new Object[0];
	    }
	    LispObject lispArgs = NIL;
	    synchronized(proxyMap) {
		lispArgs = lispArgs.push(toLispObject(proxyMap.get(proxy)));
	    }
	    lispArgs = lispArgs.push(new SimpleString(method.getName()));
	    for(int i = 0; i < args.length; i++) {
		lispArgs = lispArgs.push(toLispObject(args[i]));
	    }
	    Object retVal =
		LispThread.currentThread().execute
		(Symbol.APPLY, function, lispArgs.reverse()).javaInstance();
	    //(function.execute(lispArgs)).javaInstance();
	    /* DOES NOT WORK due to autoboxing!
	       if(retVal != null && !method.getReturnType().isAssignableFrom(retVal.getClass())) {
	       return error(new TypeError(new JavaObject(retVal), new JavaObject(method.getReturnType())));
	       }*/
	    return retVal;
	}
    }
  
  	private static final Primitive _JMAKE_INVOCATION_HANDLER =
	    new Primitive("%jmake-invocation-handler", PACKAGE_JAVA, false,
	                  "function") {
		
	      	public LispObject execute(LispObject[] args) {
	      		int length = args.length;
	      		if (length != 1) {
	      			return error(new WrongNumberOfArgumentsException(this));
	      		}
	      		if(!(args[0] instanceof Function)) {
	      			return error(new TypeError(args[0], Symbol.FUNCTION));
	      		}
	      		return new JavaObject(new LispInvocationHandler((Function) args[0]));
	      	}
	    };

    private static final Primitive _JMAKE_PROXY =
	    new Primitive("%jmake-proxy", PACKAGE_JAVA, false,
	                  "interfaces invocation-handler") {
		
	      	public LispObject execute(final LispObject[] args) {
	      		int length = args.length;
	      		if (length != 3) {
	      			return error(new WrongNumberOfArgumentsException(this));
	      		}
	      		if(!(args[0] instanceof Cons)) {
			    return error(new TypeError(args[0], new SimpleString("CONS")));
	      		}
			Class[] ifaces = new Class[args[0].length()];
			LispObject ifList = args[0];
			for(int i = 0; i < ifaces.length; i++) {
			    ifaces[i] = ifList.car().javaInstance(Class.class);
			    ifList = ifList.cdr();
			}
	      		InvocationHandler invocationHandler = ((JavaObject) args[1]).javaInstance(InvocationHandler.class);
	      		Object proxy = Proxy.newProxyInstance(
	      				JavaClassLoader.getCurrentClassLoader(),
	      				ifaces,
	      				invocationHandler);
	      		synchronized(proxyMap) {
	      			proxyMap.put(proxy, args[2]);
	      		}
	      		return new JavaObject(proxy);
	      	}
	    };    
	    
	static LispObject toLispObject(Object obj) {
		return (obj instanceof LispObject) ? (LispObject) obj : new JavaObject(obj);
	}
	    
}
