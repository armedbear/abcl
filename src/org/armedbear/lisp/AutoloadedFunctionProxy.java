/*
 * AutoloadedFunctionProxy.java
 *
 * Copyright (C) 2009 Erik Huelsmann
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

import java.util.Hashtable;



public class AutoloadedFunctionProxy extends Function {

    final private Symbol symbol;
    final private String name;
    final private LispObject cache;
    final private LispObject pack;
    final private LispObject anonymousPackage;
    final private boolean isSetfFunction;
    Function fun = null;

    public AutoloadedFunctionProxy(Symbol symbol, LispObject name,
                                   LispObject cache, LispObject pack,
                                   LispObject anonymousPackage,
                                   boolean setfFunction) {
        super();
        this.symbol = symbol;
        this.name = name.getStringValue();
        this.cache = cache;
        this.pack = pack;
        //        Debug.trace("proxying ... " + name.getStringValue());
        Debug.assertTrue(! (cache instanceof Nil));
        this.anonymousPackage = anonymousPackage;
        this.isSetfFunction = setfFunction;
    }

    final private synchronized Function load() {
        if (fun != null)
            return fun;

        LispThread thread = LispThread.currentThread();
        SpecialBindingsMark mark = thread.markSpecialBindings();

        thread.bindSpecial(AUTOLOADING_CACHE, cache);
        thread.bindSpecial(Load._FASL_ANONYMOUS_PACKAGE_, anonymousPackage);
        thread.bindSpecial(Symbol._PACKAGE_, pack);
        byte[] classbytes =
            (byte[])((Hashtable)cache.javaInstance()).get(name);
        try {
            fun = loadClassBytes(classbytes);
        }
        catch (Throwable t) {
            Debug.trace(t);
        } // ### fixme
        finally {
            thread.resetSpecialBindings(mark);
        }

        if (symbol != null) {
            if (isSetfFunction)
                put(symbol, Symbol.SETF_FUNCTION, fun);
            else
                symbol.setSymbolFunction(fun);
        }

        return fun;
    }

    @Override
    public LispObject execute()
    {
        return load().execute();
    }

    @Override
    public LispObject execute(LispObject arg)
    {
        return load().execute(arg);
    }

    @Override
    public LispObject execute(LispObject first, LispObject second)

    {
        return load().execute(first, second);
    }

    @Override
    public LispObject execute(LispObject first, LispObject second,
                              LispObject third)

    {
        return load().execute(first, second, third);
    }

    @Override
    public LispObject execute(LispObject first, LispObject second,
                              LispObject third, LispObject fourth)

    {
        return load().execute(first, second, third, fourth);
    }

    @Override
    public LispObject execute(LispObject first, LispObject second,
                              LispObject third, LispObject fourth,
                              LispObject fifth)

    {
        return load().execute(first, second, third, fourth, fifth);
    }

    @Override
    public LispObject execute(LispObject first, LispObject second,
                              LispObject third, LispObject fourth,
                              LispObject fifth, LispObject sixth)

    {
        return load().execute(first, second, third, fourth, fifth, sixth);
    }

    @Override
    public LispObject execute(LispObject first, LispObject second,
                              LispObject third, LispObject fourth,
                              LispObject fifth, LispObject sixth,
                              LispObject seventh)

    {
        return load().execute(first, second, third, fourth, fifth, sixth,
                              seventh);
    }

    @Override
    public LispObject execute(LispObject first, LispObject second,
                              LispObject third, LispObject fourth,
                              LispObject fifth, LispObject sixth,
                              LispObject seventh, LispObject eighth)

    {
        return load().execute(first, second, third, fourth, fifth, sixth,
                              seventh, eighth);
    }

    @Override
    public LispObject execute(LispObject[] args)
    {
        return load().execute(args);
    }

    @SuppressWarnings("unchecked")
    final public static LispObject loadPreloadedFunction(String name) {
      LispThread thread = LispThread.currentThread();
      LispObject value = AUTOLOADING_CACHE.symbolValue(thread);

      if (value instanceof Nil)
          return loadCompiledFunction(name);

      Hashtable cache = (Hashtable)value.javaInstance();
      byte[] bytes = (byte[])cache.get(name);
      try {
        return loadClassBytes(bytes);
      }
      catch (VerifyError e)
      {
        return error(new LispError("Class verification failed: " +
                                   e.getMessage()));
      }
      catch (Throwable t)
      {
        Debug.trace(t);
      }
      return error(new FileError("Can't read file off stream."));
    }

    final static LispObject makePreloadingContext() {
        return new JavaObject(new Hashtable());
    }

    final private static Primitive PROXY_PRELOADED_FUNCTION
        = new Primitive("proxy-preloaded-function", PACKAGE_SYS, false,
                        "symbol name")
    {
      @Override
      final public LispObject execute(LispObject symbol, LispObject name) {
        LispThread thread = LispThread.currentThread();
        Symbol sym;
        LispObject fun;
        boolean setfFun = false;

        if (symbol instanceof Symbol)
            sym = (Symbol)symbol;
        else if (isValidSetfFunctionName(symbol)) {
            sym = (Symbol)symbol.cadr();
            setfFun = true;
        } else {
            checkSymbol(symbol); // generate an error
            return null; // not reached
        }

        LispObject cache = AUTOLOADING_CACHE.symbolValue(thread);
        LispObject pack = Symbol._PACKAGE_.symbolValue(thread);

        if (cache instanceof Nil)
            return loadCompiledFunction(name.getStringValue());
        else {
            fun = new AutoloadedFunctionProxy(sym, name, cache, pack,
                                              Load._FASL_ANONYMOUS_PACKAGE_.symbolValue(thread),
                                              setfFun);
            if (setfFun)
                put(sym, Symbol.SETF_FUNCTION, fun);
            else
                sym.setSymbolFunction(fun);
        }

        return fun;
      }
   };


  final private static Primitive FUNCTION_PRELOAD
    = new Primitive("function-preload", PACKAGE_SYS, false, "name")
  {
    @SuppressWarnings("unchecked")
    @Override
    final public LispObject execute(LispObject name) {
      String namestring = name.getStringValue();
      LispThread thread = LispThread.currentThread();
      Hashtable cache
          = (Hashtable)AUTOLOADING_CACHE.symbolValue(thread).javaInstance();

      byte[] bytes = readFunctionBytes(namestring);
      cache.put(namestring, bytes);

      return T;
    }
  };

}
