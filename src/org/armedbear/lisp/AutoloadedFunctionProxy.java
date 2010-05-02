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

    public enum FunctionType
    {
        NORMAL, SETF, MACRO
    };

    /** List of symbols that need to be saved upon instantiation of a
     * proxy and restored while loading the actual function.
     */
    final static Symbol[] symsToSave =
        new Symbol[]
        {
            AUTOLOADING_CACHE, // allow loading local preloaded functions
            Load._FASL_UNINTERNED_SYMBOLS_, // vector of uninterned symbols
            Symbol._PACKAGE_,              // current package
            Symbol.LOAD_TRUENAME           // LOAD-TIME-VALUE depends on this
        };

    final private Symbol symbol;
    final private String name;
    final private LispObject cache;
    final private LispObject[] savedSyms;
    final private FunctionType fType;
    Function fun = null;

    public AutoloadedFunctionProxy(Symbol symbol, LispObject name,
                                   LispObject cache,
                                   LispObject[] savedSyms,
                                   FunctionType ft) {
        super();
        this.symbol = symbol;
        this.name = name.getStringValue();
        this.cache = cache;
        this.savedSyms = savedSyms;
        Debug.assertTrue(! (cache instanceof Nil));
        this.fType = ft;
    }

    /** Resolve this instance by returning the function we're proxy for */
    @Override
    public LispObject resolve() {
        return load();
    }


    final private synchronized Function load() {
        if (fun != null)
            return fun;

        LispThread thread = LispThread.currentThread();
        SpecialBindingsMark mark = thread.markSpecialBindings();

        for (int i = 0; i < symsToSave.length; i++)
            thread.bindSpecial(symsToSave[i], savedSyms[i]);

        // set a specific reader environment, because we may be triggered in
        // any undefined dynamic environment; we want something predictable
        thread.bindSpecial(Symbol.READ_SUPPRESS, NIL);
        thread.bindSpecial(Symbol.READ_EVAL, T);
        thread.bindSpecial(Symbol.READ_BASE, LispInteger.getInstance(10));
        // don't need to bind *READ-DEFAULT-FLOAT-FORMAT*,
        // because DUMP-FORM sets it to NIL, forcing exponent markers everywhere

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

        if (symbol != null)
            installFunction(fType, symbol, fun);

        return fun;
    }

    final static private void installFunction(FunctionType fType,
                                              Symbol sym, Function fun) {

        if (fType == FunctionType.SETF)
            put(sym, Symbol.SETF_FUNCTION, fun);
        else if (fType == FunctionType.MACRO) {
            if (sym.getSymbolFunction() instanceof SpecialOperator)
                put(sym, Symbol.MACROEXPAND_MACRO,
                    new MacroObject(sym, fun));
            else
                sym.setSymbolFunction(new MacroObject(sym, fun));
        } else
            sym.setSymbolFunction(fun);
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

      if (value instanceof Nil) {
          byte[] bytes = readFunctionBytes(new Pathname(name));
          return (bytes == null) ? null : loadClassBytes(bytes);
      }

      Hashtable cache = (Hashtable)value.javaInstance();
      byte[] bytes = (byte[])cache.get(name);
      if (bytes == null)
          return error(new LispError("Function '" + name + "' not preloaded" +
                                     " while preloading requested."));
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

    // ### proxy-preloaded-function symbol name => function
    final private static Primitive PROXY_PRELOADED_FUNCTION = new proxy_preloaded_function();
    final private static class proxy_preloaded_function extends Primitive {
        proxy_preloaded_function() {
            super("proxy-preloaded-function", PACKAGE_SYS, false,
                  "symbol name");
        }
        @Override
        final public LispObject execute(LispObject symbol, LispObject name) {
            LispThread thread = LispThread.currentThread();
            Symbol sym;
            Function fun;
            FunctionType fType = FunctionType.NORMAL;

            if (symbol instanceof Symbol)
                sym = (Symbol)symbol;
            else if (isValidSetfFunctionName(symbol)) {
                sym = (Symbol)symbol.cadr();
                fType = FunctionType.SETF;
            } else if (isValidMacroFunctionName(symbol)) {
                sym = (Symbol)symbol.cadr();
                fType = FunctionType.MACRO;
            } else {
                checkSymbol(symbol); // generate an error
                return null; // not reached
            }

            LispObject cache = AUTOLOADING_CACHE.symbolValue(thread);
            if (cache instanceof Nil)
                // during EVAL-WHEN :compile-toplevel, this function will
                // be called without a caching environment; we'll need to
                // forward to the compiled function loader
                return loadCompiledFunction(name.getStringValue());
            else {
                LispObject[] cachedSyms = new LispObject[symsToSave.length];
                for (int i = 0; i < symsToSave.length; i++)
                    cachedSyms[i] = symsToSave[i].symbolValue(thread);

                fun = new AutoloadedFunctionProxy(sym, name, cache,
                                                  cachedSyms, fType);
                fun.setClassBytes((byte[])((Hashtable)cache.javaInstance())
                                  .get(name.getStringValue()));
            }
            return fun;
        }
    }

    //  ### function-preload name => success
    final private static Primitive FUNCTION_PRELOAD = new function_preload();
    private static class function_preload extends Primitive {
        function_preload() {
            super("function-preload", PACKAGE_SYS, false, "name");
        }
        @SuppressWarnings("unchecked")
        @Override
        final public LispObject execute(LispObject name) {
            String namestring = name.getStringValue();
            LispThread thread = LispThread.currentThread();
            Hashtable cache
                = (Hashtable)AUTOLOADING_CACHE.symbolValue(thread).javaInstance();

            Pathname pathname = new Pathname(namestring);
            byte[] bytes = readFunctionBytes(pathname);
            cache.put(namestring, bytes);
            
            return T;
        }
    }
}
