/*
 * Lisp.java
 *
 * Copyright (C) 2002-2007 Peter Graves <peter@armedbear.org>
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
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
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

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.lang.reflect.Constructor;
import java.math.BigInteger;
import java.net.URL;
import java.net.URLDecoder;
import java.util.Hashtable;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;

public abstract class Lisp
{
  public static final boolean debug = true;

  public static boolean cold = true;

  public static boolean initialized;

  // Packages.
  public static final Package PACKAGE_CL =
    Packages.createPackage("COMMON-LISP", 1024);
  public static final Package PACKAGE_CL_USER =
    Packages.createPackage("COMMON-LISP-USER", 1024);
  public static final Package PACKAGE_KEYWORD =
    Packages.createPackage("KEYWORD", 1024);
  public static final Package PACKAGE_SYS =
    Packages.createPackage("SYSTEM");
  public static final Package PACKAGE_MOP =
    Packages.createPackage("MOP");
  public static final Package PACKAGE_TPL =
    Packages.createPackage("TOP-LEVEL");
  public static final Package PACKAGE_EXT =
    Packages.createPackage("EXTENSIONS");
  public static final Package PACKAGE_JVM =
    Packages.createPackage("JVM");
  public static final Package PACKAGE_LOOP =
    Packages.createPackage("LOOP");
  public static final Package PACKAGE_PROF =
    Packages.createPackage("PROFILER");
  public static final Package PACKAGE_JAVA =
    Packages.createPackage("JAVA");
  public static final Package PACKAGE_LISP =
    Packages.createPackage("LISP");
  public static final Package PACKAGE_THREADS =
    Packages.createPackage("THREADS");

  // ### nil
  public static final LispObject NIL = Nil.NIL;

  // We need NIL before we can call usePackage().
  static
  {
    try
      {
        PACKAGE_CL.addNickname("CL");
        PACKAGE_CL_USER.addNickname("CL-USER");
        PACKAGE_CL_USER.usePackage(PACKAGE_CL);
        PACKAGE_CL_USER.usePackage(PACKAGE_EXT);
        PACKAGE_CL_USER.usePackage(PACKAGE_JAVA);
        PACKAGE_SYS.addNickname("SYS");
        PACKAGE_SYS.usePackage(PACKAGE_CL);
        PACKAGE_SYS.usePackage(PACKAGE_EXT);
        PACKAGE_MOP.usePackage(PACKAGE_CL);
        PACKAGE_MOP.usePackage(PACKAGE_EXT);
        PACKAGE_MOP.usePackage(PACKAGE_SYS);
        PACKAGE_TPL.addNickname("TPL");
        PACKAGE_TPL.usePackage(PACKAGE_CL);
        PACKAGE_TPL.usePackage(PACKAGE_EXT);
        PACKAGE_EXT.addNickname("EXT");
        PACKAGE_EXT.usePackage(PACKAGE_CL);
        PACKAGE_EXT.usePackage(PACKAGE_THREADS);
        PACKAGE_JVM.usePackage(PACKAGE_CL);
        PACKAGE_JVM.usePackage(PACKAGE_EXT);
        PACKAGE_JVM.usePackage(PACKAGE_SYS);
        PACKAGE_LOOP.usePackage(PACKAGE_CL);
        PACKAGE_PROF.addNickname("PROF");
        PACKAGE_PROF.usePackage(PACKAGE_CL);
        PACKAGE_PROF.usePackage(PACKAGE_EXT);
        PACKAGE_JAVA.usePackage(PACKAGE_CL);
        PACKAGE_JAVA.usePackage(PACKAGE_EXT);
        PACKAGE_LISP.usePackage(PACKAGE_CL);
        PACKAGE_LISP.usePackage(PACKAGE_EXT);
        PACKAGE_LISP.usePackage(PACKAGE_SYS);
	PACKAGE_THREADS.usePackage(PACKAGE_CL);
      }
    catch (Throwable t)
      {
        t.printStackTrace();
      }
  }

  // End-of-file marker.
  public static final LispObject EOF = new LispObject();

  public static boolean profiling;

  public static boolean sampling;

  public static volatile boolean sampleNow;

  // args must not be null!
  public static final LispObject funcall(LispObject fun, LispObject[] args,
                                         LispThread thread)
    throws ConditionThrowable
  {
    thread._values = null;

    // 26-07-2009: For some reason we cannot "just" call the array version;
    // it causes an error (Wrong number of arguments for LOOP-FOR-IN)
    // which is probably a sign of an issue in our design?
    switch (args.length)
      {
      case 0:
        return thread.execute(fun);
      case 1:
        return thread.execute(fun, args[0]);
      case 2:
        return thread.execute(fun, args[0], args[1]);
      case 3:
        return thread.execute(fun, args[0], args[1], args[2]);
      case 4:
        return thread.execute(fun, args[0], args[1], args[2], args[3]);
      case 5:
        return thread.execute(fun, args[0], args[1], args[2], args[3],
                              args[4]);
      case 6:
        return thread.execute(fun, args[0], args[1], args[2], args[3],
                              args[4], args[5]);
      case 7:
        return thread.execute(fun, args[0], args[1], args[2], args[3],
                              args[4], args[5], args[6]);
      case 8:
        return thread.execute(fun, args[0], args[1], args[2], args[3],
                              args[4], args[5], args[6], args[7]);
      default:
        return thread.execute(fun, args);
    }
  }

  public static final LispObject macroexpand(LispObject form,
                                             final Environment env,
                                             final LispThread thread)
    throws ConditionThrowable
  {
    LispObject expanded = NIL;
    while (true)
      {
        form = macroexpand_1(form, env, thread);
        LispObject[] values = thread._values;
        if (values[1] == NIL)
          {
            values[1] = expanded;
            return form;
          }
        expanded = T;
      }
  }

  public static final LispObject macroexpand_1(final LispObject form,
                                               final Environment env,
                                               final LispThread thread)
    throws ConditionThrowable
  {
    if (form instanceof Cons)
      {
        LispObject car = ((Cons)form).car;
        if (car instanceof Symbol)
          {
            LispObject obj = env.lookupFunction(car);
            if (obj instanceof Autoload)
              {
                Autoload autoload = (Autoload) obj;
                autoload.load();
                obj = car.getSymbolFunction();
              }
            if (obj instanceof SpecialOperator)
              {
                obj = get(car, Symbol.MACROEXPAND_MACRO, null);
                if (obj instanceof Autoload)
                  {
                    Autoload autoload = (Autoload) obj;
                    autoload.load();
                    obj = get(car, Symbol.MACROEXPAND_MACRO, null);
                  }
              }
            if (obj instanceof MacroObject)
              {
                LispObject expander = ((MacroObject)obj).expander;
                if (profiling)
                  if (!sampling)
                    expander.incrementCallCount();
                LispObject hook =
                  coerceToFunction(Symbol.MACROEXPAND_HOOK.symbolValue(thread));
                return thread.setValues(hook.execute(expander, form, env),
                                        T);
              }
          }
      }
    else if (form instanceof Symbol)
      {
        Symbol symbol = (Symbol) form;
        LispObject obj = null;
        if (symbol.isSpecialVariable())
          obj = thread.lookupSpecial(symbol);
        else
          obj = env.lookup(symbol);
        if (obj == null)
          obj = symbol.getSymbolValue();
        if (obj instanceof SymbolMacro)
          return thread.setValues(((SymbolMacro)obj).getExpansion(), T);
      }
    // Not a macro.
    return thread.setValues(form, NIL);
  }

  // ### interactive-eval
  private static final Primitive INTERACTIVE_EVAL =
    new Primitive("interactive-eval", PACKAGE_SYS, true)
    {
      @Override
      public LispObject execute(LispObject object) throws ConditionThrowable
      {
        final LispThread thread = LispThread.currentThread();
        thread.setSpecialVariable(Symbol.MINUS, object);
        LispObject result;
        try
          {
            result = thread.execute(Symbol.EVAL.getSymbolFunction(), object);
          }
        catch (OutOfMemoryError e)
          {
            return error(new LispError("Out of memory."));
          }
        catch (StackOverflowError e)
          {
            thread.setSpecialVariable(_SAVED_BACKTRACE_,
                                      thread.backtrace(0));
            return error(new StorageCondition("Stack overflow."));
          }
        catch (Go go)
          {
            throw go;
          }
        catch (Throw t)
          {
            return error(new ControlError("Attempt to throw to the nonexistent tag " +
                                           t.tag.writeToString() + "."));
          }
        catch (Throwable t)
          {
            Debug.trace(t);
            thread.setSpecialVariable(_SAVED_BACKTRACE_,
                                      thread.backtrace(0));
            return error(new LispError("Caught " + t + "."));
          }
        Debug.assertTrue(result != null);
        thread.setSpecialVariable(Symbol.STAR_STAR_STAR,
                                  thread.safeSymbolValue(Symbol.STAR_STAR));
        thread.setSpecialVariable(Symbol.STAR_STAR,
                                  thread.safeSymbolValue(Symbol.STAR));
        thread.setSpecialVariable(Symbol.STAR, result);
        thread.setSpecialVariable(Symbol.PLUS_PLUS_PLUS,
                                  thread.safeSymbolValue(Symbol.PLUS_PLUS));
        thread.setSpecialVariable(Symbol.PLUS_PLUS,
                                  thread.safeSymbolValue(Symbol.PLUS));
        thread.setSpecialVariable(Symbol.PLUS,
                                  thread.safeSymbolValue(Symbol.MINUS));
        LispObject[] values = thread._values;
        thread.setSpecialVariable(Symbol.SLASH_SLASH_SLASH,
                                  thread.safeSymbolValue(Symbol.SLASH_SLASH));
        thread.setSpecialVariable(Symbol.SLASH_SLASH,
                                  thread.safeSymbolValue(Symbol.SLASH));
        if (values != null)
          {
            LispObject slash = NIL;
            for (int i = values.length; i-- > 0;)
              slash = new Cons(values[i], slash);
            thread.setSpecialVariable(Symbol.SLASH, slash);
          }
        else
          thread.setSpecialVariable(Symbol.SLASH, new Cons(result));
        return result;
      }
    };

  private static final void pushJavaStackFrames() throws ConditionThrowable
  {
      final LispThread thread = LispThread.currentThread();
      final StackTraceElement[] frames = thread.getJavaStackTrace();

      // Search for last Primitive in the StackTrace; that was the
      // last entry point from Lisp.
      int last = frames.length - 1;
      for (int i = 0; i<= last; i++) {
          if (frames[i].getClassName().startsWith("org.armedbear.lisp.Primitive"))
	    last = i;
      }
      // Do not include the first three frames:
      //   Thread.getStackTrace, LispThread.getJavaStackTrace,
      //   Lisp.pushJavaStackFrames.
      while (last > 2) {
        thread.pushStackFrame(new JavaStackFrame(frames[last]));
        last--;
      }
  }


  public static final LispObject error(LispObject condition)
    throws ConditionThrowable
  {
    pushJavaStackFrames();
    return Symbol.ERROR.execute(condition);
  }

  public static final LispObject error(LispObject condition, LispObject message)
    throws ConditionThrowable
  {
    pushJavaStackFrames();
    return Symbol.ERROR.execute(condition, Keyword.FORMAT_CONTROL, message);
  }

  public static final LispObject type_error(LispObject datum,
                                            LispObject expectedType)
    throws ConditionThrowable
  {
    return error(new TypeError(datum, expectedType));
  }

  protected static volatile boolean interrupted;

  public static synchronized final void setInterrupted(boolean b)
  {
    interrupted = b;
  }

  public static final void handleInterrupt() throws ConditionThrowable
  {
    setInterrupted(false);
    Symbol.BREAK.getSymbolFunction().execute();
    setInterrupted(false);
  }

  // Used by the compiler.
  public static final LispObject loadTimeValue(LispObject obj)
    throws ConditionThrowable
  {
    final LispThread thread = LispThread.currentThread();
    if (Symbol.LOAD_TRUENAME.symbolValue(thread) != NIL)
      return eval(obj, new Environment(), thread);
    else
      return NIL;
  }

  public static final LispObject eval(LispObject obj)
    throws ConditionThrowable
  {
    return eval(obj, new Environment(), LispThread.currentThread());
  }

  public static final LispObject eval(final LispObject obj,
                                      final Environment env,
                                      final LispThread thread)
    throws ConditionThrowable
  {
    thread._values = null;
    if (interrupted)
      handleInterrupt();
    if (thread.isDestroyed())
      throw new ThreadDestroyed();
    if (obj instanceof Symbol)
      {
        LispObject result;
        if (obj.isSpecialVariable())
          {
            if (obj.constantp())
              return obj.getSymbolValue();
            else
              result = thread.lookupSpecial(obj);
          }
        else if (env.isDeclaredSpecial(obj))
          result = thread.lookupSpecial(obj);
        else
          result = env.lookup(obj);
        if (result == null)
          {
            result = obj.getSymbolValue();
            if (result == null)
              return error(new UnboundVariable(obj));
          }
        if (result instanceof SymbolMacro)
          return eval(((SymbolMacro)result).getExpansion(), env, thread);
        return result;
      }
    else if (obj instanceof Cons)
      {
        LispObject first = ((Cons)obj).car;
        if (first instanceof Symbol)
          {
            LispObject fun = env.lookupFunction(first);
            if (fun instanceof SpecialOperator)
              {
                if (profiling)
                  if (!sampling)
                    fun.incrementCallCount();
                // Don't eval args!
                return fun.execute(((Cons)obj).cdr, env);
              }
            if (fun instanceof MacroObject)
              return eval(macroexpand(obj, env, thread), env, thread);
            if (fun instanceof Autoload)
              {
                Autoload autoload = (Autoload) fun;
                autoload.load();
                return eval(obj, env, thread);
              }
            return evalCall(fun != null ? fun : first,
                            ((Cons)obj).cdr, env, thread);
          }
        else
          {
            if (first.car() == Symbol.LAMBDA)
              {
                Closure closure = new Closure(first, env);
                return evalCall(closure, ((Cons)obj).cdr, env, thread);
              }
            else
              return error(new ProgramError("Illegal function object: " +
                                             first.writeToString()));
          }
      }
    else
      return obj;
  }

  public static final int CALL_REGISTERS_MAX = 8;

  // Also used in JProxy.java.
  protected static final LispObject evalCall(LispObject function,
                                             LispObject args,
                                             Environment env,
                                             LispThread thread)
    throws ConditionThrowable
  {
    if (args == NIL)
      return thread.execute(function);
    LispObject first = eval(args.car(), env, thread);
    args = ((Cons)args).cdr;
    if (args == NIL)
      {
        thread._values = null;
        return thread.execute(function, first);
      }
    LispObject second = eval(args.car(), env, thread);
    args = ((Cons)args).cdr;
    if (args == NIL)
      {
        thread._values = null;
        return thread.execute(function, first, second);
      }
    LispObject third = eval(args.car(), env, thread);
    args = ((Cons)args).cdr;
    if (args == NIL)
      {
        thread._values = null;
        return thread.execute(function, first, second, third);
      }
    LispObject fourth = eval(args.car(), env, thread);
    args = ((Cons)args).cdr;
    if (args == NIL)
      {
        thread._values = null;
        return thread.execute(function, first, second, third, fourth);
      }
    LispObject fifth = eval(args.car(), env, thread);
    args = ((Cons)args).cdr;
    if (args == NIL)
      {
        thread._values = null;
        return thread.execute(function, first, second, third, fourth, fifth);
      }
    LispObject sixth = eval(args.car(), env, thread);
    args = ((Cons)args).cdr;
    if (args == NIL)
      {
        thread._values = null;
        return thread.execute(function, first, second, third, fourth, fifth,
                              sixth);
      }
    LispObject seventh = eval(args.car(), env, thread);
    args = ((Cons)args).cdr;
    if (args == NIL)
      {
        thread._values = null;
        return thread.execute(function, first, second, third, fourth, fifth,
                              sixth, seventh);
      }
    LispObject eighth = eval(args.car(), env, thread);
    args = ((Cons)args).cdr;
    if (args == NIL)
      {
        thread._values = null;
        return thread.execute(function, first, second, third, fourth, fifth,
                              sixth, seventh, eighth);
      }
    // More than CALL_REGISTERS_MAX arguments.
    final int length = args.length() + CALL_REGISTERS_MAX;
    LispObject[] array = new LispObject[length];
    array[0] = first;
    array[1] = second;
    array[2] = third;
    array[3] = fourth;
    array[4] = fifth;
    array[5] = sixth;
    array[6] = seventh;
    array[7] = eighth;
    for (int i = CALL_REGISTERS_MAX; i < length; i++)
      {
        array[i] = eval(args.car(), env, thread);
        args = args.cdr();
      }
    thread._values = null;
    return thread.execute(function, array);
  }

  public static final LispObject parseBody(LispObject body,
                                           boolean documentationAllowed)
    throws ConditionThrowable
  {
      LispObject decls = NIL;
      LispObject doc = NIL;

      while (body != NIL) {
        LispObject form = body.car();
        if (documentationAllowed && form instanceof AbstractString
            && body.cdr() != NIL) {
          doc = body.car();
          documentationAllowed = false;
        } else if (form instanceof Cons && form.car() == Symbol.DECLARE)
          decls = new Cons(form, decls);
        else
          break;

        body = body.cdr();
      }
      return list(body, decls.nreverse(), doc);
  }

  public static final LispObject parseSpecials(LispObject forms)
    throws ConditionThrowable
  {
    LispObject specials = NIL;
    while (forms != NIL) {
      LispObject decls = forms.car();

      Debug.assertTrue(decls instanceof Cons);
      Debug.assertTrue(decls.car() == Symbol.DECLARE);
      decls = decls.cdr();
      while (decls != NIL) {
        LispObject decl = decls.car();

        if (decl instanceof Cons && decl.car() == Symbol.SPECIAL) {
            decl = decl.cdr();
            while (decl != NIL) {
              specials = new Cons(checkSymbol(decl.car()), specials);
              decl = decl.cdr();
            }
        }

        decls = decls.cdr();
      }

      forms = forms.cdr();
    }

    return specials;
  }

  public static final LispObject progn(LispObject body, Environment env,
                                       LispThread thread)
    throws ConditionThrowable
  {
    LispObject result = NIL;
    while (body != NIL)
      {
        result = eval(body.car(), env, thread);
        body = ((Cons)body).cdr;
      }
    return result;
  }

  // Environment wrappers.
  private static final boolean isSpecial(Symbol sym, LispObject ownSpecials,
                                         Environment env)
    throws ConditionThrowable
  {
    if (ownSpecials != null)
      {
        if (sym.isSpecialVariable())
          return true;
        for (; ownSpecials != NIL; ownSpecials = ownSpecials.cdr())
          {
            if (sym == ownSpecials.car())
              return true;
          }
      }
    return false;
  }

  protected static final void bindArg(LispObject ownSpecials,
                                      Symbol sym, LispObject value,
                                      Environment env, LispThread thread)
    throws ConditionThrowable
  {
    if (isSpecial(sym, ownSpecials, env)) {
      env.declareSpecial(sym);
      thread.bindSpecial(sym, value);
    }
    else
      env.bind(sym, value);
  }


  public static final Cons list(LispObject obj1, LispObject... remaining)
  {
    Cons theList = null;
    if (remaining.length > 0) {
      theList = new Cons(remaining[remaining.length-1]);
      for (int i = remaining.length - 2; i >= 0; i--)
        theList = new Cons(remaining[i], theList);
    }
    return (theList == null) ? new Cons(obj1) : new Cons(obj1, theList);
  }

  @Deprecated
  public static final Cons list1(LispObject obj1)
  {
    return new Cons(obj1);
  }

  @Deprecated
  public static final Cons list2(LispObject obj1, LispObject obj2)
  {
    return new Cons(obj1, new Cons(obj2));
  }

  @Deprecated
  public static final Cons list3(LispObject obj1, LispObject obj2,
                                 LispObject obj3)
  {
    return new Cons(obj1, new Cons(obj2, new Cons(obj3)));
  }

  @Deprecated
  public static final Cons list4(LispObject obj1, LispObject obj2,
                                 LispObject obj3, LispObject obj4)
  {
    return new Cons(obj1,
                    new Cons(obj2,
                             new Cons(obj3,
                                      new Cons(obj4))));
  }

  @Deprecated
  public static final Cons list5(LispObject obj1, LispObject obj2,
                                 LispObject obj3, LispObject obj4,
                                 LispObject obj5)
  {
    return new Cons(obj1,
                    new Cons(obj2,
                             new Cons(obj3,
                                      new Cons(obj4,
                                               new Cons(obj5)))));
  }

  @Deprecated
  public static final Cons list6(LispObject obj1, LispObject obj2,
                                 LispObject obj3, LispObject obj4,
                                 LispObject obj5, LispObject obj6)
  {
    return new Cons(obj1,
                    new Cons(obj2,
                             new Cons(obj3,
                                      new Cons(obj4,
                                               new Cons(obj5,
                                                        new Cons(obj6))))));
  }

  @Deprecated
  public static final Cons list7(LispObject obj1, LispObject obj2,
                                 LispObject obj3, LispObject obj4,
                                 LispObject obj5, LispObject obj6,
                                 LispObject obj7)
  {
    return new Cons(obj1,
                    new Cons(obj2,
                             new Cons(obj3,
                                      new Cons(obj4,
                                               new Cons(obj5,
                                                        new Cons(obj6,
                                                                 new Cons(obj7)))))));
  }

  @Deprecated
  public static final Cons list8(LispObject obj1, LispObject obj2,
                                 LispObject obj3, LispObject obj4,
                                 LispObject obj5, LispObject obj6,
                                 LispObject obj7, LispObject obj8)
  {
    return new Cons(obj1,
                    new Cons(obj2,
                             new Cons(obj3,
                                      new Cons(obj4,
                                               new Cons(obj5,
                                                        new Cons(obj6,
                                                                 new Cons(obj7,
                                                                          new Cons(obj8))))))));
  }

  @Deprecated
  public static final Cons list9(LispObject obj1, LispObject obj2,
                                 LispObject obj3, LispObject obj4,
                                 LispObject obj5, LispObject obj6,
                                 LispObject obj7, LispObject obj8,
                                 LispObject obj9)
  {
    return new Cons(obj1,
                    new Cons(obj2,
                             new Cons(obj3,
                                      new Cons(obj4,
                                               new Cons(obj5,
                                                        new Cons(obj6,
                                                                 new Cons(obj7,
                                                                          new Cons(obj8,
                                                                                   new Cons(obj9)))))))));
  }

  // Used by the compiler.
  public static final LispObject multipleValueList(LispObject result)
    throws ConditionThrowable
  {
    LispThread thread = LispThread.currentThread();
    LispObject[] values = thread._values;
    if (values == null)
      return new Cons(result);
    thread._values = null;
    LispObject list = NIL;
    for (int i = values.length; i-- > 0;)
      list = new Cons(values[i], list);
    return list;
  }

  // Used by the compiler for MULTIPLE-VALUE-CALLs with a single values form.
  public static final LispObject multipleValueCall1(LispObject result,
                                                    LispObject function,
                                                    LispThread thread)
    throws ConditionThrowable
  {
    LispObject[] values = thread._values;
    thread._values = null;
    if (values == null)
      return thread.execute(coerceToFunction(function), result);
    else
      return funcall(coerceToFunction(function), values, thread);
  }

  public static final void progvBindVars(LispObject symbols,
                                         LispObject values,
                                         LispThread thread)
    throws ConditionThrowable
  {
    for (LispObject list = symbols; list != NIL; list = list.cdr())
      {
        Symbol symbol = checkSymbol(list.car());
        LispObject value;
        if (values != NIL)
          {
            value = values.car();
            values = values.cdr();
          }
        else
          {
            // "If too few values are supplied, the remaining symbols are
            // bound and then made to have no value."
            value = null;
          }
        thread.bindSpecial(symbol, value);
      }
  }

  public static Symbol checkSymbol(LispObject obj) throws ConditionThrowable
  {             
          if (obj instanceof Symbol)      
                  return (Symbol) obj;         
          return (Symbol)// Not reached.       
              type_error(obj, Symbol.SYMBOL);
  }

  public static final LispObject checkList(LispObject obj)
    throws ConditionThrowable
  {
    if (obj.listp())
      return obj;
    return type_error(obj, Symbol.LIST);
  }

  public static final AbstractArray checkArray(LispObject obj)
    throws ConditionThrowable
  {
          if (obj instanceof AbstractArray)       
                  return (AbstractArray) obj;         
          return (AbstractArray)// Not reached.       
        type_error(obj, Symbol.ARRAY);
  }

  public static final AbstractVector checkVector(LispObject obj)
    throws ConditionThrowable
  {
          if (obj instanceof AbstractVector)      
                  return (AbstractVector) obj;         
          return (AbstractVector)// Not reached.       
        type_error(obj, Symbol.VECTOR);
  }

  public static final DoubleFloat checkDoubleFloat(LispObject obj)
    throws ConditionThrowable
  {
          if (obj instanceof DoubleFloat)
                  return (DoubleFloat) obj;
          return (DoubleFloat)// Not reached.
            type_error(obj, Symbol.DOUBLE_FLOAT);
  }

  public static final SingleFloat checkSingleFloat(LispObject obj)
    throws ConditionThrowable
  {
          if (obj instanceof SingleFloat)
                  return (SingleFloat) obj;
          return (SingleFloat)// Not reached.
            type_error(obj, Symbol.SINGLE_FLOAT);
  }

  public static final StackFrame checkStackFrame(LispObject obj)
    throws ConditionThrowable
  {
          if (obj instanceof StackFrame)      
                  return (StackFrame) obj;         
          return (StackFrame)// Not reached.       
	    type_error(obj, Symbol.STACK_FRAME);
  }

  static
  {
    // ### *gensym-counter*
    Symbol.GENSYM_COUNTER.initializeSpecial(Fixnum.ZERO);
  }

  public static final Symbol gensym(LispThread thread)
    throws ConditionThrowable
  {
    return gensym("G", thread);
  }

  public static final Symbol gensym(String prefix, LispThread thread)
    throws ConditionThrowable
  {
    FastStringBuffer sb = new FastStringBuffer(prefix);
    SpecialBinding binding = thread.getSpecialBinding(Symbol.GENSYM_COUNTER);
    final LispObject oldValue;
    if (binding != null) {
        oldValue = binding.value;
        if (oldValue instanceof Fixnum
                || oldValue instanceof Bignum)
          binding.value = oldValue.incr();
        else {
           Symbol.GENSYM_COUNTER.setSymbolValue(Fixnum.ZERO);
           error(new TypeError("The value of *GENSYM-COUNTER* was not a nonnegative integer. Old value: " +
                                oldValue.writeToString() + " New value: 0"));
        }
    } else {
        // we're manipulating a global resource
        // make sure we operate thread-safely
        synchronized (Symbol.GENSYM_COUNTER) {
            oldValue = Symbol.GENSYM_COUNTER.getSymbolValue();
            if (oldValue instanceof Fixnum
                    || oldValue instanceof Bignum)
                Symbol.GENSYM_COUNTER.setSymbolValue(oldValue.incr());
            else {
               Symbol.GENSYM_COUNTER.setSymbolValue(Fixnum.ZERO);
               error(new TypeError("The value of *GENSYM-COUNTER* was not a nonnegative integer. Old value: " +
                                    oldValue.writeToString() + " New value: 0"));
            }
        }
    }
      
    // Decimal representation.
    if (oldValue instanceof Fixnum)
      sb.append(((Fixnum)oldValue).value);
    else if (oldValue instanceof Bignum)
      sb.append(((Bignum)oldValue).value.toString());

    return new Symbol(new SimpleString(sb));
  }

  public static final String javaString(LispObject arg)
    throws ConditionThrowable
  {
    if (arg instanceof AbstractString)
      return arg.getStringValue();
    if (arg instanceof Symbol)
      return ((Symbol)arg).getName();
    if (arg instanceof LispCharacter)
      return String.valueOf(new char[] {((LispCharacter)arg).value});
    type_error(arg, list(Symbol.OR, Symbol.STRING, Symbol.SYMBOL,
                               Symbol.CHARACTER));
    // Not reached.
    return null;
  }

  public static final LispObject number(long n)
  {
    if (n >= Integer.MIN_VALUE && n <= Integer.MAX_VALUE)
      return Fixnum.getInstance((int)n);
    else
      return Bignum.getInstance(n);
  }

  private static final BigInteger INT_MIN = BigInteger.valueOf(Integer.MIN_VALUE);
  private static final BigInteger INT_MAX = BigInteger.valueOf(Integer.MAX_VALUE);

  public static final LispObject number(BigInteger numerator,
                                        BigInteger denominator)
    throws ConditionThrowable
  {
    if (denominator.signum() == 0)
      error(new DivisionByZero());
    if (denominator.signum() < 0)
      {
        numerator = numerator.negate();
        denominator = denominator.negate();
      }
    BigInteger gcd = numerator.gcd(denominator);
    if (!gcd.equals(BigInteger.ONE))
      {
        numerator = numerator.divide(gcd);
        denominator = denominator.divide(gcd);
      }
    if (denominator.equals(BigInteger.ONE))
      return number(numerator);
    else
      return new Ratio(numerator, denominator);
  }

  public static final LispObject number(BigInteger n)
  {
    if (n.compareTo(INT_MIN) >= 0 && n.compareTo(INT_MAX) <= 0)
      return Fixnum.getInstance(n.intValue());
    else
      return Bignum.getInstance(n);
  }

  public static final int mod(int number, int divisor)
    throws ConditionThrowable
  {
    final int r;
    try
      {
        r = number % divisor;
      }
    catch (ArithmeticException e)
      {
        error(new ArithmeticError("Division by zero."));
        // Not reached.
        return 0;
      }
    if (r == 0)
      return r;
    if (divisor < 0)
      {
        if (number > 0)
          return r + divisor;
      }
    else
      {
        if (number < 0)
          return r + divisor;
      }
    return r;
  }

  // Adapted from SBCL.
  public static final int mix(long x, long y)
  {
    long xy = x * 3 + y;
    return (int) (536870911L & (441516657L ^ xy ^ (xy >> 5)));
  }

  // Used by the compiler.
  public static final LispObject readObjectFromString(String s)
  {
    try
      {
        return new StringInputStream(s).faslRead(true, NIL, false,
                                                 LispThread.currentThread());
      }
    catch (Throwable t)
      {
        return null;
      }
  }

  public static final LispObject loadCompiledFunction(final String namestring)
    throws ConditionThrowable
  {
    final LispThread thread = LispThread.currentThread();
    final boolean absolute = Utilities.isFilenameAbsolute(namestring);
    LispObject device = NIL;
    final Pathname defaultPathname;
    if (absolute)
      {
        defaultPathname =
          coerceToPathname(Symbol.DEFAULT_PATHNAME_DEFAULTS.symbolValue(thread));
      }
    else
      {
        LispObject loadTruename = Symbol.LOAD_TRUENAME.symbolValue(thread);
        if (loadTruename instanceof Pathname)
          {
            defaultPathname = (Pathname) loadTruename;
            // We're loading a file.
            device = ((Pathname)loadTruename).getDevice();
          }
        else
          {
            defaultPathname =
              coerceToPathname(Symbol.DEFAULT_PATHNAME_DEFAULTS.symbolValue(thread));
          }
      }
    if (device instanceof Pathname)
      {
        // We're loading a fasl from j.jar.
        URL url = Lisp.class.getResource(namestring);
        if (url != null)
          {
            try
              {
                String s = url.toString();
                String zipFileName;
                String entryName;
                if (s.startsWith("jar:file:"))
                  {
                    s = s.substring(9);
                    int index = s.lastIndexOf('!');
                    if (index >= 0)
                      {
                        zipFileName = s.substring(0, index);
                        entryName = s.substring(index + 1);
                        if (entryName.length() > 0 && entryName.charAt(0) == '/')
                          entryName = entryName.substring(1);
                        if (Utilities.isPlatformWindows)
                          {
                            // "/C:/Documents%20and%20Settings/peter/Desktop/j.jar"
                            if (zipFileName.length() > 0 && zipFileName.charAt(0) == '/')
                              zipFileName = zipFileName.substring(1);
                          }
                        zipFileName = URLDecoder.decode(zipFileName, "UTF-8");
                        ZipFile zipFile = ZipCache.getZip(zipFileName);
                        try
                          {
                            ZipEntry entry = zipFile.getEntry(entryName);
                            if (entry != null)
                              {
                                long size = entry.getSize();
                                InputStream in = zipFile.getInputStream(entry);
                                LispObject obj = loadCompiledFunction(in, (int) size);
                                return obj != null ? obj : NIL;
                              }
                          }
                        finally
                          {
                            ZipCache.removeZip(zipFile.getName());
                          }
                      }
                  }
              }
            catch (VerifyError e)
              {
                return error(new LispError("Class verification failed: " +
                                            e.getMessage()));
              }
            catch (IOException e)
              {
                Debug.trace(e);
              }
            catch (Throwable t)
              {
                Debug.trace(t);
              }
          }
        return error(new LispError("Unable to load " + namestring));
      }
    Pathname pathname = new Pathname(namestring);
    final File file = Utilities.getFile(pathname, defaultPathname);
    if (file != null && file.isFile())
      {
        // The .cls file exists.
        try
          {
            LispObject obj = loadCompiledFunction(new FileInputStream(file),
                                                  (int) file.length());
            // FIXME close stream!
            if (obj != null)
              return obj;
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
        return error(new LispError("Unable to load " +
                                    pathname.writeToString()));
      }
    try
      {
        LispObject loadTruename = Symbol.LOAD_TRUENAME.symbolValue(thread);
        String zipFileName = ((Pathname)loadTruename).getNamestring();
        ZipFile zipFile = ZipCache.getZip(zipFileName);
        try
          {
            ZipEntry entry = zipFile.getEntry(namestring);
            if (entry != null)
              {
                LispObject obj = loadCompiledFunction(zipFile.getInputStream(entry),
                                                      (int) entry.getSize());
                if (obj != null)
                  return obj;
                Debug.trace("Unable to load " + namestring);
                return error(new LispError("Unable to load " + namestring));
              }
          }
        finally
          {
            ZipCache.removeZip(zipFile.getName());
          }
      }
    catch (Throwable t)
      {
        Debug.trace(t);
      }
    return error(new FileError("File not found: " + namestring,
                                new Pathname(namestring)));
  }

  private static final LispObject loadCompiledFunction(InputStream in, int size)
  {
    try
      {
        byte[] bytes = new byte[size];
        int bytesRemaining = size;
        int bytesRead = 0;
        while (bytesRemaining > 0)
          {
            int n = in.read(bytes, bytesRead, bytesRemaining);
            if (n < 0)
              break;
            bytesRead += n;
            bytesRemaining -= n;
          }
        in.close();
        if (bytesRemaining > 0)
          Debug.trace("bytesRemaining = " + bytesRemaining);

        return loadCompiledFunction(bytes);
      }
    catch (Throwable t)
      {
        Debug.trace(t);
      }
    return null;
  }

    public static final LispObject loadCompiledFunction(byte[] bytes) throws Throwable {
        Class<Object> c = (new JavaClassLoader())
            .loadClassFromByteArray(null, bytes, 0, bytes.length);
        if (c != null) {
            Constructor constructor = c.getConstructor((Class[])null);
            LispObject obj = (LispObject)constructor
                .newInstance((Object[])null);
            if (obj instanceof Function) {
              ((Function)obj).setClassBytes(bytes);
            }
            return obj;
        } else {
            return null;
        }
    }

  public static final LispObject makeCompiledClosure(LispObject template,
                                                     ClosureBinding[] context)
    throws ConditionThrowable
  {
    return ((CompiledClosure)template).dup().setContext(context);
  }

  public static final String safeWriteToString(LispObject obj)
  {
    try
      {
        return obj.writeToString();
      }
    catch (ConditionThrowable t)
      {
        return obj.toString();
      }
    catch (NullPointerException e)
      {
        Debug.trace(e);
        return "null";
      }
  }

  public static final boolean isValidSetfFunctionName(LispObject obj)
  {
    if (obj instanceof Cons)
      {
        Cons cons = (Cons) obj;
        if (cons.car == Symbol.SETF && cons.cdr instanceof Cons)
          {
            Cons cdr = (Cons) cons.cdr;
            return (cdr.car instanceof Symbol && cdr.cdr == NIL);
          }
      }
    return false;
  }

  public static final LispObject FUNCTION_NAME =
    list(Symbol.OR,
          Symbol.SYMBOL,
          list(Symbol.CONS,
                list(Symbol.EQL, Symbol.SETF),
                list(Symbol.CONS, Symbol.SYMBOL, Symbol.NULL)));

  public static final LispObject UNSIGNED_BYTE_8 =
    list(Symbol.UNSIGNED_BYTE, Fixnum.constants[8]);

  public static final LispObject UNSIGNED_BYTE_16 =
    list(Symbol.UNSIGNED_BYTE, Fixnum.constants[16]);

  public static final LispObject UNSIGNED_BYTE_32 =
    list(Symbol.UNSIGNED_BYTE, Fixnum.constants[32]);

  public static final LispObject UNSIGNED_BYTE_32_MAX_VALUE =
    Bignum.getInstance(4294967296L);

  public static final LispObject getUpgradedArrayElementType(LispObject type)
    throws ConditionThrowable
  {
    if (type instanceof Symbol)
      {
        if (type == Symbol.CHARACTER || type == Symbol.BASE_CHAR ||
            type == Symbol.STANDARD_CHAR)
          return Symbol.CHARACTER;
        if (type == Symbol.BIT)
          return Symbol.BIT;
        if (type == NIL)
          return NIL;
      }
    if (type == BuiltInClass.CHARACTER)
      return Symbol.CHARACTER;
    if (type instanceof Cons)
      {
        if (type.equal(UNSIGNED_BYTE_8))
          return type;
        if (type.equal(UNSIGNED_BYTE_16))
          return type;
        if (type.equal(UNSIGNED_BYTE_32))
          return type;
        LispObject car = type.car();
        if (car == Symbol.INTEGER)
          {
            LispObject lower = type.cadr();
            LispObject upper = type.cdr().cadr();
            // Convert to inclusive bounds.
            if (lower instanceof Cons)
              lower = lower.car().incr();
            if (upper instanceof Cons)
              upper = upper.car().decr();
            if (lower.integerp() && upper.integerp())
              {
                if (lower instanceof Fixnum && upper instanceof Fixnum)
                  {
                    int l = ((Fixnum)lower).value;
                    if (l >= 0)
                      {
                        int u = ((Fixnum)upper).value;
                        if (u <= 1)
                          return Symbol.BIT;
                        if (u <= 255)
                          return UNSIGNED_BYTE_8;
                        if (u <= 65535)
                          return UNSIGNED_BYTE_16;
                        return UNSIGNED_BYTE_32;
                      }
                  }
                if (lower.isGreaterThanOrEqualTo(Fixnum.ZERO))
                  {
                    if (lower.isLessThan(UNSIGNED_BYTE_32_MAX_VALUE))
                      {
                        if (upper.isLessThan(UNSIGNED_BYTE_32_MAX_VALUE))
                          return UNSIGNED_BYTE_32;
                      }
                  }
              }
          }
        else if (car == Symbol.EQL)
          {
            LispObject obj = type.cadr();
            if (obj instanceof Fixnum)
              {
                int val = ((Fixnum)obj).value;
                if (val >= 0)
                  {
                    if (val <= 1)
                      return Symbol.BIT;
                    if (val <= 255)
                      return UNSIGNED_BYTE_8;
                    if (val <= 65535)
                      return UNSIGNED_BYTE_16;
                    return UNSIGNED_BYTE_32;
                  }
              }
            else if (obj instanceof Bignum)
              {
                if (obj.isGreaterThanOrEqualTo(Fixnum.ZERO))
                  {
                    if (obj.isLessThan(UNSIGNED_BYTE_32_MAX_VALUE))
                      return UNSIGNED_BYTE_32;
                  }
              }
          }
        else if (car == Symbol.MEMBER)
          {
            LispObject rest = type.cdr();
            while (rest != NIL)
              {
                LispObject obj = rest.car();
                if (obj instanceof LispCharacter)
                  rest = rest.cdr();
                else
                  return T;
              }
            return Symbol.CHARACTER;
          }
      }
    return T;
  }

  public static final byte coerceLispObjectToJavaByte(LispObject obj)
    throws ConditionThrowable
  {
          return (byte)Fixnum.getValue(obj);
  }

  public static final LispObject coerceJavaByteToLispObject(byte b)
  {
    return Fixnum.constants[((int)b) & 0xff];
  }

  public static final LispCharacter checkCharacter(LispObject obj)
    throws ConditionThrowable
  {
          if (obj instanceof LispCharacter) 
                  return (LispCharacter) obj;         
          return (LispCharacter) // Not reached.       
        type_error(obj, Symbol.CHARACTER);
  }

  public static final Package checkPackage(LispObject obj)
    throws ConditionThrowable
  {
          if (obj instanceof Package)     
                  return (Package) obj;         
          return (Package) // Not reached.       
        type_error(obj, Symbol.PACKAGE);
  }

  public static final Function checkFunction(LispObject obj)
    throws ConditionThrowable
  {
          if (obj instanceof Function)    
                  return (Function) obj;         
          return (Function) // Not reached.       
        type_error(obj, Symbol.FUNCTION);
  }

  public static final Stream checkStream(LispObject obj)
    throws ConditionThrowable
  {
          if (obj instanceof Stream)      
                  return (Stream) obj;         
          return (Stream) // Not reached.       
        type_error(obj, Symbol.STREAM);
  }

  public static final Stream checkCharacterInputStream(LispObject obj)
    throws ConditionThrowable
  {
          final Stream stream = checkStream(obj);
          if (stream.isCharacterInputStream())      
                  return stream;                        
          return (Stream) // Not reached.                      
          error(new TypeError("The value " + obj.writeToString() +
                        " is not a character input stream."));
  }

  public static final Stream checkCharacterOutputStream(LispObject obj)
    throws ConditionThrowable
  {
          final Stream stream = checkStream(obj);
          if (stream.isCharacterOutputStream())      
                  return stream;                        
        return (Stream) // Not reached.
        error(new TypeError("The value " + obj.writeToString() +
                            " is not a character output stream."));
  }

  public static final Stream checkBinaryInputStream(LispObject obj)
    throws ConditionThrowable
  {
          final Stream stream = checkStream(obj);
          if (stream.isBinaryInputStream())      
                  return stream;                        
        return (Stream) // Not reached.
        error(new TypeError("The value " + obj.writeToString() +
                             " is not a binary input stream."));
  }
  
  public static final Stream outSynonymOf(LispObject obj)
  throws ConditionThrowable
  {       
          if (obj instanceof Stream)
            return (Stream) obj;
          if (obj == T)
            return checkCharacterOutputStream(Symbol.TERMINAL_IO.symbolValue());
          if (obj == NIL)
            return checkCharacterOutputStream(Symbol.STANDARD_OUTPUT.symbolValue());
          return (Stream)         // Not reached.
          type_error(obj, Symbol.STREAM);
  }

  public static final Stream inSynonymOf(LispObject obj)
    throws ConditionThrowable
  {
    if (obj instanceof Stream)
      return (Stream) obj;
    if (obj == T)
      return checkCharacterInputStream(Symbol.TERMINAL_IO.symbolValue());
    if (obj == NIL)
      return checkCharacterInputStream(Symbol.STANDARD_INPUT.symbolValue());
          return (Stream)         // Not reached.
          type_error(obj, Symbol.STREAM);
  }

  public static final void writeByte(int n, LispObject obj)
    throws ConditionThrowable
  {
    if (n < 0 || n > 255)
      type_error(Fixnum.getInstance(n), UNSIGNED_BYTE_8);
    checkStream(obj)._writeByte(n);
  }

  public static final Readtable checkReadtable(LispObject obj)
    throws ConditionThrowable
  {
          if (obj instanceof Readtable)   
                  return (Readtable) obj;         
          return (Readtable)// Not reached.       
          type_error(obj, Symbol.READTABLE);
  }
  
  public final static AbstractString checkString(LispObject obj) 
   throws ConditionThrowable 
  {
          if (obj instanceof AbstractString)            
                  return (AbstractString) obj;                    
          return (AbstractString)// Not reached.               
              type_error(obj, Symbol.STRING);
  }
  
  public final static LispClass checkClass(LispObject obj) 
   throws ConditionThrowable 
   {
          if (obj instanceof LispClass)         
                  return (LispClass) obj;                         
          return (LispClass)// Not reached.                    
                type_error(obj, Symbol.CLASS);
   }   

  public final static Layout checkLayout(LispObject obj) 
   throws ConditionThrowable 
  {
          if (obj instanceof Layout)            
                  return (Layout) obj;                    
          return (Layout)// Not reached.               
                type_error(obj, Symbol.LAYOUT);
  }

  public static final Readtable designator_readtable(LispObject obj)
    throws ConditionThrowable
  {
    if (obj == NIL)
      obj = STANDARD_READTABLE.symbolValue();
    if (obj == null)
        throw new NullPointerException();
    return checkReadtable(obj);
  }

  public static final Environment checkEnvironment(LispObject obj)
    throws ConditionThrowable
  {
          if (obj instanceof Environment)         
                  return (Environment) obj;         
          return (Environment)// Not reached.       
        type_error(obj, Symbol.ENVIRONMENT);
  }

  public static final void checkBounds(int start, int end, int length)
    throws ConditionThrowable
  {
    if (start < 0 || end < 0 || start > end || end > length)
      {
        FastStringBuffer sb = new FastStringBuffer("The bounding indices ");
        sb.append(start);
        sb.append(" and ");
        sb.append(end);
        sb.append(" are bad for a sequence of length ");
        sb.append(length);
        sb.append('.');
        error(new TypeError(sb.toString()));
      }
  }

  public static final LispObject coerceToFunction(LispObject obj)
    throws ConditionThrowable
  {
    if (obj instanceof Function)
      return obj;
    if (obj instanceof StandardGenericFunction)
      return obj;
    if (obj instanceof Symbol)
      {
        LispObject fun = obj.getSymbolFunction();
        if (fun instanceof Function)
          return (Function) fun;
      }
    else if (obj instanceof Cons && obj.car() == Symbol.LAMBDA)
      return new Closure(obj, new Environment());
    error(new UndefinedFunction(obj));
    // Not reached.
    return null;
  }

  // Returns package or throws exception.
  public static final Package coerceToPackage(LispObject obj)
    throws ConditionThrowable
  {
    if (obj instanceof Package)
      return (Package) obj;
    Package pkg = Packages.findPackage(javaString(obj));
    if (pkg != null)
      return pkg;
    error(new PackageError(obj.writeToString() + " is not the name of a package."));
    // Not reached.
    return null;
  }

  public static Pathname coerceToPathname(LispObject arg)
    throws ConditionThrowable
  {
    if (arg instanceof Pathname)
      return (Pathname) arg;
    if (arg instanceof AbstractString)
      return Pathname.parseNamestring((AbstractString)arg);
    if (arg instanceof FileStream)
      return ((FileStream)arg).getPathname();
    type_error(arg, list(Symbol.OR, Symbol.PATHNAME,
                               Symbol.STRING, Symbol.FILE_STREAM));
    // Not reached.
    return null;
  }

  public LispObject assq(LispObject item, LispObject alist)
    throws ConditionThrowable
  {
    while (alist instanceof Cons)
      {
        LispObject entry = ((Cons)alist).car;
        if (entry instanceof Cons)
          {
            if (((Cons)entry).car == item)
              return entry;
          }
        else if (entry != NIL)
          return type_error(entry, Symbol.LIST);
        alist = ((Cons)alist).cdr;
      }
    if (alist != NIL)
      return type_error(alist, Symbol.LIST);
    return NIL;
  }

  public static final boolean memq(LispObject item, LispObject list)
    throws ConditionThrowable
  {
    while (list instanceof Cons)
      {
        if (item == ((Cons)list).car)
          return true;
        list = ((Cons)list).cdr;
      }
    if (list != NIL)
      type_error(list, Symbol.LIST);
    return false;
  }

  public static final boolean memql(LispObject item, LispObject list)
    throws ConditionThrowable
  {
    while (list instanceof Cons)
      {
        if (item.eql(((Cons)list).car))
          return true;
        list = ((Cons)list).cdr;
      }
    if (list != NIL)
      type_error(list, Symbol.LIST);
    return false;
  }

  // Property lists.
  public static final LispObject getf(LispObject plist, LispObject indicator,
                                      LispObject defaultValue)
    throws ConditionThrowable
  {
    LispObject list = plist;
    while (list != NIL)
      {
        if (list.car() == indicator)
          return list.cadr();
        if (list.cdr() instanceof Cons)
          list = list.cddr();
        else
          return error(new TypeError("Malformed property list: " +
                                      plist.writeToString()));
      }
    return defaultValue;
  }

  public static final LispObject get(LispObject symbol, LispObject indicator)
    throws ConditionThrowable
  {
    LispObject list = checkSymbol(symbol).getPropertyList();
    while (list != NIL)
      {
        if (list.car() == indicator)
          return list.cadr();
        list = list.cddr();
      }
    return NIL;
  }

  public static final LispObject get(LispObject symbol, LispObject indicator,
                                     LispObject defaultValue)
    throws ConditionThrowable
  {
    LispObject list = checkSymbol(symbol).getPropertyList();
    while (list != NIL)
      {
        if (list.car() == indicator)
          return list.cadr();
        list = list.cddr();
      }
    return defaultValue;
  }

  public static final LispObject put(Symbol symbol, LispObject indicator,
                                     LispObject value)
    throws ConditionThrowable
  {
    LispObject list = symbol.getPropertyList();
    while (list != NIL)
      {
        if (list.car() == indicator)
          {
            // Found it!
            LispObject rest = list.cdr();
            rest.setCar(value);
            return value;
          }
        list = list.cddr();
      }
    // Not found.
    symbol.setPropertyList(new Cons(indicator,
                                    new Cons(value,
                                             symbol.getPropertyList())));
    return value;
  }

  public static final LispObject putf(LispObject plist, LispObject indicator,
                                      LispObject value)
    throws ConditionThrowable
  {
    LispObject list = plist;
    while (list != NIL)
      {
        if (list.car() == indicator)
          {
            // Found it!
            LispObject rest = list.cdr();
            rest.setCar(value);
            return plist;
          }
        list = list.cddr();
      }
    // Not found.
    return new Cons(indicator, new Cons(value, plist));
  }

  public static final LispObject remprop(Symbol symbol, LispObject indicator)
    throws ConditionThrowable
  {
    LispObject list = checkList(symbol.getPropertyList());
    LispObject prev = null;
    while (list != NIL)
      {
        if (!(list.cdr() instanceof Cons))
          error(new ProgramError("The symbol " + symbol.writeToString() +
                                  " has an odd number of items in its property list."));
        if (list.car() == indicator)
          {
            // Found it!
            if (prev != null)
              prev.setCdr(list.cddr());
            else
              symbol.setPropertyList(list.cddr());
            return T;
          }
        prev = list.cdr();
        list = list.cddr();
      }
    // Not found.
    return NIL;
  }

  public static final String format(LispObject formatControl,
                                    LispObject formatArguments)
    throws ConditionThrowable
  {
    final LispThread thread = LispThread.currentThread();
    String control = formatControl.getStringValue();
    LispObject[] args = formatArguments.copyToArray();
    StringBuffer sb = new StringBuffer();
    if (control != null)
      {
        final int limit = control.length();
        int j = 0;
        final int NEUTRAL = 0;
        final int TILDE = 1;
        int state = NEUTRAL;
        for (int i = 0; i < limit; i++)
          {
            char c = control.charAt(i);
            if (state == NEUTRAL)
              {
                if (c == '~')
                  state = TILDE;
                else
                  sb.append(c);
              }
            else if (state == TILDE)
              {
                if (c == 'A' || c == 'a')
                  {
                    if (j < args.length)
                      {
                        LispObject obj = args[j++];
                        SpecialBinding lastSpecialBinding = thread.lastSpecialBinding;
                        thread.bindSpecial(Symbol.PRINT_ESCAPE, NIL);
                        thread.bindSpecial(Symbol.PRINT_READABLY, NIL);
                        sb.append(obj.writeToString());
                        thread.lastSpecialBinding = lastSpecialBinding;
                      }
                  }
                else if (c == 'S' || c == 's')
                  {
                    if (j < args.length)
                      {
                        LispObject obj = args[j++];
                        SpecialBinding lastSpecialBinding = thread.lastSpecialBinding;
                        thread.bindSpecial(Symbol.PRINT_ESCAPE, T);
                        try {
                            sb.append(obj.writeToString());
                        }
                        finally {
                            thread.lastSpecialBinding = lastSpecialBinding;
                        }
                      }
                  }
                else if (c == 'D' || c == 'd')
                  {
                    if (j < args.length)
                      {
                        LispObject obj = args[j++];
                        SpecialBinding lastSpecialBinding = thread.lastSpecialBinding;
                        thread.bindSpecial(Symbol.PRINT_ESCAPE, NIL);
                        thread.bindSpecial(Symbol.PRINT_RADIX, NIL);
                        thread.bindSpecial(Symbol.PRINT_BASE, Fixnum.constants[10]);
                        try {
                            sb.append(obj.writeToString());
                        }
                        finally {
                            thread.lastSpecialBinding = lastSpecialBinding;
                        }
                      }
                  }
                else if (c == 'X' || c == 'x')
                  {
                    if (j < args.length)
                      {
                        LispObject obj = args[j++];
                        SpecialBinding lastSpecialBinding = thread.lastSpecialBinding;
                        thread.bindSpecial(Symbol.PRINT_ESCAPE, NIL);
                        thread.bindSpecial(Symbol.PRINT_RADIX, NIL);
                        thread.bindSpecial(Symbol.PRINT_BASE, Fixnum.constants[16]);
                        try {
                            sb.append(obj.writeToString());
                        }
                        finally {
                            thread.lastSpecialBinding = lastSpecialBinding;
                        }
                      }
                  }
                else if (c == '%')
                  {
                    sb.append('\n');
                  }
                state = NEUTRAL;
              }
            else
              {
                // There are no other valid states.
                Debug.assertTrue(false);
              }
          }
      }
    return sb.toString();
  }

  public static final Symbol intern(String name, Package pkg)
  {
    return pkg.intern(name);
  }

  // Used by the compiler.
  public static final Symbol internInPackage(String name, String packageName)
    throws ConditionThrowable
  {
    Package pkg = Packages.findPackage(packageName);
    if (pkg == null)
      error(new LispError(packageName + " is not the name of a package."));
    return pkg.intern(name);
  }

  public static final Symbol internKeyword(String s)
  {
    return PACKAGE_KEYWORD.intern(s);
  }

  // The compiler's object table.
  private static final Hashtable<String,LispObject> objectTable =
          new Hashtable<String,LispObject>();

  public static final LispObject recall(SimpleString key)
  {
    return (LispObject) objectTable.remove(key.getStringValue());
  }

  // ### remember
  public static final Primitive REMEMBER =
    new Primitive("remember", PACKAGE_SYS, true)
    {
      @Override
      public LispObject execute(LispObject key, LispObject value)
        throws ConditionThrowable
      {
        objectTable.put(key.getStringValue(), value);
        return NIL;
      }
    };

  public static final Symbol internSpecial(String name, Package pkg,
                                           LispObject value)
  {
    Symbol symbol = pkg.intern(name);
    symbol.setSpecial(true);
    symbol.setSymbolValue(value);
    return symbol;
  }

  public static final Symbol internConstant(String name, Package pkg,
                                            LispObject value)
  {
    Symbol symbol = pkg.intern(name);
    symbol.initializeConstant(value);
    return symbol;
  }

  public static final Symbol exportSpecial(String name, Package pkg,
                                           LispObject value)
  {
    Symbol symbol = pkg.intern(name);
    try
      {
        pkg.export(symbol); // FIXME Inefficient!
      }
    catch (ConditionThrowable t)
      {
        Debug.trace(t);
      }
    symbol.setSpecial(true);
    symbol.setSymbolValue(value);
    return symbol;
  }

  public static final Symbol exportConstant(String name, Package pkg,
                                            LispObject value)
  {
    Symbol symbol = pkg.intern(name);
    try
      {
        pkg.export(symbol); // FIXME Inefficient!
      }
    catch (ConditionThrowable t)
      {
        Debug.trace(t);
      }
    symbol.initializeConstant(value);
    return symbol;
  }

  static
  {
    String userDir = System.getProperty("user.dir");
    if (userDir != null && userDir.length() > 0)
      {
        if (userDir.charAt(userDir.length() - 1) != File.separatorChar)
          userDir = userDir.concat(File.separator);
      }
    // This string will be converted to a pathname when Pathname.java is loaded.
    Symbol.DEFAULT_PATHNAME_DEFAULTS.initializeSpecial(new SimpleString(userDir));
  }

  static
  {
    Symbol._PACKAGE_.initializeSpecial(PACKAGE_CL_USER);
  }

  public static final Package getCurrentPackage()
  {
    return (Package) Symbol._PACKAGE_.symbolValueNoThrow();
  }

  private static Stream stdin = new Stream(System.in, Symbol.CHARACTER, true);

  private static Stream stdout = new Stream(System.out, Symbol.CHARACTER, true);

  static
  {
    Symbol.STANDARD_INPUT.initializeSpecial(stdin);
    Symbol.STANDARD_OUTPUT.initializeSpecial(stdout);
    Symbol.ERROR_OUTPUT.initializeSpecial(stdout);
    Symbol.TRACE_OUTPUT.initializeSpecial(stdout);
    Symbol.TERMINAL_IO.initializeSpecial(new TwoWayStream(stdin, stdout, true));
    Symbol.QUERY_IO.initializeSpecial(new TwoWayStream(stdin, stdout, true));
    Symbol.DEBUG_IO.initializeSpecial(new TwoWayStream(stdin, stdout, true));
  }

  public static final void resetIO(Stream in, Stream out)
  {
    stdin = in;
    stdout = out;
    Symbol.STANDARD_INPUT.setSymbolValue(stdin);
    Symbol.STANDARD_OUTPUT.setSymbolValue(stdout);
    Symbol.ERROR_OUTPUT.setSymbolValue(stdout);
    Symbol.TRACE_OUTPUT.setSymbolValue(stdout);
    Symbol.TERMINAL_IO.setSymbolValue(new TwoWayStream(stdin, stdout, true));
    Symbol.QUERY_IO.setSymbolValue(new TwoWayStream(stdin, stdout, true));
    Symbol.DEBUG_IO.setSymbolValue(new TwoWayStream(stdin, stdout, true));
  }

  // Used in org/armedbear/j/JLisp.java.
  public static final void resetIO()
  {
    resetIO(new Stream(System.in, Symbol.CHARACTER, true),
            new Stream(System.out, Symbol.CHARACTER, true));
  }

  public static final TwoWayStream getTerminalIO()
  {
    return (TwoWayStream) Symbol.TERMINAL_IO.symbolValueNoThrow();
  }

  public static final Stream getStandardInput()
  {
    return (Stream) Symbol.STANDARD_INPUT.symbolValueNoThrow();
  }

  public static final Stream getStandardOutput() throws ConditionThrowable
  {
    return checkCharacterOutputStream(Symbol.STANDARD_OUTPUT.symbolValue());
  }

  static
  {
    Symbol.CURRENT_READTABLE.initializeSpecial(new Readtable());
  }

  // ### +standard-readtable+
  // internal symbol
  public static final Symbol STANDARD_READTABLE =
    internConstant("+STANDARD-READTABLE+", PACKAGE_SYS, new Readtable());

  public static final Readtable currentReadtable() throws ConditionThrowable
  {
    return (Readtable) Symbol.CURRENT_READTABLE.symbolValue();
  }

  static
  {
    Symbol.READ_SUPPRESS.initializeSpecial(NIL);
    Symbol.DEBUGGER_HOOK.initializeSpecial(NIL);
  }

  static
  {
    Symbol.MOST_POSITIVE_FIXNUM.initializeConstant(Fixnum.getInstance(Integer.MAX_VALUE));
    Symbol.MOST_NEGATIVE_FIXNUM.initializeConstant(Fixnum.getInstance(Integer.MIN_VALUE));
    Symbol.MOST_POSITIVE_JAVA_LONG.initializeConstant(Bignum.getInstance(Long.MAX_VALUE));
    Symbol.MOST_NEGATIVE_JAVA_LONG.initializeConstant(Bignum.getInstance(Long.MIN_VALUE));
  }

  public static void exit(int status)
  {
    Interpreter interpreter = Interpreter.getInstance();
    if (interpreter != null)
      interpreter.kill(status);
  }

  // ### t
  public static final Symbol T = Symbol.T;
  static
  {
    T.initializeConstant(T);
  }

  static
  {
    Symbol.READ_EVAL.initializeSpecial(T);
  }

  // ### *features*
  static
  {
    Symbol.FEATURES.initializeSpecial(NIL);
    String osName = System.getProperty("os.name");
    if (osName.startsWith("Linux"))
      {
        Symbol.FEATURES.setSymbolValue(list(Keyword.ARMEDBEAR,
                                             Keyword.ABCL,
                                             Keyword.COMMON_LISP,
                                             Keyword.ANSI_CL,
                                             Keyword.UNIX,
                                             Keyword.LINUX,
                                             Keyword.CDR6));
      }
    else if (osName.startsWith("SunOS"))
      {
        Symbol.FEATURES.setSymbolValue(list(Keyword.ARMEDBEAR,
                                             Keyword.ABCL,
                                             Keyword.COMMON_LISP,
                                             Keyword.ANSI_CL,
                                             Keyword.UNIX,
                                             Keyword.SUNOS,
                                             Keyword.CDR6));
      }
    else if (osName.startsWith("Mac OS X"))
      {
        Symbol.FEATURES.setSymbolValue(list(Keyword.ARMEDBEAR,
                                             Keyword.ABCL,
                                             Keyword.COMMON_LISP,
                                             Keyword.ANSI_CL,
                                             Keyword.UNIX,
                                             Keyword.DARWIN,
                                             Keyword.CDR6));
      }
    else if (osName.startsWith("FreeBSD"))
      {
        Symbol.FEATURES.setSymbolValue(list(Keyword.ARMEDBEAR,
                                             Keyword.ABCL,
                                             Keyword.COMMON_LISP,
                                             Keyword.ANSI_CL,
                                             Keyword.UNIX,
                                             Keyword.FREEBSD,
                                             Keyword.CDR6));
      }
    else if (osName.startsWith("OpenBSD"))
      {
        Symbol.FEATURES.setSymbolValue(list(Keyword.ARMEDBEAR,
                                             Keyword.ABCL,
                                             Keyword.COMMON_LISP,
                                             Keyword.ANSI_CL,
                                             Keyword.UNIX,
                                             Keyword.OPENBSD,
                                             Keyword.CDR6));
      }
    else if (osName.startsWith("NetBSD"))
      {
        Symbol.FEATURES.setSymbolValue(list(Keyword.ARMEDBEAR,
                                             Keyword.ABCL,
                                             Keyword.COMMON_LISP,
                                             Keyword.ANSI_CL,
                                             Keyword.UNIX,
                                             Keyword.NETBSD,
                                             Keyword.CDR6));
      }
    else if (osName.startsWith("Windows"))
      {
        Symbol.FEATURES.setSymbolValue(list(Keyword.ARMEDBEAR,
                                             Keyword.ABCL,
                                             Keyword.COMMON_LISP,
                                             Keyword.ANSI_CL,
                                             Keyword.WINDOWS,
                                             Keyword.CDR6));
      }
    else
      {
        Symbol.FEATURES.setSymbolValue(list(Keyword.ARMEDBEAR,
                                             Keyword.ABCL,
                                             Keyword.COMMON_LISP,
                                             Keyword.ANSI_CL,
                                             Keyword.CDR6));
      }
  }
  static
  {
    final String version = System.getProperty("java.version");
    if (version.startsWith("1.5"))
      {
        Symbol.FEATURES.setSymbolValue(new Cons(Keyword.JAVA_1_5,
                                                Symbol.FEATURES.getSymbolValue()));
      }
    else if (version.startsWith("1.6"))
      {
        Symbol.FEATURES.setSymbolValue(new Cons(Keyword.JAVA_1_6,
                                                Symbol.FEATURES.getSymbolValue()));
      }
    else if (version.startsWith("1.7"))
      {
        Symbol.FEATURES.setSymbolValue(new Cons(Keyword.JAVA_1_7,
                                                Symbol.FEATURES.getSymbolValue()));
      }
  }
  static
  {
    String os_arch = System.getProperty("os.arch");
    if(os_arch != null) {
      if (os_arch.equals("amd64"))
        Symbol.FEATURES.setSymbolValue(new Cons(Keyword.X86_64,
                                                Symbol.FEATURES.getSymbolValue()));
      else if (os_arch.equals("x86"))
        Symbol.FEATURES.setSymbolValue(new Cons(Keyword.X86,
                                                Symbol.FEATURES.getSymbolValue()));
    }
  }

  static
  {
    Symbol.MODULES.initializeSpecial(NIL);
  }

  static
  {
    Symbol.LOAD_VERBOSE.initializeSpecial(NIL);
    Symbol.LOAD_PRINT.initializeSpecial(NIL);
    Symbol.LOAD_PATHNAME.initializeSpecial(NIL);
    Symbol.LOAD_TRUENAME.initializeSpecial(NIL);
    Symbol.COMPILE_VERBOSE.initializeSpecial(T);
    Symbol.COMPILE_PRINT.initializeSpecial(T);
    Symbol._COMPILE_FILE_PATHNAME_.initializeSpecial(NIL);
    Symbol.COMPILE_FILE_TRUENAME.initializeSpecial(NIL);
  }

  // ### *load-depth*
  // internal symbol
  public static final Symbol _LOAD_DEPTH_ =
    internSpecial("*LOAD-DEPTH*", PACKAGE_SYS, Fixnum.ZERO);

  // ### *load-stream*
  // internal symbol
  public static final Symbol _LOAD_STREAM_ =
    internSpecial("*LOAD-STREAM*", PACKAGE_SYS, NIL);

  // ### *source*
  // internal symbol
  public static final Symbol _SOURCE_ =
    exportSpecial("*SOURCE*", PACKAGE_SYS, NIL);

  // ### *source-position*
  // internal symbol
  public static final Symbol _SOURCE_POSITION_ =
    exportSpecial("*SOURCE-POSITION*", PACKAGE_SYS, NIL);

  // ### *autoload-verbose*
  // internal symbol
  public static final Symbol _AUTOLOAD_VERBOSE_ =
    exportSpecial("*AUTOLOAD-VERBOSE*", PACKAGE_EXT, NIL);

  // ### *compile-file-type*
  public static final String COMPILE_FILE_TYPE = "abcl";
  public static final Symbol _COMPILE_FILE_TYPE_ =
    internConstant("*COMPILE-FILE-TYPE*", PACKAGE_SYS,
                   new SimpleString(COMPILE_FILE_TYPE));

  // ### *compile-file-zip*
  public static final Symbol _COMPILE_FILE_ZIP_ =
    exportSpecial("*COMPILE-FILE-ZIP*", PACKAGE_SYS, T);

  static
  {
    Symbol.MACROEXPAND_HOOK.initializeSpecial(Symbol.FUNCALL);
  }

  public static final int ARRAY_DIMENSION_MAX = Integer.MAX_VALUE;
  static
  {
    // ### array-dimension-limit
    Symbol.ARRAY_DIMENSION_LIMIT.initializeConstant(Fixnum.getInstance(ARRAY_DIMENSION_MAX));
  }

  // ### char-code-limit
  // "The upper exclusive bound on the value returned by the function CHAR-CODE."
  public static final int CHAR_MAX = 256;
  static
  {
    Symbol.CHAR_CODE_LIMIT.initializeConstant(Fixnum.getInstance(CHAR_MAX));
  }

  static
  {
    Symbol.READ_BASE.initializeSpecial(Fixnum.constants[10]);
  }

  static
  {
    Symbol.READ_DEFAULT_FLOAT_FORMAT.initializeSpecial(Symbol.SINGLE_FLOAT);
  }

  // Printer control variables.
  static
  {
    Symbol.PRINT_ARRAY.initializeSpecial(T);
    Symbol.PRINT_BASE.initializeSpecial(Fixnum.constants[10]);
    Symbol.PRINT_CASE.initializeSpecial(Keyword.UPCASE);
    Symbol.PRINT_CIRCLE.initializeSpecial(NIL);
    Symbol.PRINT_ESCAPE.initializeSpecial(T);
    Symbol.PRINT_GENSYM.initializeSpecial(T);
    Symbol.PRINT_LENGTH.initializeSpecial(NIL);
    Symbol.PRINT_LEVEL.initializeSpecial(NIL);
    Symbol.PRINT_LINES.initializeSpecial(NIL);
    Symbol.PRINT_MISER_WIDTH.initializeSpecial(NIL);
    Symbol.PRINT_PPRINT_DISPATCH.initializeSpecial(NIL);
    Symbol.PRINT_PRETTY.initializeSpecial(NIL);
    Symbol.PRINT_RADIX.initializeSpecial(NIL);
    Symbol.PRINT_READABLY.initializeSpecial(NIL);
    Symbol.PRINT_RIGHT_MARGIN.initializeSpecial(NIL);
  }

  public static final Symbol _PRINT_STRUCTURE_ =
    exportSpecial("*PRINT-STRUCTURE*", PACKAGE_EXT, T);

  // ### *current-print-length*
  public static final Symbol _CURRENT_PRINT_LENGTH_ =
    exportSpecial("*CURRENT-PRINT-LENGTH*", PACKAGE_SYS, Fixnum.ZERO);

  // ### *current-print-level*
  public static final Symbol _CURRENT_PRINT_LEVEL_ =
    exportSpecial("*CURRENT-PRINT-LEVEL*", PACKAGE_SYS, Fixnum.ZERO);

  public static final Symbol _PRINT_FASL_ =
    internSpecial("*PRINT-FASL*", PACKAGE_SYS, NIL);

  static
  {
    Symbol._RANDOM_STATE_.initializeSpecial(new RandomState());
  }

  static
  {
    Symbol.STAR.initializeSpecial(NIL);
    Symbol.STAR_STAR.initializeSpecial(NIL);
    Symbol.STAR_STAR_STAR.initializeSpecial(NIL);
    Symbol.MINUS.initializeSpecial(NIL);
    Symbol.PLUS.initializeSpecial(NIL);
    Symbol.PLUS_PLUS.initializeSpecial(NIL);
    Symbol.PLUS_PLUS_PLUS.initializeSpecial(NIL);
    Symbol.SLASH.initializeSpecial(NIL);
    Symbol.SLASH_SLASH.initializeSpecial(NIL);
    Symbol.SLASH_SLASH_SLASH.initializeSpecial(NIL);
  }

  // Floating point constants.
  static
  {
    Symbol.PI.initializeConstant(new DoubleFloat(Math.PI));
    Symbol.SHORT_FLOAT_EPSILON.initializeConstant(new SingleFloat((float)5.960465E-8));
    Symbol.SINGLE_FLOAT_EPSILON.initializeConstant(new SingleFloat((float)5.960465E-8));
    Symbol.DOUBLE_FLOAT_EPSILON.initializeConstant(new DoubleFloat((double)1.1102230246251568E-16));
    Symbol.LONG_FLOAT_EPSILON.initializeConstant(new DoubleFloat((double)1.1102230246251568E-16));
    Symbol.SHORT_FLOAT_NEGATIVE_EPSILON.initializeConstant(new SingleFloat(2.9802326e-8f));
    Symbol.SINGLE_FLOAT_NEGATIVE_EPSILON.initializeConstant(new SingleFloat(2.9802326e-8f));
    Symbol.DOUBLE_FLOAT_NEGATIVE_EPSILON.initializeConstant(new DoubleFloat((double)5.551115123125784E-17));
    Symbol.LONG_FLOAT_NEGATIVE_EPSILON.initializeConstant(new DoubleFloat((double)5.551115123125784E-17));
    Symbol.MOST_POSITIVE_SHORT_FLOAT.initializeConstant(new SingleFloat(Float.MAX_VALUE));
    Symbol.MOST_POSITIVE_SINGLE_FLOAT.initializeConstant(new SingleFloat(Float.MAX_VALUE));
    Symbol.MOST_POSITIVE_DOUBLE_FLOAT.initializeConstant(new DoubleFloat(Double.MAX_VALUE));
    Symbol.MOST_POSITIVE_LONG_FLOAT.initializeConstant(new DoubleFloat(Double.MAX_VALUE));
    Symbol.LEAST_POSITIVE_SHORT_FLOAT.initializeConstant(new SingleFloat(Float.MIN_VALUE));
    Symbol.LEAST_POSITIVE_SINGLE_FLOAT.initializeConstant(new SingleFloat(Float.MIN_VALUE));
    Symbol.LEAST_POSITIVE_DOUBLE_FLOAT.initializeConstant(new DoubleFloat(Double.MIN_VALUE));
    Symbol.LEAST_POSITIVE_LONG_FLOAT.initializeConstant(new DoubleFloat(Double.MIN_VALUE));
    Symbol.LEAST_POSITIVE_NORMALIZED_SHORT_FLOAT.initializeConstant(new SingleFloat(1.17549435e-38f));
    Symbol.LEAST_POSITIVE_NORMALIZED_SINGLE_FLOAT.initializeConstant(new SingleFloat(1.17549435e-38f));
    Symbol.LEAST_POSITIVE_NORMALIZED_DOUBLE_FLOAT.initializeConstant(new DoubleFloat(2.2250738585072014e-308d));
    Symbol.LEAST_POSITIVE_NORMALIZED_LONG_FLOAT.initializeConstant(new DoubleFloat(2.2250738585072014e-308d));
    Symbol.MOST_NEGATIVE_SHORT_FLOAT.initializeConstant(new SingleFloat(- Float.MAX_VALUE));
    Symbol.MOST_NEGATIVE_SINGLE_FLOAT.initializeConstant(new SingleFloat(- Float.MAX_VALUE));
    Symbol.MOST_NEGATIVE_DOUBLE_FLOAT.initializeConstant(new DoubleFloat(- Double.MAX_VALUE));
    Symbol.MOST_NEGATIVE_LONG_FLOAT.initializeConstant(new DoubleFloat(- Double.MAX_VALUE));
    Symbol.LEAST_NEGATIVE_SHORT_FLOAT.initializeConstant(new SingleFloat(- Float.MIN_VALUE));
    Symbol.LEAST_NEGATIVE_SINGLE_FLOAT.initializeConstant(new SingleFloat(- Float.MIN_VALUE));
    Symbol.LEAST_NEGATIVE_DOUBLE_FLOAT.initializeConstant(new DoubleFloat(- Double.MIN_VALUE));
    Symbol.LEAST_NEGATIVE_LONG_FLOAT.initializeConstant(new DoubleFloat(- Double.MIN_VALUE));
    Symbol.LEAST_NEGATIVE_NORMALIZED_SHORT_FLOAT.initializeConstant(new SingleFloat(-1.17549435e-38f));
    Symbol.LEAST_NEGATIVE_NORMALIZED_SINGLE_FLOAT.initializeConstant(new SingleFloat(-1.17549435e-38f));
    Symbol.LEAST_NEGATIVE_NORMALIZED_DOUBLE_FLOAT.initializeConstant(new DoubleFloat(-2.2250738585072014e-308d));
    Symbol.LEAST_NEGATIVE_NORMALIZED_LONG_FLOAT.initializeConstant(new DoubleFloat(-2.2250738585072014e-308d));
  }

  static
  {
    Symbol.BOOLE_CLR.initializeConstant(Fixnum.ZERO);
    Symbol.BOOLE_SET.initializeConstant(Fixnum.ONE);
    Symbol.BOOLE_1.initializeConstant(Fixnum.TWO);
    Symbol.BOOLE_2.initializeConstant(Fixnum.constants[3]);
    Symbol.BOOLE_C1.initializeConstant(Fixnum.constants[4]);
    Symbol.BOOLE_C2.initializeConstant(Fixnum.constants[5]);
    Symbol.BOOLE_AND.initializeConstant(Fixnum.constants[6]);
    Symbol.BOOLE_IOR.initializeConstant(Fixnum.constants[7]);
    Symbol.BOOLE_XOR.initializeConstant(Fixnum.constants[8]);
    Symbol.BOOLE_EQV.initializeConstant(Fixnum.constants[9]);
    Symbol.BOOLE_NAND.initializeConstant(Fixnum.constants[10]);
    Symbol.BOOLE_NOR.initializeConstant(Fixnum.constants[11]);
    Symbol.BOOLE_ANDC1.initializeConstant(Fixnum.constants[12]);
    Symbol.BOOLE_ANDC2.initializeConstant(Fixnum.constants[13]);
    Symbol.BOOLE_ORC1.initializeConstant(Fixnum.constants[14]);
    Symbol.BOOLE_ORC2.initializeConstant(Fixnum.constants[15]);
  }

  static
  {
    // ### call-arguments-limit
    Symbol.CALL_ARGUMENTS_LIMIT.initializeConstant(Fixnum.constants[50]);
  }

  static
  {
    // ### lambda-parameters-limit
    Symbol.LAMBDA_PARAMETERS_LIMIT.initializeConstant(Fixnum.constants[50]);
  }

  static
  {
    // ### multiple-values-limit
    Symbol.MULTIPLE_VALUES_LIMIT.initializeConstant(Fixnum.constants[20]);
  }

  static
  {
    // ### internal-time-units-per-second
    Symbol.INTERNAL_TIME_UNITS_PER_SECOND.initializeConstant(Fixnum.getInstance(1000));
  }

  // ### call-registers-limit
  public static final Symbol CALL_REGISTERS_LIMIT =
    exportConstant("CALL-REGISTERS-LIMIT", PACKAGE_SYS,
                   Fixnum.constants[CALL_REGISTERS_MAX]);

  // ### *warn-on-redefinition*
  public static final Symbol _WARN_ON_REDEFINITION_ =
    exportSpecial("*WARN-ON-REDEFINITION*", PACKAGE_EXT, T);

  // ### *saved-backtrace*
  public static final Symbol _SAVED_BACKTRACE_ =
    exportSpecial("*SAVED-BACKTRACE*", PACKAGE_EXT, NIL);

  // ### *batch-mode*
  public static final Symbol _BATCH_MODE_ =
    exportSpecial("*BATCH-MODE*", PACKAGE_EXT, NIL);

  // ### *noinform*
  public static final Symbol _NOINFORM_ =
    exportSpecial("*NOINFORM*", PACKAGE_SYS, NIL);

  // ### *disassembler*
  public static final Symbol _DISASSEMBLER_ =
    exportSpecial("*DISASSEMBLER*", PACKAGE_EXT,
                  new SimpleString("jad -a -p")); // or "jad -dis -p"

  // ### *speed* compiler policy
  public static final Symbol _SPEED_ =
    exportSpecial("*SPEED*", PACKAGE_SYS, Fixnum.ONE);

  // ### *space* compiler policy
  public static final Symbol _SPACE_ =
    exportSpecial("*SPACE*", PACKAGE_SYS, Fixnum.ONE);

  // ### *safety* compiler policy
  public static final Symbol _SAFETY_ =
    exportSpecial("*SAFETY*", PACKAGE_SYS, Fixnum.ONE);

  // ### *debug* compiler policy
  public static final Symbol _DEBUG_ =
    exportSpecial("*DEBUG*", PACKAGE_SYS, Fixnum.ONE);

  // ### *explain* compiler policy
  public static final Symbol _EXPLAIN_ =
    exportSpecial("*EXPLAIN*", PACKAGE_SYS, NIL);

  // ### *enable-inline-expansion*
  public static final Symbol _ENABLE_INLINE_EXPANSION_ =
    exportSpecial("*ENABLE-INLINE-EXPANSION*", PACKAGE_EXT, T);

  // ### *require-stack-frame*
  public static final Symbol _REQUIRE_STACK_FRAME_ =
    exportSpecial("*REQUIRE-STACK-FRAME*", PACKAGE_EXT, NIL);

  static
  {
    Symbol.SUPPRESS_COMPILER_WARNINGS.initializeSpecial(NIL);
  }

  public static final Symbol _COMPILE_FILE_ENVIRONMENT_ =
    exportSpecial("*COMPILE-FILE-ENVIRONMENT*", PACKAGE_SYS, NIL);

  public static final LispObject UNBOUND_VALUE = new LispObject()
    {
      @Override
      public String writeToString()
      {
        return "#<UNBOUND>";
      }
    };

  public static final LispObject NULL_VALUE = new LispObject()
    {
      @Override
      public String writeToString()
      {
        return "null";
      }
    };

  public static final Symbol _SLOT_UNBOUND_ =
    exportConstant("+SLOT-UNBOUND+", PACKAGE_SYS, UNBOUND_VALUE);

  public static final Symbol _CL_PACKAGE_ =
    exportConstant("+CL-PACKAGE+", PACKAGE_SYS, PACKAGE_CL);

  public static final Symbol _KEYWORD_PACKAGE_ =
    exportConstant("+KEYWORD-PACKAGE+", PACKAGE_SYS, PACKAGE_KEYWORD);

  // ### *backquote-count*
  public static final Symbol _BACKQUOTE_COUNT_ =
    internSpecial("*BACKQUOTE-COUNT*", PACKAGE_SYS, Fixnum.ZERO);

  // ### *bq-vector-flag*
  public static final Symbol _BQ_VECTOR_FLAG_ =
    internSpecial("*BQ-VECTOR-FLAG*", PACKAGE_SYS, list(new Symbol("bqv")));

  // ### *traced-names*
  public static final Symbol _TRACED_NAMES_ =
    exportSpecial("*TRACED-NAMES*", PACKAGE_SYS, NIL);

  // Floating point traps.
  protected static boolean TRAP_OVERFLOW  = true;
  protected static boolean TRAP_UNDERFLOW = true;


  // Extentions
  static {
    Symbol._INSPECTOR_HOOK_.initializeSpecial(NIL);
  }

  private static final void loadClass(String className)
  {
    try
      {
        Class.forName(className);
      }
    catch (ClassNotFoundException e)
      {
        e.printStackTrace();
      }
  }

  static
  {
    loadClass("org.armedbear.lisp.Primitives");
    loadClass("org.armedbear.lisp.SpecialOperators");
    loadClass("org.armedbear.lisp.Extensions");
    loadClass("org.armedbear.lisp.CompiledClosure");
    loadClass("org.armedbear.lisp.Autoload");
    loadClass("org.armedbear.lisp.AutoloadMacro");
    loadClass("org.armedbear.lisp.cxr");
    loadClass("org.armedbear.lisp.Do");
    loadClass("org.armedbear.lisp.dolist");
    loadClass("org.armedbear.lisp.dotimes");
    loadClass("org.armedbear.lisp.Pathname");
    loadClass("org.armedbear.lisp.LispClass");
    loadClass("org.armedbear.lisp.BuiltInClass");
    loadClass("org.armedbear.lisp.StructureObject");
    loadClass("org.armedbear.lisp.ash");
    loadClass("org.armedbear.lisp.Java");
    cold = false;
  }
}
