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
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.io.StringReader;
import java.math.BigInteger;
import java.net.URL;
import java.nio.charset.Charset;
import java.util.Hashtable;
import java.util.concurrent.ConcurrentHashMap;

public final class Lisp
{
  public static final boolean debug = true;

  public static boolean cold = true;

  public static boolean initialized;

  // Packages.
  public static final Package PACKAGE_CL =
    Packages.createPackage("COMMON-LISP", 2048); // EH 10-10-2010: Actual number = 1014
  public static final Package PACKAGE_CL_USER =
    Packages.createPackage("COMMON-LISP-USER", 1024);
  public static final Package PACKAGE_KEYWORD =
    Packages.createPackage("KEYWORD", 1024);
  public static final Package PACKAGE_SYS =
    Packages.createPackage("SYSTEM", 2048); // EH 10-10-2010: Actual number = 1216
  public static final Package PACKAGE_MOP =
    Packages.createPackage("MOP", 512); // EH 10-10-2010: Actual number = 277
  public static final Package PACKAGE_TPL =
    Packages.createPackage("TOP-LEVEL", 128); // EH 10-10-2010: Actual number = 6
  public static final Package PACKAGE_EXT =
    Packages.createPackage("EXTENSIONS", 256); // EH 10-10-2010: Actual number = 131
  public static final Package PACKAGE_JVM =
    Packages.createPackage("JVM", 2048); // EH 10-10-2010: Actual number = 1518
  public static final Package PACKAGE_LOOP =
    Packages.createPackage("LOOP", 512); // EH 10-10-2010: Actual number = 305
  public static final Package PACKAGE_PROF =
    Packages.createPackage("PROFILER");
  public static final Package PACKAGE_JAVA =
    Packages.createPackage("JAVA");
  public static final Package PACKAGE_LISP =
    Packages.createPackage("LISP");
  public static final Package PACKAGE_THREADS =
    Packages.createPackage("THREADS");
  public static final Package PACKAGE_FORMAT =
    Packages.createPackage("FORMAT");
  public static final Package PACKAGE_XP =
    Packages.createPackage("XP");
  public static final Package PACKAGE_PRECOMPILER =
    Packages.createPackage("PRECOMPILER");
  public static final Package PACKAGE_SEQUENCE =
    Packages.createPackage("SEQUENCE", 128); // EH 10-10-2010: Actual number 62


  @DocString(name="nil")
  public static final Symbol NIL = Nil.NIL;

  // We need NIL before we can call usePackage().
  static
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
    PACKAGE_THREADS.usePackage(PACKAGE_EXT);
    PACKAGE_THREADS.usePackage(PACKAGE_SYS);
    PACKAGE_FORMAT.usePackage(PACKAGE_CL);
    PACKAGE_FORMAT.usePackage(PACKAGE_EXT);
    PACKAGE_XP.usePackage(PACKAGE_CL);
    PACKAGE_PRECOMPILER.addNickname("PRE");
    PACKAGE_PRECOMPILER.usePackage(PACKAGE_CL);
    PACKAGE_PRECOMPILER.usePackage(PACKAGE_EXT);
    PACKAGE_PRECOMPILER.usePackage(PACKAGE_SYS);
    PACKAGE_SEQUENCE.usePackage(PACKAGE_CL);
  }

  // End-of-file marker.
  public static final LispObject EOF = new LispObject();

  // String hash randomization base
  // Sets a base offset hashing value per JVM session, as an antidote to
  // http://www.nruns.com/_downloads/advisory28122011.pdf
  //    (Denial of Service through hash table multi-collisions)
  public static final int randomStringHashBase =
          (int)(new java.util.Date().getTime());
  
  public static boolean profiling;

  public static boolean sampling;

  public static volatile boolean sampleNow;

  // args must not be null!
  public static final LispObject funcall(LispObject fun, LispObject[] args,
                                         LispThread thread)

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

  {
    if (form instanceof Cons)
      {
        LispObject car = ((Cons)form).car;
        if (car instanceof Symbol)
          {
            LispObject obj = env.lookupFunction(car);
            if (obj instanceof AutoloadMacro)
              {
                // Don't autoload function objects here:
                // we want that to happen upon the first use.
                // in case of macro functions, this *is* the first use.
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
        LispObject obj = env.lookup(symbol);
        if (obj == null) {
          obj = symbol.getSymbolMacro();
        }
        if (obj instanceof SymbolMacro) {
          return thread.setValues(((SymbolMacro)obj).getExpansion(), T);
        }
      }
    // Not a macro.
    return thread.setValues(form, NIL);
  }

  @DocString(name="interactive-eval")
  private static final Primitive INTERACTIVE_EVAL =
    new Primitive("interactive-eval", PACKAGE_SYS, true)
    {
      @Override
      public LispObject execute(LispObject object)
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
            return error(new StorageCondition("Out of memory " + e.getMessage()));
          }
        catch (StackOverflowError e)
          {
            thread.setSpecialVariable(_SAVED_BACKTRACE_,
                                      thread.backtrace(0));
            return error(new StorageCondition("Stack overflow."));
          }
        catch (ControlTransfer c)
          {
            throw c;
          }
        catch (ProcessingTerminated c)
          {
            throw c;
          }
        catch (IntegrityError c)
          {
            throw c;
          }
        catch (Throwable t) // ControlTransfer handled above
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

  private static final void pushJavaStackFrames()
  {
      final LispThread thread = LispThread.currentThread();
      final StackTraceElement[] frames = thread.getJavaStackTrace();

      // frames[0] java.lang.Thread.getStackTrace
      // frames[1] org.armedbear.lisp.LispThread.getJavaStackTrace
      // frames[2] org.armedbear.lisp.Lisp.pushJavaStackFrames

      if (frames.length > 5
        && frames[3].getClassName().equals("org.armedbear.lisp.Lisp")
        && frames[3].getMethodName().equals("error")
        && frames[4].getClassName().startsWith("org.armedbear.lisp.Lisp")
        && frames[4].getMethodName().equals("eval")) {
          // Error condition arising from within Lisp.eval(), so no
          // Java stack frames should be visible to the consumer of the stack abstraction
          return;
      }
      // Search for last Primitive in the StackTrace; that was the
      // last entry point from Lisp.
      int last = frames.length - 1;
      for (int i = 0; i<= last; i++) {
          if (frames[i].getClassName().startsWith("org.armedbear.lisp.Primitive"))
            last = i;
      }
      // Do not include the first three frames which, as noted above, constitute
      // the invocation of this method.
      while (last > 2) {
        thread.pushStackFrame(new JavaStackFrame(frames[last]));
        last--;
      }
  }


  public static final LispObject error(LispObject condition)
  {
    pushJavaStackFrames();
    return Symbol.ERROR.execute(condition);
  }

  public static final int ierror(LispObject condition)
  {
    error(condition);
    return 0; // Not reached
  }

  public static final String serror(LispObject condition)
  {
    error(condition);
    return ""; // Not reached
  }


  public static final LispObject error(LispObject condition, LispObject message)
  {
    pushJavaStackFrames();
    return Symbol.ERROR.execute(condition, Keyword.FORMAT_CONTROL, message);
  }

  public static final int ierror(LispObject condition, LispObject message)
  {
    error(condition, message);
    return 0; // Not reached
  }

  public static final String serror(LispObject condition, LispObject message)
  {
    error(condition, message);
    return ""; // Not reached
  }



  public static final LispObject type_error(LispObject datum,
                                            LispObject expectedType)

  {
    return error(new TypeError(datum, expectedType));
  }

  public static volatile boolean interrupted;

  public static synchronized final void setInterrupted(boolean b)
  {
    interrupted = b;
  }

  public static final void handleInterrupt()
  {
    setInterrupted(false);
    Symbol.BREAK.getSymbolFunction().execute();
    setInterrupted(false);
  }

  // Used by the compiler.
  public static final LispObject loadTimeValue(LispObject obj)

  {
    final LispThread thread = LispThread.currentThread();
    if (Symbol.LOAD_TRUENAME.symbolValue(thread) != NIL)
      return eval(obj, new Environment(), thread);
    else
      return NIL;
  }

  public static final LispObject eval(LispObject obj)

  {
    return eval(obj, new Environment(), LispThread.currentThread());
  }

  public static final LispObject eval(final LispObject obj,
                                      final Environment env,
                                      final LispThread thread)

  {
    thread._values = null;
    if (interrupted)
      handleInterrupt();
    if (thread.isDestroyed())
      throw new ThreadDestroyed();
    if (obj instanceof Symbol)
      {
        Symbol symbol = (Symbol)obj;
        LispObject result;
        if (symbol.isSpecialVariable())
          {
            if (symbol.constantp())
              return symbol.getSymbolValue();
            else
              result = thread.lookupSpecial(symbol);
          }
        else if (env.isDeclaredSpecial(symbol))
          result = thread.lookupSpecial(symbol);
        else
          result = env.lookup(symbol);
        if (result == null)
          {
            result = symbol.getSymbolMacro();
            if (result == null) {
                result = symbol.getSymbolValue();
            }
            if(result == null) {
              return error(new UnboundVariable(obj));
            }
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
            if (first instanceof Cons && first.car() == Symbol.LAMBDA)
              {
                Closure closure = new Closure(first, env);
                return evalCall(closure, ((Cons)obj).cdr, env, thread);
              }
            else
              return error(new ProgramError("Illegal function object: " +
                                             first.princToString()));
          }
      }
    else
      return obj;
  }

  public static final int CALL_REGISTERS_MAX = 8;

  // Also used in JProxy.java.
  public static final LispObject evalCall(LispObject function,
                                             LispObject args,
                                             Environment env,
                                             LispThread thread)

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

  {
    LispObject result = NIL;
    while (body != NIL)
      {
        result = eval(body.car(), env, thread);
        body = ((Cons)body).cdr;
      }
    return result;
  }

  public static final LispObject preprocessTagBody(LispObject body,
                                                   Environment env)

  {
    LispObject localTags = NIL; // Tags that are local to this TAGBODY.
    while (body != NIL)
      {
        LispObject current = body.car();
        body = ((Cons)body).cdr;
        if (current instanceof Cons)
          continue;
        // It's a tag.
        env.addTagBinding(current, body);
        localTags = new Cons(current, localTags);
      }
    return localTags;
  }

  /** Throws a Go exception to cause a non-local transfer
   * of control event, after checking that the extent of
   * the catching tagbody hasn't ended yet.
   *
   * This version is used by the compiler.
   */
  public static final LispObject nonLocalGo(LispObject tagbody,
                                            LispObject tag)

  {
    if (tagbody == null)
      return error(new ControlError("Unmatched tag "
                                    + tag.princToString() +
                                    " for GO outside lexical extent."));

    throw new Go(tagbody, tag);
  }

  /** Throws a Go exception to cause a non-local transfer
   * of control event, after checking that the extent of
   * the catching tagbody hasn't ended yet.
   *
   * This version is used by the interpreter.
   */
  static final LispObject nonLocalGo(Binding binding,
                                     LispObject tag)
  {
    if (binding.env.inactive)
      return error(new ControlError("Unmatched tag "
                                    + binding.symbol.princToString() +
                                    " for GO outside of lexical extent."));

    throw new Go(binding.env, binding.symbol);
  }

  /** Throws a Return exception to cause a non-local transfer
   * of control event, after checking that the extent of
   * the catching block hasn't ended yet.
   *
   * This version is used by the compiler.
   */
  public static final LispObject nonLocalReturn(LispObject blockId,
                                                LispObject blockName,
                                                LispObject result)

  {
    if (blockId == null)
      return error(new ControlError("Unmatched block "
                                    + blockName.princToString() + " for " +
                                    "RETURN-FROM outside lexical extent."));

    throw new Return(blockId, result);
  }

  /** Throws a Return exception to cause a non-local transfer
   * of control event, after checking that the extent of
   * the catching block hasn't ended yet.
   *
   * This version is used by the interpreter.
   */
  static final LispObject nonLocalReturn(Binding binding,
                                         Symbol block,
                                         LispObject result)
  {
    if (binding == null)
      {
        return error(new LispError("No block named " + block.getName() +
                                   " is currently visible."));
      }

    if (binding.env.inactive)
      return error(new ControlError("Unmatched block "
                                    + binding.symbol.princToString() +
                                    " for RETURN-FROM outside of" +
                                    " lexical extent."));

    throw new Return(binding.symbol, binding.value, result);
  }

  public static final LispObject processTagBody(LispObject body,
                                                LispObject localTags,
                                                Environment env)

  {
    LispObject remaining = body;
    LispThread thread = LispThread.currentThread();
    while (remaining != NIL)
      {
        LispObject current = remaining.car();
        if (current instanceof Cons)
          {
            try {
              // Handle GO inline if possible.
              if (((Cons)current).car == Symbol.GO)
                {
                  if (interrupted)
                    handleInterrupt();
                  LispObject tag = current.cadr();
                  Binding binding = env.getTagBinding(tag);
                  if (binding == null)
                    return error(new ControlError("No tag named " +
                                                  tag.princToString() +
                                                  " is currently visible."));
                  else if (memql(tag, localTags))
                    {
                      if (binding.value != null)
                        {
                          remaining = binding.value;
                          continue;
                        }
                    }
                  throw new Go(binding.env, tag);
                }
              eval(current, env, thread);
            }
            catch (Go go)
              {
                LispObject tag;
                if (go.getTagBody() == env
                    && memql(tag = go.getTag(), localTags))
                  {
                    Binding binding = env.getTagBinding(tag);
                    if (binding != null && binding.value != null)
                      {
                        remaining = binding.value;
                        continue;
                      }
                  }
                throw go;
              }
          }
        remaining = ((Cons)remaining).cdr;
      }
    thread._values = null;
    return NIL;
  }

  // Environment wrappers.
  static final boolean isSpecial(Symbol sym, LispObject ownSpecials)
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

  public static final void bindArg(LispObject ownSpecials,
                                      Symbol sym, LispObject value,
                                      Environment env, LispThread thread)

  {
    if (isSpecial(sym, ownSpecials)) {
      env.declareSpecial(sym);
      thread.bindSpecial(sym, value);
    }
    else
      env.bind(sym, value);
  }

  public static void bindArg(boolean special, Symbol sym, LispObject value,
                             Environment env, LispThread thread)
  {
      if (special) {
          env.declareSpecial(sym);
          thread.bindSpecial(sym, value);
      }
      else
          env.bind(sym, value);
  }

  public static LispObject list(LispObject[] obj) {
      LispObject theList = NIL;
      if (obj.length > 0)
      for (int i = obj.length - 1; i >= 0; i--)
          theList = new Cons(obj[i], theList);
      return theList;
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

  public static Symbol checkSymbol(LispObject obj)
  {             
          if (obj instanceof Symbol)      
                  return (Symbol) obj;         
          return (Symbol)// Not reached.       
              type_error(obj, Symbol.SYMBOL);
  }
  
  public static final LispObject checkList(LispObject obj)

  {
    if (obj.listp())
      return obj;
    return type_error(obj, Symbol.LIST);
  }

  public static final AbstractArray checkArray(LispObject obj)

  {
          if (obj instanceof AbstractArray)       
                  return (AbstractArray) obj;         
          return (AbstractArray)// Not reached.       
        type_error(obj, Symbol.ARRAY);
  }

  public static final AbstractVector checkVector(LispObject obj)

  {
          if (obj instanceof AbstractVector)      
                  return (AbstractVector) obj;         
          return (AbstractVector)// Not reached.       
        type_error(obj, Symbol.VECTOR);
  }

  public static final DoubleFloat checkDoubleFloat(LispObject obj)

  {
          if (obj instanceof DoubleFloat)
                  return (DoubleFloat) obj;
          return (DoubleFloat)// Not reached.
            type_error(obj, Symbol.DOUBLE_FLOAT);
  }

  public static final SingleFloat checkSingleFloat(LispObject obj)

  {
          if (obj instanceof SingleFloat)
                  return (SingleFloat) obj;
          return (SingleFloat)// Not reached.
            type_error(obj, Symbol.SINGLE_FLOAT);
  }

  public static final StackFrame checkStackFrame(LispObject obj)

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

  {
    return gensym("G", thread);
  }

  public static final Symbol gensym(String prefix, LispThread thread)

  {
    StringBuilder sb = new StringBuilder(prefix);
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
                                oldValue.princToString() + " New value: 0"));
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
                                    oldValue.princToString() + " New value: 0"));
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
  public static LispObject readObjectFromString(String s)
  {
      return readObjectFromReader(new StringReader(s));
  }
  
  final static Charset UTF8CHARSET = Charset.forName("UTF-8");
  public static LispObject readObjectFromStream(InputStream s)
  {
      return readObjectFromReader(new InputStreamReader(s));
  }
  
  public static LispObject readObjectFromReader(Reader r)
  {
    LispThread thread = LispThread.currentThread();
    SpecialBindingsMark mark = thread.markSpecialBindings();
    try {
        thread.bindSpecial(Symbol.READ_BASE, LispInteger.getInstance(10));
        thread.bindSpecial(Symbol.READ_EVAL, Symbol.T);
        thread.bindSpecial(Symbol.READ_SUPPRESS, Nil.NIL);
        // No need to bind read default float format: all floats are written
        // with their correct exponent markers due to the fact that DUMP-FORM
        // binds read-default-float-format to NIL

        // No need to bind the default read table, because the default fasl
        // read table is used below
        return new Stream(Symbol.SYSTEM_STREAM, r).read(true, NIL, false,
                                             LispThread.currentThread(),
                                             Stream.faslReadtable);
    }
    finally {
        thread.resetSpecialBindings(mark);
    }
  }
  
  @Deprecated
  public static final LispObject loadCompiledFunction(final String namestring)
  {
      Pathname name = new Pathname(namestring);
      byte[] bytes = readFunctionBytes(name);
      if (bytes != null)
        return loadClassBytes(bytes);

      return null;
  }

  public static byte[] readFunctionBytes(final Pathname name) {
      final LispThread thread = LispThread.currentThread();
      Pathname load = null;
      LispObject truenameFasl = Symbol.LOAD_TRUENAME_FASL.symbolValue(thread);
      LispObject truename = Symbol.LOAD_TRUENAME.symbolValue(thread);
      if (truenameFasl instanceof Pathname) {
          load = Pathname.mergePathnames(name, (Pathname)truenameFasl, Keyword.NEWEST);
      } else if (truename instanceof Pathname) {
          load = Pathname.mergePathnames(name, (Pathname) truename, Keyword.NEWEST);
      } else {
          if (!Pathname.truename(name).equals(NIL)) {
              load = name;
          } else {
              load = null;
          }
      }
      InputStream input = null;
      if (load != null) {
          input = load.getInputStream();
      } else { 
          // Make a last-ditch attempt to load from the boot classpath XXX OSGi hack
          URL url = null;
          try {
              url = Lisp.class.getResource(name.getNamestring());
              input = url.openStream();
          } catch (IOException e) {
	      System.err.println("Failed to read class bytes from boot class " + url);
              error(new LispError("Failed to read class bytes from boot class " + url));
          }
      }
      byte[] bytes = new byte[4096];
      try {
          if (input == null) {
                  Debug.trace("Pathname: " + name);
                  Debug.trace("load: " + load);
                  Debug.trace("LOAD_TRUENAME_FASL: " + truenameFasl);
                  Debug.trace("LOAD_TRUENAME: " + truename);
                  Debug.assertTrue(input != null);
          }

          int n = 0;
          java.io.ByteArrayOutputStream baos = new java.io.ByteArrayOutputStream();
          try {
              while (n >= 0) {
                  n = input.read(bytes, 0, 4096);
                if (n >= 0) {
                    baos.write(bytes, 0, n);
                }
            }
          } catch (IOException e) {
              Debug.trace("Failed to read bytes from "
                          + "'" + name.getNamestring() + "'");
              return null;
          }
          bytes = baos.toByteArray();
      } finally {
          try {
              input.close();
          } catch (IOException e) {
              Debug.trace("Failed to close InputStream: " + e);
          }
      }
      return bytes;
  }

    public static final Function makeCompiledFunctionFromClass(Class<?> c) {
      try {
	if (c != null) {
	    Function obj = (Function)c.newInstance();
	    return obj;
        } else {
            return null;
        }
      }
      catch (InstantiationException e) {} // ### FIXME
      catch (IllegalAccessException e) {} // ### FIXME

      return null;
    }


  public static final LispObject loadCompiledFunction(InputStream in, int size)
  {
      byte[] bytes = readFunctionBytes(in, size);
      if (bytes != null)
        return loadClassBytes(bytes);
      else
        return error(new FileError("Can't read file off stream."));
  }



  private static final byte[] readFunctionBytes(InputStream in, int size)
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

        return bytes;
      }
    catch (IOException t)
      {
        Debug.trace(t); // FIXME: call error()?
      }
    return null;
  }

    public static final Function loadClassBytes(byte[] bytes)
    {
    	return loadClassBytes(bytes, new JavaClassLoader());
    }

    public static final Function loadClassBytes(byte[] bytes,
                                                JavaClassLoader cl)
    {
        Class<?> c = cl.loadClassFromByteArray(null, bytes, 0, bytes.length);
	Function obj = makeCompiledFunctionFromClass(c);
	if (obj != null) {
	    obj.setClassBytes(bytes);
	}
	return obj;
    }


  public static final LispObject makeCompiledClosure(LispObject template,
                                                     ClosureBinding[] context)

  {
    return ((CompiledClosure)template).dup().setContext(context);
  }

  public static final String safeWriteToString(LispObject obj)
  {
    try {
        return obj.printObject();
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

  public static final boolean isValidMacroFunctionName(LispObject obj)
  {
    if (obj instanceof Cons)
      {
        Cons cons = (Cons) obj;
        if (cons.car == Symbol.MACRO_FUNCTION && cons.cdr instanceof Cons)
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

  {
          return (byte)Fixnum.getValue(obj);
  }

  public static final LispObject coerceJavaByteToLispObject(byte b)
  {
    return Fixnum.constants[((int)b) & 0xff];
  }

  public static final LispCharacter checkCharacter(LispObject obj)

  {
          if (obj instanceof LispCharacter) 
                  return (LispCharacter) obj;         
          return (LispCharacter) // Not reached.       
        type_error(obj, Symbol.CHARACTER);
  }

  public static final Package checkPackage(LispObject obj)

  {
          if (obj instanceof Package)     
                  return (Package) obj;         
          return (Package) // Not reached.       
        type_error(obj, Symbol.PACKAGE);
  }

  public static Pathname checkPathname(LispObject obj)
  {
          if (obj instanceof Pathname)     
                  return (Pathname) obj;         
          return (Pathname) // Not reached.       
        type_error(obj, Symbol.PATHNAME);
  }

  public static final Function checkFunction(LispObject obj)

  {
          if (obj instanceof Function)    
                  return (Function) obj;         
          return (Function) // Not reached.       
        type_error(obj, Symbol.FUNCTION);
  }

  public static final Stream checkStream(LispObject obj)

  {
      if (obj instanceof Stream)
                  return (Stream) obj;
          return (Stream) // Not reached.
        type_error(obj, Symbol.STREAM);
  }

  public static final Stream checkCharacterInputStream(LispObject obj)

  {
          final Stream stream = checkStream(obj);
          if (stream.isCharacterInputStream())      
                  return stream;                        
          return (Stream) // Not reached.                      
          error(new TypeError("The value " + obj.princToString() +
                        " is not a character input stream."));
  }

  public static final Stream checkCharacterOutputStream(LispObject obj)

  {
          final Stream stream = checkStream(obj);
          if (stream.isCharacterOutputStream())      
                  return stream;                        
        return (Stream) // Not reached.
        error(new TypeError("The value " + obj.princToString() +
                            " is not a character output stream."));
  }

  public static final Stream checkBinaryInputStream(LispObject obj)

  {
          final Stream stream = checkStream(obj);
          if (stream.isBinaryInputStream())      
                  return stream;                        
        return (Stream) // Not reached.
        error(new TypeError("The value " + obj.princToString() +
                             " is not a binary input stream."));
  }
  
  public static final Stream outSynonymOf(LispObject obj)

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

  {
    if (n < 0 || n > 255)
      type_error(Fixnum.getInstance(n), UNSIGNED_BYTE_8);
    checkStream(obj)._writeByte(n);
  }

  public static final Readtable checkReadtable(LispObject obj)

  {
          if (obj instanceof Readtable)   
                  return (Readtable) obj;         
          return (Readtable)// Not reached.       
          type_error(obj, Symbol.READTABLE);
  }
  
  public final static AbstractString checkString(LispObject obj) 

  {
          if (obj instanceof AbstractString)            
                  return (AbstractString) obj;                    
          return (AbstractString)// Not reached.               
              type_error(obj, Symbol.STRING);
  }
  
  public final static Layout checkLayout(LispObject obj) 

  {
          if (obj instanceof Layout)            
                  return (Layout) obj;                    
          return (Layout)// Not reached.               
                type_error(obj, Symbol.LAYOUT);
  }

  public static final Readtable designator_readtable(LispObject obj)

  {
    if (obj == NIL)
      obj = STANDARD_READTABLE.symbolValue();
    if (obj == null)
        throw new NullPointerException();
    return checkReadtable(obj);
  }

  public static final Environment checkEnvironment(LispObject obj)

  {
          if (obj instanceof Environment)         
                  return (Environment) obj;         
          return (Environment)// Not reached.       
        type_error(obj, Symbol.ENVIRONMENT);
  }

  public static final void checkBounds(int start, int end, int length)

  {
    if (start < 0 || end < 0 || start > end || end > length)
      {
        StringBuilder sb = new StringBuilder("The bounding indices ");
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
    if (obj instanceof Cons && obj.car() == Symbol.NAMED_LAMBDA) {
        LispObject name = obj.cadr();
        if (name instanceof Symbol || isValidSetfFunctionName(name)) {
            return new Closure(name,
                               new Cons(Symbol.LAMBDA, obj.cddr()),
                               new Environment());
        }
        return type_error(name, FUNCTION_NAME);
    }
    error(new UndefinedFunction(obj));
    // Not reached.
    return null;
  }

  // Returns package or throws exception.
  public static final Package coerceToPackage(LispObject obj)

  {
    if (obj instanceof Package)
      return (Package) obj;
    Package pkg = Packages.findPackage(javaString(obj));
    if (pkg != null)
      return pkg;
    error(new PackageError(obj.princToString() + " is not the name of a package."));
    // Not reached.
    return null;
  }

  public static Pathname coerceToPathname(LispObject arg)

  {
    if (arg instanceof Pathname)
      return (Pathname) arg;
    if (arg instanceof AbstractString)
      return Pathname.parseNamestring((AbstractString)arg);
    if (arg instanceof FileStream)
      return ((FileStream)arg).getPathname();
    if (arg instanceof JarStream)
      return ((JarStream)arg).getPathname();
    if (arg instanceof URLStream)
      return ((URLStream)arg).getPathname();
    type_error(arg, list(Symbol.OR, Symbol.PATHNAME,
                         Symbol.STRING, Symbol.FILE_STREAM,
                         Symbol.JAR_STREAM, Symbol.URL_STREAM));
    // Not reached.
    return null;
  }

  public static LispObject assq(LispObject item, LispObject alist)

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
                                      plist.princToString()));
      }
    return defaultValue;
  }

  public static final LispObject get(LispObject symbol, LispObject indicator)

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

  {
    LispObject list = checkList(symbol.getPropertyList());
    LispObject prev = null;
    while (list != NIL)
      {
        if (!(list.cdr() instanceof Cons))
          error(new ProgramError("The symbol " + symbol.princToString() +
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
                        final SpecialBindingsMark mark = thread.markSpecialBindings();
                        thread.bindSpecial(Symbol.PRINT_ESCAPE, NIL);
                        thread.bindSpecial(Symbol.PRINT_READABLY, NIL);
                        try {
                            sb.append(obj.printObject());
                        }
                        finally {
                            thread.resetSpecialBindings(mark);
                        }
                      }
                  }
                else if (c == 'S' || c == 's')
                  {
                    if (j < args.length)
                      {
                        LispObject obj = args[j++];
                        final SpecialBindingsMark mark = thread.markSpecialBindings();
                        thread.bindSpecial(Symbol.PRINT_ESCAPE, T);
                        try {
                            sb.append(obj.printObject());
                        }
                        finally {
                            thread.resetSpecialBindings(mark);
                        }
                      }
                  }
                else if (c == 'D' || c == 'd')
                  {
                    if (j < args.length)
                      {
                        LispObject obj = args[j++];
                        final SpecialBindingsMark mark = thread.markSpecialBindings();
                        thread.bindSpecial(Symbol.PRINT_ESCAPE, NIL);
                        thread.bindSpecial(Symbol.PRINT_RADIX, NIL);
                        thread.bindSpecial(Symbol.PRINT_BASE, Fixnum.constants[10]);
                        try {
                            sb.append(obj.printObject());
                        }
                        finally {
                            thread.resetSpecialBindings(mark);
                        }
                      }
                  }
                else if (c == 'X' || c == 'x')
                  {
                    if (j < args.length)
                      {
                        LispObject obj = args[j++];
                        final SpecialBindingsMark mark = thread.markSpecialBindings();
                        thread.bindSpecial(Symbol.PRINT_ESCAPE, NIL);
                        thread.bindSpecial(Symbol.PRINT_RADIX, NIL);
                        thread.bindSpecial(Symbol.PRINT_BASE, Fixnum.constants[16]);
                        try {
                            sb.append(obj.printObject());
                        }
                        finally {
                            thread.resetSpecialBindings(mark);
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
  static final ConcurrentHashMap<String,LispObject> objectTable =
          new ConcurrentHashMap<String,LispObject>();

  public static LispObject recall(String key)
  {
    return objectTable.remove(key);
  }

  public static LispObject recall(SimpleString key)
  {
    return objectTable.remove(key.getStringValue());
  }

  // ### remember
  public static final Primitive REMEMBER =
    new Primitive("remember", PACKAGE_SYS, true)
    {
      @Override
      public LispObject execute(LispObject key, LispObject value)

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
    pkg.export(symbol); // FIXME Inefficient!
    symbol.setSpecial(true);
    symbol.setSymbolValue(value);
    return symbol;
  }

  public static final Symbol exportConstant(String name, Package pkg,
                                            LispObject value)
  {
    Symbol symbol = pkg.intern(name);
    pkg.export(symbol); // FIXME Inefficient!
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
    resetIO(new Stream(Symbol.SYSTEM_STREAM, System.in, Symbol.CHARACTER, true),
            new Stream(Symbol.SYSTEM_STREAM, System.out, Symbol.CHARACTER, true));
  }

  public static final TwoWayStream getTerminalIO()
  {
    return (TwoWayStream) Symbol.TERMINAL_IO.symbolValueNoThrow();
  }

  public static final Stream getStandardInput()
  {
    return (Stream) Symbol.STANDARD_INPUT.symbolValueNoThrow();
  }

  public static final Stream getStandardOutput()
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

  public static final Readtable currentReadtable()
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
    final String osName = System.getProperty("os.name");
    final String javaVersion = System.getProperty("java.version");
    final String osArch = System.getProperty("os.arch");

    // Common features
    LispObject featureList = list(Keyword.ARMEDBEAR, Keyword.ABCL,
                                  Keyword.COMMON_LISP, Keyword.ANSI_CL,
                                  Keyword.CDR6, Keyword.MOP);
    // OS type
    if (osName.startsWith("Linux"))
      featureList = Primitives.APPEND.execute(list(Keyword.UNIX,
                                                  Keyword.LINUX),
                                             featureList);
    else if (osName.startsWith("SunOS"))
      featureList = Primitives.APPEND.execute(list(Keyword.UNIX,
                                                   Keyword.SUNOS,
                                                   Keyword.SOLARIS),
                                              featureList);
    else if (osName.startsWith("Mac OS X") ||
             osName.startsWith("Darwin"))
      featureList = Primitives.APPEND.execute(list(Keyword.UNIX,
                                                   Keyword.DARWIN),
                                              featureList);
    else if (osName.startsWith("FreeBSD"))
      featureList = Primitives.APPEND.execute(list(Keyword.UNIX,
                                                   Keyword.FREEBSD),
                                              featureList);
    else if (osName.startsWith("OpenBSD"))
      featureList = Primitives.APPEND.execute(list(Keyword.UNIX,
                                                   Keyword.OPENBSD),
                                              featureList);
    else if (osName.startsWith("NetBSD"))
      featureList = Primitives.APPEND.execute(list(Keyword.UNIX,
                                                   Keyword.NETBSD),
                                              featureList);
    else if (osName.startsWith("Windows"))
      featureList = new Cons(Keyword.WINDOWS, featureList);
    // Java version
    if (javaVersion.startsWith("1.5")) {
        featureList = new Cons(Keyword.JAVA_1_5, featureList);
    } else if (javaVersion.startsWith("1.6")) {
        featureList = new Cons(Keyword.JAVA_1_6, featureList);
    } else if (javaVersion.startsWith("1.7")) {
        featureList = new Cons(Keyword.JAVA_1_7, featureList);
    }
    // Processor architecture
    if(osArch != null) {
      if (osArch.equals("amd64") || osArch.equals("x86_64"))
        featureList = new Cons(Keyword.X86_64, featureList);
      else if (osArch.equals("x86") || osArch.equals("i386"))
        featureList = new Cons(Keyword.X86, featureList);
    }
    Symbol.FEATURES.initializeSpecial(featureList);
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
    Symbol.LOAD_TRUENAME_FASL.initializeSpecial(NIL);
    Symbol.COMPILE_VERBOSE.initializeSpecial(T);
    Symbol.COMPILE_PRINT.initializeSpecial(T);
    Symbol._COMPILE_FILE_PATHNAME_.initializeSpecial(NIL);
    Symbol.COMPILE_FILE_TRUENAME.initializeSpecial(NIL);
  }

  // ### *double-colon-package-separators*
  // internal symbol
  public static final Symbol DOUBLE_COLON_PACKAGE_SEPARATORS =
    internSpecial("*DOUBLE-COLON-PACKAGE-SEPARATORS*", PACKAGE_SYS, NIL);

  // ### *load-depth*
  // internal symbol
  public static final Symbol _LOAD_DEPTH_ =
    internSpecial("*LOAD-DEPTH*", PACKAGE_SYS, Fixnum.ZERO);

  // ### *load-stream*
  // internal symbol
  public static final Symbol _LOAD_STREAM_ =
    internSpecial("*LOAD-STREAM*", PACKAGE_SYS, NIL);

    // ### *fasl-loader*
    public static final Symbol _FASL_LOADER_ =
	exportSpecial("*FASL-LOADER*", PACKAGE_SYS, NIL);

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

  // ### *preloading-cache*
 public static final Symbol AUTOLOADING_CACHE =
   internSpecial("*AUTOLOADING-CACHE*", PACKAGE_SYS, NIL);

  // ### *compile-file-type*
  public static final Symbol _COMPILE_FILE_TYPE_ =
   exportSpecial("*COMPILE-FILE-TYPE*", PACKAGE_SYS, new SimpleString("abcl"));    
  
  // ### *compile-file-class-extension*
  public static final Symbol _COMPILE_FILE_CLASS_EXTENSION_ =
   exportSpecial("*COMPILE-FILE-CLASS-EXTENSION*", PACKAGE_SYS, new SimpleString("cls"));

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
  public static final int CHAR_MAX = Character.MAX_VALUE;
  static
  {
    Symbol.CHAR_CODE_LIMIT.initializeConstant(Fixnum.getInstance(CHAR_MAX + 1));
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
    Symbol.MULTIPLE_VALUES_LIMIT.initializeConstant(Fixnum.constants[32]);
  }

  static
  {
    // ### internal-time-units-per-second
    Symbol.INTERNAL_TIME_UNITS_PER_SECOND.initializeConstant(Fixnum.getInstance(1000));
  }

  static
  {
    Symbol.LAMBDA_LIST_KEYWORDS
      .initializeConstant(list(Symbol.AND_OPTIONAL,
                               Symbol.AND_REST,
                               Symbol.AND_KEY,
                               Symbol.AND_AUX,
                               Symbol.AND_BODY,
                               Symbol.AND_WHOLE,
                               Symbol.AND_ALLOW_OTHER_KEYS,
                               Symbol.AND_ENVIRONMENT));
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

  // ### *command-line-argument-list*
  public static final Symbol _COMMAND_LINE_ARGUMENT_LIST_ =
    exportSpecial("*COMMAND-LINE-ARGUMENT-LIST*", PACKAGE_EXT, NIL);

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

  public static final LispObject UNBOUND_VALUE = new unboundValue();
  static class unboundValue extends LispObject
  {
    @Override
    public String printObject()
    {
      return unreadableString("UNBOUND", false);
    }
  }

  public static final LispObject NULL_VALUE = new nullValue();
  static class nullValue extends LispObject
  {
    @Override
    public String printObject()
    {
      return unreadableString("null", false);
    }
  }

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
        Debug.trace(e);
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
    loadClass("org.armedbear.lisp.AutoloadGeneralizedReference");
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
    loadClass("org.armedbear.lisp.PackageFunctions");
    cold = false;
  }

    private static Stream stdin = new Stream(Symbol.SYSTEM_STREAM, System.in, Symbol.CHARACTER, true);

    private static Stream stdout = new Stream(Symbol.SYSTEM_STREAM,System.out, Symbol.CHARACTER, true);

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

  private static final SpecialOperator WITH_INLINE_CODE = new with_inline_code();
  private static class with_inline_code extends SpecialOperator {
    with_inline_code() {
      super("with-inline-code", PACKAGE_JVM, true, "(&optional target repr) &body body");
    }
    @Override
    public LispObject execute(LispObject args, Environment env)
    {
	return error(new SimpleError("This is a placeholder. It should only be called in compiled code, and tranformed by the compiler using special form handlers."));
    }
  }

}
