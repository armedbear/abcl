/*
 * Do.java
 *
 * Copyright (C) 2003-2006 Peter Graves
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

public final class Do extends Lisp
{
  // ### do
  private static final SpecialOperator DO =
    new SpecialOperator(Symbol.DO, "varlist endlist &body body")
    {
      public LispObject execute(LispObject args, Environment env)
        throws ConditionThrowable
      {
        return _do(args, env, false);
      }
    };

  // ### do*
  private static final SpecialOperator DO_STAR =
    new SpecialOperator(Symbol.DO_STAR, "varlist endlist &body body")
    {
      public LispObject execute(LispObject args, Environment env)
        throws ConditionThrowable
      {
        return _do(args, env, true);
      }
    };

  private static final LispObject _do(LispObject args, Environment env,
                                      boolean sequential)
    throws ConditionThrowable
  {
    LispObject varlist = args.car();
    LispObject second = args.cadr();
    LispObject end_test_form = second.car();
    LispObject result_forms = second.cdr();
    LispObject body = args.cddr();
    // Process variable specifications.
    final int numvars = varlist.length();
    Symbol[] vars = new Symbol[numvars];
    LispObject[] initforms = new LispObject[numvars];
    LispObject[] stepforms = new LispObject[numvars];
    for (int i = 0; i < numvars; i++)
      {
        final LispObject varspec = varlist.car();
        if (varspec instanceof Cons)
          {
            vars[i] = checkSymbol(varspec.car());
            initforms[i] = varspec.cadr();
            // Is there a step form?
            if (varspec.cddr() != NIL)
              stepforms[i] = varspec.caddr();
          }
        else
          {
            // Not a cons, must be a symbol.
            vars[i] = checkSymbol(varspec);
            initforms[i] = NIL;
          }
        varlist = varlist.cdr();
      }
    final LispThread thread = LispThread.currentThread();
    final LispObject stack = thread.getStack();
    final SpecialBinding lastSpecialBinding = thread.lastSpecialBinding;
    // Process declarations.
    LispObject specials = NIL;
    while (body != NIL)
      {
        LispObject obj = body.car();
        if (obj instanceof Cons && obj.car() == Symbol.DECLARE)
          {
            LispObject decls = obj.cdr();
            while (decls != NIL)
              {
                LispObject decl = decls.car();
                if (decl instanceof Cons && decl.car() == Symbol.SPECIAL)
                  {
                    LispObject names = decl.cdr();
                    while (names != NIL)
                      {
                        specials = new Cons(names.car(), specials);
                        names = names.cdr();
                      }
                  }
                decls = decls.cdr();
              }
            body = body.cdr();
          }
        else
          break;
      }
    final Environment ext = new Environment(env);
    for (int i = 0; i < numvars; i++)
      {
        Symbol var = vars[i];
        LispObject value = eval(initforms[i], (sequential ? ext : env), thread);
        if (specials != NIL && memq(var, specials))
            thread.bindSpecial(var, value);
        else if (var.isSpecialVariable())
          thread.bindSpecial(var, value);
        else
          ext.bind(var, value);
      }
    LispObject list = specials;
    while (list != NIL)
      {
        ext.declareSpecial(checkSymbol(list.car()));
        list = list.cdr();
      }
    // Look for tags.
    LispObject remaining = body;
    while (remaining != NIL)
      {
        LispObject current = remaining.car();
        remaining = remaining.cdr();
        if (current instanceof Cons)
          continue;
        // It's a tag.
        ext.addTagBinding(current, remaining);
      }
    try
      {
        // Implicit block.
        ext.addBlock(NIL, new LispObject());
        while (true)
          {
            // Execute body.
            // Test for termination.
            if (eval(end_test_form, ext, thread) != NIL)
              break;
            remaining = body;
            while (remaining != NIL)
              {
                LispObject current = remaining.car();
                if (current instanceof Cons)
                  {
                    try
                      {
                        // Handle GO inline if possible.
                        if (current.car() == Symbol.GO)
                          {
                            LispObject tag = current.cadr();
                            Binding binding = ext.getTagBinding(tag);
                            if (binding != null && binding.value != null)
                              {
                                remaining = binding.value;
                                continue;
                              }
                            throw new Go(tag);
                          }
                        eval(current, ext, thread);
                      }
                    catch (Go go)
                      {
                        LispObject tag = go.getTag();
                        Binding binding = ext.getTagBinding(tag);
                        if (binding != null && binding.value != null)
                          {
                            remaining = binding.value;
                            thread.setStack(stack);
                            continue;
                          }
                        throw go;
                      }
                  }
                remaining = remaining.cdr();
              }
            // Update variables.
            if (sequential)
              {
                for (int i = 0; i < numvars; i++)
                  {
                    LispObject step = stepforms[i];
                    if (step != null)
                      {
                        Symbol symbol = vars[i];
                        LispObject value = eval(step, ext, thread);
                        if (symbol.isSpecialVariable()
                            || ext.isDeclaredSpecial(symbol))
                          thread.rebindSpecial(symbol, value);
                        else
                          ext.rebind(symbol, value);
                      }
                  }
              }
            else
              {
                // Evaluate step forms.
                LispObject results[] = new LispObject[numvars];
                for (int i = 0; i < numvars; i++)
                  {
                    LispObject step = stepforms[i];
                    if (step != null)
                      {
                        LispObject result = eval(step, ext, thread);
                        results[i] = result;
                      }
                  }
                // Update variables.
                for (int i = 0; i < numvars; i++)
                  {
                    if (results[i] != null)
                      {
                        Symbol symbol = vars[i];
                        LispObject value = results[i];
                        if (symbol.isSpecialVariable()
                            || ext.isDeclaredSpecial(symbol))
                          thread.rebindSpecial(symbol, value);
                        else
                          ext.rebind(symbol, value);
                      }
                  }
              }
            if (interrupted)
              handleInterrupt();
          }
        LispObject result = progn(result_forms, ext, thread);
        return result;
      }
    catch (Return ret)
      {
        if (ret.getTag() == NIL)
          {
            thread.setStack(stack);
            return ret.getResult();
          }
        throw ret;
      }
    finally
      {
        thread.lastSpecialBinding = lastSpecialBinding;
      }
  }
}
