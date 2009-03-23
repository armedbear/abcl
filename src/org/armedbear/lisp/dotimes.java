/*
 * dotimes.java
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

public final class dotimes extends SpecialOperator
{
  private dotimes()
  {
    super(Symbol.DOTIMES);
  }

  @Override
  public LispObject execute(LispObject args, Environment env)
    throws ConditionThrowable
  {
    LispObject bodyForm = args.cdr();
    args = args.car();
    Symbol var = checkSymbol(args.car());
    LispObject countForm = args.cadr();
    final LispThread thread = LispThread.currentThread();
    LispObject resultForm = args.cdr().cdr().car();
    SpecialBinding lastSpecialBinding = thread.lastSpecialBinding;
    final LispObject stack = thread.getStack();
    // Process declarations.
    LispObject specials = NIL;
    while (bodyForm != NIL)
      {
        LispObject obj = bodyForm.car();
        if (obj instanceof Cons && obj.car() == Symbol.DECLARE)
          {
            LispObject decls = obj.cdr();
            while (decls != NIL)
              {
                LispObject decl = decls.car();
                if (decl instanceof Cons && decl.car() == Symbol.SPECIAL)
                  {
                    LispObject vars = decl.cdr();
                    while (vars != NIL)
                      {
                        specials = new Cons(vars.car(), specials);
                        vars = vars.cdr();
                      }
                  }
                decls = decls.cdr();
              }
            bodyForm = bodyForm.cdr();
          }
        else
          break;
      }
    try
      {
        LispObject limit = eval(countForm, env, thread);
        Environment ext = new Environment(env);
        LispObject localTags = NIL; // Tags that are local to this TAGBODY.
        // Look for tags.
        LispObject remaining = bodyForm;
        while (remaining != NIL)
          {
            LispObject current = remaining.car();
            remaining = remaining.cdr();
            if (current instanceof Cons)
              continue;
            // It's a tag.
            ext.addTagBinding(current, remaining);
            localTags = new Cons(current, localTags);
          }
        // Implicit block.
        ext.addBlock(NIL, new LispObject());
        LispObject result;
        // Establish a reusable binding.
        final Object binding;
        if (specials != NIL && memq(var, specials))
          {
            thread.bindSpecial(var, null);
            binding = thread.getSpecialBinding(var);
            ext.declareSpecial(var);
          }
        else if (var.isSpecialVariable())
          {
            thread.bindSpecial(var, null);
            binding = thread.getSpecialBinding(var);
          }
        else
          {
            ext.bind(var, null);
            binding = ext.getBinding(var);
          }
        while (specials != NIL)
          {
            ext.declareSpecial(checkSymbol(specials.car()));
            specials = specials.cdr();
          }
        if (limit instanceof Fixnum)
          {
            int count = ((Fixnum)limit).value;
            int i;
            for (i = 0; i < count; i++)
              {
                if (binding instanceof SpecialBinding)
                  ((SpecialBinding)binding).value = Fixnum.getInstance(i);
                else
                  ((Binding)binding).value = Fixnum.getInstance(i);
                LispObject body = bodyForm;
                while (body != NIL)
                  {
                    LispObject current = body.car();
                    if (current instanceof Cons)
                      {
                        try
                          {
                            // Handle GO inline if possible.
                            if (current.car() == Symbol.GO)
                              {
                                LispObject tag = current.cadr();
                                if (memql(tag, localTags))
                                  {
                                    Binding b = ext.getTagBinding(tag);
                                    if (b != null && b.value != null)
                                      {
                                        body = b.value;
                                        continue;
                                      }
                                  }
                                throw new Go(tag);
                              }
                            eval(current, ext, thread);
                          }
                        catch (Go go)
                          {
                            LispObject tag = go.getTag();
                            if (memql(tag, localTags))
                              {
                                Binding b = ext.getTagBinding(tag);
                                if (b != null && b.value != null)
                                  {
                                    body = b.value;
                                    thread.setStack(stack);
                                    continue;
                                  }
                              }
                            throw go;
                          }
                      }
                    body = body.cdr();
                  }
                if (interrupted)
                  handleInterrupt();
              }
            if (binding instanceof SpecialBinding)
              ((SpecialBinding)binding).value = Fixnum.getInstance(i);
            else
              ((Binding)binding).value = Fixnum.getInstance(i);
            result = eval(resultForm, ext, thread);
          }
        else if (limit instanceof Bignum)
          {
            LispObject i = Fixnum.ZERO;
            while (i.isLessThan(limit))
              {
                if (binding instanceof SpecialBinding)
                  ((SpecialBinding)binding).value = i;
                else
                  ((Binding)binding).value = i;
                LispObject body = bodyForm;
                while (body != NIL)
                  {
                    LispObject current = body.car();
                    if (current instanceof Cons)
                      {
                        try
                          {
                            // Handle GO inline if possible.
                            if (current.car() == Symbol.GO)
                              {
                                LispObject tag = current.cadr();
                                if (memql(tag, localTags))
                                  {
                                    Binding b = ext.getTagBinding(tag);
                                    if (b != null && b.value != null)
                                      {
                                        body = b.value;
                                        continue;
                                      }
                                  }
                                throw new Go(tag);
                              }
                            eval(current, ext, thread);
                          }
                        catch (Go go)
                          {
                            LispObject tag = go.getTag();
                            if (memql(tag, localTags))
                              {
                                Binding b = ext.getTagBinding(tag);
                                if (b != null && b.value != null)
                                  {
                                    body = b.value;
                                    thread.setStack(stack);
                                    continue;
                                  }
                              }
                            throw go;
                          }
                      }
                    body = body.cdr();
                  }
                i = i.incr();
                if (interrupted)
                  handleInterrupt();
              }
            if (binding instanceof SpecialBinding)
              ((SpecialBinding)binding).value = i;
            else
              ((Binding)binding).value = i;
            result = eval(resultForm, ext, thread);
          }
        else
          return error(new TypeError(limit, Symbol.INTEGER));
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

  private static final dotimes DOTIMES = new dotimes();
}
