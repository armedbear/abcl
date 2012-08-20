/*
 * Closure.java
 *
 * Copyright (C) 2002-2008 Peter Graves
 * Copyright (C) 2008 Ville Voutilainen
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

import static org.armedbear.lisp.Lisp.*;

public class Closure extends Function
{
  // Parameter types.
  public static final int REQUIRED = 0;
  public static final int OPTIONAL = 1;
  public static final int KEYWORD  = 2;
  public static final int REST     = 3;
  public static final int AUX      = 4;

  private final LispObject body;
  private final LispObject executionBody;
  private final Environment environment;

  private final Symbol[] freeSpecials;
  private final ArgumentListProcessor arglist;

    /** Construct a closure object with a lambda-list described
     * by these parameters.
     *
     *
     * @param required Required parameters or an empty array for none
     * @param optional Optional parameters or an empty array for none
     * @param keyword Keyword parameters or an empty array for none
     * @param keys NIL if the lambda-list doesn't contain &amp;key, T otherwise
     * @param rest the &amp;rest parameter, or NIL if none
     * @param moreKeys NIL if &amp;allow-other-keys not present, T otherwise
     */
  public Closure(ArgumentListProcessor arglist) {
      // stuff we don't need: we're a compiled function
      body = null;
      executionBody = null;
      environment = null;
      this.arglist = arglist;
      freeSpecials = new Symbol[0];
  }


  public Closure(LispObject lambdaExpression, Environment env)
  {
    this(null, lambdaExpression, env);
  }

  public Closure(final LispObject name, final LispObject lambdaExpression,
                 final Environment env)

  {
    super(name, lambdaExpression.cadr());
    final LispObject lambdaList = lambdaExpression.cadr();
    setLambdaList(lambdaList);
    if (!(lambdaList == NIL || lambdaList instanceof Cons))
      error(new ProgramError("The lambda list " + lambdaList.princToString() +
                           " is invalid."));
    this.body = lambdaExpression.cddr();
    LispObject bodyAndDecls = parseBody(this.body, false);
    this.executionBody = bodyAndDecls.car();
    LispObject specials = parseSpecials(bodyAndDecls.NTH(1));

    this.environment = env;

    arglist = new ArgumentListProcessor(this, lambdaList, specials);
    freeSpecials = arglist.freeSpecials(specials);
  }

  @Override
  public LispObject typep(LispObject typeSpecifier)
  {
    if (typeSpecifier == Symbol.COMPILED_FUNCTION)
      return NIL;
    return super.typep(typeSpecifier);
  }

  public final LispObject getVariableList()
  {
    Symbol[] variables = arglist.getVariables();
    LispObject result = NIL;
    for (int i = variables.length; i-- > 0;)
      result = new Cons(variables[i], result);
    return result;
  }

  // Returns body as a list.
  public final LispObject getBody()
  {
    return body;
  }

  public final Environment getEnvironment()
  {
    return environment;
  }

  @Override
  public LispObject execute()
  {
      return execute(new LispObject[0]);
  }
    
  @Override
  public LispObject execute(LispObject arg)
  {
        return execute(new LispObject[] {arg});
  }

  @Override
  public LispObject execute(LispObject first, LispObject second)
  {
        return execute(new LispObject[] {first, second});
  }

  @Override
  public LispObject execute(LispObject first, LispObject second,
                            LispObject third)
  {
        return execute(new LispObject[] {first, second, third});
  }

  @Override
  public LispObject execute(LispObject first, LispObject second,
                            LispObject third, LispObject fourth)
  {
        return execute(new LispObject[] {first, second, third, fourth});
  }

  @Override
  public LispObject execute(LispObject first, LispObject second,
                            LispObject third, LispObject fourth,
                            LispObject fifth)
  {
        return execute(new LispObject[] {first, second, third, fourth, fifth});
  }

  @Override
  public LispObject execute(LispObject first, LispObject second,
                            LispObject third, LispObject fourth,
                            LispObject fifth, LispObject sixth)
  {
        return execute(new LispObject[] {first, second, third, fourth, fifth,
                                  sixth});
  }

  @Override
  public LispObject execute(LispObject first, LispObject second,
                            LispObject third, LispObject fourth,
                            LispObject fifth, LispObject sixth,
                            LispObject seventh)
  {
        return execute(new LispObject[] {first, second, third, fourth, fifth,
                                  sixth, seventh});
  }

  @Override
  public LispObject execute(LispObject first, LispObject second,
                            LispObject third, LispObject fourth,
                            LispObject fifth, LispObject sixth,
                            LispObject seventh, LispObject eighth)
  {
        return execute(new LispObject[] {first, second, third, fourth, fifth,
                                  sixth, seventh, eighth});
  }

  @Override
  public LispObject execute(LispObject[] args)
  {
    final LispThread thread = LispThread.currentThread();
    final SpecialBindingsMark mark = thread.markSpecialBindings();
    Environment ext = new Environment(environment);
    args = arglist.match(args, environment, ext, thread);
    arglist.bindVars(args, ext, thread);
    for (Symbol special : freeSpecials)
      ext.declareSpecial(special);
    try
      {
        return progn(executionBody, ext, thread);
      }
    finally
      {
        thread.resetSpecialBindings(mark);
      }
  }

  protected final LispObject[] processArgs(LispObject[] args, LispThread thread)
  {
    return arglist.match(args, environment, environment, thread);
  }
}
