/*
 * ClosureTemplateFunction.java
 *
 * Copyright (C) 2004-2005 Peter Graves
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

public class ClosureTemplateFunction extends Closure
        implements Cloneable
{

  public LispObject[] ctx;

  public ClosureTemplateFunction(LispObject lambdaList)
    throws ConditionThrowable
  {
    super(list2(Symbol.LAMBDA, lambdaList), null);
  }

  final public ClosureTemplateFunction setContext(LispObject[] context)
  {
    ctx = context;
    return this;
  }

  final public ClosureTemplateFunction dup()
  {
      ClosureTemplateFunction result = null;
      try {
	  result = (ClosureTemplateFunction)super.clone();
      } catch (CloneNotSupportedException e) {
      }
      return result;
  }



    // execute methods have the semantic meaning
    // "evaluate this object"
  @Override
  public final LispObject execute() throws ConditionThrowable
  {
      return _execute(ctx);
  }

  @Override
  public final LispObject execute(LispObject arg) throws ConditionThrowable
  {
      return _execute(ctx, arg);
  }

  @Override
  public final LispObject execute(LispObject first, LispObject second)
    throws ConditionThrowable
  {
      return _execute(ctx, first, second);
  }

  @Override
  public final LispObject execute(LispObject first, LispObject second,
                                  LispObject third)
    throws ConditionThrowable
  {
      return _execute(ctx, first, second, third);
  }

  @Override
  public final LispObject execute(LispObject first, LispObject second,
                                  LispObject third, LispObject fourth)
    throws ConditionThrowable
  {
      return _execute(ctx, first, second, third, fourth);
  }

  @Override
  public final LispObject execute(LispObject first, LispObject second,
                                  LispObject third, LispObject fourth,
                                  LispObject fifth)
    throws ConditionThrowable
  {
      return _execute(ctx, first, second, third, fourth, fifth);
  }

  @Override
  public final LispObject execute(LispObject first, LispObject second,
                                  LispObject third, LispObject fourth,
                                  LispObject fifth, LispObject sixth)
    throws ConditionThrowable
  {
      return _execute(ctx, first, second, third, fourth, fifth, sixth);
  }

  @Override
  public final LispObject execute(LispObject first, LispObject second,
                                  LispObject third, LispObject fourth,
                                  LispObject fifth, LispObject sixth,
                                  LispObject seventh)
    throws ConditionThrowable
  {
      return _execute(ctx, first, second, third, fourth, fifth, sixth, seventh);
  }

  @Override
  public final LispObject execute(LispObject first, LispObject second,
                                  LispObject third, LispObject fourth,
                                  LispObject fifth, LispObject sixth,
                                  LispObject seventh, LispObject eighth)
    throws ConditionThrowable
  {
      return _execute(ctx, first, second, third, fourth, fifth,
              sixth, seventh, eighth);
  }

  @Override
  public final LispObject execute(LispObject[] args)
    throws ConditionThrowable
  {
    return _execute(ctx, args);
  }

  private final LispObject notImplemented() throws ConditionThrowable
  {
    return error(new WrongNumberOfArgumentsException(this));
  }


    // _execute methods have the semantic meaning
    // "evaluate this template with these values"

  // Zero args.
  public LispObject _execute(LispObject[] context) throws ConditionThrowable
  {
    LispObject[] args = new LispObject[0];
    return _execute(context, args);
  }

  // One arg.
  public LispObject _execute(LispObject[] context, LispObject first)
    throws ConditionThrowable
  {
    LispObject[] args = new LispObject[1];
    args[0] = first;
    return _execute(context, args);
  }

  // Two args.
  public LispObject _execute(LispObject[] context, LispObject first,
                            LispObject second)
    throws ConditionThrowable
  {
    LispObject[] args = new LispObject[2];
    args[0] = first;
    args[1] = second;
    return _execute(context, args);
  }

  // Three args.
  public LispObject _execute(LispObject[] context, LispObject first,
                            LispObject second, LispObject third)
    throws ConditionThrowable
  {
    LispObject[] args = new LispObject[3];
    args[0] = first;
    args[1] = second;
    args[2] = third;
    return _execute(context, args);
  }

  // Four args.
  public LispObject _execute(LispObject[] context, LispObject first,
                            LispObject second, LispObject third,
                            LispObject fourth)
    throws ConditionThrowable
  {
    LispObject[] args = new LispObject[4];
    args[0] = first;
    args[1] = second;
    args[2] = third;
    args[3] = fourth;
    return _execute(context, args);
  }

  // Five args.
  public LispObject _execute(LispObject[] context, LispObject first,
                            LispObject second, LispObject third,
                            LispObject fourth, LispObject fifth)
    throws ConditionThrowable
  {
    LispObject[] args = new LispObject[5];
    args[0] = first;
    args[1] = second;
    args[2] = third;
    args[3] = fourth;
    args[4] = fifth;
    return _execute(context, args);
  }

  // Six args.
  public LispObject _execute(LispObject[] context, LispObject first,
                            LispObject second, LispObject third,
                            LispObject fourth, LispObject fifth,
                            LispObject sixth)
    throws ConditionThrowable
  {
    LispObject[] args = new LispObject[6];
    args[0] = first;
    args[1] = second;
    args[2] = third;
    args[3] = fourth;
    args[4] = fifth;
    args[5] = sixth;
    return _execute(context, args);
  }

  // Seven args.
  public LispObject _execute(LispObject[] context, LispObject first,
                            LispObject second, LispObject third,
                            LispObject fourth, LispObject fifth,
                            LispObject sixth, LispObject seventh)
    throws ConditionThrowable
  {
    LispObject[] args = new LispObject[7];
    args[0] = first;
    args[1] = second;
    args[2] = third;
    args[3] = fourth;
    args[4] = fifth;
    args[5] = sixth;
    args[6] = seventh;
    return _execute(context, args);
  }

  // Eight args.
  public LispObject _execute(LispObject[] context, LispObject first,
                            LispObject second, LispObject third,
                            LispObject fourth, LispObject fifth,
                            LispObject sixth, LispObject seventh,
                            LispObject eighth)
    throws ConditionThrowable
  {
    LispObject[] args = new LispObject[8];
    args[0] = first;
    args[1] = second;
    args[2] = third;
    args[3] = fourth;
    args[4] = fifth;
    args[5] = sixth;
    args[6] = seventh;
    args[7] = eighth;
    return _execute(context, args);
  }

  // Arg array.
  public LispObject _execute(LispObject[] context, LispObject[] args)
    throws ConditionThrowable
  {
    return notImplemented();
  }
}
