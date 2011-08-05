/*
 * LispStackFrame.java
 *
 * Copyright (C) 2009 Mark Evenson
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

public class LispStackFrame 
  extends StackFrame
{
  public final LispObject operator;
  private final LispObject first;
  private final LispObject second;
  private final LispObject third;
  private final LispObject[] args;

  private final class UnavailableArgument extends LispObject 
  {
    public UnavailableArgument () { }
    @Override
    public String printObject() { 
      return unreadableString("unavailable arg", false); 
    }
  }

  private final LispObject UNAVAILABLE_ARG = new UnavailableArgument();

  public LispStackFrame(LispObject operator)
  {
    this.operator = operator;
    first = null;
    second = null;
    third = null;
    args = null;
  }

  public LispStackFrame(LispObject operator, LispObject arg)
  {
    this.operator = operator;
    first = arg;
    second = null;
    third = null;
    args = null;
  }

  public LispStackFrame(LispObject operator, LispObject first,
			LispObject second)
  {
    this.operator = operator;
    this.first = first;
    this.second = second;
    third = null;
    args = null;
  }

  public LispStackFrame(LispObject operator, LispObject first,
			LispObject second, LispObject third)

  {
    this.operator = operator;
    this.first = first;
    this.second = second;
    this.third = third;
    args = null;
  }

  public LispStackFrame(LispObject operator, LispObject... args)
  {
    this.operator = operator;
    first = null;
    second = null;
    third = null;
    this.args = args;
  }

   @Override
   public LispObject typeOf() { 
     return Symbol.LISP_STACK_FRAME; 
   }
  
   @Override
   public LispObject classOf() { 
     return BuiltInClass.LISP_STACK_FRAME; 
   }

   @Override
   public String printObject() 
   { 
     String result = "";
     final String LISP_STACK_FRAME = "LISP-STACK-FRAME";
     try {
	 result = Symbol.PRIN1_TO_STRING.execute(this.toLispList()).printObject();
     } catch (Throwable t) { // error while printing stack
       Debug.trace("Serious printing error: ");
       Debug.trace(t);
       result = unreadableString(LISP_STACK_FRAME);
     }
     return result;
   }

  @Override
  public LispObject typep(LispObject typeSpecifier) 

  {
    if (typeSpecifier == Symbol.LISP_STACK_FRAME)
      return T;
    if (typeSpecifier == BuiltInClass.LISP_STACK_FRAME)
      return T;
    return super.typep(typeSpecifier);
   }

  public LispObject toLispList() 

  {
    LispObject result = argsToLispList();
    if (operator instanceof Operator) {
      LispObject lambdaName = ((Operator)operator).getLambdaName();
      if (lambdaName != null && lambdaName != Lisp.NIL)
	return result.push(lambdaName);
    }
    return result.push(operator);
  }

  private LispObject argsToLispList() 

  {
    LispObject result = Lisp.NIL;
    if (args != null) {
      for (int i = 0; i < args.length; i++)
        // `args' come here from LispThread.execute. I don't know
        // how it comes that some callers pass NULL ptrs around but
        // we better do not create conses with their CAR being NULL;
        // it'll horribly break printing such a cons; and probably
        // other bad things may happen, too. --TCR, 2009-09-17.
        if (args[i] == null)
          result = result.push(UNAVAILABLE_ARG);
        else
          result = result.push(args[i]);
    } else {
      do {
	if (first != null)
	  result = result.push(first);
	else
	  break;
	if (second != null)
	  result = result.push(second);
	else
	  break;
	if (third != null)
	  result = result.push(third);
	else
	  break;
      } while (false);
    }
    return result.nreverse();
  }

  public SimpleString toLispString() 

  {
    String result;
    try {
      result = this.toLispList().printObject();
    } catch (Throwable t) { // error while printing stack
      Debug.trace("Serious printing error: ");
      Debug.trace(t);
      result = unreadableString("LISP-STACK-FRAME");
    }
    return new SimpleString(result);
  }

  public LispObject getOperator() {
    return operator;
  }

  @Override 
  public LispObject getParts() 

  {
    LispObject result = NIL;
    result = result.push(new Cons("OPERATOR", getOperator()));
    LispObject args = argsToLispList();
    if (args != NIL) {
      result = result.push(new Cons("ARGS", args));
    }
			 
    return result.nreverse();
  }
}
