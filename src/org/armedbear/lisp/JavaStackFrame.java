/*
 * JavaStackFrame.java
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

import org.armedbear.lisp.protocol.Inspectable;

public class JavaStackFrame 
  extends StackFrame
  implements Inspectable
{
  public final StackTraceElement javaFrame;

  public JavaStackFrame(StackTraceElement javaFrame)
  {
    this.javaFrame = javaFrame;
  }

  @Override
  public LispObject typeOf() { 
    return Symbol.JAVA_STACK_FRAME; 
  }

  @Override
  public LispObject classOf()   { return BuiltInClass.JAVA_STACK_FRAME; }

  @Override
  public String printObject() { 
    final String JAVA_STACK_FRAME = "JAVA-STACK-FRAME";
    return unreadableString(JAVA_STACK_FRAME + " "
				+ toLispString().toString());
  }

  @Override
  public LispObject typep(LispObject typeSpecifier) 

  {
     if (typeSpecifier == Symbol.JAVA_STACK_FRAME)
       return T;
     if (typeSpecifier == BuiltInClass.JAVA_STACK_FRAME)
       return T;
     return super.typep(typeSpecifier);
   }

  static final Symbol CLASS = internKeyword("CLASS");
  static final Symbol METHOD = internKeyword("METHOD");
  static final Symbol FILE = internKeyword("FILE");
  static final Symbol LINE = internKeyword("LINE");
  static final Symbol NATIVE_METHOD = internKeyword("NATIVE-METHOD");

  public LispObject toLispList()
  {
    LispObject result = Lisp.NIL;
    
    if ( javaFrame == null) 
      return result;

    result = result.push(CLASS);
    result = result.push(new SimpleString(javaFrame.getClassName()));
    result = result.push(METHOD);
    result = result.push(new SimpleString(javaFrame.getMethodName()));
    result = result.push(FILE);
    result = result.push(new SimpleString(javaFrame.getFileName()));
    result = result.push(LINE);
    result = result.push(Fixnum.getInstance(javaFrame.getLineNumber()));
    if (javaFrame.isNativeMethod()) {
      result = result.push(NATIVE_METHOD);
      result = result.push(Symbol.T);
    }

    return result.nreverse();
  }

  @Override
  public SimpleString toLispString() 

  {
    return new SimpleString(javaFrame.toString());
  }

  @Override
  public LispObject getParts() 

  { 
    LispObject result = NIL;
    result = result.push(new Cons("CLASS", 
				  new SimpleString(javaFrame.getClassName())));
    result = result.push(new Cons("METHOD", 
				  new SimpleString(javaFrame.getMethodName())));
    result = result.push(new Cons("FILE", 
				  new SimpleString(javaFrame.getFileName())));
    result = result.push(new Cons("LINE",
				  Fixnum.getInstance(javaFrame.getLineNumber())));
    result = result.push(new Cons("NATIVE-METHOD",
				  LispObject.getInstance(javaFrame.isNativeMethod())));
    return result.nreverse();
  }
}
