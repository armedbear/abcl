/*
 * StackFrame.java
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

public abstract class StackFrame 
  extends LispObject
{
  @Override
    public LispObject typep(LispObject typeSpecifier) 

   {
     if (typeSpecifier == Symbol.STACK_FRAME)
       return T;
     if (typeSpecifier == BuiltInClass.STACK_FRAME)
       return T;
     return super.typep(typeSpecifier);
   }
  
  StackFrame next;
  Environment env = null;

  void setNext(StackFrame nextFrame) {
    this.next = nextFrame;
  }
  StackFrame getNext() {
    return this.next;
  }

  /** Sets the applicable environment for this stack frame to 'env',
   * returning the last value.
   */
  public Environment setEnv(Environment env) {
    Environment e = this.env;
    this.env = env;
    return e;
  }
  /** Gets the current lexical environment of this stack frame. */
  public Environment getEnv() {
    return env;
  }
  public abstract LispObject toLispList();
  public abstract SimpleString toLispString();
}
