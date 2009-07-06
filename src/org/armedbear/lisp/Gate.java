/*
 * Gate.java
 *
 * Copyright (C) 2009 Tobias Rittweiler
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

/** 
 *   A GATE is an object with two states, open and closed. It is
 *   created with MAKE-GATE. Its state can be opened (OPEN-GATE) or
 *   closed (CLOSE-GATE) and can be explicitly tested with
 *   GATE-OPEN-P. Usually though, a thread awaits the opening of a
 *   gate by WAIT-OPEN-GATE.
 */
final public class Gate extends LispObject
{
  private boolean open;

  private Gate(boolean open) 
  { 
    this.open = open; 
  }

  @Override
  public LispObject typeOf()    { return Symbol.GATE; }

  @Override
  public LispObject classOf()   { return BuiltInClass.GATE; }

  @Override
  public String writeToString() { return unreadableString("GATE"); }

  @Override
  public LispObject typep(LispObject typeSpecifier) 
    throws ConditionThrowable 
  {
    if (typeSpecifier == Symbol.GATE)
      return T;
    if (typeSpecifier == BuiltInClass.GATE)
      return T;
    return super.typep(typeSpecifier);
  }

  public boolean isOpen() {
    return open;
  }

  public synchronized void close() {
    open = false;
  }

  public synchronized void open()  {
    open = true;
    notifyAll();
  }

  public synchronized void waitForOpen(long timeout) 
    throws InterruptedException 
  {
    if (open)
      return;
    wait(timeout);
  }


  private static final void checkForGate(LispObject arg) 
    throws ConditionThrowable
  {
    if (arg instanceof Gate)
      return;
    type_error(arg, Symbol.GATE);
  }

  // ### make-gate => gate
  private static final Primitive MAKE_GATE 
    = new Primitive("make-gate", PACKAGE_EXT, true, "openp",
		    "Creates a gate with initial state OPENP.") {
	@Override
	public LispObject execute(LispObject arg) 
	  throws ConditionThrowable
	{
	  return new Gate(arg.getBooleanValue());
	}
      };

  // ### open-gate-p gate => generalized-boolean
  private static final Primitive OPEN_GATE_P 
    = new Primitive("open-gate-p", PACKAGE_EXT, true, "gate",
		    "Boolean predicate as to whether GATE is open or not.") {
      @Override
      public LispObject execute(LispObject arg) 
	throws ConditionThrowable
      {
	checkForGate(arg);
	return ((Gate) arg).isOpen() ? T : NIL;
      }
    };


  // ### open-gate gate => generalized-boolean
  private static final Primitive OPEN_GATE 
    = new Primitive("open-gate", PACKAGE_EXT, true, "gate",
		    "Makes the state of GATE open.")
    {
      @Override
      public LispObject execute(LispObject arg) throws ConditionThrowable
      {
	checkForGate(arg);
	((Gate) arg).open();
	return T;
      }
    };

  // ### close-gate gate
  private static final Primitive CLOSE_GATE 
    = new Primitive("close-gate", PACKAGE_EXT, true, "gate",
		    "Makes the state of GATE closed.") {
      @Override
      public LispObject execute(LispObject arg) 
	throws ConditionThrowable
      {
	checkForGate(arg);
	((Gate)arg).close();
	return T;
      }
    };


  // ### wait-open-gate gate
  private static final Primitive WAIT_OPEN_GATE 
    = new Primitive("wait-open-gate", PACKAGE_EXT, true, 
		    "gate &optional timeout",
		    "Wait for GATE to be open with an optional TIMEOUT in seconds." ) {
	@Override
	public LispObject execute(LispObject gate) 
	  throws ConditionThrowable 
	{
	  return execute(gate, Fixnum.ZERO);
	}
        
	@Override
	public LispObject execute(LispObject gate, LispObject timeout) 
	  throws ConditionThrowable
	{
	  checkForGate(gate);

	  long msecs = LispThread.javaSleepInterval(timeout);
	  try {
	    ((Gate)gate).waitForOpen(msecs);
	    return T;
	  } catch (InterruptedException e) {
	    return error(new LispError("The thread "
				       + LispThread.currentThread().writeToString()
				       + " was interrupted."));
	  }
	}
      };
}

