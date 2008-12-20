/*
 * Mailbox.java
 *
 * Copyright (C) 2004-2007 Peter Graves, Andras Simon
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

import java.util.LinkedList;
import java.util.NoSuchElementException;

public final class Mailbox extends LispObject
{
  private LinkedList<LispObject> box = new LinkedList<LispObject>();

  public LispObject typeOf()
  {
    return Symbol.MAILBOX;
  }

  public LispObject classOf()
  {
    return BuiltInClass.MAILBOX;
  }

  public LispObject typep(LispObject typeSpecifier) throws ConditionThrowable
  {
    if (typeSpecifier == Symbol.MAILBOX)
      return T;
    if (typeSpecifier == BuiltInClass.MAILBOX)
      return T;
    return super.typep(typeSpecifier);
  }

  private void send(LispObject o)
  {
    synchronized(this)
      {
        box.add(o);
        notify();
      }
  }

  private LispObject read()
  {
    synchronized(this)
      {
        while (box.isEmpty())
          {
            try
              {
                wait();
              }
            catch(InterruptedException e)
              {
                throw new RuntimeException(e);
              }
          }
        return (LispObject) box.removeFirst();
      }
  }

  private LispObject peek()
  {
    synchronized(this)
      {
        try
          {
            return (LispObject) box.getFirst();
          }
        catch(NoSuchElementException e)
          {
            return NIL;
          }
      }
  }

  private LispObject empty()
  {
    return box.isEmpty() ? T : NIL;
  }

  public String writeToString() throws ConditionThrowable
  {
    return unreadableString(Symbol.MAILBOX);
  }

  // ### make-mailbox
  private static final Primitive MAKE_MAILBOX =
    new Primitive("make-mailbox", PACKAGE_EXT, true, "")
    {
      public LispObject execute() throws ConditionThrowable
      {
        return new Mailbox();
      }
    };

  // ### mailbox-send mailbox object
  private static final Primitive MAILBOX_SEND =
    new Primitive("mailbox-send", PACKAGE_EXT, true, "mailbox object")
    {
      public LispObject execute(LispObject first, LispObject second)
        throws ConditionThrowable
      {
        if (first instanceof Mailbox)
          {
            Mailbox mbox = (Mailbox) first;
            mbox.send(second);
            return T;
          }
        else
          return type_error(first, Symbol.MAILBOX);
      }
    };

  // ### mailbox-read mailbox
  private static final Primitive MAILBOX_READ =
    new Primitive("mailbox-read", PACKAGE_EXT, true, "mailbox")
    {
      public LispObject execute(LispObject arg) throws ConditionThrowable
      {
        if (arg instanceof Mailbox)
          {
            Mailbox mbox = (Mailbox) arg;
            return mbox.read();
          }
        else
          return type_error(arg, Symbol.MAILBOX);
      }
    };

  // ### mailbox-peek mailbox
  private static final Primitive MAILBOX_PEEK =
    new Primitive("mailbox-peek", PACKAGE_EXT, true, "mailbox")
    {
      public LispObject execute(LispObject arg) throws ConditionThrowable
      {
        if (arg instanceof Mailbox)
          {
            Mailbox mbox = (Mailbox) arg;
            return mbox.peek();
          }
        else
          return type_error(arg, Symbol.MAILBOX);
      }
    };

  // ### mailbox-empty-p mailbox
  private static final Primitive MAILBOX_EMPTY_P =
    new Primitive("mailbox-empty-p", PACKAGE_EXT, true, "mailbox")
    {
      public LispObject execute(LispObject arg) throws ConditionThrowable
      {
        if (arg instanceof Mailbox)
          {
            Mailbox mbox = (Mailbox) arg;
            return mbox.empty();
          }
        else
            return type_error(arg, Symbol.MAILBOX);
      }
    };
}
