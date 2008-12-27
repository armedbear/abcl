/*
 * Condition.java
 *
 * Copyright (C) 2003-2007 Peter Graves
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

public class Condition extends StandardObject
{
  protected String message;

  public Condition() throws ConditionThrowable
  {
    super(StandardClass.CONDITION);
    Debug.assertTrue(slots.length == 2);
    setFormatArguments(NIL);
  }

  protected Condition(LispClass cls) throws ConditionThrowable
  {
    super(cls);
    Debug.assertTrue(slots.length >= 2);
    setFormatArguments(NIL);
  }

  public Condition(LispClass cls, int length)
  {
    super(cls, length);
  }

  public Condition(LispObject initArgs) throws ConditionThrowable
  {
    super(StandardClass.CONDITION);
    Debug.assertTrue(slots.length == 2);
    initialize(initArgs);
  }

  protected void initialize(LispObject initArgs) throws ConditionThrowable
  {
    LispObject control = null;
    LispObject arguments = null;
    LispObject first, second;
    while (initArgs instanceof Cons)
      {
        first = initArgs.car();
        initArgs = initArgs.cdr();
        second = initArgs.car();
        initArgs = initArgs.cdr();
        if (first == Keyword.FORMAT_CONTROL)
          {
            if (control == null)
              control = second;
          }
        else if (first == Keyword.FORMAT_ARGUMENTS)
          {
            if (arguments == null)
              arguments = second;
          }
      }
    if (control != null)
      setFormatControl(control);
    if (arguments == null)
      arguments = NIL;
    setFormatArguments(arguments);
  }

  public Condition(String message)
  {
    super(StandardClass.CONDITION);
    Debug.assertTrue(slots.length == 2);
    try
      {
        setFormatControl(message);
        setFormatArguments(NIL);
      }
    catch (Throwable t)
      {
        Debug.trace(t);
      }
  }

  public final LispObject getFormatControl() throws ConditionThrowable
  {
    return getInstanceSlotValue(Symbol.FORMAT_CONTROL);
  }

  public final void setFormatControl(LispObject formatControl)
    throws ConditionThrowable
  {
    setInstanceSlotValue(Symbol.FORMAT_CONTROL, formatControl);
  }

  public final void setFormatControl(String s) throws ConditionThrowable
  {
    setFormatControl(new SimpleString(s));
  }

  public final LispObject getFormatArguments() throws ConditionThrowable
  {
    return getInstanceSlotValue(Symbol.FORMAT_ARGUMENTS);
  }

  public final void setFormatArguments(LispObject formatArguments)
    throws ConditionThrowable
  {
    setInstanceSlotValue(Symbol.FORMAT_ARGUMENTS, formatArguments);
  }

  public String getMessage() throws ConditionThrowable
  {
    return message;
  }

  @Override
  public LispObject typeOf()
  {
    LispClass c = getLispClass();
    if (c != null)
      return c.getSymbol();
    return Symbol.CONDITION;
  }

  @Override
  public LispObject classOf()
  {
    LispClass c = getLispClass();
    if (c != null)
      return c;
    return StandardClass.CONDITION;
  }

  @Override
  public LispObject typep(LispObject type) throws ConditionThrowable
  {
    if (type == Symbol.CONDITION)
      return T;
    if (type == StandardClass.CONDITION)
      return T;
    return super.typep(type);
  }

  public String getConditionReport() throws ConditionThrowable
  {
    String s = getMessage();
    if (s != null)
      return s;
    LispObject formatControl = getFormatControl();
    if (formatControl != NIL)
      {
        try
          {
            return format(formatControl, getFormatArguments());
          }
        catch (Throwable t) {}
      }
    return unreadableString(typeOf().writeToString());
  }

  @Override
  public String writeToString() throws ConditionThrowable
  {
    final LispThread thread = LispThread.currentThread();
    if (Symbol.PRINT_ESCAPE.symbolValue(thread) == NIL)
      {
        String s = getMessage();
        if (s != null)
          return s;
        LispObject formatControl = getFormatControl();
        if (formatControl instanceof Function)
          {
            StringOutputStream stream = new StringOutputStream();
            Symbol.APPLY.execute(formatControl, stream, getFormatArguments());
            return stream.getString().getStringValue();
          }
        if (formatControl instanceof AbstractString)
          {
            LispObject f = Symbol.FORMAT.getSymbolFunction();
            if (f == null || f instanceof Autoload)
              return format(formatControl, getFormatArguments());
            return Symbol.APPLY.execute(f, NIL, formatControl, getFormatArguments()).getStringValue();
          }
      }
    final int maxLevel;
    LispObject printLevel = Symbol.PRINT_LEVEL.symbolValue(thread);
    if (printLevel instanceof Fixnum)
      maxLevel = ((Fixnum)printLevel).value;
    else
      maxLevel = Integer.MAX_VALUE;
    LispObject currentPrintLevel =
      _CURRENT_PRINT_LEVEL_.symbolValue(thread);
    int currentLevel = ((Fixnum)currentPrintLevel).value;
    if (currentLevel >= maxLevel)
      return "#";
    return unreadableString(typeOf().writeToString());
  }
}
