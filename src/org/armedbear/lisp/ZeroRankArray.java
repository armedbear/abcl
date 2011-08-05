/*
 * ZeroRankArray.java
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

import static org.armedbear.lisp.Lisp.*;

public final class ZeroRankArray extends AbstractArray
{
    private final LispObject elementType;
    private final boolean adjustable;

    private LispObject data;

    public ZeroRankArray(LispObject elementType, LispObject data,
                         boolean adjustable)
    {
        this.elementType = elementType;
        this.data = data;
        this.adjustable = adjustable;
    }

    @Override
    public LispObject typeOf()
    {
        if (adjustable)
            return list(Symbol.ARRAY, elementType, NIL);
        else
            return list(Symbol.SIMPLE_ARRAY, elementType, NIL);
    }

    @Override
    public LispObject classOf()
    {
        return BuiltInClass.ARRAY;
    }

    @Override
    public LispObject typep(LispObject type)
    {
        if (type == Symbol.SIMPLE_ARRAY)
            return adjustable ? NIL : T;
        return super.typep(type);
    }

    @Override
    public int getRank()
    {
        return 0;
    }

    @Override
    public LispObject getDimensions()
    {
        return NIL;
    }

    @Override
    public int getDimension(int n)
    {
        error(new TypeError("Bad array dimension (" + n + ") for array of rank 0."));
        // Not reached.
        return -1;
    }

    @Override
    public LispObject getElementType()
    {
        return elementType;
    }

    @Override
    public int getTotalSize()
    {
        return 1;
    }

    @Override
    public LispObject AREF(int index)
    {
        if (index == 0)
            return data;
        else
            return error(new TypeError("Bad row major index " + index + "."));
    }

    @Override
    public void aset(int index, LispObject obj)
    {
        if (obj.typep(elementType) == NIL)
            error(new TypeError(obj, elementType));
        if (index == 0)
            data = obj;
        else
            error(new TypeError("Bad row major index " + index + "."));
    }

    @Override
    public void fill(LispObject obj)
    {
        if (obj.typep(elementType) == NIL)
            error(new TypeError(obj, elementType));
        data = obj;
    }

    @Override
    public String printObject()
    {
        final LispThread thread = LispThread.currentThread();
        boolean printReadably = (Symbol.PRINT_READABLY.symbolValue(thread) != NIL);
        if (printReadably) {
            if (elementType != T) {
                error(new PrintNotReadable(list(Keyword.OBJECT, this)));
                // Not reached.
                return null;
            }
        }
        if (printReadably || Symbol.PRINT_ARRAY.symbolValue(thread) != NIL) {
            StringBuffer sb = new StringBuffer("#0A");
            if (data == this && Symbol.PRINT_CIRCLE.symbolValue(thread) != NIL) {
                StringOutputStream stream = new StringOutputStream();
                thread.execute(Symbol.OUTPUT_OBJECT.getSymbolFunction(),
                               data, stream);
                sb.append(stream.getString().getStringValue());
            } else
                sb.append(data.printObject());
            return sb.toString();
        }
        StringBuffer sb = new StringBuffer();
        if (!adjustable)
            sb.append("SIMPLE-");
        sb.append("ARRAY ");
        sb.append(elementType.printObject());
        sb.append(" NIL");
        return unreadableString(sb.toString());
    }

  @Override
  public AbstractArray adjustArray(int[] dims,
                                              LispObject initialElement,
                                              LispObject initialContents)
    {
      if (isAdjustable()) {
          // initial element doesn't matter:
          // we're not creating new elements
          if (initialContents != null)
              data = initialContents;
          return this;
      } else {
          return new ZeroRankArray(elementType,
                  initialContents != null ? initialContents :
                      initialElement != null ? initialElement : data, false);
      }
  }

  @Override
  public AbstractArray adjustArray(int[] dims,
                                              AbstractArray displacedTo,
                                              int displacement)
    {
      error(new TypeError("Displacement not supported for array of rank 0."));
      return null;
  }
}
