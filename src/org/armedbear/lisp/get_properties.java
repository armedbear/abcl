/*
 * get_properties.java
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

import static org.armedbear.lisp.Lisp.*;

// ### get-properties
public final class get_properties extends Primitive
{
  private get_properties()
  {
    super(Symbol.GET_PROPERTIES, "plist indicator-list");
  }

  @Override
  public LispObject execute(LispObject first, LispObject second)

  {
    final LispThread thread = LispThread.currentThread();
    LispObject plist = first;
    while (plist != NIL)
      {
        if (plist.cdr() instanceof Cons)
          {
            LispObject indicator = ((Cons)plist).car;
            LispObject indicators = second;
            while (indicators instanceof Cons)
              {
                if (indicator == ((Cons)indicators).car)
                  return thread.setValues(indicator, plist.cadr(), plist);
                indicators = ((Cons)indicators).cdr;
              }
            if (indicators != NIL)
              return type_error(indicators, Symbol.LIST);
            plist = plist.cddr();
          }
        else
          return type_error(plist.cdr(), Symbol.CONS);
      }
    return thread.setValues(NIL, NIL, NIL);
  }

  private static final Primitive GET_PROPERTIES = new get_properties();
}
