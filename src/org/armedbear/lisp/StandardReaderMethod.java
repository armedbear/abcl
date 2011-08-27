/*
 * StandardReaderMethod.java
 *
 * Copyright (C) 2005 Peter Graves
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

public final class StandardReaderMethod extends StandardMethod
{
  public StandardReaderMethod()
  {
    super(StandardClass.STANDARD_READER_METHOD,
          StandardClass.STANDARD_READER_METHOD.getClassLayout().getLength());
  }

  private static final Primitive READER_METHOD_SLOT_NAME 
      = new pf_reader_method_slot_name();
  @DocString(name="reader-method-slot-name",
             args="reader-method")
  private static final class pf_reader_method_slot_name extends Primitive
  {
      pf_reader_method_slot_name()
      {
          super("reader-method-slot-name", PACKAGE_MOP, false, "reader-method");
      }
      @Override
      public LispObject execute(LispObject arg)
      {
          if (arg instanceof StandardReaderMethod)
              return ((StandardReaderMethod)arg).slots[StandardReaderMethodClass.SLOT_INDEX_SLOT_NAME];
          return type_error(arg, Symbol.STANDARD_READER_METHOD);
      }
  };

  private static final Primitive SET_READER_METHOD_SLOT_NAME
      = new pf_set_reader_method_slot_name(); 
    @DocString(name="set-reader-method-slot-name",
               args="reader-method slot-name")
  private static final class pf_set_reader_method_slot_name extends Primitive
  {
      pf_set_reader_method_slot_name()
      {
          super("set-reader-method-slot-name", PACKAGE_MOP, false,
                "reader-method slot-name");
      }
      @Override
      public LispObject execute(LispObject first, LispObject second)

      {
          if (first instanceof StandardReaderMethod)
          {
              ((StandardReaderMethod)first).slots[StandardReaderMethodClass.SLOT_INDEX_SLOT_NAME] = second;
              return second;
          }
              return type_error(first, Symbol.STANDARD_READER_METHOD);
      }
  };
}
