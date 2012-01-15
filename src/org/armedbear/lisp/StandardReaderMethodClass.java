/*
 * StandardReaderMethodClass.java
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

public final class StandardReaderMethodClass extends StandardClass
{
  // From StandardMethodClass.java:
  public static final int SLOT_INDEX_GENERIC_FUNCTION = 0;
  public static final int SLOT_INDEX_LAMBDA_LIST      = 1;
  public static final int SLOT_INDEX_KEYWORDS         = 2;
  public static final int SLOT_INDEX_OTHER_KEYWORDS_P = 3;
  public static final int SLOT_INDEX_SPECIALIZERS     = 4;
  public static final int SLOT_INDEX_QUALIFIERS       = 5;
  public static final int SLOT_INDEX_FUNCTION         = 6;
  public static final int SLOT_INDEX_FAST_FUNCTION    = 7;
  public static final int SLOT_INDEX_DOCUMENTATION    = 8;


  // Added:
  public static final int SLOT_INDEX_SLOT_NAME        = 9;

  public StandardReaderMethodClass()
  {
    super(Symbol.STANDARD_READER_METHOD,
          list(StandardClass.STANDARD_READER_METHOD));
    Package pkg = PACKAGE_SYS;
    LispObject[] instanceSlotNames =
      {
        Symbol.GENERIC_FUNCTION,
        pkg.intern("LAMBDA-LIST"),
        pkg.intern("KEYWORDS"),
        pkg.intern("OTHER_KEYWORDS_P"),
        pkg.intern("SPECIALIZERS"),
        pkg.intern("QUALIFIERS"),
        Symbol.FUNCTION,
        pkg.intern("FAST-FUNCTION"),
        Symbol.DOCUMENTATION,
        pkg.intern("SLOT-NAME")
      };
    setClassLayout(new Layout(this, instanceSlotNames, NIL));
    setFinalized(true);
  }

  @Override
  public LispObject allocateInstance()
  {
    return new StandardReaderMethod();
  }
}
