/*
 * StandardObjectFunctions.java
 *
 * Copyright (C) 2003-2005 Peter Graves
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

public class StandardObjectFunctions
{
  // ### %std-allocate-instance class => instance
  private static final Primitive _STD_ALLOCATE_INSTANCE =
    new Primitive("%std-allocate-instance", PACKAGE_SYS, true, "class")
    {
      @Override
      public LispObject execute(LispObject arg)
      {
        if (arg == StandardClass.STANDARD_CLASS)
          return new StandardClass();
        if (arg instanceof StandardClass)
            return ((StandardClass)arg).allocateInstance();
        if (arg.typep(StandardClass.STANDARD_CLASS) != NIL) {
            LispObject l = Symbol.CLASS_LAYOUT.execute(arg);
            if (! (l instanceof Layout))
                return error(new ProgramError("Invalid standard class layout for: " + arg.writeToString()));
            
            return new StandardObject((Layout)l);
        }
        return type_error(arg, Symbol.STANDARD_CLASS);
      }
    };
}
