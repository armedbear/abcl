/*
 * floor.java
 *
 * Copyright (C) 2004 Peter Graves
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

// ### floor number &optional divisor
public final class floor extends Primitive
{
    private floor()
    {
        super("floor", "number &optional divisor");
    }

    @Override
    public LispObject execute(LispObject number)

    {
        LispObject quotient = number.truncate(Fixnum.ONE);
        final LispThread thread = LispThread.currentThread();
        LispObject remainder = thread._values[1];
        if (!remainder.zerop()) {
            if (number.minusp()) {
                quotient = quotient.decr();
                remainder = remainder.incr();
                thread._values[0] = quotient;
                thread._values[1] = remainder;
            }
        }
        return quotient;
    }

    @Override
    public LispObject execute(LispObject number, LispObject divisor)

    {
        LispObject quotient = number.truncate(divisor);
        final LispThread thread = LispThread.currentThread();
        LispObject remainder = thread._values[1];
        boolean adjust = false;
        if (!remainder.zerop()) {
            if (divisor.minusp()) {
                if (number.plusp())
                    adjust = true;
            } else {
                if (number.minusp())
                    adjust = true;
            }
        }
        if (adjust) {
            quotient = quotient.decr();
            remainder = remainder.add(divisor);
            thread._values[0] = quotient;
            thread._values[1] = remainder;
        }
        return quotient;
    }

    private static final Primitive FLOOR = new floor();
}
