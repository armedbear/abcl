/*
 * logeqv.java
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

import java.math.BigInteger;

// ### logeqv
// logeqv &rest integers => result-integer
// equivalence (exclusive nor)
public final class logeqv extends Primitive
{
    private logeqv()
    {
        super("logeqv", "&rest integers");
    }

    @Override
    public LispObject execute()
    {
        return Fixnum.MINUS_ONE;
    }

    @Override
    public LispObject execute(LispObject arg)
    {
        return checkInteger(arg);
    }

    @Override
    public LispObject execute(LispObject[] args)
    {
        BigInteger result = null;
        for (int i = 0; i < args.length; i++) {
            LispObject arg = args[i];
            BigInteger n;
            if (arg instanceof Fixnum)
                n = ((Fixnum)arg).getBigInteger();
            else if (arg instanceof Bignum)
                n = ((Bignum)arg).value;
            else
                return type_error(arg, Symbol.INTEGER);
            if (result == null)
                result = n;
            else
                result = result.xor(n).not();
        }
        return number(result);
    }

    private static final Primitive LOGEQV = new logeqv();
}
