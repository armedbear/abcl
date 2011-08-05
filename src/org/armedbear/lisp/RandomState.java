/*
 * RandomState.java
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

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.math.BigInteger;
import java.util.Random;

public final class RandomState extends LispObject
{
    private Random random;

    public RandomState()
    {
        random = new Random();
    }

    public RandomState(RandomState rs)
    {
        try {
            ByteArrayOutputStream byteOut = new ByteArrayOutputStream();
            ObjectOutputStream out = new ObjectOutputStream(byteOut);
            out.writeObject(rs.random);
            out.close();
            ByteArrayInputStream byteIn = new ByteArrayInputStream(byteOut.toByteArray());
            ObjectInputStream in = new ObjectInputStream(byteIn);
            random = (Random) in.readObject();
            in.close();
        }
        catch (Throwable t) { // ANY exception gets converted to a lisp error
            error(new LispError("Unable to copy random state."));
        }
    }

    @Override
    public LispObject typeOf()
    {
        return Symbol.RANDOM_STATE;
    }

    @Override
    public LispObject classOf()
    {
        return BuiltInClass.RANDOM_STATE;
    }

    @Override
    public LispObject typep(LispObject type)
    {
        if (type == Symbol.RANDOM_STATE)
            return T;
        if (type == BuiltInClass.RANDOM_STATE)
            return T;
        return super.typep(type);
    }

    @Override
    public String printObject()
    {
        return unreadableString(Symbol.RANDOM_STATE);
    }

    public LispObject random(LispObject arg)
    {
        if (arg instanceof Fixnum) {
            int limit = ((Fixnum)arg).value;
            if (limit > 0) {
                int n = random.nextInt((int)limit);
                return Fixnum.getInstance(n);
            }
        } else if (arg instanceof Bignum) {
            BigInteger limit = ((Bignum)arg).value;
            if (limit.signum() > 0) {
                int bitLength = limit.bitLength();
                BigInteger rand = new BigInteger(bitLength + 1, random);
                BigInteger remainder = rand.remainder(limit);
                return number(remainder);
            }
        } else if (arg instanceof SingleFloat) {
            float limit = ((SingleFloat)arg).value;
            if (limit > 0) {
                float rand = random.nextFloat();
                return new SingleFloat(rand * limit);
            }
        } else if (arg instanceof DoubleFloat) {
            double limit = ((DoubleFloat)arg).value;
            if (limit > 0) {
                double rand = random.nextDouble();
                return new DoubleFloat(rand * limit);
            }
        }
        return type_error(arg, list(Symbol.OR,
                                          list(Symbol.INTEGER, Fixnum.ONE),
                                          list(Symbol.FLOAT, list(Fixnum.ZERO))));
    }

    // ### random limit &optional random-state => random-number
    private static final Primitive RANDOM =
        new Primitive(Symbol.RANDOM, "limit &optional random-state")
    {
        @Override
        public LispObject execute(LispObject arg)
        {
            RandomState randomState =
                (RandomState) Symbol._RANDOM_STATE_.symbolValue();
            return randomState.random(arg);
        }
        @Override
        public LispObject execute(LispObject first, LispObject second)

        {
            if (second instanceof RandomState) {
                RandomState randomState = (RandomState) second;
                return randomState.random(first);
            }
            return type_error(first, Symbol.RANDOM_STATE);
        }
    };

    // ### make-random-state &optional state
    private static final Primitive MAKE_RANDOM_STATE =
        new Primitive(Symbol.MAKE_RANDOM_STATE, "&optional state")
    {
        @Override
        public LispObject execute()
        {
            return new RandomState((RandomState)Symbol._RANDOM_STATE_.symbolValue());
        }
        @Override
        public LispObject execute(LispObject arg)

        {
            if (arg == NIL)
                return new RandomState((RandomState)Symbol._RANDOM_STATE_.symbolValue());
            if (arg == T)
                return new RandomState();
            if (arg instanceof RandomState)
                return new RandomState((RandomState)arg);
            return type_error(arg, Symbol.RANDOM_STATE);
        }
    };

    // ### random-state-p
    private static final Primitive RANDOM_STATE_P =
        new Primitive(Symbol.RANDOM_STATE_P, "object")
    {
        @Override
        public LispObject execute(LispObject arg)
        {
            return arg instanceof RandomState ? T : NIL;
        }
    };
}
