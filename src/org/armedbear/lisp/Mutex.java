/*
 * Mutex.java
 *
 * Copyright (C) 2004-2007 Peter Graves
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

/*
  File: Mutex.java

  Originally written by Doug Lea and released into the public domain.
  This may be used for any purposes whatsoever without acknowledgment.
  Thanks for the assistance and support of Sun Microsystems Labs,
  and everyone contributing, testing, and using this code.

  History:
  Date       Who                What
  11Jun1998  dl               Create public version
*/

package org.armedbear.lisp;

public final class Mutex extends LispObject
{
    private boolean inUse;

    public LispObject typeOf()
    {
        return Symbol.MUTEX;
    }

    public LispObject classOf()
    {
        return BuiltInClass.MUTEX;
    }

    public LispObject typep(LispObject typeSpecifier) throws ConditionThrowable
    {
        if (typeSpecifier == Symbol.MUTEX)
            return T;
        if (typeSpecifier == BuiltInClass.MUTEX)
            return T;
        return super.typep(typeSpecifier);
    }

    public void acquire() throws InterruptedException
    {
        if (Thread.interrupted())
            throw new InterruptedException();
        synchronized (this) {
            try {
                while (inUse)
                    wait();
                inUse = true;
            }
            catch (InterruptedException e) {
                notify();
                throw e;
            }
        }
    }

    public synchronized void release()  {
        inUse = false;
        notify();
    }


    public String writeToString()
    {
        return unreadableString("MUTEX");
    }

    // ### make-mutex => mutex
    private static final Primitive MAKE_MUTEX =
        new Primitive("make-mutex", PACKAGE_EXT, true, "")
    {
        public LispObject execute() throws ConditionThrowable
        {
            return new Mutex();
        }
    };

    // ### get-mutex mutex => generalized-boolean
    private static final Primitive GET_MUTEX =
        new Primitive("get-mutex", PACKAGE_EXT, true, "mutex")
    {
        public LispObject execute(LispObject arg) throws ConditionThrowable
        {
            try {
                ((Mutex)arg).acquire();
                return T;
            }
            catch (ClassCastException e) {
                return error(new TypeError("The value " + arg.writeToString() +
                                            " is not a mutex."));
            }
            catch (InterruptedException e) {
                return error(new LispError(
                    "The thread " + LispThread.currentThread().writeToString() +
                    " was interrupted."));
            }
        }
    };

    // ### release-mutex mutex
    private static final Primitive RELEASE_MUTEX =
        new Primitive("release-mutex", PACKAGE_EXT, true, "mutex")
    {
        public LispObject execute(LispObject arg) throws ConditionThrowable
        {
            try {
                ((Mutex)arg).release();
                return T;
            }
            catch (ClassCastException e) {
                return error(new TypeError("The value " + arg.writeToString() +
                                            " is not a mutex."));
            }
        }
    };
}
