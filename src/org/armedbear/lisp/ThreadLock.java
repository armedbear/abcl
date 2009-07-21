/*
 * ThreadLock.java
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

public final class ThreadLock extends LispObject
{
    private LispThread thread;

    private void lock() throws ConditionThrowable
    {
        LispThread currentThread = LispThread.currentThread();
        if (!currentThread.equals(thread)) {
            while (thread != null) {
                synchronized(this) {
                    try {
                        wait();
                    } catch(InterruptedException e) {
                        throw new RuntimeException(e);
                    }
                }
            }
            thread = currentThread;
        }
    }

    private void unlock() throws ConditionThrowable
    {
        if (thread.equals(LispThread.currentThread())) {
            synchronized(this) {
                thread = null;
                notifyAll();
            }
        }
    }

    @Override
    public String writeToString()
    {
        return unreadableString("THREAD-LOCK");
    }

    // ### make-thread-lock
    private static final Primitive MAKE_THREAD_LOCK =
        new Primitive("make-thread-lock", PACKAGE_THREADS, true)
    {
        @Override
        public LispObject execute() throws ConditionThrowable
        {
            return new ThreadLock();
        }
    };

    // ### thread-lock lock
    private static final Primitive THREAD_LOCK =
        new Primitive("thread-lock", PACKAGE_THREADS, true)
    {
        @Override
        public LispObject execute(LispObject arg) throws ConditionThrowable
        {
            ThreadLock threadLock = (ThreadLock) arg;
            threadLock.lock();
            return NIL;
        }
    };

    // ### thread-unlock lock
    private static final Primitive THREAD_UNLOCK =
        new Primitive("thread-unlock", PACKAGE_THREADS, true)
    {
        @Override
        public LispObject execute(LispObject arg) throws ConditionThrowable
        {
            ThreadLock threadLock = (ThreadLock) arg;
            threadLock.unlock();
            return NIL;
        }
    };

    static {
      //FIXME: this block has been added for pre-0.16 compatibility
      // and can be removed the latest at release 0.22
      try {
	PACKAGE_EXT.export(Symbol.intern("MAKE-THREAD-LOCK", PACKAGE_THREADS));
	PACKAGE_EXT.export(Symbol.intern("THREAD-LOCK", PACKAGE_THREADS));
	PACKAGE_EXT.export(Symbol.intern("THREAD-UNLOCK", PACKAGE_THREADS));
      } catch (ConditionThrowable t) {
	Debug.bug();
      }
    }
}
