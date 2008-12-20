/*
 * Mutex.java
 *
 * Copyright (C) 2002-2004 Peter Graves
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
 */

package org.armedbear.j;

public final class Mutex
{
    private boolean inUse;

    public final boolean isInUse()
    {
        return inUse;
    }

    public void acquire() throws InterruptedException
    {
        if (Thread.interrupted())
            throw new InterruptedException();
        synchronized(this) {
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

    public synchronized void release()
    {
        if (inUse) {
            inUse = false;
            notify();
        } else
             Debug.bug("Mutex.release() mutex not in use!");
    }

    public boolean attempt() throws InterruptedException
    {
        return attempt(0);
    }

    public boolean attempt(long msecs) throws InterruptedException
    {
        if (Thread.interrupted())
            throw new InterruptedException();
        synchronized(this) {
            if (!inUse) {
                inUse = true;
                return true;
            } else if (msecs <= 0) {
                return false;
            } else {
                long waitTime = msecs;
                long start = System.currentTimeMillis();
                try {
                    for (;;) {
                        wait(waitTime);
                        if (!inUse) {
                            inUse = true;
                            return true;
                        } else {
                            waitTime = msecs - (System.currentTimeMillis() - start);
                            if (waitTime <= 0)
                                return false;
                        }
                    }
                }
                catch (InterruptedException e) {
                    notify();
                    throw e;
                }
            }
        }
    }
}
