/*
 * ReadWriteLock.java
 *
 * Copyright (C) 2002 Peter Graves
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

public final class ReadWriteLock
{
    private int activeReaders;
    private int activeWriters;
    private int waitingReaders;
    private int waitingWriters;
    private Thread writerThread;
    private int lockCount;

    public synchronized void lockRead() throws InterruptedException
    {
        if (activeReaders != 0 || allowRead()) {
            ++activeReaders;
            return;
        }
        // Reaching here, either a write is in progress or waitingWriters > 0.
        // If the current thread holds the write lock, we'll deadlock.
        if (Thread.currentThread() == writerThread)
            Debug.bug();
        ++waitingReaders;
        while (!allowRead()) {
            try {
                wait();
            }
            catch (InterruptedException e) {
                --waitingReaders; // Roll back state.
                throw e;
            }
        }
        --waitingReaders;
        ++activeReaders;
    }

    public synchronized void unlockRead()
    {
        Debug.assertTrue(activeReaders > 0);
        --activeReaders;
        notifyAll();
    }

    public synchronized void lockWrite() throws InterruptedException
    {
        if (writerThread != null) {
            // Write in progress.
            if (Thread.currentThread() == writerThread) {
                // Same thread.
                ++lockCount;
                return;
            }
        }
        if (allowWrite()) {
            claimWriteLock();
            return;
        }
        ++waitingWriters;
        while (!allowWrite()) {
            try {
                wait();
            }
            catch (InterruptedException e) {
                --waitingWriters;
                throw e;
            }
        }
        --waitingWriters;
        claimWriteLock();
    }

    public synchronized void unlockWrite()
    {
        Debug.assertTrue(activeWriters == 1);
        Debug.assertTrue(lockCount > 0);
        Debug.assertTrue(Thread.currentThread() == writerThread);
        if (--lockCount == 0) {
            --activeWriters;
            writerThread = null;
            notifyAll();
        }
    }

    public synchronized boolean isWriteLocked()
    {
        Debug.assertTrue(activeWriters == 0 || activeWriters == 1);
        return activeWriters == 1;
    }

    private final boolean allowRead()
    {
        return waitingWriters == 0 && activeWriters == 0;
    }

    private final boolean allowWrite()
    {
        return activeReaders == 0 && activeWriters == 0;
    }

    private void claimWriteLock()
    {
        ++activeWriters;
        Debug.assertTrue(writerThread == null);
        writerThread = Thread.currentThread();
        Debug.assertTrue(lockCount == 0);
        lockCount = 1;
    }
}
