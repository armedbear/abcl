/*
 * GarbageCollectionTask.java
 *
 * Copyright (C) 2000-2002 Peter Graves
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

public final class GarbageCollectionTask extends IdleThreadTask
{
    private long lastRunMillis;

    public GarbageCollectionTask()
    {
        setIdle(1000); // Do gc after user has been idle for 1 second.
        setRunnable(runnable);
    }

    private synchronized final long getLastRunMillis()
    {
        return lastRunMillis;
    }

    private synchronized final void setLastRunMillis(long when)
    {
        lastRunMillis = when;
    }

    private final Runnable runnable = new Runnable() {
        public void run()
        {
            // Only do gc if there has been a user event since the last time
            // we did gc.
            if (Dispatcher.getLastEventMillis() > getLastRunMillis()) {
                Runtime.getRuntime().gc();
                setLastRunMillis(System.currentTimeMillis());
            }
        }
    };
}
