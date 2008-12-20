/*
 * IdleThreadTask.java
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

import javax.swing.SwingUtilities;

public class IdleThreadTask implements Runnable
{
    private Runnable runnable;
    private boolean invokeLater;
    private long idle;

    public IdleThreadTask()
    {
    }

    public IdleThreadTask(Runnable runnable, long idle, boolean invokeLater)
    {
        this.runnable = runnable;
        this.idle = idle;
        this.invokeLater = invokeLater;
    }

    public void setRunnable(Runnable runnable)
    {
        this.runnable = runnable;
    }

    public synchronized final long getIdle()
    {
        return idle;
    }

    public synchronized final void setIdle(long idle)
    {
        this.idle = idle;
    }

    public final void setInvokeLater(boolean b)
    {
        invokeLater = b;
    }

    public final void run()
    {
        if (runnable != null) {
            if (invokeLater)
                SwingUtilities.invokeLater(runnable);
            else
                runnable.run();
        }
    }
}
