/*
 * PendingOperations.java
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

import java.util.ArrayList;

public final class PendingOperations implements Runnable
{
    private ArrayList operations;

    public PendingOperations()
    {
    }

    public synchronized void add(Object object)
    {
        if (operations == null)
            operations = new ArrayList();
        operations.add(object);
    }

    public synchronized void remove(Object object)
    {
        operations.remove(object);
        if (operations.size() == 0)
            notify();
    }

    public synchronized void run()
    {
        if (operations != null && operations.size() > 0) {
            Editor.currentEditor().status("Completing pending operations...");
            while (operations.size() > 0) {
                try {
                    wait();
                }
                catch (InterruptedException e) {
                    Log.error(e);
                }
            }
        }
    }
}
