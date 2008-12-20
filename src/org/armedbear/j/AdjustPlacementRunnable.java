/*
 * AdjustPlacementRunnable.java
 *
 * Copyright (C) 2000-2003 Peter Graves
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

public final class AdjustPlacementRunnable implements Runnable
{
    private final Frame frame;
    private final int extendedState;

    public AdjustPlacementRunnable(Frame frame, int extendedState)
    {
        this.frame = frame;
        this.extendedState = extendedState;
    }

    public void run()
    {
        if (extendedState != 0) {
            frame.storeExtendedState(extendedState);
            frame.setExtendedState(extendedState);
        }
        final Editor editor = frame.getCurrentEditor();
        editor.setFocusToDisplay();
        Runnable r = new Runnable() {
            public void run()
            {
                // Must call setDisplayReady(true) before calling reframe().
                Editor.setDisplayReady(true);
                editor.reframe();
                if (editor.getBuffer().isBusy())
                    editor.setWaitCursor();
                else
                    editor.setDefaultCursor();
            }
        };
        SwingUtilities.invokeLater(r);
        IdleThread.startIdleThread();
    }
}
