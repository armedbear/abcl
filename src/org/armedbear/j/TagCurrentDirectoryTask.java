/*
 * TagCurrentDirectoryTask.java
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

public final class TagCurrentDirectoryTask extends IdleThreadTask
{
    private long lastRun;

    public TagCurrentDirectoryTask()
    {
        setIdle(300000); // 5 minutes
        setRunnable(runnable);
    }

    private final Runnable runnable = new Runnable() {
        public void run()
        {
            long now = System.currentTimeMillis();
            if (lastRun == 0 || now - lastRun > getIdle()) {
                // Add current directory to tag file manager's queue.
                Buffer buffer = Editor.currentEditor().getBuffer();
                File file = buffer.getFile();
                if (file == null)
                    return;
                if (file.isRemote())
                    return;
                if (buffer.isTaggable()) {
                    File dir = file.getParentFile();
                    if (dir != null)
                        Editor.getTagFileManager().addToQueue(dir, buffer.getMode());
                }
                lastRun = now;
            }
        }
    };
}
