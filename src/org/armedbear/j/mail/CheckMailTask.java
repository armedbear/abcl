/*
 * CheckMailTask.java
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

package org.armedbear.j.mail;

import java.util.ArrayList;
import org.armedbear.j.Buffer;
import org.armedbear.j.BufferIterator;
import org.armedbear.j.BufferList;
import org.armedbear.j.Editor;
import org.armedbear.j.IdleThreadTask;
import org.armedbear.j.Log;
import org.armedbear.j.Property;

public final class CheckMailTask extends IdleThreadTask
{
    private static CheckMailTask instance;

    private long lastRun;

    private CheckMailTask()
    {
        setIdle(10000); // User must be idle for 10 seconds.
        setRunnable(runnable);
    }

    public static synchronized CheckMailTask getInstance()
    {
        if (instance == null)
            instance = new CheckMailTask();
        return instance;
    }

    private final Runnable runnable = new Runnable()
    {
        public void run()
        {
            if (!Editor.preferences().getBooleanProperty(Property.CHECK_ENABLED))
                return;
            if (!Editor.isMailEnabled())
                return;
            // Only check every 10 seconds.
            if (System.currentTimeMillis() - lastRun > 10000) {
                // Make a list of mailboxes to check. We don't want to keep the
                // buffer list locked while we do the actual check!
                ArrayList mailboxes = new ArrayList();
                BufferList bufferList = Editor.getBufferList();
                synchronized (bufferList) {
                    for (BufferIterator it = new BufferIterator(); it.hasNext();) {
                        Buffer buf = it.nextBuffer();
                        if (buf instanceof ImapMailbox || buf instanceof PopMailbox)
                            mailboxes.add(buf);
                    }
                }
                // Now check the mailboxes in the list.
                for (int i = 0; i < mailboxes.size(); i++) {
                    Mailbox mb = (Mailbox) mailboxes.get(i);
                    if (bufferList.contains(mb))
                        check(mb);
                }
                lastRun = System.currentTimeMillis();
            }
        }
    };

    private void check(final Mailbox mb)
    {
        // Avoid locking unnecessarily.
        if (!mb.getBooleanProperty(Property.CHECK_ENABLED))
            return;
        int interval = mb.getIntegerProperty(Property.CHECK_INTERVAL);
        if (interval <= 0)
            return;
        long now = System.currentTimeMillis();
        if (now - mb.getLastCheckMillis() < interval * 1000)
            return;
        // Wait 2 minutes after last error.
        if (now - mb.getLastErrorMillis() < 120000)
            return;
        int fg = mb.getIntegerProperty(Property.CHECK_IDLE_FOREGROUND);
        int bg = mb.getIntegerProperty(Property.CHECK_IDLE_BACKGROUND);
        if (!mb.isIdle(fg, bg))
            return;
        if (mb.lock()) {
            // Double check.
            if (mb.isIdle(fg, bg)) {
                // Still idle.
                mb.setBusy(true);
                mb.setWaitCursor();
                // Starts a new thread, unlocks mailbox when done.
                mb.getNewMessages(false);
                // lastCheckMillis will be updated again at the end of
                // GetNewMessagesProcess.run().
                mb.setLastCheckMillis(System.currentTimeMillis());
            } else {
                // Not idle.
                mb.unlock();
            }
        }
    }
}
