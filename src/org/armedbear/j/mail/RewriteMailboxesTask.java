/*
 * RewriteMailboxesTask.java
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

package org.armedbear.j.mail;

import org.armedbear.j.Buffer;
import org.armedbear.j.BufferIterator;
import org.armedbear.j.Editor;
import org.armedbear.j.IdleThreadTask;

public final class RewriteMailboxesTask extends IdleThreadTask
{
    private static final long REWRITE_MAILBOXES_IDLE = 5000; // 5 seconds

    private static RewriteMailboxesTask instance;

    private RewriteMailboxesTask()
    {
        setIdle(REWRITE_MAILBOXES_IDLE);
        setRunnable(runnable);
    }

    public static synchronized RewriteMailboxesTask getInstance()
    {
        if (instance == null)
            instance = new RewriteMailboxesTask();
        return instance;
    }

    private final Runnable runnable = new Runnable() {
        private long lastRun;

        public void run()
        {
            if (!Editor.isMailEnabled())
                return;
            long now = System.currentTimeMillis();
            if (lastRun == 0 || now - lastRun > REWRITE_MAILBOXES_IDLE) {
                synchronized (Editor.getBufferList()) {
                    for (BufferIterator it = new BufferIterator(); it.hasNext();) {
                        Buffer buf = it.nextBuffer();
                        if (buf instanceof PopMailbox) {
                            final PopMailbox mb = (PopMailbox) buf;
                            // User must be idle for 5 minutes if mailbox is in
                            // foreground, 1 minute if mailbox is in background.
                            if (mb.isDirty() && mb.isIdle(300, 60)) {
                                if (mb.lock()) {
                                    // Double check.
                                    if (mb.isDirty() && mb.isIdle(300, 60)) {
                                        mb.setBusy(true);
                                        mb.setWaitCursor();
                                        Runnable r = new Runnable() {
                                            public void run()
                                            {
                                                mb.rewriteMailbox(false);
                                                mb.setBusy(false);
                                                mb.unlock();
                                                mb.setDefaultCursor();
                                            }
                                        };
                                        Thread t = new Thread(r);
                                        t.setPriority(Thread.MIN_PRIORITY);
                                        t.start();
                                    } else
                                        mb.unlock();
                                }
                            }
                        }
                    }
                }
                lastRun = now;
            }
        }
    };
}
