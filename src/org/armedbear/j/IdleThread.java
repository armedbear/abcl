/*
 * IdleThread.java
 *
 * Copyright (C) 1998-2003 Peter Graves
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

import java.util.Vector;
import javax.swing.SwingUtilities;

public class IdleThread extends Thread
{
    private Vector tasks = new Vector();

    private static IdleThread idleThread;

    private IdleThread()
    {
        super("idle");
    }

    public static synchronized IdleThread getInstance()
    {
        if (idleThread == null)
            startIdleThread();
        return idleThread;
    }

    public static synchronized void startIdleThread()
    {
        if (idleThread == null) {
            idleThread = new IdleThread();
            idleThread.init();
            idleThread.setPriority(Thread.MIN_PRIORITY);
            idleThread.setDaemon(true);
            idleThread.start();
        }
    }

    public static synchronized void runFollowContextTask(IdleThreadTask task)
    {
        if (followContextTask == null) {
            followContextTask = task;
            followContextTask.run();
            if (idleThread != null)
                idleThread.addTask(followContextTask);
        }
    }

    public static synchronized void killFollowContextTask()
    {
        if (followContextTask != null) {
            if (idleThread != null)
                idleThread.removeTask(followContextTask);
            followContextTask = null;
        }
    }

    private synchronized void init()
    {
        addTask(parseBuffersTask);
        addTask(updateHorizontalScrollBarsTask);
        addTask(updateSidebarTask);
        addTask(gcTask);
        addTask(autosaveTask);
        addTask(saveStateTask);
        addTask(tagCurrentDirectoryTask);
        if (Editor.isDebugEnabled())
            addListThreadsTask();
    }

    private synchronized IdleThreadTask getTask(int index)
    {
        if (index < 0 || index >= tasks.size())
            return null;
        return (IdleThreadTask) tasks.get(index);
    }

    private synchronized void addTask(IdleThreadTask task)
    {
        tasks.add(task);
    }

    public synchronized void maybeAddTask(IdleThreadTask task)
    {
        for (int i = tasks.size(); i-- > 0;) {
            if (tasks.get(i) == task)
                return; // Already added.
        }
        tasks.add(task);
    }

    private synchronized void removeTask(IdleThreadTask task)
    {
        tasks.remove(task);
    }

    public void run()
    {
        while (true) {
            try {
                Thread.sleep(500);
            }
            catch (InterruptedException e) {}
            final long lastEventMillis = Dispatcher.getLastEventMillis();
            final long idle = System.currentTimeMillis() - lastEventMillis;
            if (idle > 500) {
                int i = 0;
                IdleThreadTask task;
                while ((task = getTask(i++)) != null) {
                    if (task.getIdle() == 0)
                        ;
                    else if (idle > task.getIdle())
                        task.run();
                    if (Dispatcher.getLastEventMillis() != lastEventMillis)
                        break; // User has done something.
                }
            }
        }
    }

    private Runnable updateSidebarRunnable = new Runnable() {
        public void run()
        {
            Sidebar.refreshSidebarInAllFrames();
        }
    };

    private IdleThreadTask updateSidebarTask =
        new IdleThreadTask(updateSidebarRunnable, 500, true);

    private Runnable parseBuffersRunnable = new Runnable()
    {
        public void run()
        {
            synchronized (Editor.getBufferList()) {
                for (BufferIterator iter = new BufferIterator(); iter.hasNext();) {
                    Buffer buf = iter.nextBuffer();
                    if (!buf.needsParsing())
                        continue;
                    boolean changed = false;
                    try {
                        buf.lockRead();
                    }
                    catch (InterruptedException e) {
                        Log.error(e);
                        return;
                    }
                    try {
                        changed = buf.getFormatter().parseBuffer();
                    }
                    finally {
                        buf.unlockRead();
                    }
                    if (changed)
                        buf.repaint();
                }
            }
        }
    };

    private IdleThreadTask parseBuffersTask =
        new IdleThreadTask(parseBuffersRunnable, 500, false);

    private Runnable updateHorizontalScrollBarsRunnable = new Runnable() {
        public void run()
        {
            for (EditorIterator it = new EditorIterator(); it.hasNext();) {
                Editor ed = it.nextEditor();
                if (ed.getHorizontalScrollBar() != null) {
                    Buffer buf = ed.getBuffer();
                    if (buf != null) {
                        if (buf.validateMaximumColumns())
                            ed.updateHorizontalScrollBar();
                    }
                }
            }
        }
    };

    private IdleThreadTask updateHorizontalScrollBarsTask =
        new IdleThreadTask(updateHorizontalScrollBarsRunnable, 500, true);

    private Runnable autosaveRunnable = new Runnable() {
        public void run()
        {
            Editor editor = Editor.currentEditor();
            if (editor == null)
                return;
            Buffer buffer = editor.getBuffer();
            if (buffer != null)
                buffer.autosave();
        }
    };

    private IdleThreadTask autosaveTask =
        new IdleThreadTask(autosaveRunnable, 2000, false);

    private Runnable saveStateRunnable = new Runnable() {
        private long lastRun = 0;
        public void run()
        {
            Debug.assertTrue(SwingUtilities.isEventDispatchThread());
            if (Dispatcher.getLastEventMillis() > lastRun) {
                Editor editor = Editor.currentEditor();
                if (editor != null)
                    editor.saveState();
                RecentFiles.getInstance().save();
                lastRun = System.currentTimeMillis();
            }
        }
    };

    private IdleThreadTask saveStateTask =
        new IdleThreadTask(saveStateRunnable, 5000, true);

    private IdleThreadTask gcTask = new GarbageCollectionTask();

    private IdleThreadTask tagCurrentDirectoryTask =
        new TagCurrentDirectoryTask();

    private static IdleThreadTask followContextTask;

    private void addListThreadsTask()
    {
        Runnable listThreadsRunnable = new Runnable() {
            private long lastRun = 0;
            public void run()
            {
                int minutes = Editor.preferences().getIntegerProperty(
                    Property.LIST_THREADS);
                if (minutes > 0) {
                    long millis = minutes * 60000;
                    if (System.currentTimeMillis() - lastRun > millis) {
                        Debug.listThreads();
                        lastRun = System.currentTimeMillis();
                    }
                }
            }
        };
        addTask(new IdleThreadTask(listThreadsRunnable, 10000, true));
    }
}
