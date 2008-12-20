/*
 * Debug.java
 *
 * Copyright (C) 1998-2005 Peter Graves
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

public final class Debug
{
    // Assertions.
    public static final void assertTrue(boolean b)
    {
        if (!b) {
            Log.error("Assertion failed!");
            AssertionException e = new AssertionException();
            Log.error(e);
            throw e;
        }
    }

    public static final void assertFalse(boolean b)
    {
        if (b) {
            Log.error("Assertion failed!");
            AssertionException e = new AssertionException();
            Log.error(e);
            throw e;
        }
    }

    // Does not throw an exception.
    public static void bug(String s)
    {
        Log.error("BUG! " + s);
        bug();
    }

    // Does not throw an exception.
    public static void bug()
    {
        Log.error(new Exception("BUG!"));
    }

    // A kinder, gentler form of assertion.
    public static void bugIfNot(boolean b)
    {
        if (!b)
            bug();
    }

    public static void bugIf(boolean b)
    {
        if (b)
            bug();
    }

    public static void dumpStack()
    {
        if (Editor.isDebugEnabled())
            Log.debug(new Exception("Stack trace"));
    }

    public static void throttle()
    {
        if (Editor.isDebugEnabled() && !SwingUtilities.isEventDispatchThread()) {
            String throttle = Editor.preferences().getStringProperty("throttle");
            if (throttle != null) {
                try {
                    int delay = Integer.parseInt(throttle);
                    Thread.sleep(delay);
                }
                catch (NumberFormatException e ) {
                    Log.error(e);
                }
                catch (InterruptedException e) {}
            }
        }
    }

    public static void listThreads()
    {
        int threadCount = Thread.currentThread().activeCount();
        Thread[] threads = new Thread[threadCount];
        threadCount = Thread.currentThread().enumerate(threads);
        FastStringBuffer sb = new FastStringBuffer();
        Log.debug("----- listThreads -----");
        for (int i = 0; i < threadCount; i++) {
            Thread thread = threads[i];
            sb.setText(thread.getName());
            sb.append(' '); // Follow with at least one space.
            while (sb.length() < 24)
                sb.append(' ');
            sb.append(thread.getPriority());
            if (thread.isDaemon())
                sb.append(" (daemon)");
            Log.debug(sb.toString());
        }
        int processCount = 0;
        String output = null;
        if (Platform.isPlatformLinux()) {
            String[] cmdarray = {"bash", "-c",
                "ps -o pid,pri,%cpu,rss,start,time,command"};
            output = Utilities.exec(cmdarray);
        } else if (Platform.isPlatformSunOS()) {
            String[] cmdarray = {"sh", "-c",
                "ps -efo pid,pri,pcpu,rss,time,args"};
            output = Utilities.exec(cmdarray);
        }
        if (output != null) {
            FastStringReader reader = new FastStringReader(output);
            String s = reader.readLine();
            if (s != null)
                Log.debug(s);
            while ((s = reader.readLine()) != null) {
                if (s.indexOf("java") >= 0) {
                    Log.debug(s);
                    ++processCount;
                }
            }
        }
        sb.setText("listThreads: ");
        sb.append(threadCount);
        sb.append(" Java threads");
        if (processCount > 0) {
            sb.append(", ");
            sb.append(processCount);
            sb.append(" processes");
        }
        Log.debug(sb.toString());
    }
}
