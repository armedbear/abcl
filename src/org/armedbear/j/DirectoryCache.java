/*
 * DirectoryCache.java
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

import java.util.Iterator;
import java.util.Vector;

public final class DirectoryCache
{
    private static final int timeout = 1800000; // 30 minutes

    private static DirectoryCache cache;

    private Vector entries = new Vector();

    public static synchronized DirectoryCache getDirectoryCache()
    {
        if (cache == null) {
            cache = new DirectoryCache();
            IdleThread idleThread = IdleThread.getInstance();
            if (idleThread != null)
                idleThread.maybeAddTask(PruneDirectoryCacheTask.getInstance());
        }
        return cache;
    }

    public synchronized String getListing(File file)
    {
        String netPath = file.netPath();
        for (int i = entries.size(); i-- > 0;) {
            DirectoryCacheEntry entry = (DirectoryCacheEntry) entries.get(i);
            if (entry.getFile().netPath().equals(netPath)) {
                if (entry.getWhen() + timeout < System.currentTimeMillis()) {
                    Log.debug("removing cache entry for " + entry.getFile().netPath());
                    entries.remove(i);
                    return null;
                }
                return entry.getListing();
            }
        }
        return null;
    }

    public synchronized void put(File file, String listing)
    {
        String netPath = file.netPath();
        for (int i = entries.size(); i-- > 0;) {
            DirectoryCacheEntry entry = (DirectoryCacheEntry) entries.get(i);
            if (entry.getFile().netPath().equals(netPath)) {
                entries.remove(i);
                break;
            }
        }
        if (listing != null && listing.length() > 0) {
            entries.add(new DirectoryCacheEntry(file, listing,
                System.currentTimeMillis()));
        }
    }

    public synchronized void purge(String hostname)
    {
        for (int i = entries.size(); i-- > 0;) {
            DirectoryCacheEntry entry = (DirectoryCacheEntry) entries.get(i);
            if (entry.getFile().getHostName().equals(hostname)) {
                Log.debug("removing cache entry for " + entry.getFile().netPath());
                entries.remove(i);
            }
        }
    }

    public synchronized void purge(File file)
    {
        String netPath = file.netPath();
        for (int i = entries.size(); i-- > 0;) {
            DirectoryCacheEntry entry = (DirectoryCacheEntry) entries.get(i);
            if (entry.getFile().netPath().equals(netPath)) {
                Log.debug("removing cache entry for " + netPath);
                entries.remove(i);
            }
        }
    }

    private static class PruneDirectoryCacheTask extends IdleThreadTask
    {
        private static PruneDirectoryCacheTask instance;

        private long lastRun;

        private PruneDirectoryCacheTask()
        {
            lastRun = System.currentTimeMillis();
            setIdle(300000); // User must be idle for 5 minutes.
            setRunnable(runnable);
        }

        private static synchronized PruneDirectoryCacheTask getInstance()
        {
            if (instance == null)
                instance = new PruneDirectoryCacheTask();
            return instance;
        }

        private final Runnable runnable = new Runnable() {
            public void run()
            {
                // Only check every 5 minutes.
                if (System.currentTimeMillis() - lastRun > 300000) {
                    long now = System.currentTimeMillis();
                    synchronized (cache) {
                        Iterator it = cache.entries.iterator();
                        while (it.hasNext()) {
                            DirectoryCacheEntry entry =
                                (DirectoryCacheEntry) it.next();
                            if (entry.getWhen() + timeout < now)
                                it.remove();
                        }
                    }
                    lastRun = now;
                }
            }
        };
    }
}
