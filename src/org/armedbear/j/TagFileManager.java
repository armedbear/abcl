/*
 * TagFileManager.java
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

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.Iterator;
import java.util.List;
import java.util.Vector;

public final class TagFileManager extends Thread
{
    // Version of tag file format.
    private static final String VERSION = "1";

    private final File tagFileDir;
    private final TagFileCatalog catalog;

    private Vector queue = new Vector();
    private boolean enabled = true;

    private TagFileCache cache;

    public TagFileManager()
    {
        super("tag file manager");
        setPriority(Thread.MIN_PRIORITY);
        setDaemon(true);
        tagFileDir =
            File.getInstance(Directories.getEditorDirectory(), "tagfiles");
        catalog = new TagFileCatalog(tagFileDir);
        if (initialize())
            start();
    }

    public void run()
    {
        while (true) {
            QueueEntry entry = getEntryFromQueue();
            refreshTagFile(entry);
        }
    }

    private synchronized boolean initialize()
    {
        if (!tagFileDir.isDirectory()) {
            tagFileDir.mkdirs();
            if (!tagFileDir.isDirectory()) {
                Log.error("TagFileManager.run can't make directory ".concat(tagFileDir.canonicalPath()));
                queue = null;
                return false;
            }
        }
        catalog.load();
        cleanup();
        return true;
    }

    public synchronized void addToQueue(File dir, Mode mode)
    {
        if (queue == null)
            return;
        if (dir.isRemote())
            return;
        QueueEntry entry = new QueueEntry(dir, mode);
        for (int i = queue.size()-1; i >= 0; i--) {
            if (entry.equals(queue.get(i)))
                return;
        }
        queue.add(entry);
        notify();
    }

    public synchronized void setEnabled(boolean b)
    {
        boolean wasEnabled = enabled;
        enabled = b;
        if (enabled && !wasEnabled)
            notify();
    }

    private boolean ready()
    {
        return enabled && queue.size() > 0;
    }

    private synchronized QueueEntry getEntryFromQueue()
    {
        while (!ready()) {
            try {
                wait();
            }
            catch (InterruptedException e) {
                Log.error(e);
            }
        }
        return (QueueEntry) queue.remove(0);
    }

    private final File getTagFile(File dir, Mode mode)
    {
        return catalog.getTagFile(dir, mode);
    }

    public void makeTagFile(File dir, Mode mode)
    {
        Debug.assertTrue(mode != null);
        try {
            if (dir.isRemote())
                return;
            File oldTagfile = getTagFile(dir, mode);
            File tagfile = Utilities.getTempFile(tagFileDir);
            if (tagfile != null) {
                String[] files = dir.list();
                if (files != null) {
                    BufferedWriter writer =
                        new BufferedWriter(new OutputStreamWriter(
                            tagfile.getOutputStream()));
                    writer.write(VERSION);
                    writer.write('\n');
                    for (int i = 0; i < files.length; i++) {
                        File file = File.getInstance(dir, files[i]);
                        if (mode.accepts(file.getName()) && file.isFile()) {
                            SystemBuffer buf = new SystemBuffer(file);
                            buf.load();
                            Tagger tagger = mode.getTagger(buf);
                            tagger.run();
                            tagger.writeTags(writer);
                            buf._empty();
                        }
                    }
                    writer.flush();
                    writer.close();
                    if (tagfile.length() == 0) {
                        tagfile.delete();
                    } else {
                        catalog.addEntry(dir, tagfile, mode);
                        catalog.save();
                        if (oldTagfile != null) {
                            oldTagfile.delete();
                            if (cache != null)
                                cache.remove(oldTagfile);
                        }
                    }
                }
            }
        }
        catch (Exception e) {
            Log.error(e);
        }
    }

    private boolean isTagFileOutOfDate(QueueEntry entry)
    {
        if (entry.directory.isRemote())
            return false;
        File tagfile = getTagFile(entry.directory, entry.mode);
        if (tagfile == null)
            return true;
        if (!tagfile.exists())
            return true;
        long tagfileLastModified = tagfile.lastModified();
        String[] files = entry.directory.list();
        if (files != null) {
            for (int i = 0; i < files.length; i++) {
                File file = File.getInstance(entry.directory, files[i]);
                if (!file.isFile())
                    continue;
                if (entry.mode.accepts(file.getName()))
                    if (file.lastModified() > tagfileLastModified)
                        return true;
            }
        }
        return false;
    }

    private void refreshTagFile(QueueEntry queueEntry)
    {
        if (isTagFileOutOfDate(queueEntry))
            makeTagFile(queueEntry.directory, queueEntry.mode);
    }

    private synchronized void cleanup()
    {
        final int days = 5;
        final long cutoff = System.currentTimeMillis() - 24 * 60 * 60 * 1000 * days;
        String[] files = tagFileDir.list();
        for (int i = 0; i < files.length; i++) {
            String name = files[i];
            if (name.equals("catalog")) {
                // It's the catalog file.
                continue;
            }
            File file = File.getInstance(tagFileDir, name);
            if (!file.isFile())
                continue;
            if (catalog.containsTagFileName(name) && file.lastModified() > cutoff)
                continue;
            file.delete();
        }
        catalog.update();
    }

    public List getTags(File directory, Mode mode)
    {
        File tagFile = getTagFile(directory, mode);
        if (tagFile == null) {
            Log.debug("getTags no tag file " + directory + " " + mode);
            return null;
        }
        if (!tagFile.isFile()) {
            Log.debug("getTags tag file doesn't exist");
            return null;
        }
        List tags = null;
        // First checked cached tag files.
        if (cache != null)
            tags = cache.getTags(tagFile);
        if (tags == null) {
            try {
                BufferedReader reader =
                    new BufferedReader(new InputStreamReader(tagFile.getInputStream()));
                String s = reader.readLine();
                if (s != null && s.equals(VERSION)) {
                    tags = new ArrayList();
                    while ((s = reader.readLine()) != null) {
                        GlobalTag tag = GlobalTag.makeGlobalTag(s);
                        if (tag != null)
                            tags.add(tag);
                    }
                } else {
                    Log.warn("getTags wrong version " + directory + " " +
                        mode);
                }
                reader.close();
            }
            catch (IOException e) {
                Log.error(e);
            }
            if (tags != null) {
                if (cache == null)
                    cache = new TagFileCache();
                cache.add(directory, mode.toString(), tagFile, tags);
            } else
                tagFile.delete();
        }
        return tags;
    }

    private static class QueueEntry
    {
        final File directory;
        final Mode mode;

        QueueEntry(File directory, Mode mode)
        {
            this.directory = directory;
            this.mode = mode;
        }

        public boolean equals(Object obj)
        {
            if (this == obj)
                return true;
            if (obj instanceof QueueEntry) {
                QueueEntry qe = (QueueEntry) obj;
                if (!directory.equals(qe.directory))
                    return false;
                // Same directory.
                if (mode == qe.mode)
                    return true;
                if (mode != null && mode.equals(qe.mode))
                    return true;
            }
            return false;
        }
    }

    private static class TagFileCache
    {
        private static final int MAX_FILES = 5;

        private ArrayList list = new ArrayList(MAX_FILES);

        TagFileCache() {}

        synchronized List getTags(File tagFile)
        {
            Iterator iter = list.iterator();
            while (iter.hasNext()) {
                CacheEntry entry = (CacheEntry) iter.next();
                if (entry.tagFile.equals(tagFile)) {
                    entry.lastAccess = System.currentTimeMillis();
                    // Move entry to top of list.
                    ArrayList newList = new ArrayList(MAX_FILES);
                    newList.add(entry);
                    for (int i = 0; i < list.size(); i++) {
                        CacheEntry e = (CacheEntry) list.get(i);
                        if (e != entry)
                            newList.add(e);
                    }
                    Debug.assertTrue(newList.size() == list.size());
                    list = newList;
                    checkOrder();
                    return entry.tags;
                }
            }
            return null;
        }

        synchronized void add(File directory, String modeName,
            File tagFile, List tags)
        {
            CacheEntry entry = new CacheEntry(directory, modeName, tagFile, tags);
            ArrayList newList = new ArrayList(MAX_FILES);
            newList.add(entry);
            int count = 1;
            for (int i = 0; i < list.size() && count < MAX_FILES; i++) {
                CacheEntry e = (CacheEntry) list.get(i);
                if (!e.tagFile.equals(tagFile)) {
                    newList.add(e);
                    ++count;
                }
            }
            list = newList;
            checkOrder();
        }

        synchronized void remove(File tagFile)
        {
            Iterator iter = list.iterator();
            while (iter.hasNext()) {
                CacheEntry entry = (CacheEntry) iter.next();
                if (entry.tagFile.equals(tagFile)) {
                    iter.remove();
                    checkOrder();
                    Log.debug("cache remove size = " + list.size());
                    return;
                }
            }
            // Not found.
        }

        // Only called from synchronized methods.
        void checkOrder()
        {
            if (Editor.isDebugEnabled()) {
                for (int i = 0; i < list.size()-1; i++) {
                    CacheEntry entry1 = (CacheEntry) list.get(i);
                    CacheEntry entry2 = (CacheEntry) list.get(i+1);
                    if (entry1.lastAccess < entry2.lastAccess)
                        Debug.bug();
                }
//                 dump();
            }
        }

        // Only called from synchronized methods.
//         void dump()
//         {
//             for (int i = 0; i < list.size(); i++)
//                 Log.debug(String.valueOf(i) + " " + list.get(i).toString());
//         }
    }

    private static class CacheEntry
    {
        final File directory; // Needed for debugging only!
        final String modeName; // Needed for debugging only!
        final File tagFile;
        List tags;
        long lastAccess; // Needed for debugging only!

        CacheEntry(File directory, String modeName, File tagFile,
            List tags)
        {
            this.directory = directory;
            this.modeName = modeName;
            this.tagFile = tagFile;
            this.tags = tags;
            this.lastAccess = System.currentTimeMillis();
        }

        public String toString()
        {
            return directory.canonicalPath() + " " + modeName + " " +
                String.valueOf(lastAccess);
        }
    }
}
