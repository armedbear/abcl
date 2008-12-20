/*
 * RecentFiles.java
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
import java.util.List;

public final class RecentFiles implements Constants
{
    private static final int MAX_ENTRIES = 100;

    // Singleton.
    private static RecentFiles instance;

    private final ArrayList entries = new ArrayList();
    private final File file;
    private int version;
    private boolean changed;

    private RecentFiles()
    {
        file = File.getInstance(Directories.getEditorDirectory(), "recent");
        if (file != null && file.isFile())
            load();
    }

    public static synchronized RecentFiles getInstance()
    {
        if (instance == null)
            Editor.protect(instance = new RecentFiles());
        return instance;
    }

    public synchronized final List getEntries()
    {
        return entries;
    }

    private RecentFilesEntry findEntry(File file)
    {
        if (file != null) {
            final int limit = entries.size();
            for (int i = 0; i < limit; i++) {
                RecentFilesEntry entry = (RecentFilesEntry) entries.get(i);
                if (entry.matches(file))
                    return entry;
            }
        }
        return null;
    }

    private static boolean excluded(Buffer buffer)
    {
        if (buffer.isTransient())
            return true;
        if (buffer instanceof Directory)
            return true;
        if (buffer instanceof RemoteBuffer)
            return true;
        if (buffer.getModeId() == MAN_MODE)
            return true;
        if (buffer instanceof ImageBuffer)
            return true;
        if (buffer.getModeId() == SEND_MAIL_MODE)
            return true;
        if (buffer.isUntitled())
            return true;
        File file = buffer.getFile();
        if (file == null)
            return true;
        File parent = file.getParentFile();
        if (parent != null && parent.equals(Directories.getTempDirectory()))
            return true;
        return false;
    }

    public synchronized void bufferActivated(Buffer buffer)
    {
        if (excluded(buffer))
            return;
        File file = buffer.getFile();
        if (file != null) {
            long now = System.currentTimeMillis();
            RecentFilesEntry entry = findEntry(file);
            if (entry == null)
                entry = new RecentFilesEntry(file);
            if (entry.firstVisit == 0)
                entry.firstVisit = now;
            entry.lastVisit = now;
            store(entry);
            changed = true;
        }
    }

    public synchronized void bufferDeactivated(Buffer buffer, Position dot)
    {
        if (dot == null)
            return;
        if (excluded(buffer))
            return;
        File file = buffer.getFile();
        if (file != null) {
            RecentFilesEntry entry = findEntry(file);
            if (entry != null) {
                entry.lineNumber = dot.lineNumber();
                entry.offs = dot.getOffset();
                store(entry);
                changed = true;
            }
        }
    }

    private void store(RecentFilesEntry newEntry)
    {
        Debug.assertTrue(newEntry != null);
        final int limit = entries.size();
        for (int i = 0; i < limit; i++) {
            RecentFilesEntry entry = (RecentFilesEntry) entries.get(i);
            if (entry.equals(newEntry)) {
                if (i == 0) {
                    entries.set(0, newEntry);
                    return;
                }
                entries.remove(i);
                break;
            }
        }
        // Add new entry.
        entries.add(0, newEntry);
    }

    private void load()
    {
        if (file == null)
            return;
        if (!file.isFile())
            return;
        Debug.assertTrue(entries.size() == 0);
        try {
            BufferedReader reader =
                new BufferedReader(new InputStreamReader(file.getInputStream()));
            String s = reader.readLine();
            if (s != null) {
                try {
                    version = Integer.parseInt(s);
                }
                catch (NumberFormatException e) {}
                if (version == RecentFilesEntry.getVersion()) {
                    while ((s = reader.readLine()) != null) {
                        try {
                            // Constructor will throw exception if string is invalid.
                            RecentFilesEntry entry = new RecentFilesEntry(s);
                            entries.add(entry);
                        }
                        catch (Exception e) {
                            Log.error("malformed recent files entry");
                        }
                    }
                }
            }
            reader.close();
        }
        catch (IOException e) {
            Log.error(e);
        }
        if (entries.size() == 0)
            file.delete();
    }

    public synchronized void save()
    {
        if (!changed)
            return;
        if (file == null)
            return;
        if (entries.size() == 0)
            return;
        try {
            BufferedWriter writer =
                new BufferedWriter(new OutputStreamWriter(file.getOutputStream()));
            writer.write(String.valueOf(RecentFilesEntry.getVersion()));
            writer.newLine();
            int limit = entries.size();
            if (limit > MAX_ENTRIES)
                limit = MAX_ENTRIES;
            for (int i = 0; i < limit; i++) {
                RecentFilesEntry entry = (RecentFilesEntry) entries.get(i);
                writer.write(entry.toString());
                writer.newLine();
            }
            writer.flush();
            writer.close();
            changed = false;
        }
        catch (Exception e) {
            Log.error(e);
        }
    }
}
