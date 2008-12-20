/*
 * TagFileCatalog.java
 *
 * Copyright (C) 1998-2002 Peter Graves
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
import java.util.Date;

public final class TagFileCatalog
{
    private File tagfileDir;
    private File catalogFile;
    private ArrayList entries = new ArrayList();

    public TagFileCatalog(File tagfileDir)
    {
        this.tagfileDir = tagfileDir;
        catalogFile = File.getInstance(tagfileDir, "catalog");
    }

    public synchronized void addEntry(File dir, File tagfile, Mode mode)
    {
        String directoryPath = dir.canonicalPath();
        String modeName = mode.toString();
        for (int i = entries.size()-1; i >= 0; i--) {
            CatalogEntry entry = getEntry(i);
            if (entry.directoryPath.equals(directoryPath)) {
                if (entry.modeName == null) {
                    entry.tagfileName = tagfile.getName();
                    entry.modeName = modeName;
                    return;
                } else if (entry.modeName.equals(modeName)) {
                    entry.tagfileName = tagfile.getName();
                    return;
                }
            }
        }
        // Not found.
        entries.add(new CatalogEntry(directoryPath, modeName, tagfile.getName()));
    }

    public synchronized boolean containsTagFileName(String name)
    {
        for (int i = entries.size()-1; i >= 0; i--) {
            CatalogEntry entry = getEntry(i);
            if (entry.tagfileName.equals(name))
                return true;
        }
        // Not found.
        return false;
    }

    public synchronized File getTagFile(File dir, Mode mode)
    {
        Debug.assertTrue(mode != null);
        String directoryPath = dir.canonicalPath();
        String modeName = mode.toString();
        for (int i = entries.size()-1; i >= 0; i--) {
            CatalogEntry entry = getEntry(i);
            if (directoryPath.equals(entry.directoryPath) && modeName.equals(entry.modeName))
                return File.getInstance(tagfileDir, entry.tagfileName);
        }
        // Not found.
        return null;
    }

    public synchronized void update()
    {
        boolean changed = false;
        for (int i = entries.size()-1; i >= 0; i--) {
            CatalogEntry entry = getEntry(i);
            File file = File.getInstance(tagfileDir, entry.tagfileName);
            if (!file.exists()) {
                entries.remove(i);
                changed = true;
            }
        }
        if (changed)
            save();
    }

    public synchronized void load()
    {
        try {
            if (catalogFile.isFile()) {
                BufferedReader reader =
                    new BufferedReader(new InputStreamReader(catalogFile.getInputStream()));
                String s;
                while ((s = reader.readLine()) != null) {
                    if (s.trim().startsWith("#"))
                        continue;
                    int i = s.indexOf('\t');
                    if (i < 0) {
                        // Old or invalid format.
                        Log.error("TagFileCatalog.load old or invalid format");
                        entries.clear();
                        break;
                    }
                    String tagFileName = s.substring(0, i);
                    String remaining = s.substring(i+1).trim();
                    i = remaining.indexOf('\t');
                    if (i < 0) {
                        // Invalid format.
                        Log.error("TagFileCatalog.load invalid format");
                        entries.clear();
                        break;
                    }
                    String modeName = remaining.substring(0, i);
                    String directoryPath = remaining.substring(i+1).trim();
                    entries.add(new CatalogEntry(directoryPath, modeName, tagFileName));
                }
                reader.close();
            }
        }
        catch (IOException e) {
            Log.error(e);
        }
    }

    public synchronized void save()
    {
        try {
            BufferedWriter writer =
                new BufferedWriter(new OutputStreamWriter(catalogFile.getOutputStream()));
            writer.write("# " + new Date().toString() + '\n');
            final int limit = entries.size();
            for (int i = 0; i < limit; i++) {
                CatalogEntry entry = getEntry(i);
                writer.write(entry.tagfileName);
                writer.write('\t');
                writer.write(entry.modeName);
                writer.write('\t');
                writer.write(entry.directoryPath);
                writer.write('\n');
            }
            writer.flush();
            writer.close();
        }
        catch (IOException e) {
            Log.error(e);
        }
    }

    private final CatalogEntry getEntry(int i)
    {
        return (CatalogEntry) entries.get(i);
    }

    private static class CatalogEntry
    {
        final String directoryPath;
        String modeName;
        String tagfileName;

        CatalogEntry(String directoryPath, String modeName, String tagfileName)
        {
            this.directoryPath = directoryPath;
            this.modeName = modeName;
            this.tagfileName = tagfileName;
        }

        public String toString()
        {
            return tagfileName + " " + directoryPath + " " + modeName;
        }
    }
}
