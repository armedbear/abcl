/*
 * Cache.java
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

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.net.HttpURLConnection;
import java.net.URL;
import java.util.Vector;

public final class Cache
{
    private static final File cacheDir =
        File.getInstance(Directories.getEditorDirectory(), "cache");

    private static Cache cache;

    private File catalogFile;
    private Vector catalog;

    private Cache()
    {
    }

    public static Cache getCache()
    {
        if (cache == null) {
            cache = new Cache();
            if (!cache.initialize())
                cache = null;
        }
        return cache;
    }

    // Currently this just deletes everything in the cache directory.
    // Called from Editor.maybeExit.
    public static void cleanup()
    {
        if (cacheDir.isDirectory()) {
            String[] files = cacheDir.list();
            for (int i = files.length-1; i >= 0; i--) {
                File file = File.getInstance(cacheDir, files[i]);
                file.delete();
            }
        }
    }

    private boolean initialize()
    {
        if (!cacheDir.isDirectory())
            cacheDir.mkdirs();
        if (!cacheDir.isDirectory())
            return false;
        catalogFile = File.getInstance(cacheDir, "catalog");
        catalog = loadCatalog();
        return true;
    }

    public File get(String netPath)
    {
        for (int i = catalog.size()-1; i >= 0; i--) {
            StringPair pair = (StringPair) catalog.get(i);
            if (pair.second.equals(netPath))
                return File.getInstance(cacheDir, pair.first);
        }
        return null;
    }

    public File put(String netPath)
    {
        File file = null;
        try {
            URL url = new URL(netPath);
            HttpURLConnection connection =
                (HttpURLConnection) url.openConnection();
            InputStream in = connection.getInputStream();
            if (in != null) {
                file = Utilities.getTempFile(cacheDir);
                OutputStream out = file.getOutputStream();
                byte[] buf = new byte[4096];
                int bytesRead;
                while ((bytesRead = in.read(buf)) > 0)
                    out.write(buf, 0, bytesRead);
                out.close();
                in.close();
            }
        }
        catch (IOException e) {
            Log.error(e);
            if (file.exists())
                file.delete();
            file = null;
        }
        if (file != null) {
            catalog.add(new StringPair(file.getName(), netPath));
            saveCatalog();
        }
        return file;
    }

    private Vector loadCatalog()
    {
        Vector v = new Vector();
        if (catalogFile.exists()) {
            try {
                BufferedReader reader = new BufferedReader(
                    new InputStreamReader(catalogFile.getInputStream()));
                String s;
                while ((s = reader.readLine()) != null) {
                    int index = s.indexOf(' ');
                    if (index >= 0)
                        v.add(new StringPair(s.substring(0, index), s.substring(index+1)));
                }
            }
            catch (IOException e) {
                Log.error(e);
            }
        }
        return v;
    }

    private void saveCatalog()
    {
        try {
            BufferedWriter writer = new BufferedWriter(
                new OutputStreamWriter(catalogFile.getOutputStream()));
            for (int i = 0; i < catalog.size(); i++) {
                StringPair pair = (StringPair) catalog.get(i);
                writer.write(pair.first);
                writer.write(' ');
                writer.write(pair.second);
                writer.newLine();
            }
            writer.flush();
            writer.close();
        }
        catch (IOException e) {
            Log.error(e);
        }
    }
}
