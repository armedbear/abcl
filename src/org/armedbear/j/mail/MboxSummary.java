/*
 * MboxSummary.java
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

package org.armedbear.j.mail;

import java.io.BufferedInputStream;
import java.io.InvalidClassException;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;
import org.armedbear.j.File;
import org.armedbear.j.Log;
import org.armedbear.j.Utilities;

public final class MboxSummary implements Serializable
{
    private final ArrayList entries;
    private final String path;
    private long lastModified;
    private long length;

    public MboxSummary(File mailboxFile, List entries)
    {
        this.entries = new ArrayList(entries);
        path = mailboxFile.canonicalPath();
        lastModified = mailboxFile.lastModified();
        length = mailboxFile.length();
    }

    public synchronized ArrayList getEntries()
    {
        return entries;
    }

    public synchronized long length()
    {
        return length;
    }

    public synchronized long lastModified()
    {
        return lastModified;
    }

    public synchronized void write(File file)
    {
        try {
            Log.debug("MboxSummary.write");
            long start = System.currentTimeMillis();
            File temp = Utilities.getTempFile();
            ObjectOutputStream objectOut = new ObjectOutputStream(temp.getOutputStream());
            objectOut.writeObject(this);
            objectOut.flush();
            objectOut.close();
            Utilities.deleteRename(temp, file);
            long elapsed = System.currentTimeMillis() - start;
            Log.debug("MboxSummary.write completed " + elapsed + " ms");
        }
        catch (Exception e) {
            Log.error(e);
        }
    }

    public static MboxSummary read(File file)
    {
        Log.debug("MboxSummary.read");
        if (file == null || !file.isFile())
            return null;
        ObjectInputStream in = null;
        try {
            in = new ObjectInputStream(new BufferedInputStream(file.getInputStream()));
            MboxSummary summary = (MboxSummary) in.readObject();
            File mailboxFile = File.getInstance(summary.path);
            if (mailboxFile != null && mailboxFile.isFile()) {
                if (summary.length == mailboxFile.length())
                    if (summary.lastModified == mailboxFile.lastModified())
                        return summary;
            }
        }
        catch (InvalidClassException e) {
            // Expected if an incompatible change has been made to the
            // serialized class. No big deal.
            Log.debug(e);
        }
        catch (Exception e) {
            Log.error(e);
        }
        finally {
            if (in != null) {
                try {
                    in.close();
                }
                catch (IOException e) {
                    Log.error(e);
                }
            }
        }
        return null;
    }
}
