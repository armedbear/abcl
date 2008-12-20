/*
 * Autosave.java
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

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.Enumeration;
import java.util.Properties;

public final class Autosave implements Constants
{
    private static final String CATALOG_NAME = "catalog";
    private static Properties catalog;
    private static File catalogFile;

    private static boolean autosaveEnabled = true;
    private static File autosaveDirectory;
    private static boolean initialized;

    public static synchronized File getAutosaveDirectory()
    {
        if (!initialized) {
            autosaveDirectory =
                File.getInstance(Directories.getEditorDirectory(), "autosave");
            if (autosaveDirectory != null) {
                if (!autosaveDirectory.isDirectory()) {
                    autosaveDirectory.mkdirs();
                    if (!autosaveDirectory.isDirectory()) {
                        autosaveDirectory = null;
                        autosaveEnabled = false;
                    }
                }
            }
            initialized = true;
        }
        return autosaveDirectory;
    }

    public static synchronized final boolean isAutosaveEnabled()
    {
        return autosaveEnabled;
    }

    public static synchronized void put(String netPath, String alias)
    {
        if (catalog == null)
            catalog = new Properties();
        catalog.put(netPath, alias);
    }

    // Update catalog file when a buffer is renamed.
    public static synchronized void rename(String oldName, String newName)
    {
        if (catalog != null && oldName != null) {
            String alias = (String) catalog.remove(oldName);
            if (alias != null) {
                catalog.put(newName, alias);
                flush();
            }
        }
    }

    public static synchronized void flush()
    {
        if (catalogFile == null) {
            if (getAutosaveDirectory() == null)
                return;
            catalogFile = File.getInstance(getAutosaveDirectory(), CATALOG_NAME);
        }
        try {
            OutputStream out = catalogFile.getOutputStream();
            catalog.store(out, null);
            out.flush();
            out.close();
        }
        catch (IOException e) {
            Log.error(e);
        }
    }

    public static synchronized void deleteCatalogFile()
    {
        if (catalogFile == null) {
            if (getAutosaveDirectory() == null)
                return;
            catalogFile = File.getInstance(getAutosaveDirectory(), CATALOG_NAME);
        }
        catalogFile.delete();
    }

    public static synchronized void recover()
    {
        if (catalogFile == null) {
            if (getAutosaveDirectory() == null)
                return;
            catalogFile = File.getInstance(getAutosaveDirectory(), CATALOG_NAME);
        }
        if (!catalogFile.exists())
            return; // No catalog file.
        if (catalog == null)
            catalog = new Properties();
        try {
            InputStream in = catalogFile.getInputStream();
            catalog.load(in);
            in.close();
        }
        catch (IOException e) {
            Log.error(e);
        }
        Enumeration e = catalog.propertyNames();
        while (e.hasMoreElements()) {
            String netPath = (String) e.nextElement();
            String alias = catalog.getProperty(netPath);
            if (alias != null)
                queryRecoverFile(netPath, alias);
        }
        catalogFile.delete();
    }

    private static void queryRecoverFile(String netPath, String alias)
    {
        File autosaveFile = File.getInstance(getAutosaveDirectory(), alias);
        if (!autosaveFile.exists()) {
            // Nothing we can do.
            catalog.remove(netPath);
            return;
        }
        final File file = File.getInstance(netPath);
        if (file == null)
            return; // Shouldn't happen.
        boolean confirmed = false;
        File dir = file.getParentFile();
        if (dir != null && dir.equals(Directories.getDraftsFolder()))
            confirmed = true;
        if (!confirmed) {
            String prompt = "Recover " + netPath + " from autosave file?";
            int response = ConfirmDialog.showConfirmDialog(null, prompt, "Autosave");
            if (response == RESPONSE_YES)
                confirmed = true;
        }
        if (confirmed) {
            if (file.isRemote()) {
                String recoverPath = file.getHostName() + '/' + file.canonicalPath();
                Log.debug("recoverPath = |" + recoverPath + "|");
                File recoverFile = File.getInstance(getRecoverDirectory(), recoverPath);
                Log.debug("recoverFile = |" + recoverFile.netPath() + "|");
                File parentDir = recoverFile.getParentFile();
                if (!parentDir.isDirectory())
                    parentDir.mkdirs();
                if (parentDir.isDirectory()) {
                    recoverFile.delete();
                    // BUG! Error handling! What if rename and copy both fail?
                    if (autosaveFile.renameTo(recoverFile) || Utilities.copyFile(autosaveFile, recoverFile)) {
                        autosaveFile.delete();
                        catalog.remove(netPath);
                        String message = "File has been saved as " + recoverFile.canonicalPath() + ".";
                        MessageDialog.showMessageDialog(message, "Autosave");
                    }
                }
            } else {
                // Trick it into loading the contents from the autosave file
                //  by pretending to be a cache file.
                Buffer buf = Buffer.createBuffer(file, autosaveFile, null);
                if (buf != null) {
                    // Mark as changed!
                    buf.incrementModCount();
                    // Now that the buffer owns the autosave, we can remove the
                    //  autosave from the catalog.  (Even if we didn't, the
                    //  catalog file would still get deleted...)
                    catalog.remove(netPath);
                }
            }
        } else {
            String prompt = "Delete autosave file for " + netPath + "?";
            int response = ConfirmDialog.showConfirmDialog(null, prompt, "Autosave");
            if (response == RESPONSE_YES) {
                autosaveFile.delete();
                catalog.remove(netPath);
            }
        }
    }

    private static final File getRecoverDirectory()
    {
        return File.getInstance(Directories.getEditorDirectory(), "recover");
    }
}
