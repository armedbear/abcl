/*
 * Directories.java
 *
 * Copyright (C) 1998-2007 Peter Graves
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
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
 */

package org.armedbear.j;

public final class Directories
{
    private static File userHomeDirectory;      // ~
    private static File editorDirectory;        // ~/.j
    private static File tempDirectory;          // ~/.j/temp
    private static File mailDirectory;          // ~/.j/mail
    private static File draftsFolder;           // ~/.j/mail/local/drafts
    private static File registersDirectory;     // ~/.j/registers

    public static void initialize(File userHome)
    {
        userHomeDirectory = userHome;
        if (userHomeDirectory == null) {
            // Home directory was not specified on the command line.
            if (Platform.isPlatformWindows()) {
                // Look for existing .j directory.
                FastStringBuffer sb = new FastStringBuffer("C:\\");
                for (char c = 'C'; c <= 'Z'; c++) {
                    sb.setCharAt(0, c);
                    File dir = File.getInstance(sb.toString());
                    if (dir != null && dir.isDirectory() && dir.canWrite()) {
                        File subdir = File.getInstance(dir, ".j");
                        if (subdir != null && subdir.isDirectory())
                            userHomeDirectory = dir;
                        break;
                    }
                }
                if (userHomeDirectory == null)
                    // No existing .j directory.
                    userHomeDirectory = File.getInstance(System.getenv("APPDATA"));
            } else {
                // Not Windows.
                userHomeDirectory =
                    File.getInstance(System.getProperty("user.home"));
            }
            if (userHomeDirectory == null)
                Editor.fatal("Use \"--home\" option to specify home directory.");
        }
        // Make sure required directories exist and are writable.
        editorDirectory =
            provideDirectory(File.getInstance(userHomeDirectory, ".j"));
        tempDirectory =
            provideDirectory(File.getInstance(editorDirectory, "temp"));
        mailDirectory =
            provideDirectory(File.getInstance(editorDirectory, "mail"));
        draftsFolder =
            provideDirectory(File.getInstance(mailDirectory, "local/drafts"));
        registersDirectory =
            provideDirectory(File.getInstance(editorDirectory, "registers"));
        // Avoid garbage collection.
        Editor.protect(Directories.class);
    }

    private static File provideDirectory(final File dir)
    {
        if (dir == null)
            return null;
        if (!dir.isDirectory()) {
            dir.mkdirs();
            if (!dir.isDirectory())
                Editor.fatal("Unable to create directory " + dir);
        }
        if (!dir.canWrite())
            Editor.fatal("The directory " + dir + " is not writable");
        return dir;
    }

    public static final File getUserHomeDirectory()
    {
        return userHomeDirectory;
    }

    public static File getEditorDirectory()
    {
        return editorDirectory;
    }

    public static File getTempDirectory()
    {
        return tempDirectory;
    }

    public static final File getMailDirectory()
    {
        return mailDirectory;
    }

    public static final File getDraftsFolder()
    {
        return draftsFolder;
    }

    public static final File getRegistersDirectory()
    {
        return registersDirectory;
    }

    public static final void cleanTempDirectory()
    {
        if (tempDirectory != null && tempDirectory.isDirectory()) {
            String[] files = tempDirectory.list();
            for (int i = files.length; i-- > 0;)
                File.getInstance(tempDirectory, files[i]).delete();
        }
    }

    public static final void moveUnsentMessagesToDraftsFolder()
    {
        File unsentMessagesDirectory =
            File.getInstance(mailDirectory, "unsent");
        if (unsentMessagesDirectory == null) {
            Debug.bug();
            return;
        }
        if (!unsentMessagesDirectory.isDirectory())
            return; // Nothing to do.
        String[] files = unsentMessagesDirectory.list();
        if (files.length == 0) {
            unsentMessagesDirectory.delete();
            return;
        }
        File draftsFolder = getDraftsFolder();
        if (draftsFolder == null) {
            Debug.bug();
            return;
        }
        Log.info("moving unsent messages to drafts folder...");
        if (!draftsFolder.isDirectory()) {
            draftsFolder.mkdirs();
            if (!draftsFolder.isDirectory()) {
                Log.error("unable to create directory " + draftsFolder);
                return;
            }
        }
        if (!draftsFolder.canWrite()) {
            Log.error(draftsFolder.netPath() + " is not writable");
        }
        for (int i = 0; i < files.length; i++) {
            File source =
                File.getInstance(unsentMessagesDirectory, files[i]);
            File destination =
                File.getInstance(draftsFolder, files[i]);
            Log.debug("moving " + source + " to " + destination);
            if (!source.renameTo(destination))
                Log.error("error moving " + source + " to " + destination);
        }
        files = unsentMessagesDirectory.list();
        if (files.length == 0) {
            Log.debug("removing empty directory " + unsentMessagesDirectory);
            unsentMessagesDirectory.delete();
        } else
            Log.debug("not removing directory " + unsentMessagesDirectory +
                " (directory is not empty)");
    }
}
