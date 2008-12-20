/*
 * FtpLoadProcess.java
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

import javax.swing.SwingUtilities;

public final class FtpLoadProcess extends LoadProcess implements BackgroundProcess,
    Constants
{
    private FtpSession session;
    private boolean fileIsDirectory;
    private String listing;

    public FtpLoadProcess(Buffer buffer, FtpFile file, FtpSession session)
    {
        super(buffer, file);
        this.session = session;
    }

    public final String getListing()
    {
        return listing;
    }

    public final boolean fileIsDirectory()
    {
        return fileIsDirectory;
    }

    public void run()
    {
        Debug.assertTrue(buffer != null);
        buffer.setBackgroundProcess(this);
        doLoad();
        buffer.setBackgroundProcess(null);
    }

    private void doLoad()
    {
        Debug.assertTrue(session != null);
        Debug.assertTrue(session.isLocked());
        session.setProgressNotifier(progressNotifier);
        if (!session.verifyConnected()) {
            if (!cancelled) {
                // Error!
                if (errorRunnable != null) {
                    String text = session.getErrorText();
                    if (text == null || text.length() == 0) {
                        text = "Unable to connect to " + file.getHostName() + " on port " + file.getPort();
                    }
                    errorRunnable.setMessage(text);
                    SwingUtilities.invokeLater(errorRunnable);
                }
            }
            return;
        }
        if (file.canonicalPath() == null || file.canonicalPath().equals(""))
            file.setCanonicalPath(session.getLoginDirectory());
        int result = ERROR;
        if (session.isDirectory(file.canonicalPath())) {
            // Directory.
            fileIsDirectory = true;
            listing = session.getDirectoryListing((FtpFile)file);
            if (listing != null)
                result = SUCCESS;
        } else if (session.isFile(file.canonicalPath())) {
            // Normal file.
            listing = session.getDirectoryListingForFile(file.canonicalPath());
            cache = Utilities.getTempFile();
            if (cache != null)
                result = session.get(file, cache, 0);
        } else {
            // File not found.
            if (errorRunnable != null)
                errorRunnable.setMessage("File not found");
        }
        buffer.setBusy(false);
        if (result == SUCCESS) {
            if (successRunnable != null)
                SwingUtilities.invokeLater(successRunnable);
        } else {
            deleteCache();
            if (result == CANCELLED) {
                if (cancelRunnable != null)
                    SwingUtilities.invokeLater(cancelRunnable);
            } else {
                // Error!
                if (errorRunnable != null)
                    SwingUtilities.invokeLater(errorRunnable);
            }
        }
        // And in any case...
        session.unlock();
    }

    private void deleteCache()
    {
        if (cache != null) {
            if (cache.isFile())
                cache.delete();
            cache = null;
        }
    }
}
