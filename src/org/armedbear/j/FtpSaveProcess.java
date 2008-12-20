/*
 * FtpSaveProcess.java
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

import javax.swing.SwingUtilities;

public class FtpSaveProcess implements BackgroundProcess, Constants
{
    private final Buffer buffer;
    private final File source;
    private FtpFile destination;
    private final FtpSession session;

    // Option for saveAs(), saveCopy().
    private boolean confirmOverwrite;

    // Option for save().
    private boolean confirmIfDestinationChanged;

    // Title for message dialogs.
    private String title;

    private Runnable successRunnable;
    private ProgressNotifier progressNotifier;
    private String listing;
    private boolean force;

    public FtpSaveProcess(Buffer buffer, File source, FtpFile destination,
                          FtpSession session)
    {
        this.buffer = buffer;
        Debug.assertTrue(buffer != null);
        this.source = source;
        Debug.assertTrue(source != null);
        this.destination = destination;
        Debug.assertTrue(destination != null);
        this.session = session;
        Debug.assertTrue(session != null);
        Debug.assertTrue(session.isLocked());
    }

    public final void setConfirmOverwrite(boolean b)
    {
        confirmOverwrite = b;
    }

    public final void setConfirmIfDestinationChanged(boolean b)
    {
        confirmIfDestinationChanged = b;
    }

    public final void setTitle(String s)
    {
        title = s;
    }

    public final void setSuccessRunnable(Runnable r)
    {
        successRunnable = r;
    }

    public final String getListing()
    {
        return listing;
    }

    public void start()
    {
        Debug.assertTrue(SwingUtilities.isEventDispatchThread());
        if (!buffer.isLocked()) {
            Log.debug("start() buffer is not locked");
            if (!buffer.lock()) {
                MessageDialog.showMessageDialog("Buffer is busy", buffer.getFile().netPath());
                session.unlock();
                return;
            }
            Log.debug("start() buffer locked OK");
        }
        Debug.assertTrue(buffer.isLocked());
        buffer.setBusy(true);
        for (EditorIterator it = new EditorIterator(); it.hasNext();) {
            Editor ed = it.nextEditor();
            if (ed.getBuffer() == buffer)
                ed.setWaitCursor();
        }
        new Thread(this).start();
    }

    public void run()
    {
        Debug.assertTrue(buffer.isLocked());
        try {
            buffer.setBackgroundProcess(this);
            if (source.isFile())
                doSave();
            buffer.setBackgroundProcess(null);
        }
        finally {
            buffer.unlock();
        }
    }

    public void doSave()
    {
        Log.debug("doSave force = " + force);
        Debug.assertTrue(buffer != null);
        Debug.assertTrue(source != null);
        Debug.assertTrue(!SwingUtilities.isEventDispatchThread());

        final String hostName = destination.getHostName();
        progressNotifier = new StatusBarProgressNotifier(buffer);
        session.setProgressNotifier(progressNotifier);
        if (!session.verifyConnected()) {
            String message = session.getErrorText();
            if (message == null)
                message = "Unable to connect to " + hostName + " on port " + destination.getPort();
            errorRunnable.setMessage(message);
            buffer.setBusy(false);
            SwingUtilities.invokeLater(errorRunnable);
            session.unlock();
            return;
        }
        final boolean destinationExists = session.exists(destination.canonicalPath());
        int permissions = destinationExists ? session.getPermissions(destination) : 0;
        if (!force) {
            if (confirmOverwrite) {
                // Check to see if destination file exists on remote host.
                if (session.exists(destination.canonicalPath())) {
                    // Confirm before overwriting. Leave the session
                    // locked for now.
                    SwingUtilities.invokeLater(confirmOverwriteRunnable);
                    return;
                }
            }
            if (confirmIfDestinationChanged && destinationExists) {
                // Check to see if destination file has changed on remote host.
                String s = session.getDirectoryListingForFile(destination.canonicalPath());
                if (!s.equals(buffer.getListing())) {
                    // Destination file has changed since it was loaded. Leave
                    // the session locked for now.
                    SwingUtilities.invokeLater(confirmDestinationChangedRunnable);
                    return;
                }
            }
        }
        final boolean saveInPlace = buffer.getBooleanProperty(Property.SAVE_IN_PLACE);
        int result = session.put(source, destination, source.length(), saveInPlace);
        if (result == SUCCESS) {
            if (permissions != 0 && !saveInPlace)
                session.chmod(destination, permissions);
            listing = session.getDirectoryListingForFile(destination.canonicalPath());
            buffer.setBusy(false);
            if (successRunnable != null)
                SwingUtilities.invokeLater(successRunnable);
        } else if (result == CANCELLED) {
            SwingUtilities.invokeLater(cancelRunnable);
        } else {
            // Error.
            buffer.setBusy(false);
            if (errorRunnable != null) {
                errorRunnable.setMessage(session.getErrorText());
                SwingUtilities.invokeLater(errorRunnable);
            }
        }
        // And in any case...
        session.unlock();
    }

    public synchronized void cancel()
    {
        if (progressNotifier != null) {
            progressNotifier.cancel();
            progressNotifier.progressStop();
            progressNotifier.setText("Transfer cancelled, cleaning up...");
        }
    }

    // Confirm overwrite of existing destination file for saveAs() and
    // saveCopy().
    private final Runnable confirmOverwriteRunnable = new Runnable() {
        public void run()
        {
            Debug.assertTrue(SwingUtilities.isEventDispatchThread());
            Debug.assertTrue(session.isLocked());
            final Editor editor = Editor.currentEditor();
            editor.setDefaultCursor();
            String text = "Overwrite existing file " + destination.getName() + " on " + destination.getHostName() + "?";
            final boolean confirmed = editor.confirm(title, text);
            if (confirmed) {
                force = true;
                start();
            } else {
                buffer.setBusy(false);
                session.unlock();
            }
        }
    };

    // Confirm save if destination file has changed on the remote host since
    // it was loaded.
    private final Runnable confirmDestinationChangedRunnable = new Runnable() {
        public void run()
        {
            Debug.assertTrue(SwingUtilities.isEventDispatchThread());
            final Editor editor = Editor.currentEditor();
            editor.setDefaultCursor();
            String text = destination.getName() + " has changed on " + destination.getHostName() + ". Save anyway?";
            final boolean confirmed = editor.confirm(title, text);
            if (confirmed) {
                force = true;
                start();
            } else {
                buffer.setBusy(false);
                session.unlock();
            }
        }
    };

    private final Runnable cancelRunnable = new Runnable() {
        public void run()
        {
            buffer.setBusy(false);
            for (EditorIterator it = new EditorIterator(); it.hasNext();) {
                Editor ed = it.nextEditor();
                if (ed.getBuffer() == buffer) {
                    ed.status("Transfer cancelled");
                    ed.setDefaultCursor();
                }
            }
            MessageDialog.showMessageDialog("Transfer cancelled", title);
        }
    };

    private final ErrorRunnable errorRunnable = new ErrorRunnable("Save failed");
}
