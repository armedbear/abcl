/*
 * RemoteBuffer.java
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

import javax.swing.SwingUtilities;

public final class RemoteBuffer extends Buffer implements Constants
{
    private FtpSession session;
    private Buffer buffer;
    private final ProgressNotifier progressNotifier;
    private String ref;
    private boolean render;
    private HttpLoadProcess httpLoadProcess;
    private FtpLoadProcess ftpLoadProcess;
    private SshLoadProcess sshLoadProcess;

    public RemoteBuffer(File file)
    {
        super();
        initializeUndo();
        setFile(file);
        type = TYPE_NORMAL;
        autosaveEnabled = false;
        readOnly = true;
        progressNotifier = new StatusBarProgressNotifier(this);
        setProperty(Property.VERTICAL_RULE, 0);
        setProperty(Property.SHOW_LINE_NUMBERS, false);
    }

    public RemoteBuffer(HttpFile file, String ref)
    {
        this(file);
        this.ref = ref;
        render = true;
    }

    public RemoteBuffer(FtpFile file, FtpSession session)
    {
        this(file);
        this.session = session;
    }

    private int initialLineNumber;
    private int initialOffset;

    public void setInitialDotPos(int lineNumber, int offset)
    {
        // We just want to store the paraemters we're called with here, so we
        // can call setInitialDotPos() on the "real" buffer later.
        initialLineNumber = lineNumber;
        initialOffset = offset;
    }

    public int load()
    {
        setLoaded(true);
        mode = Editor.getModeList().getMode(PLAIN_TEXT_MODE);
        formatter = mode.getFormatter(this);

        final File file = getFile();
        if (file instanceof FtpFile) {
            if (session == null) {
                Debug.bug("RemoteBuffer.load session is null");
                session = FtpSession.getSession((FtpFile) file);
                if (session == null)
                    return LOAD_FAILED; // Report error!
            }
            setBusy(true);
            ftpLoadProcess = new FtpLoadProcess(this, (FtpFile)file, session);
            ftpLoadProcess.setProgressNotifier(progressNotifier);
            ftpLoadProcess.setSuccessRunnable(ftpLoadSuccessRunnable);
            ftpLoadProcess.setErrorRunnable(ftpLoadErrorRunnable);
            ftpLoadProcess.start();
            return LOAD_PENDING;
        } else if (file instanceof HttpFile) {
            setBusy(true);
            httpLoadProcess = new HttpLoadProcess(this, (HttpFile)file);
            httpLoadProcess.setProgressNotifier(progressNotifier);
            httpLoadProcess.setSuccessRunnable(httpLoadSuccessRunnable);
            httpLoadProcess.setErrorRunnable(httpLoadErrorRunnable);
            httpLoadProcess.start();
        } else if (file instanceof SshFile) {
            setBusy(true);
            sshLoadProcess = new SshLoadProcess(this, (SshFile)file);
            sshLoadProcess.setSuccessRunnable(sshLoadSuccessRunnable);
            sshLoadProcess.setErrorRunnable(sshLoadErrorRunnable);
            sshLoadProcess.start();
        }
        return LOAD_COMPLETED;
    }

    private Runnable ftpLoadSuccessRunnable = new Runnable() {
        public void run()
        {
            File file = ftpLoadProcess.getFile();
            String listing = ftpLoadProcess.getListing();
            if (ftpLoadProcess.fileIsDirectory()) {
                buffer = new Directory(file, listing);
                buffer.load();
            } else {
                File cache = ftpLoadProcess.getCache();
                if (cache == null)
                    return;
                buffer = createBuffer(file, cache, listing);
            }
            // Success.
            replaceBufferRunnable.run();
        }
    };

    private ErrorRunnable ftpLoadErrorRunnable = new ErrorRunnable("Load failed") {
        public void run()
        {
            kill();
            super.run();
        }
    };

    private Runnable httpLoadSuccessRunnable = new Runnable()
    {
        public void run()
        {
            File cache = httpLoadProcess.getCache();
            if (cache != null) {
                // Success.
                // We may have followed a redirect.
                final File file = httpLoadProcess.getFile();
                setFile(file);
                if (render) {
                    buffer = WebBuffer.createWebBuffer(file, cache, ref);
                    buffer.load();
                } else {
                    buffer = createBuffer(file, cache, getListing());
                    // Set the buffer's mode.
                    ModeList modeList = Editor.getModeList();
                    String contentType = httpLoadProcess.getContentType();
                    Mode mode;
                    if (contentType != null && contentType.toLowerCase().startsWith("text/html"))
                        mode = modeList.getMode(HTML_MODE);
                    else {
                        mode = modeList.getModeForFileName(file.getName());
                        if (mode == null)
                            mode = modeList.getMode(PLAIN_TEXT_MODE);
                    }
                    buffer.setMode(mode);
                    buffer.load();
                }
                replaceBufferRunnable.run();
            }
        }
    };

    private ErrorRunnable httpLoadErrorRunnable = new ErrorRunnable("Load failed") {
        public void run()
        {
            Log.debug("httpLoadErrorRunnable.run");
            Editor editor = Editor.currentEditor();
            if (editor.getBuffer() == RemoteBuffer.this) {
                editor.status("");
                editor.setDefaultCursor();
            }
            super.run();
            kill();
        }
    };

    private Runnable sshLoadSuccessRunnable = new Runnable() {
        public void run()
        {
            File file = sshLoadProcess.getFile();
            String listing = sshLoadProcess.getListing();
            if (sshLoadProcess.fileIsDirectory()) {
                buffer = new Directory(file, listing);
                buffer.load();
            } else {
                File cache = sshLoadProcess.getCache();
                buffer = createBuffer(file, cache, listing);
            }
            // Success.
            replaceBufferRunnable.run();
        }
    };

    private Runnable replaceBufferRunnable = new Runnable() {
        public void run()
        {
            if (Editor.getBufferList().contains(RemoteBuffer.this)) {
                int result;
                try {
                    if (!buffer.initialized())
                        buffer.initialize();
                    result = buffer.load();
                }
                catch (OutOfMemoryError e) {
                    buffer.kill();
                    RemoteBuffer.this.kill();
                    Runnable r = new Runnable() {
                        public void run()
                        {
                            MessageDialog.showMessageDialog(
                                Editor.currentEditor(),
                                "Insufficient memory to load buffer",
                                "Error");
                        }
                    };
                    SwingUtilities.invokeLater(r);
                    result = LOAD_FAILED;
                }
                if (result == LOAD_COMPLETED) {
                    buffer.setInitialDotPos(initialLineNumber, initialOffset);
                    Editor.getBufferList().replace(RemoteBuffer.this, buffer);
                }
            } else
                buffer.kill();
        }
    };

    private ErrorRunnable sshLoadErrorRunnable = new ErrorRunnable("Load failed") {
        public void run()
        {
            if (Editor.getBufferList().contains(RemoteBuffer.this)) {
                if (isEmpty())
                    kill();
                else
                    setBusy(false);
            }
            if (sshLoadProcess.cancelled()) {
                for (EditorIterator it = new EditorIterator(); it.hasNext();)
                    it.nextEditor().updateDisplay();
                Editor.currentEditor().status("Cancelled");
            } else
                super.run();
        }
    };

    public void dispose()
    {
        if (progressNotifier != null)
            progressNotifier.cancel();
        super.dispose();
    }

    public String getTitle()
    {
        return getFile().getHostName();
    }

    // For the buffer list.
    public String toString()
    {
        return getFile().getHostName();
    }

    public File getCurrentDirectory()
    {
        return Directories.getUserHomeDirectory();
    }
}
