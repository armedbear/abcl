/*
 * LoadProcess.java
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

public abstract class LoadProcess implements BackgroundProcess, Runnable, Cancellable
{
    protected Buffer buffer;
    protected File file;
    protected Runnable successRunnable;
    protected ErrorRunnable errorRunnable;
    protected ProgressNotifier progressNotifier;
    protected File cache;
    protected boolean cancelled;

    private String errorText;
    private Thread thread;

    protected LoadProcess(Buffer buffer, File file)
    {
        this.buffer = buffer;
        this.file = file;
    }

    public final File getFile()
    {
        return file;
    }

    public final void setSuccessRunnable(Runnable r)
    {
        successRunnable = r;
    }

    public final void setCancelRunnable(Runnable r)
    {
        cancelRunnable = r;
    }

    public final void setErrorRunnable(ErrorRunnable r)
    {
        errorRunnable = r;
    }

    public void setProgressNotifier(ProgressNotifier progressNotifier)
    {
        this.progressNotifier = progressNotifier;
    }

    public final File getCache()
    {
        return cache;
    }

    public final boolean cancelled()
    {
        return cancelled;
    }

    public final String getErrorText()
    {
        return errorText;
    }

    protected final void setErrorText(String s)
    {
        errorText = s;
    }

    public void start()
    {
        buffer.setBusy(true);
        for (EditorIterator it = new EditorIterator(); it.hasNext();) {
            Editor ed = it.nextEditor();
            if (ed.getBuffer() == buffer)
                ed.setWaitCursor();
        }
        thread = new Thread(this);
        thread.start();
    }

    public void cancel()
    {
        if (thread != null)
            thread.interrupt();
        cancelled = true;
        if (progressNotifier != null) {
            progressNotifier.cancel();
            progressNotifier.progressStop();
            progressNotifier.setText("Transfer cancelled");
        }
    }

    // Can be overridden.
    protected Runnable cancelRunnable = new Runnable() {
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
            MessageDialog.showMessageDialog("Transfer cancelled", file.netPath());
        }
    };
}
