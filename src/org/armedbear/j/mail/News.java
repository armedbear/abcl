/*
 * News.java
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

package org.armedbear.j.mail;

import java.io.BufferedWriter;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStreamWriter;
import javax.swing.SwingUtilities;
import org.armedbear.j.Buffer;
import org.armedbear.j.Directories;
import org.armedbear.j.Editor;
import org.armedbear.j.EditorIterator;
import org.armedbear.j.File;
import org.armedbear.j.Line;
import org.armedbear.j.Log;
import org.armedbear.j.MessageDialog;
import org.armedbear.j.ProgressNotifier;
import org.armedbear.j.StatusBarProgressNotifier;

public final class News extends Buffer
{
    private static final File newsDir =
        File.getInstance(Directories.getEditorDirectory(), "news");

    private final NntpSession session;
    private boolean error;

    public News(NntpSession session)
    {
        this.session = session;
        supportsUndo = false;
        mode = NewsGroupsMode.getMode();
        formatter = mode.getFormatter(this);
        readOnly = true;
        title = session.getHost();
        setInitialized(true);
    }

    public String getHost()
    {
        return session.getHost();
    }

    public int load()
    {
        setBusy(true);
        new Thread(loadRunnable).start();
        setLoaded(true);
        return LOAD_COMPLETED;
    }

    private Runnable loadRunnable = new Runnable() {
        public void run()
        {
            try {
                lockWrite();
            }
            catch (InterruptedException e) {
                Log.error(e);
                return;
            }
            try {
                _load();
            }
            finally {
                unlockWrite();
            }
            if (error)
                SwingUtilities.invokeLater(errorRunnable);
            else
                SwingUtilities.invokeLater(updateDisplayRunnable);
        }
    };

    private void _load()
    {
        File file = File.getInstance(newsDir, session.getHost());
        if (newsDir.isDirectory()) {
            if (file.isFile()) {
                try {
                    InputStream in = file.getInputStream();
                    if (in != null) {
                        load(in, null);
                        in.close();
                    }
                }
                catch (IOException e) {
                    Log.error(e);
                }
            }
        } else
            newsDir.mkdirs();
        if (getFirstLine() == null) {
            if (session.connect()) {
                if (session.writeLine("LIST")) {
                    String response = session.readLine();
                    if (response.startsWith("215")) {
                        session.setEcho(false);
                        int count = 0;
                        ProgressNotifier progressNotifier =
                            new StatusBarProgressNotifier(this);
                        progressNotifier.progressStart();
                        while (true) {
                            String s = session.readLine();
                            if (s == null)
                                break;
                            if (s.equals("."))
                                break;
                            int index = s.indexOf(' ');
                            if (index >= 0)
                                appendLine(s.substring(0, index));
                            else
                                appendLine(s);
                            ++count;
                            progressNotifier.progress(String.valueOf(count));
                        }
                        if (newsDir.isDirectory()) {
                            try {
                                BufferedWriter writer = new BufferedWriter(
                                    new OutputStreamWriter(file.getOutputStream()));
                                for (Line line = getFirstLine(); line != null; line = line.next()) {
                                    writer.write(line.getText());
                                    writer.write('\n');
                                }
                                writer.flush();
                                writer.close();
                            }
                            catch (IOException e) {
                                Log.error(e);
                            }
                        }
                        progressNotifier.progressStop();
                        session.setEcho(NntpSession.DEFAULT_ECHO);
                    }
                }
                session.disconnect();
            }
        }
        if (getFirstLine() == null) {
            error = true;
            appendLine("");
        }
        renumber();
    }

    private Runnable errorRunnable = new Runnable() {
        public void run()
        {
            kill();
            String errorText = session.getErrorText();
            if (errorText != null)
                MessageDialog.showMessageDialog(errorText, "Error");
        }
    };

    private Runnable updateDisplayRunnable = new Runnable() {
        public void run()
        {
            setBusy(false);
            invalidate();
            for (EditorIterator it = new EditorIterator(); it.hasNext();) {
                Editor ed = it.nextEditor();
                if (ed.getBuffer() == News.this) {
                    ed.setDot(getFirstLine(), 0);
                    ed.moveCaretToDotCol();
                    ed.setTopLine(getFirstLine());
                    ed.setUpdateFlag(REPAINT);
                    ed.updateDisplay();
                }
            }
        }
    };
}
