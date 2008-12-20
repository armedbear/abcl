/*
 * CompilationBuffer.java
 *
 * Copyright (C) 1998-2004 Peter Graves
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
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.ArrayList;
import javax.swing.SwingUtilities;

public final class CompilationBuffer extends CompilationErrorBuffer
    implements Runnable
{
    private String command;
    private String expandedCommand;
    private Position posEndOfBuffer;
    private File currentDir;
    private Process process;
    private int exitValue;
    private File exitValueFile;

    public CompilationBuffer(String command, File directory)
    {
        setCommand(command);
        currentDir = directory;
        mode = CompilationMode.getMode();
        try {
            lockWrite();
        }
        catch (InterruptedException e) {
            Log.debug(e);
            return;
        }
        try {
            appendLine("");
            renumber();
        }
        finally {
            unlockWrite();
        }
        setLoaded(true);
        posEndOfBuffer = new Position(getFirstLine(), 0);
    }

    public synchronized final void initialize()
    {
        setTitle(expandedCommand = expandCommand(command));
        setInitialized(true);
    }

    public final void setCommand(String command)
    {
        this.command = command;
    }

    public final int exitValue()
    {
        return exitValue;
    }

    public void empty()
    {
        try {
            lockWrite();
        }
        catch (InterruptedException e) {
            Log.debug(e);
            return;
        }
        try {
            super.empty();
            appendLine("");
            renumber();
            setLoaded(true);
            posEndOfBuffer = new Position(getFirstLine(), 0);
            setCurrentError(null);
        }
        finally {
            unlockWrite();
        }
        for (EditorIterator it = new EditorIterator(); it.hasNext();) {
            Editor ed = it.nextEditor();
            if (ed.getBuffer() == this) {
                ed.setDot(getFirstLine(), 0);
                ed.setTopLine(getFirstLine());
                ed.setMark(null);
            }
        }
    }

    public void run()
    {
        long start = System.currentTimeMillis();
        if (expandedCommand.startsWith("(")) {
            // Lisp.
            FastStringBuffer sb = new FastStringBuffer();
            sb.append("(with-output-to-string (s) ");
            sb.append("(let ((*standard-output* s)) ");
            sb.append(expandedCommand);
            sb.append(" ))");
            try {
                org.armedbear.lisp.LispObject result =
                    JLisp.runLispCommand(sb.toString());
                appendLater(result.getStringValue());
            }
            catch (Throwable t) {
                Log.debug(t);
            }
        } else {
            startProcess();
            if (process != null) {
                CompilationBufferReaderThread stdoutThread =
                    new CompilationBufferReaderThread(process.getInputStream());
                stdoutThread.start();
                CompilationBufferReaderThread stderrThread =
                    new CompilationBufferReaderThread(process.getErrorStream());
                stderrThread.start();
                try {
                    exitValue = process.waitFor();
                    if (exitValueFile != null && exitValueFile.isFile()) {
                        exitValue = getExitValueFromFile(exitValueFile);
                        exitValueFile.delete();
                        exitValueFile = null;
                    }
                    stdoutThread.join();
                    stderrThread.join();
                    long elapsed = System.currentTimeMillis() - start;
                    FastStringBuffer sb = new FastStringBuffer();
                    sb.append("\nCompilation ");
                    if (exitValue == 0) {
                        sb.append("finished (");
                        sb.append(String.valueOf(elapsed));
                        sb.append(" milliseconds)");
                    } else
                        sb.append("exited abnormally");
                    sb.append("\n");
                    appendLater(sb.toString());
                }
                catch (InterruptedException e) {
                    Log.error(e);
                }
            } else
                appendLater("Unable to start compilation process\n");
        }
        Editor.getTagFileManager().setEnabled(true);
    }

    private void startProcess()
    {
        process = null;
        exitValue = -1;
        try {
            if (Platform.isPlatformWindows()) {
                String cmd = "cd /d \"" + currentDir.canonicalPath() +
                    "\" && " + expandedCommand;
                ArrayList list = new ArrayList();
                list.add("cmd.exe");
                list.add("/c");
                list.addAll(Utilities.tokenize(cmd));
                final int size = list.size();
                String[] cmdarray = new String[size];
                for (int i = 0; i < size; i++)
                    cmdarray[i] = (String) list.get(i);
                process = Runtime.getRuntime().exec(cmdarray);
            } else {
                // Not Windows. Assume Unix.
                if (Utilities.haveJpty()) {
                    exitValueFile = Utilities.getTempFile();
                    FastStringBuffer sb = new FastStringBuffer();
                    sb.append("(\\cd \"");
                    sb.append(currentDir.canonicalPath());
                    sb.append("\" && ");
                    sb.append(expandedCommand);
                    sb.append("; echo $? > ");
                    sb.append(exitValueFile.canonicalPath());
                    sb.append(')');
                    final String cmd = sb.toString();
                    String[] cmdarray = {"jpty", "/bin/sh", "-c", cmd};
                    process = Runtime.getRuntime().exec(cmdarray);
                } else {
                    String cmd = "(\\cd \"" + currentDir.canonicalPath() +
                        "\" && " + expandedCommand + ")";
                    String[] cmdarray = {"/bin/sh", "-c", cmd};
                    process = Runtime.getRuntime().exec(cmdarray);
                }
            }
        }
        catch (Throwable t) {
            Log.error(t);
        }
    }

    private String expandCommand(String s)
    {
        int length = s.length();
        FastStringBuffer sb = new FastStringBuffer();
        boolean inQuote = false;
        for (int i = 0; i < length; i++) {
            char c = s.charAt(i);
            if (inQuote) {
                sb.append(c);
                if (c == '"')
                    inQuote = false;
            } else {
                // Not in quote.
                if (c == '"') {
                    sb.append(c);
                    inQuote = true;
                } else if (c == 'h') {
                    boolean replaced = false;
                    if (s.regionMatches(i, "here", 0, 4)) {
                        // "here" must be delimited by spaces.
                        if (i == 0 || s.charAt(i-1) == ' ') {
                            if (i+4 == length || s.charAt(i+4) == ' ') {
                                File file = parentBuffer.getFile();
                                if (file != null) {
                                    String cp = file.canonicalPath();
                                    if (cp.indexOf(' ') >= 0) {
                                        // Enclose filename in double quotes
                                        // since it contains an embedded space.
                                        sb.append('"');
                                        sb.append(cp);
                                        sb.append('"');
                                    } else
                                        sb.append(cp);
                                    replaced = true;
                                }
                            }
                        }
                    }
                    if (replaced)
                        i += 3;
                    else
                        sb.append(c);
                } else
                    sb.append(c);
            }
        }
        return sb.toString();
    }

    private int getExitValueFromFile(File file)
    {
        int ret = -1;
        if (file != null) {
            try {
                InputStream inputStream = file.getInputStream();
                BufferedReader reader =
                    new BufferedReader(new InputStreamReader(inputStream));
                String s = reader.readLine();
                reader.close();
                try {
                    ret = Integer.parseInt(s);
                }
                catch (NumberFormatException e) {}
            }
            catch (IOException e) {}
        }
        return ret;
    }

    public static void killCompilation()
    {
        for (BufferIterator it = new BufferIterator(); it.hasNext();) {
            Buffer buf = it.nextBuffer();
            if (buf instanceof CompilationBuffer) {
                ((CompilationBuffer)buf).killProcess();
                break;
            }
        }
    }

    private synchronized void killProcess()
    {
        if (process != null) {
            process.destroy();
            try {
                process.waitFor();
                process = null;
            }
            catch (InterruptedException e) {
                Log.error(e);
            }
        }
    }

    public void dispose()
    {
        killProcess();
    }

    private void appendLater(final String s)
    {
        Runnable runnable = new Runnable() {
            public void run()
            {
                Position pos = posEndOfBuffer;
                insertString(pos, s);
                if (needsRenumbering())
                    renumber();
                for (EditorIterator it = new EditorIterator(); it.hasNext();) {
                    Editor ed = it.nextEditor();
                    if (ed.getBuffer() == CompilationBuffer.this) {
                        ed.eob();
                        ed.getDisplay().setReframe(-2);
                        ed.setUpdateFlag(REPAINT);
                        ed.updateDisplay();
                    }
                }
                resetUndo();
            }
        };
        SwingUtilities.invokeLater(runnable);
    }

    public String getFileNameForDisplay()
    {
        return getTitle();
    }

    public File getCurrentDirectory()
    {
        return currentDir;
    }

    public void setCurrentDirectory(File directory)
    {
        currentDir = directory;
    }

    // For the buffer list.
    public String toString()
    {
        return command;
    }

    private class CompilationBufferReaderThread extends ReaderThread
    {
        public CompilationBufferReaderThread(InputStream inputStream)
        {
            super(inputStream);
        }

        public void update(final String s)
        {
            appendLater(s);
        }
    }
}
