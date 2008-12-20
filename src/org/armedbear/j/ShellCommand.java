/*
 * ShellCommand.java
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

import java.io.BufferedWriter;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStreamWriter;
import java.util.ArrayList;

public final class ShellCommand implements Runnable
{
    private final String cmdline;
    private final File workingDirectory;
    private final String input;
    private final StringBuffer output = new StringBuffer();
    private int exitValue = -1;

    public ShellCommand(String cmdline)
    {
        this(cmdline, null, null);
    }

    public ShellCommand(String cmdline, File workingDirectory)
    {
        this(cmdline, workingDirectory, null);
    }

    public ShellCommand(String cmdline, File workingDirectory, String input)
    {
        this.cmdline = cmdline;
        this.workingDirectory = workingDirectory;
        this.input = input;
    }

    public final String getOutput()
    {
        return output.toString();
    }

    public final int exitValue()
    {
        return exitValue;
    }

    private void appendOutput(String s)
    {
        output.append(s);
    }

    public void run()
    {
        Process process = null;
        try {
            if (cmdline != null) {
                if (Platform.isPlatformUnix()) {
                    if (workingDirectory != null) {
                        FastStringBuffer sb = new FastStringBuffer("\\cd \"");
                        sb.append(workingDirectory.canonicalPath());
                        sb.append("\" && ");
                        sb.append(cmdline);
                        String[] cmdarray = {"/bin/sh", "-c", sb.toString()};
                        process = Runtime.getRuntime().exec(cmdarray);
                    } else {
                        String[] cmdarray = {"/bin/sh", "-c", cmdline};
                        process = Runtime.getRuntime().exec(cmdarray);
                    }
                } else if (Platform.isPlatformWindows()) {
                    ArrayList list = new ArrayList();
                    list.add("cmd.exe");
                    list.add("/c");
                    if (workingDirectory != null) {
                        FastStringBuffer sb = new FastStringBuffer("cd /d \"");
                        sb.append(workingDirectory.canonicalPath());
                        sb.append("\" && ");
                        sb.append(cmdline);
                        list.addAll(Utilities.tokenize(sb.toString()));
                    } else
                        list.addAll(Utilities.tokenize(cmdline));
                    final int size = list.size();
                    String[] cmdarray = new String[size];
                    for (int i = 0; i < size; i++)
                        cmdarray[i] = (String) list.get(i);
                    process = Runtime.getRuntime().exec(cmdarray);
                }
            }
        }
        catch (IOException e) {
            Log.error(e);
        }
        if (process != null) {
            ShellCommandReaderThread stdoutThread =
                new ShellCommandReaderThread(process.getInputStream());
            stdoutThread.start();
            ShellCommandReaderThread stderrThread =
                new ShellCommandReaderThread(process.getErrorStream());
            stderrThread.start();
            if (input != null) {
                BufferedWriter writer =
                    new BufferedWriter(new OutputStreamWriter(process.getOutputStream()));
                try {
                    writer.write(input);
                    writer.flush();
                    writer.close();
                }
                catch (IOException e) {
                    Log.error(e);
                }
            }
            try {
                exitValue = process.waitFor();
            }
            catch (InterruptedException e) {
                Log.error(e);
            }
            try {
                stdoutThread.join();
            }
            catch (InterruptedException e) {
                Log.error(e);
            }
            try {
                stderrThread.join();
            }
            catch (InterruptedException e) {
                Log.error(e);
            }
        }
    }

    private class ShellCommandReaderThread extends ReaderThread
    {
        // If this constructor is private, we run into jikes 1.15 bug #2256.
        ShellCommandReaderThread(InputStream inputStream)
        {
            super(inputStream);
        }

        public void update(final String s)
        {
            appendOutput(s);
        }
    }

    public static void shellCommand()
    {
        if (!Platform.isPlatformUnix())
            return;
        final Editor editor = Editor.currentEditor();
        InputDialog d =
            new InputDialog(editor, "Command:", "Shell Command", null);
        d.setHistory(new History("shellCommand.command"));
        editor.centerDialog(d);
        d.show();
        String command = d.getInput();
        if (command == null)
            return;
        command = command.trim();
        if (command.length() == 0)
            return;
        AsynchronousShellCommand.startShellCommand(editor, command);
    }

    public static void shellCommand(String command)
    {
        if (!Platform.isPlatformUnix())
            return;
        AsynchronousShellCommand.startShellCommand(Editor.currentEditor(),
            command);
    }
}
