/*
 * ShellCommand.java
 *
 * Copyright (C) 2000-2005 Peter Graves
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
 *
 * As a special exception, the copyright holders of this library give you
 * permission to link this library with independent modules to produce an
 * executable, regardless of the license terms of these independent
 * modules, and to copy and distribute the resulting executable under
 * terms of your choice, provided that you also meet, for each linked
 * independent module, the terms and conditions of the license of that
 * module.  An independent module is a module which is not derived from
 * or based on this library.  If you modify this library, you may extend
 * this exception to your version of the library, but you are not
 * obligated to do so.  If you do not wish to do so, delete this
 * exception statement from your version.
 */

package org.armedbear.lisp;

import static org.armedbear.lisp.Lisp.*;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.List;

public final class ShellCommand implements Runnable
{
    private final String command;
    private final String directory;
    private final Stream outputStream;
    private final StringBuffer output;

    private int exitValue = -1;

    public ShellCommand(String command, String directory, Stream outputStream)

    {
        this.command = command;
        this.directory = directory;
        this.outputStream = outputStream;
        this.output = (outputStream == null) ? new StringBuffer() : null;
    }

    public final String getOutput()
    {
        return (output != null) ? output.toString() : "";
    }

    final int exitValue()
    {
        return exitValue;
    }

    void processOutput(String s)
    {
        if (outputStream != null)
            outputStream._writeString(s);
        else
            output.append(s);
    }

    public void run()
    {
        Process process = null;
        try {
            if (command != null) {
                if (Utilities.isPlatformUnix) {
                    if (directory != null) {
                        StringBuilder sb = new StringBuilder("\\cd \"");
                        sb.append(directory);
                        sb.append("\" && ");
                        sb.append(command);
                        String[] cmdarray = {"/bin/sh", "-c", sb.toString()};
                        process = Runtime.getRuntime().exec(cmdarray);
                    } else {
                        String[] cmdarray = {"/bin/sh", "-c", command};
                        process = Runtime.getRuntime().exec(cmdarray);
                    }
                } else if (Utilities.isPlatformWindows) {
                    ArrayList<String> list = new ArrayList<String>();
                    list.add("cmd.exe");
                    list.add("/c");
                    if (directory != null) {
                        StringBuilder sb = new StringBuilder("cd /d \"");
                        sb.append(directory);
                        sb.append("\" && ");
                        sb.append(command);
                        list.addAll(tokenize(sb.toString()));
                    } else
                        list.addAll(tokenize(command));
                    final int size = list.size();
                    String[] cmdarray = new String[size];
                    for (int i = 0; i < size; i++)
                        cmdarray[i] = (String) list.get(i);
                    process = Runtime.getRuntime().exec(cmdarray);
                }
            }
        }
        catch (IOException e) {
            Debug.trace(e);
        }
        if (process != null) {
            ReaderThread stdoutThread =
                new ReaderThread(process.getInputStream());
            stdoutThread.start();
            ReaderThread stderrThread =
                new ReaderThread(process.getErrorStream());
            stderrThread.start();
            try {
                exitValue = process.waitFor();
            }
            catch (InterruptedException e) {
                Debug.trace(e);
            }
            try {
                stdoutThread.join();
            }
            catch (InterruptedException e) {
                Debug.trace(e);
            }
            try {
                stderrThread.join();
            }
            catch (InterruptedException e) {
                Debug.trace(e);
            }
        }
    }

    // Does not handle embedded single-quoted strings.
    private static List<String> tokenize(String s)
    {
        ArrayList<String> list = new ArrayList<String>();
        StringBuffer sb = new StringBuffer();
        boolean inQuote = false;
        final int limit = s.length();
        for (int i = 0; i < limit; i++) {
            char c = s.charAt(i);
            switch (c) {
                case ' ':
                    if (inQuote)
                        sb.append(c);
                    else if (sb.length() > 0) {
                        list.add(sb.toString());
                        sb.setLength(0);
                    }
                    break;
                case '"':
                    if (inQuote) {
                        if (sb.length() > 0) {
                            list.add(sb.toString());
                            sb.setLength(0);
                        }
                        inQuote = false;
                    } else
                        inQuote = true;
                    break;
                default:
                    sb.append(c);
                    break;
            }
        }
        if (sb.length() > 0)
            list.add(sb.toString());
        return list;
    }

    private class ReaderThread extends Thread
    {
        private char[] buf = new char[4096];
        private final InputStream inputStream;
        private final BufferedReader reader;
        private boolean done = false;

        public ReaderThread(InputStream inputStream)
        {
            this.inputStream = inputStream;
            reader = new BufferedReader(new InputStreamReader(inputStream));
        }

        @Override
        public void run()
        {
            while (!done) {
                String s = read();
                if (s == null)
                    return;
                processOutput(s);
            }
        }

        private String read()
        {
            StringBuffer sb = new StringBuffer();
            try {
                do {
                    int numChars = reader.read(buf, 0, buf.length); // Blocks.
                    if (numChars < 0) {
                        done = true;
                        break;
                    }
                    if (numChars > 0)
                        sb.append(buf, 0, numChars);
                    Thread.sleep(10);
                } while (reader.ready());
            }
            catch (IOException e) {
                return null;
            }
            catch (InterruptedException e) {
                return null;
            }
            return sb.toString();
        }
    }

    // run-shell-command command &key directory (output *standard-output*)
    // ### %run-shell-command command directory output => exit-code
    private static final Primitive _RUN_SHELL_COMMAND =
        new Primitive("%run-shell-command", PACKAGE_SYS, false)
    {
        @Override
        public LispObject execute(LispObject first, LispObject second,
                                  LispObject third)

        {
            if (!(Utilities.isPlatformUnix || Utilities.isPlatformWindows)) {
              return error(new LispError("run-shell-command not implemented for "
                                       + System.getProperty("os.name")));
            }
            else {
                String command = first.getStringValue();
                String namestring = null;
                Stream outputStream = null;
                if (second != NIL) {
                    Pathname pathname = coerceToPathname(second);
                    namestring = pathname.getNamestring();
                    if (namestring == null) {
                        return error(new FileError("Pathname has no namestring: " + pathname.writeToString(),
                                                    pathname));
                    }
                }
                if (third != NIL)
                    outputStream = checkStream(third);
                ShellCommand shellCommand = new ShellCommand(command, namestring,
                                                             outputStream);
                shellCommand.run();
                if (outputStream != null)
                    outputStream._finishOutput();
                return number(shellCommand.exitValue());
            }
        }
    };
}
