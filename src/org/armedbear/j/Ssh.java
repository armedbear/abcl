/*
 * Ssh.java
 *
 * Copyright (C) 2002-2004 Peter Graves
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
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.util.ArrayList;

public final class Ssh
{
    private InputStreamReader reader;
    private OutputStreamWriter writer;
    private String[] cmdarray;
    private String password;
    private String errorText;
    private boolean succeeded;

    public Ssh()
    {
    }

    public final String getErrorText()
    {
        return errorText;
    }

    public boolean copy(File source, File destination)
    {
        SshFile remote = null;
        if (source instanceof SshFile)
            remote = (SshFile) source;
        else if (destination instanceof SshFile)
            remote = (SshFile) destination;
        if (remote == null) {
            Debug.bug("Ssh.copy no remote file");
            return false;
        }
        String userName = remote.getUserName();
        password = remote.getPassword();
        ArrayList list = new ArrayList();
        list.add("jpty");
        list.add("scp");
        list.add("-q");
        if (remote.getPort() != SshFile.DEFAULT_PORT) {
            list.add("-P");
            list.add(String.valueOf(remote.getPort()));
        }
        FastStringBuffer sb = new FastStringBuffer();
        if (source instanceof SshFile) {
            if (userName != null) {
                sb.append(userName);
                sb.append('@');
            }
            sb.append(source.getHostName());
            sb.append(':');
        }
        sb.append(escape(source.canonicalPath()));
        list.add(sb.toString());
        sb.setLength(0);
        if (destination instanceof SshFile) {
            if (userName != null) {
                sb.append(userName);
                sb.append('@');
            }
            sb.append(destination.getHostName());
            sb.append(':');
        }
        sb.append(escape(destination.canonicalPath()));
        list.add(sb.toString());
        String[] array = new String[list.size()];
        cmdarray = (String[]) list.toArray(array);
        run();
        return succeeded;
    }

    private static final String safeChars =
        "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789_-./\\";

    // Escapes unsafe characters.
    private static final String escape(String s)
    {
        final int length = s.length();
        FastStringBuffer sb = new FastStringBuffer(length * 2);
        for (int i = 0; i < length; i++) {
            char c = s.charAt(i);
            if (safeChars.indexOf(c) < 0)
                sb.append('\\');
            sb.append(c);
        }
        return sb.toString();
    }

    public void run()
    {
        Process process = null;
        int result = -1; // Assume error.
        try {
            process = Runtime.getRuntime().exec(cmdarray);
        }
        catch (Throwable t) {
            Log.error(t);
            return;
        }
        writer = new OutputStreamWriter(process.getOutputStream());
        reader = new InputStreamReader(process.getInputStream());
        SshReaderThread thread = new SshReaderThread();
        thread.start();
        try {
            thread.join();
            result = process.waitFor();
        }
        catch (InterruptedException e) {
            Log.error(e);
        }
        if (result == 0) {
            errorText = thread.getResponse();
            if (errorText == null) {
                succeeded = true;
            } else {
                errorText = errorText.trim();
                if (errorText.length() == 0) {
                    succeeded = true;
                } else {
                    // No error if response is a single line starting with
                    // "warning:".
                    if (errorText.toLowerCase().startsWith("warning:"))
                        if (errorText.indexOf('\n') < 0)
                            succeeded = true;
                }
            }
        }
    }

    private void sendPassword()
    {
        if (password != null) {
            try {
                writer.write(password);
                writer.write("\n");
                writer.flush();
            }
            catch (IOException e) {
                Log.error(e);
            }
        } else
            Debug.bug();
    }

    private class SshReaderThread extends Thread
    {
        private char[] buf = new char[4096];
        private boolean done = false;
        private String response;

        // If this constructor is private, we run into jikes 1.15 bug #2256.
        /*private*/ SshReaderThread()
        {
        }

        public final String getResponse()
        {
            return response;
        }

        public void run()
        {
            FastStringBuffer sb = new FastStringBuffer();
            while (true) {
                final String s = read();
                if (s == null) {
                    response = sb.toString();
                    return;
                }
                if (done) {
                    if (s.length() > 0)
                        sb.append(s);
                    response = sb.toString();
                    return;
                }
                if (isPasswordPrompt(s)) {
                    sendPassword();
                    sb.setLength(0);
                } else
                    sb.append(s);
            }
        }

        private String read()
        {
            FastStringBuffer sb = new FastStringBuffer();
            try {
                do {
                    int numChars = reader.read(buf, 0, buf.length); // Blocks.
                    if (numChars < 0) {
                        done = true;
                        break;
                    }
                    if (numChars > 0)
                        sb.append(buf, 0, numChars);
                } while (reader.ready());
            }
            catch (Exception e) {
                Log.error(e);
                return null;
            }
            return sb.toString();
        }
    }

    private boolean isPasswordPrompt(String s)
    {
        String trim = s.trim().toLowerCase();
        if (trim.endsWith("password:"))
            return true;
        if (trim.endsWith("response:"))
            return true;
        if (trim.startsWith("enter passphrase ") && trim.endsWith(":"))
            return true;
        return false;
    }
}
