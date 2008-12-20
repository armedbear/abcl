/*
 * Man.java
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

import java.io.IOException;
import java.io.InputStream;
import java.util.StringTokenizer;

public final class Man extends Buffer
{
    private boolean apropos;

    public Man(String topic, File tempFile)
    {
        super();
        // Check for -k switch.
        StringTokenizer st = new StringTokenizer(topic);
        while (st.hasMoreTokens()) {
            String s = st.nextToken();
            if (s.equals("-k")) {
                apropos = true;
                break;
            }
        }
        initializeUndo();
        setFile(tempFile);
        type = TYPE_MAN;
        readOnly = true;
        title = ManMode.getTitle(topic);
        mode = Editor.getModeList().getMode(MAN_MODE);
        formatter = new ManFormatter(this, apropos);
        setTransient(true);
        setInitialized(true);
    }

    public final boolean isApropos()
    {
        return apropos;
    }

    public int load()
    {
        if (!isLoaded()) {
            try {
                lockWrite();
            }
            catch (InterruptedException e) {
                Log.debug(e);
                return LOAD_FAILED;
            }
            try {
                final File toBeLoaded = getFile();
                if (toBeLoaded.isFile()) {
                    try {
                        _load(toBeLoaded.getInputStream());
                    }
                    catch (IOException e) {
                        Log.error(e);
                    }
                    // Handle zero length files.
                    if (getFirstLine() == null) {
                        appendLine("");
                        lineSeparator = System.getProperty("line.separator");
                    }
                    setLastModified(toBeLoaded.lastModified());
                    renumber();
                    formatter.parseBuffer();
                }
                Line line = getFirstLine();
                while (line != null && line.isBlank()) {
                    line = line.next();
                    setFirstLine(line);
                }
                final Line firstLine = getFirstLine();
                if (firstLine == null) {
                    appendLine("");
                    setLoaded(true);
                    return LOAD_FAILED;
                }
                firstLine.setPrevious(null);
                String header = firstLine.getText();
                for (line = firstLine.next(); line != null; line = line.next()) {
                    if (line.isBlank() && line.previous() != null && line.previous().isBlank())
                        remove(line);
                    else if (line.getText().equals(header))
                        remove(line);
                }
                renumber();
                setLoaded(true);
            }
            finally {
                unlockWrite();
            }
        }
        return LOAD_COMPLETED;
    }

    private void _load(InputStream istream)
    {
        byte[] buf = new byte[4096];
        int totalBytes = 0;
        FastStringBuffer sb = new FastStringBuffer(256);
        boolean skipLF = false;
        int bytesRead;
        try {
            while ((bytesRead  = istream.read(buf)) > 0) {
                for (int i = 0; i < bytesRead; i++) {
                    byte b = buf[i];
                    switch (b) {
                        case 13:
                            appendLine(sb.toString());
                            sb.setLength(0);
                            skipLF = true;
                            break;
                        case 10:
                            if (skipLF) {
                                // LF after CR.
                                if (lineSeparator == null)
                                    lineSeparator = "\r\n";
                                skipLF = false;
                            } else {
                                // LF without preceding CR.
                                if (lineSeparator == null)
                                    lineSeparator = "\n";
                                appendLine(sb.toString());
                                sb.setLength(0);
                            }
                            break;
                        default:
                            // Normal char.
                            if (skipLF) {
                                // Something other than LF after CR.
                                if (lineSeparator == null)
                                    lineSeparator = "\r";
                                skipLF = false;
                            }
                            sb.append((char) (b & 0xff));
                            break;
                    }
                }
            }
            if (sb.length() > 0)
                appendLine(sb.toString());
            setLoaded(true);
        }
        catch (IOException e) {
            Log.error(e);
        }
    }

    public final void appendLine(String s)
    {
        appendLine(new ManLine(s));
    }

    private void remove(Line line)
    {
        Line prev = line.previous();
        Line next = line.next();
        if (prev != null)
            prev.setNext(next);
        if (next != null)
            next.setPrevious(prev);
    }

    public final File getCurrentDirectory()
    {
        return Directories.getUserHomeDirectory();
    }

    public final String getFileNameForDisplay()
    {
        return "";
    }
}
