/*
 * MailboxFileWriter.java
 *
 * Copyright (C) 2000-2002 Peter Graves
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
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.io.Writer;
import org.armedbear.j.File;
import org.armedbear.j.Log;

public final class MailboxFileWriter extends BufferedWriter
{
    private long offset;

    private MailboxFileWriter(Writer out)
    {
        super(out);
    }

    public static MailboxFileWriter getInstance(File file, boolean append)
    {
        try {
            FileOutputStream outputStream =
                new FileOutputStream(file.canonicalPath(), append);
            MailboxFileWriter writer = new MailboxFileWriter(
                new OutputStreamWriter(outputStream, "ISO-8859-1"));
            if (append && file.isFile())
                writer.offset = file.length();
            return writer;
        }
        catch (Exception e) {
            Log.error(e);
            return null;
        }
    }

    public final long getOffset()
    {
        return offset;
    }

    public void write(int c) throws IOException
    {
        super.write(c);
        ++offset;
    }

    public void write(char[] cbuf, int off, int len) throws IOException
    {
        super.write(cbuf, off, len);
        offset += len;
    }

    public void write(String s, int off, int len) throws IOException
    {
        super.write(s, off, len);
        offset += len;
    }

    public void newLine() throws IOException
    {
        super.write('\n'); // Always use '\n' as line terminator.
        ++offset;
    }

    public void flush() throws IOException
    {
        super.flush();
    }

    public void close() throws IOException
    {
        super.close();
    }
}
