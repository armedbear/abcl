/*
 * ReaderThread.java
 *
 * Copyright (C) 2000-2005 Peter Graves
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
import java.io.UnsupportedEncodingException;

public class ReaderThread extends Thread
{
    private char[] buf = new char[4096];
    private InputStream inputStream;
    private BufferedReader reader;
    private boolean done = false;
    private int timeOut = 10; // Milliseconds.

    public ReaderThread(InputStream inputStream)
    {
        super("reader thread");
        this.inputStream = inputStream;
        String encoding =
            Editor.preferences().getStringProperty(Property.DEFAULT_ENCODING);
        try {
            reader = new BufferedReader(new InputStreamReader(inputStream,
                                                              encoding));
        }
        catch (UnsupportedEncodingException e) {
            Log.debug(e);
            reader = new BufferedReader(new InputStreamReader(inputStream));
        }
    }

    public void setTimeOut(int n)
    {
        timeOut = n;
    }

    public void run()
    {
        while (!done) {
            String s = read();
            if (s == null)
                return;
            update(filter(s));
        }
    }

    public void cancel()
    {
        interrupt();
        if (inputStream != null) {
            try {
                inputStream.close();
            }
            catch (IOException e) {
                Log.error(e);
            }
            inputStream = null;
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
                if (timeOut > 0)
                    Thread.sleep(timeOut);
                else
                    Thread.yield();
            }
            while (reader.ready());
        }
        catch (IOException e) {
            return null;
        }
        catch (InterruptedException e) {
            return null;
        }
        catch (Throwable t) {
            return null;
        }
        return sb.toString();
    }

    public String filter(String s)
    {
        return s;
    }

    public void update(String s)
    {
    }
}
