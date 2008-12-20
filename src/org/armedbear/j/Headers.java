/*
 * Headers.java
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

public final class Headers
{
    // These are indexes into the array of values. We only store values for the
    // headers we're interested in.
    public static final int CC                        =  0;
    public static final int CONTENT_DISPOSITION       =  1;
    public static final int CONTENT_TRANSFER_ENCODING =  2;
    public static final int CONTENT_TYPE              =  3;
    public static final int DATE                      =  4;
    public static final int FROM                      =  5;
    public static final int IN_REPLY_TO               =  6;
    public static final int MESSAGE_ID                =  7;
    public static final int REFERENCES                =  8;
    public static final int REPLY_TO                  =  9;
    public static final int SET_COOKIE                = 10;
    public static final int SUBJECT                   = 11;
    public static final int TO                        = 12;
    public static final int X_J_STATUS                = 13;
    public static final int X_UIDL                    = 14;

    private static final int MAX_HEADERS = 15;

    private String[] values = new String[MAX_HEADERS];

    private Headers()
    {
    }

    public String getValue(String name)
    {
        int index = getIndex(name);
        if (index >= 0)
            return values[index];
        return null;
    }

    public String getValue(int index)
    {
        if (index < MAX_HEADERS)
            return values[index];
        Debug.bug();
        return null;
    }

    private void setValue(String name, String value)
    {
        int index = getIndex(name);
        if (index >= 0)
            values[index] = value;
    }

    private int getIndex(String name)
    {
        if (name.length() == 0)
            return -1;
        name = name.toLowerCase();
        switch (name.charAt(0)) {
            case 'c':
                if (name.equals("cc"))
                    return CC;
                if (name.equals("content-disposition"))
                    return CONTENT_DISPOSITION;
                if (name.equals("content-transfer-encoding"))
                    return CONTENT_TRANSFER_ENCODING;
                if (name.equals("content-type"))
                    return CONTENT_TYPE;
                break;
            case 'd':
                if (name.equals("date"))
                    return DATE;
                break;
            case 'f':
                if (name.equals("from"))
                    return FROM;
                break;
            case 'i':
                if (name.equals("in-reply-to"))
                    return IN_REPLY_TO;
                break;
            case 'm':
                if (name.equals("message-id"))
                    return MESSAGE_ID;
                break;
            case 'r':
                if (name.equals("references"))
                    return REFERENCES;
                if (name.equals("reply-to"))
                    return REPLY_TO;
                break;
            case 's':
                if (name.equals("set-cookie"))
                    return SET_COOKIE;
                if (name.equals("subject"))
                    return SUBJECT;
                break;
            case 't':
                if (name.equals("to"))
                    return TO;
                break;
            case 'x':
                if (name.equals("x-j-status"))
                    return X_J_STATUS;
                if (name.equals("x-uidl"))
                    return X_UIDL;
                break;
            default:
                break;
        }
        return -1;
    }

    public static Headers parse(String s)
    {
        Headers headers = new Headers();
        FastStringBuffer sb = new FastStringBuffer();
        String name = null;
        int begin = 0;
        int end;
        while (true) {
            end = s.indexOf('\n', begin);
            if (end < 0) {
                // Done.
                if (name != null && sb.length() > 0)
                    headers.setValue(name, sb.toString());
                break;
            }
            String line = s.substring(begin, end);
            // Trim trailing '\r' if any.
            if (line.length() > 0 && line.charAt(line.length()-1) == '\r')
                line = line.substring(0, line.length()-1);
            if (line.length() == 0) {
                // Done.
                if (name != null && sb.length() > 0)
                    headers.setValue(name, sb.toString());
                break;
            }
            begin = end + 1;
            char c = line.charAt(0);
            if (c == ' ' || c == '\t') {
                // Continuation.
                sb.append(line);
                continue;
            }
            if (name != null && sb.length() > 0)
                headers.setValue(name, sb.toString());
            // New header.
            name = null;
            sb.setLength(0);
            int i = line.indexOf(':');
            if (i < 0) {
                Log.error("can't parse header |" + s + "|");
                return headers;
            }
            // Store names in lower case.
            name = line.substring(0, i).trim();
            // Field value.
            sb.append(line.substring(i+1).trim());
        }
        return headers;
    }
}
