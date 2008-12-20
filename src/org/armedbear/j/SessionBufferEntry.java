/*
 * SessionBufferEntry.java
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

import java.util.Iterator;

public final class SessionBufferEntry
{
    private static final String lineSeparator =
        System.getProperty("line.separator");

    private Buffer buffer;
    private String path;
    private String modeName;
    private int dotLineNumber;
    private int dotOffset;
    private long lastActivated;

    public SessionBufferEntry()
    {
    }

    public SessionBufferEntry(Buffer buffer, int index)
    {
        this.buffer = buffer;
        if (buffer.getFile() != null)
            path = buffer.getFile().netPath();
        else
            path = "";
        if (buffer.getMode() != null)
            modeName = buffer.getMode().toString();
        else
            modeName = "";
        lastActivated = buffer.getLastActivated();
        View view = buffer.getLastView();
        if (view != null) {
            dotLineNumber = view.lineNumber;
            dotOffset = view.offs;
        }
    }

    public final String getPath()
    {
        return path;
    }

    public final void setPath(String path)
    {
        this.path = path;
    }

    public final Mode getMode()
    {
        return Editor.getModeList().getModeFromModeName(modeName);
    }

    public final void setMode(String modeName)
    {
        this.modeName = modeName;
    }

    public final int getModeId()
    {
        return Editor.getModeList().getModeIdFromModeName(modeName);
    }

    public final int getDotLineNumber()
    {
        return dotLineNumber;
    }

    public final void setDotLineNumber(int lineNumber)
    {
        dotLineNumber = lineNumber;
    }

    public final int getDotOffset()
    {
        return dotOffset;
    }

    public final void setDotOffset(int offset)
    {
        dotOffset = offset;
    }

    public final long getLastActivated()
    {
        return lastActivated;
    }

    public final void setLastActivated(long l)
    {
        lastActivated = l;
    }

    public String toXml()
    {
        FastStringBuffer sb = new FastStringBuffer();
        sb.append("    <buffer");
        sb.append(" path=\"");
        sb.append(path);
        sb.append('"');
        sb.append(" mode=\"");
        sb.append(modeName);
        sb.append('"');
        sb.append(" dot=\"");
        sb.append(String.valueOf(dotLineNumber));
        sb.append(',');
        sb.append(String.valueOf(dotOffset));
        sb.append('"');
        sb.append(" when=\"");
        sb.append(String.valueOf(lastActivated));
        sb.append('"');
        PropertyList properties = buffer.getProperties();
        if (properties.size() > 0) {
            sb.append(">");
            sb.append(lineSeparator);
            Iterator it = properties.keyIterator();
            if (it != null) {
                while (it.hasNext()) {
                    Property property = (Property) it.next();
                    Object value = properties.getProperty(property);
                    sb.append(propertyToXml(property.getDisplayName(),
                        value.toString()));
                }
            }
            sb.append("  </buffer>");
        } else
            sb.append("/>");
        return sb.toString();
    }

    private static String propertyToXml(String name, String value)
    {
        FastStringBuffer sb = new FastStringBuffer("      <property name=\"");
        sb.append(name);
        sb.append("\" value=\"");
        sb.append(value);
        sb.append("\"/>");
        sb.append(lineSeparator);
        return sb.toString();
    }
}
