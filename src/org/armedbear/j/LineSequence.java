/*
 * LineSequence.java
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

public final class LineSequence
{
    private Line first;
    private Line last;
    private int size;

    public LineSequence()
    {
    }

    public LineSequence(Line line)
    {
        first = last = line.copy();
        size = 1;
    }

    // Copies lines.
    public LineSequence(Line first, Line last)
    {
        Line line = first;
        while (line != null) {
            appendLine(line.copy());
            if (line == last)
                break;
            line = line.next();
        }
    }

    public final Line getFirstLine()
    {
        return first;
    }

    public final Line getLastLine()
    {
        return last;
    }

    public final int size()
    {
        return size;
    }

    // Does not copy line.
    public void appendLine(Line line)
    {
        line.setPrevious(last);
        if (last != null)
            last.setNext(line);
        last = line;
        if (first == null)
            first = line;
        ++size;
    }

    public String toString()
    {
        FastStringBuffer sb = new FastStringBuffer();
        for (Line line = first; line != null; line = line.next()) {
            sb.append(line.getText());
            sb.append('\n');
            if (line == last)
                break;
        }
        return sb.toString();
    }
}
