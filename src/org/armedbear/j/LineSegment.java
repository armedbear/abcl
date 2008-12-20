/*
 * LineSegment.java
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

public class LineSegment
{
    private String text;
    private int begin;
    private int end;
    private int format;

    public LineSegment(String text, int format)
    {
        this.text = text;
        begin = 0;
        end = text.length();
        this.format = format;
    }

    public LineSegment(String text, int begin, int end, int format)
    {
        this.text = text;
        this.begin = begin;
        this.end = end;
        this.format = format;
    }

    public final String getText()
    {
        return text.substring(begin, end);
    }

    public final int getFormat()
    {
        return format;
    }

    public final void setFormat(int format)
    {
        this.format = format;
    }

    public final int length()
    {
        return end - begin;
    }

    public final void getChars(int srcBegin, int srcEnd, char[] dst, int dstBegin)
    {
        text.getChars(begin + srcBegin, begin + srcEnd, dst, dstBegin);
    }

    public final String substring(int beginIndex)
    {
        return text.substring(begin + beginIndex, end);
    }
}
