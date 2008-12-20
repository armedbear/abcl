/*
 * WebLine.java
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

import java.io.UnsupportedEncodingException;

public class WebLine extends AbstractLine implements Line
{
    private int flags;
    private String text;
    private LineSegmentList segmentList;

    // Offset of start of line in original document.
    private int sourceOffset;

    // Constructs an empty line.
    public WebLine(int sourceOffset)
    {
        this.sourceOffset = sourceOffset;
    }

    public WebLine(LineSegmentList segmentList, int sourceOffset)
    {
        this.segmentList = segmentList;
        this.sourceOffset = sourceOffset;
    }

    public final int flags()
    {
        return flags;
    }

    public final void setFlags(int flags)
    {
        this.flags = flags;
    }

    public final String getText()
    {
        if (text == null) {
            if (segmentList != null) {
                FastStringBuffer sb = new FastStringBuffer(256);
                for (int i = 0; i < segmentList.size(); i++)
                    sb.append(segmentList.getSegment(i).getText());
                text = sb.toString();
            } else
                text = "";
        }
        return text;
    }

    public final int getSourceOffset()
    {
        return sourceOffset;
    }

    public HtmlLineSegment findSegment(int offset)
    {
        if (segmentList == null)
            return null;
        int begin = 0;
        for (int i = 0; i < segmentList.size(); i++) {
            HtmlLineSegment segment = (HtmlLineSegment) segmentList.getSegment(i);
            String segmentText = segment.getText();
            int end = begin + segmentText.length();
            if (offset >= begin && offset < end)
                return segment;
            begin = end;
        }
        return null;
    }

    public final void setText(String s)
    {
        text = s;
    }

    public final char charAt(int i)
    {
        return getText().charAt(i);
    }

    public final String substring(int beginIndex)
    {
        return getText().substring(beginIndex);
    }

    public final String substring(int beginIndex, int endIndex)
    {
        return getText().substring(beginIndex, endIndex);
    }

    public final String trim()
    {
        return getText().trim();
    }

    public final int length()
    {
        return getText().length();
    }

    public final byte[] getBytes(String encoding) throws UnsupportedEncodingException
    {
        return getText().getBytes(encoding);
    }

    public final boolean isBlank()
    {
        if (text == null)
            text = getText();
        for (int i = text.length(); i-- > 0;)
            if (!Character.isWhitespace(text.charAt(i)))
                return false;
        return true;
    }

    public final LineSegmentList getSegmentList()
    {
        return segmentList;
    }
}
