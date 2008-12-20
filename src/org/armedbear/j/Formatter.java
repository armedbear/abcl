/*
 * Formatter.java
 *
 * Copyright (C) 1998-2005 Peter Graves
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

import java.awt.Color;
import java.awt.Font;

public abstract class Formatter implements Constants
{
    protected Buffer buffer;

    protected FormatTable formatTable;
    protected Color colorCaret;
    protected Color colorBackground;
    protected Color colorCurrentLineBackground;
    protected Color colorSelectionBackground;
    protected Color colorMatchingBracketBackground;

    protected LineSegmentList segmentList = new LineSegmentList();

    public abstract LineSegmentList formatLine(Line line);
    public abstract FormatTable getFormatTable();

    public boolean parseBuffer()
    {
        buffer.setNeedsParsing(false);
        return false;
    }

    protected final boolean isKeyword(String s)
    {
        return buffer.isKeyword(s);
    }

    public Color getCaretColor()
    {
        if (colorCaret == null) {
             colorCaret = buffer.getMode().getColorProperty(Property.COLOR_CARET);
             if (colorCaret == null) {
                 colorCaret = buffer.getMode().getColorProperty(Property.COLOR_TEXT);
                 if (colorCaret == null)
                     colorCaret = DefaultTheme.getColor("caret");
             }
        }
        return colorCaret;
    }

    public Color getBackgroundColor()
    {
        if (colorBackground == null) {
            colorBackground = buffer.getMode().getColorProperty(Property.COLOR_BACKGROUND);
            if (colorBackground == null)
                colorBackground = DefaultTheme.getColor("background");
        }
        return colorBackground;
    }

    public Color getCurrentLineBackgroundColor()
    {
        if (colorCurrentLineBackground == null) {
            colorCurrentLineBackground = buffer.getMode().getColorProperty(Property.COLOR_CURRENT_LINE_BACKGROUND);
            if (colorCurrentLineBackground == null)
                colorCurrentLineBackground = DefaultTheme.getColor("currentLineBackground");
        }
        return colorCurrentLineBackground;
    }

    public Color getSelectionBackgroundColor()
    {
        if (colorSelectionBackground == null) {
            colorSelectionBackground = buffer.getMode().getColorProperty(Property.COLOR_SELECTION_BACKGROUND);
            if (colorSelectionBackground == null)
                colorSelectionBackground = DefaultTheme.getColor("selectionBackground");
        }
        return colorSelectionBackground;
    }

    public Color getMatchingBracketBackgroundColor()
    {
        if (colorMatchingBracketBackground == null) {
            colorMatchingBracketBackground = buffer.getMode().getColorProperty(Property.COLOR_MATCHING_BRACKET_BACKGROUND);
            if (colorMatchingBracketBackground == null)
                colorMatchingBracketBackground = DefaultTheme.getColor("matchingBracketBackground");
        }
        return colorMatchingBracketBackground;
    }

    public Color getColor(int format)
    {
        FormatTableEntry entry = getFormatTable().lookup(format);
        if (entry != null)
            return entry.getColor();
        return DefaultTheme.getColor("text");
    }

    public int getStyle(int format)
    {
        FormatTableEntry entry = getFormatTable().lookup(format);
        if (entry != null)
            return entry.getStyle();
        return Font.PLAIN;
    }

    public FormatTableEntry getFormatTableEntry(int format)
    {
        return getFormatTable().lookup(format);
    }

    public boolean getUnderline(int format)
    {
        return false;
    }

    public void reset()
    {
        colorCaret = null;
        colorBackground = null;
        colorCurrentLineBackground = null;
        colorSelectionBackground = null;
        colorMatchingBracketBackground = null;
        formatTable = null;
    }

    protected final void addSegment(String text, int begin, int end, int format)
    {
        segmentList.addSegment(new LineSegment(text, begin, end, format));
    }

    protected final void addSegment(String text, int begin, int format)
    {
        segmentList.addSegment(new LineSegment(text, begin, text.length(), format));
    }

    protected final void addSegment(String text, int format)
    {
        segmentList.addSegment(new LineSegment(text, format));
    }

    protected final LineSegment getLastSegment()
    {
        return segmentList.getLastSegment();
    }

    protected final void clearSegmentList()
    {
        segmentList.clear();
    }

    protected final String getDetabbedText(Line line)
    {
        if (Editor.tabsAreVisible())
            return Utilities.makeTabsVisible(line.getText(), buffer.getTabWidth());
        return Utilities.detab(line.getText(), buffer.getTabWidth());
    }
}
