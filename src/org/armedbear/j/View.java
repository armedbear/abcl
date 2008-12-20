/*
 * View.java
 *
 * Copyright (C) 1998-2003 Peter Graves
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

import org.armedbear.j.mail.MailboxEntry;

public final class View implements Cloneable
{
    Position dot;
    Position mark;
    Selection selection;
    private boolean isColumnSelection;
    Line topLine;
    int pixelsAboveTopLine;
    int shift;
    int caretCol;
    int lineNumber;
    int offs;
    int topLineNumber;
    long timestamp;

    private MailboxEntry topEntry;
    private MailboxEntry dotEntry;
    private NavigationComponent sidebarComponent;

    public View()
    {
    }

    public View(SessionBufferEntry entry)
    {
        lineNumber = entry.getDotLineNumber();
        offs = entry.getDotOffset();
    }

    public Position getDot()
    {
        return dot;
    }

    public int getDotOffset()
    {
        return offs;
    }

    public void setDot(Position pos)
    {
        dot = pos != null ? new Position(pos) : null;
    }

    public Position getMark()
    {
        return mark;
    }

    public void setMark(Position pos)
    {
        mark = pos != null ? new Position(pos) : null;
    }

    public boolean isColumnSelection()
    {
        return isColumnSelection;
    }

    public void setColumnSelection(boolean b)
    {
        isColumnSelection = b;
    }

    public int getDotLineNumber()
    {
        return lineNumber;
    }

    public Line getTopLine()
    {
        return topLine;
    }

    public int getTopLineNumber()
    {
        return topLineNumber;
    }

    public MailboxEntry getTopEntry()
    {
        return topEntry;
    }

    public void setTopEntry(MailboxEntry entry)
    {
        topEntry = entry;
    }

    public MailboxEntry getDotEntry()
    {
        return dotEntry;
    }

    public void setDotEntry(MailboxEntry entry)
    {
        dotEntry = entry;
    }

    public Selection getSelection()
    {
        return selection;
    }

    public int getShift()
    {
        return shift;
    }

    public int getCaretCol()
    {
        return caretCol;
    }

    public void setCaretCol(int col)
    {
        caretCol = col;
    }

    public NavigationComponent getSidebarComponent()
    {
        return sidebarComponent;
    }

    public void setSidebarComponent(NavigationComponent c)
    {
        sidebarComponent = c;
    }

    public void invalidate()
    {
        if (dot != null) {
            lineNumber = dot.lineNumber();
            offs = dot.getOffset();
        }
        dot = null;
        mark = null;
        selection = null;
        isColumnSelection = false;
        if (topLine != null)
            topLineNumber = topLine.lineNumber();
        topLine = null;
        pixelsAboveTopLine = 0;
        if (!(sidebarComponent instanceof DirectoryTree))
            sidebarComponent = null;
    }

    protected Object clone()
    {
        View view = new View();
        if (dot != null)
            view.dot = new Position(dot);
        if (mark != null)
            view.mark = new Position(mark);
        view.selection = selection;
        view.isColumnSelection = isColumnSelection;
        view.topLine = topLine;
        view.shift = shift;
        view.caretCol = caretCol;
        view.lineNumber = lineNumber;
        view.offs = offs;
        view.topLineNumber = topLineNumber;
        view.pixelsAboveTopLine = pixelsAboveTopLine;
        return view;
    }
}
