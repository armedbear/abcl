/*
 * PrintPainter.java
 *
 * Copyright (C) 2002 Peter Graves
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

import java.awt.Graphics;
import java.awt.print.Printable;
import java.awt.print.PrinterException;

public final class PrintPainter implements Printable
{
    private final Editor editor;
    private final Buffer buffer;
    private final Region region;
    private int currentPage = -1;
    private Line topLine;
    private Line line;
    private Line endLine;

    public PrintPainter(Editor editor)
    {
        this.editor = editor;
        buffer = editor.getBuffer();
        region = null;
        line = buffer.getFirstLine();
    }

    public PrintPainter(Editor editor, Region region)
    {
        this.editor = editor;
        buffer = editor.getBuffer();
        this.region = region;
        line = region.getBeginLine();
        endLine = region.getEndLine();
    }

    public int print(Graphics g, java.awt.print.PageFormat pf, int pageIndex) throws PrinterException
    {
        if (pageIndex != currentPage) {
            currentPage = pageIndex;
            topLine = line;
        } else
            line = topLine;
        if (topLine == null)
            return NO_SUCH_PAGE;
        PageFormat pageFormat = (PageFormat) pf;
        int fieldWidth = 5; // Safe.
        boolean printLineNumbers = buffer.getBooleanProperty(Property.SHOW_LINE_NUMBERS);
        if (printLineNumbers) {
            int maxLineNumber = topLine.lineNumber() + pageFormat.getLinesPerPage();
            if (endLine != null && endLine.lineNumber() + 1 < maxLineNumber)
                maxLineNumber = endLine.lineNumber() + 1;
            fieldWidth = String.valueOf(maxLineNumber).length();
        }
        g.setFont(pageFormat.getFont());
        pageFormat.printHeader(g, pageIndex);
        for (int i = 0; i < pageFormat.getLinesPerPage(); i++) {
            if (line == null)
                break;
            if (line == endLine)
                break;
            String s = Utilities.detab(line.getText(), buffer.getTabWidth());
            if (printLineNumbers) {
                FastStringBuffer sb = new FastStringBuffer(Utilities.rightJustify(line.lineNumber() + 1, fieldWidth));
                sb.append(' ');
                sb.append(s);
                s = sb.toString();
            }
            int y = pageFormat.getY(i);
            g.drawString(s, (int) pageFormat.getImageableX(), y);
            line = line.next();
        }
        pageFormat.printFooter(g, pageIndex);
        return Printable.PAGE_EXISTS;
    }
}
