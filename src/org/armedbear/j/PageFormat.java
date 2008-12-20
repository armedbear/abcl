/*
 * PageFormat.java
 *
 * Copyright (C) 2002-2008 Peter Graves
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
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
 */

package org.armedbear.j;

import java.awt.Color;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.Graphics2D;
import java.awt.Graphics;
import java.awt.Toolkit;
import java.awt.print.Paper;
import java.util.Date;

public final class PageFormat extends java.awt.print.PageFormat
{
    private static final int INCH = 72;

    private int linesPerPage;
    private Font font;
    private Font headerFont;
    private Font footerFont;
    private int lineHeight;
    private String header;
    private String date;
    private int pageCount;

    public PageFormat(Editor editor, Region region)
    {
        super();
        Buffer buffer = editor.getBuffer();
        if (buffer.getFile() != null) {
            header = buffer.getFile().netPath();
            if (header.length() > 60)
                header = buffer.getFile().getName();
            if (region != null) {
                FastStringBuffer sb = new FastStringBuffer(header);
                sb.append(" (lines ");
                sb.append(region.getBeginLine().lineNumber() + 1);
                sb.append('-');
                sb.append(region.getEndLine().lineNumber());
                sb.append(')');
                header = sb.toString();
            }
        }
        date = new Date().toString();
        String fontName = buffer.getStringProperty(Property.PRINTER_FONT_NAME);
        int fontSize = buffer.getIntegerProperty(Property.PRINTER_FONT_SIZE);
        if (fontSize <= 0)
            fontSize = 10; // default
        font = new Font(fontName, Font.PLAIN, fontSize);
        headerFont = footerFont = new Font(fontName, Font.BOLD, fontSize);
        lineHeight = fontSize + 1;
        Paper paper = getPaper();
        paper.setImageableArea(0.5 * INCH, 0.5 * INCH, 7.5 * INCH, 10 * INCH);
        setPaper(paper);
        int height = (int) getImageableHeight();
        // Adjust for descenders on last line.
        FontMetrics fm = Toolkit.getDefaultToolkit().getFontMetrics(font);
        height -= fm.getMaxDescent();
        linesPerPage = height / lineHeight;
        if (header != null)
            linesPerPage -= 2;
        linesPerPage -= 3; // footer
    }

    public final Font getFont()
    {
        return font;
    }

    public final int getLinesPerPage()
    {
        return linesPerPage;
    }

    public final int getY(int i)
    {
        int y = (int) getImageableY();
        if (header != null)
            y += lineHeight * 2;
        y += lineHeight * (i+1);
        return y;
    }

    public void printHeader(Graphics g, int pageIndex)
    {
        if (header != null) {
            Graphics2D g2d = (Graphics2D) g.create();
            g2d.setPaint(Color.black);
            g2d.setFont(headerFont);
            int x = (int) getImageableX();
            int y = (int) getImageableY() + lineHeight;
            g2d.drawString(header, x, y);
        }
    }

    public void printFooter(Graphics g, int pageIndex)
    {
        Graphics2D g2d = (Graphics2D) g.create();
        g2d.setPaint(Color.black);
        g2d.setFont(footerFont);
        int x = (int) getImageableX();
        int y = (int) getImageableY();
        if (header != null)
            y += lineHeight * 2;
        y += lineHeight * (linesPerPage + 2);
        g2d.drawString(date, x, y);
        FastStringBuffer sb = new FastStringBuffer("Page ");
        sb.append(pageIndex + 1);
        if (pageCount != 0) {
            sb.append(" of ");
            sb.append(pageCount);
        }
        String s = sb.toString();
        FontMetrics fm = Toolkit.getDefaultToolkit().getFontMetrics(footerFont);
        x = (int) getImageableX() + (int) getImageableWidth() - fm.stringWidth(s);
        g2d.drawString(s, x, y);
    }

    public void setPageCount(int pageCount)
    {
        this.pageCount = pageCount;
    }
}
