/*
 * PrintCommands.java
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

import java.awt.print.Book;
import java.awt.print.PrinterException;
import java.awt.print.PrinterJob;

public final class PrintCommands
{
    public static void print()
    {
        final Editor editor = Editor.currentEditor();
        if (editor.getMark() != null &&
            editor.getMarkLine() != editor.getDotLine() &&
            editor.getDotOffset() == 0 &&
            editor.getMarkOffset() == 0) {
            printRegion();
        } else {
            printBuffer();
        }
    }

    public static void printRegion()
    {
        final Editor editor = Editor.currentEditor();
        if (editor.getMark() == null)
            return;
        if (editor.getMarkLine() == editor.getDotLine())
            return;
        if (editor.getDotOffset() != 0)
            return;
        if (editor.getMarkOffset() != 0)
            return;
        Region r = new Region(editor);
        int lineCount = r.getEndLine().lineNumber() - r.getBeginLine().lineNumber();
        final String title = "Print Region";
        FastStringBuffer sb = new FastStringBuffer("Print selected region (");
        sb.append(lineCount);
        sb.append(" line");
        if (lineCount > 1)
            sb.append('s');
        sb.append(")?");
        String prompt = sb.toString();
        if (!editor.confirm(title, prompt))
            return;
        editor.setWaitCursor();
        PrinterJob job = PrinterJob.getPrinterJob();
        PageFormat pageFormat = new PageFormat(editor, r);
        PrintPainter painter = new PrintPainter(editor, r);
        Book book = new Book();
        int pages = lineCount / pageFormat.getLinesPerPage();
        if (lineCount % pageFormat.getLinesPerPage() != 0)
            ++pages;
        book.append(painter, pageFormat, pages); // All pages.
        pageFormat.setPageCount(pages);
        job.setPageable(book);
        try {
            job.print();
        }
        catch (PrinterException e) {
            Log.error(e);
        }
        editor.setDefaultCursor();
    }

    public static void printBuffer()
    {
        final Editor editor = Editor.currentEditor();
        final Buffer buffer = editor.getBuffer();
        final String title = "Print Buffer";
        int lineCount = buffer.getLineCount();
        FastStringBuffer sb = new FastStringBuffer("Print current buffer in its entirety (");
        sb.append(lineCount);
        sb.append(" line");
        if (lineCount > 1)
            sb.append('s');
        sb.append(")?");
        String prompt = sb.toString();
        if (!editor.confirm(title, prompt))
            return;
        editor.setWaitCursor();
        PrinterJob job = PrinterJob.getPrinterJob();
        PageFormat pageFormat = new PageFormat(editor, null);
        PrintPainter painter = new PrintPainter(editor);
        Book book = new Book();
        int pages = lineCount / pageFormat.getLinesPerPage();
        if (lineCount % pageFormat.getLinesPerPage() != 0)
            ++pages;
        book.append(painter, pageFormat, pages); // All pages.
        pageFormat.setPageCount(pages);
        job.setPageable(book);
        try {
            job.print();
        }
        catch (PrinterException e) {
            Log.error(e);
        }
        editor.setDefaultCursor();
    }
}
