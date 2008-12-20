/*
 * Table.java
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

// An HTML table.
public final class Table
{
    private int columnIndex;
    private int[] widths = new int[10];

    public Table()
    {
    }

    public final int getColumnIndex()
    {
        return columnIndex;
    }

    public final void nextRow()
    {
        columnIndex = -1;
    }

    public void nextColumn()
    {
        ++columnIndex;
        Debug.assertTrue(columnIndex >= 0);
        if (columnIndex >= widths.length) {
            int[] newArray = new int[widths.length * 2 + 2];
            System.arraycopy(widths, 0, newArray, 0, widths.length);
            widths = newArray;
        }
    }

    // Sets width of current column.
    public void setColumnWidth(int width)
    {
        Debug.assertTrue(columnIndex >= 0);
        Debug.assertTrue(columnIndex < widths.length);
        if (width > widths[columnIndex])
            widths[columnIndex] = width;
    }

    // Returns width of current column.
    public int getColumnWidth()
    {
        Debug.assertTrue(columnIndex >= 0);
        Debug.assertTrue(columnIndex < widths.length);
        return widths[columnIndex];
    }

    // Returns minumum offset of start of current column, based on column
    // widths of columns to the left of it.
    public int getMinimumOffset()
    {
        Debug.assertTrue(columnIndex >= 0);
        Debug.assertTrue(columnIndex < widths.length);
        int offset = 0;
        for (int i = 0; i < columnIndex; i++)
            offset += widths[i];
        return offset;
    }
}
