/*
 * DiffOutputBuffer.java
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

public final class DiffOutputBuffer extends Buffer
{
    private final File directory;
    private final int vcType;

    public DiffOutputBuffer(Buffer parentBuffer, String output, int vcType)
    {
        super();
        this.parentBuffer = parentBuffer;
        directory =
            (parentBuffer == null) ? null : parentBuffer.getCurrentDirectory();
        this.vcType = vcType;
        init();
        setText(output);
    }

    public DiffOutputBuffer(File directory, String output, int vcType)
    {
        super();
        this.directory = directory;
        this.vcType = vcType;
        init();
        setText(output);
    }

    public final File getCurrentDirectory()
    {
        return directory;
    }

    private void init()
    {
        supportsUndo  = false;
        type = TYPE_OUTPUT;
        mode = DiffMode.getMode();
        formatter = new DiffFormatter(this);
        lineSeparator = System.getProperty("line.separator");
        readOnly = true;
        setProperty(Property.VERTICAL_RULE, 0);
        setProperty(Property.SHOW_LINE_NUMBERS, false);
        setTransient(true);
        setInitialized(true);
    }

    public final File getDirectory()
    {
        return directory;
    }

    public final int getVCType()
    {
        return vcType;
    }

    public String getFileNameForDisplay()
    {
        return title != null ? title : "";
    }
}
