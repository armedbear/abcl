/*
 * PythonMode.java
 *
 * Copyright (C) 2002-2003 Peter Graves
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

import java.awt.event.KeyEvent;

public final class PythonMode extends AbstractMode implements Constants, Mode
{
    private static final PythonMode mode = new PythonMode();

    private PythonMode()
    {
        super(PYTHON_MODE, PYTHON_MODE_NAME);
        keywords = new Keywords(this);
    }

    public static final PythonMode getMode()
    {
        return mode;
    }

    public boolean canIndent()
    {
        return true;
    }

    public boolean canIndentPaste()
    {
        return false;
    }

    public final SyntaxIterator getSyntaxIterator(Position pos)
    {
        return new PythonSyntaxIterator(pos);
    }

    public final String getCommentStart()
    {
        return "#";
    }

    public final Formatter getFormatter(Buffer buffer)
    {
        return new PythonFormatter(buffer);
    }

    protected void setKeyMapDefaults(KeyMap km)
    {
        km.mapKey(KeyEvent.VK_TAB, 0, "tab");
        km.mapKey(KeyEvent.VK_TAB, SHIFT_MASK, "slideOut");
        km.mapKey(KeyEvent.VK_TAB, CTRL_MASK, "insertTab");
        km.mapKey(KeyEvent.VK_ENTER, 0, "newlineAndIndent");
        km.mapKey(KeyEvent.VK_I, ALT_MASK, "cycleIndentSize");
    }

    public final boolean isTaggable()
    {
        return true;
    }

    public final Tagger getTagger(SystemBuffer buffer)
    {
        return new PythonTagger(buffer);
    }

    public int getCorrectIndentation(Line line, Buffer buffer)
    {
        return new PythonIndenter(line, buffer).getCorrectIndentation();
    }

    public final boolean isIdentifierStart(char c)
    {
        if (c > 127)
            return false;
        return values[c] == 1;
    }

    public final boolean isIdentifierPart(char c)
    {
        if (c > 127)
            return false;
        return values[c] != 0;
    }

    private static final byte values[] =
    {
        0, 0, 0, 0, 0, 0, 0, 0, // 0x00-0x07
        0, 0, 0, 0, 0, 0, 0, 0, // 0x09-0xff
        0, 0, 0, 0, 0, 0, 0, 0, // 0x10-0x17
        0, 0, 0, 0, 0, 0, 0, 0, // 0x18-0x1f
        0, 0, 0, 0, 0, 0, 0, 0, // 0x20-0x27   !"#$%&'
        0, 0, 0, 0, 0, 0, 0, 0, // 0x28-0x2f  ()*+,-./
        2, 2, 2, 2, 2, 2, 2, 2, // 0x30-0x37  01234567
        2, 2, 0, 0, 0, 0, 0, 0, // 0x38-0x40  89:;<=>?
        0, 1, 1, 1, 1, 1, 1, 1, // 0x41-0x47  @ABCDEFG
        1, 1, 1, 1, 1, 1, 1, 1, // 0x48-0x4f  HIJKLMNO
        1, 1, 1, 1, 1, 1, 1, 1, // 0x50-0x57  PQRSTUVW
        1, 1, 1, 0, 0, 0, 0, 1, // 0x58-0x5f  XYZ[\]^_
        0, 1, 1, 1, 1, 1, 1, 1, // 0x60-0x67  `abcdefg
        1, 1, 1, 1, 1, 1, 1, 1, // 0x68-0x6f  hijklmno
        1, 1, 1, 1, 1, 1, 1, 1, // 0x70-0x77  pqrstuvw
        1, 1, 1, 0, 0, 0, 0, 0  // 0x78-0x7f  xyz{|}~
    };
}
