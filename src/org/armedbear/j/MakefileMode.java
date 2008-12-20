/*
 * MakefileMode.java
 *
 * Copyright (C) 2000-2002 Peter Graves
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

import java.awt.event.InputEvent;
import java.awt.event.KeyEvent;

public final class MakefileMode extends AbstractMode implements Constants, Mode
{
    private static final MakefileMode mode = new MakefileMode();

    private MakefileMode()
    {
        super(MAKEFILE_MODE, MAKEFILE_MODE_NAME);
        keywords = new Keywords(this);
        setProperty(Property.USE_TABS, true);
    }

    public static final MakefileMode getMode()
    {
        return mode;
    }

    public boolean canIndent()
    {
        return true;
    }

    public String getCommentStart()
    {
        return "# ";
    }

    public Formatter getFormatter(Buffer buffer)
    {
        return new MakefileFormatter(buffer);
    }

    protected void setKeyMapDefaults(KeyMap km)
    {
        km.mapKey(KeyEvent.VK_ENTER, 0, "newlineAndIndent");
        km.mapKey(KeyEvent.VK_F9, 0, "compile");
        km.mapKey(KeyEvent.VK_F9, InputEvent.CTRL_MASK, "recompile");
    }

    public int getCorrectIndentation(Line line, Buffer buffer)
    {
        Line model = getModel(line);
        if (model == null)
            return 0;
        return buffer.getIndentation(model);
    }

    private static Line getModel(Line line)
    {
        Line model = line.previous();
        while (model != null) {
            if (model.isBlank() || model.charAt(0) == '#')
                model = model.previous();
            else
                break;
        }
        return model;
    }

    private static final String validChars =
        "-./0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ_abcdefghijklmnopqrstuvwxyz";

    public boolean isIdentifierStart(char c)
    {
        return validChars.indexOf(c) >= 0;
    }

    public boolean isIdentifierPart(char c)
    {
        return validChars.indexOf(c) >= 0;
    }
}
