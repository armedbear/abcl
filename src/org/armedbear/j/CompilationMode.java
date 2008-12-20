/*
 * CompilationMode.java
 *
 * Copyright (C) 2002-2005 Peter Graves
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

public final class CompilationMode extends AbstractMode implements Constants, Mode
{
    private static final CompilationMode mode = new CompilationMode();

    private CompilationMode()
    {
        super(COMPILATION_MODE, COMPILATION_MODE_NAME);
        setProperty(Property.VERTICAL_RULE, 0);
        setProperty(Property.SHOW_LINE_NUMBERS, false);
        setProperty(Property.SHOW_CHANGE_MARKS, false);
    }

    public static final CompilationMode getMode()
    {
        return mode;
    }

    protected void setKeyMapDefaults(KeyMap km)
    {
        km.mapKey(KeyEvent.VK_F9, 0, "compile");
        km.mapKey(KeyEvent.VK_F9, CTRL_MASK, "recompile");
        km.mapKey(KeyEvent.VK_ENTER, 0, "thisError");
        km.mapKey(KeyEvent.VK_G, CTRL_MASK | SHIFT_MASK, "thisError");
        km.mapKey(VK_DOUBLE_MOUSE_1, 0, "thisError");
        km.mapKey(VK_MOUSE_2, 0, "thisError");
        km.mapKey('q', "tempBufferQuit");
    }

    public Formatter getFormatter(Buffer buffer)
    {
        return new PlainTextFormatter(buffer);
    }
}
