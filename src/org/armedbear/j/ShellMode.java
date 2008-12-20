/*
 * ShellMode.java
 *
 * Copyright (C) 1999-2003 Peter Graves
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

public final class ShellMode extends AbstractMode implements Constants, Mode
{
    private static final ShellMode mode = new ShellMode();

    protected ShellMode()
    {
        super(SHELL_MODE, SHELL_MODE_NAME);
        setProperty(Property.VERTICAL_RULE, 0);
        setProperty(Property.SHOW_LINE_NUMBERS, false);
        setProperty(Property.SHOW_CHANGE_MARKS, false);
    }

    public static final ShellMode getMode()
    {
        return mode;
    }

    protected void setKeyMapDefaults(KeyMap km)
    {
        km.mapKey(KeyEvent.VK_HOME, 0, "shellHome");
        km.mapKey(KeyEvent.VK_BACK_SPACE, 0, "shellBackspace");
        km.mapKey(KeyEvent.VK_ESCAPE, 0, "shellEscape");
        km.mapKey(KeyEvent.VK_P, CTRL_MASK, "shellPreviousInput");
        km.mapKey(KeyEvent.VK_N, CTRL_MASK, "shellNextInput");
        km.mapKey(KeyEvent.VK_P, CTRL_MASK | ALT_MASK, "shellPreviousPrompt");
        km.mapKey(KeyEvent.VK_N, CTRL_MASK | ALT_MASK, "shellNextPrompt");
        km.mapKey(KeyEvent.VK_ENTER, 0, "shellEnter");
        km.mapKey(KeyEvent.VK_TAB, 0, "shellTab");
        km.mapKey(KeyEvent.VK_C, CTRL_MASK | ALT_MASK, "shellInterrupt");
        km.mapKey(KeyEvent.VK_T, CTRL_MASK, "findTag");
        km.mapKey(KeyEvent.VK_F9, CTRL_MASK, "recompile");
        km.mapKey(')', "closeParen");
    }

    public Formatter getFormatter(Buffer buffer)
    {
        return new ShellFormatter(buffer);
    }
}
