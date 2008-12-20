/*
 * JdbMode.java
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

package org.armedbear.j.jdb;

import org.armedbear.j.AbstractMode;
import org.armedbear.j.Buffer;
import org.armedbear.j.Constants;
import org.armedbear.j.Formatter;
import org.armedbear.j.Frame;
import org.armedbear.j.JavaMode;
import org.armedbear.j.Mode;
import org.armedbear.j.Property;
import org.armedbear.j.ToolBar;

public final class JdbMode extends AbstractMode implements Constants, Mode
{
    private static final JdbMode mode = new JdbMode();

    private JdbMode()
    {
        super(JDB_MODE, JDB_MODE_NAME);
        setProperty(Property.VERTICAL_RULE, 0);
        setProperty(Property.SHOW_LINE_NUMBERS, false);
        setProperty(Property.SHOW_CHANGE_MARKS, false);
    }

    public static final JdbMode getMode()
    {
        return mode;
    }

    public ToolBar getToolBar(Frame frame)
    {
        return JavaMode.getMode().getToolBar(frame);
    }

    public Formatter getFormatter(Buffer buffer)
    {
        return new JdbFormatter(buffer);
    }
}
