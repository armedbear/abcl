/*
 * NewsGroupsMode.java
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

package org.armedbear.j.mail;

import java.awt.event.KeyEvent;
import org.armedbear.j.AbstractMode;
import org.armedbear.j.Constants;
import org.armedbear.j.KeyMap;
import org.armedbear.j.Mode;
import org.armedbear.j.Property;

public final class NewsGroupsMode extends AbstractMode implements Constants,
    Mode
{
    private static final Mode mode = new NewsGroupsMode();

    private NewsGroupsMode()
    {
        super(NEWS_GROUPS_MODE, NEWS_GROUPS_MODE_NAME);
        setProperty(Property.VERTICAL_RULE, 0);
        setProperty(Property.SHOW_LINE_NUMBERS, false);
    }

    public static final Mode getMode()
    {
        return mode;
    }

    protected final void setKeyMapDefaults(KeyMap km)
    {
        km.mapKey(KeyEvent.VK_ENTER, 0, "openGroupAtDot");
    }
}
