/*  
 * FormatTableEntry.java
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

import java.awt.Color;

/*package*/ class FormatTableEntry
{
    private final int format;
    private final Color color;
    private final int style;

    /*package*/ FormatTableEntry(int format, Color color, int style)
    {
        this.format = format;
        this.color = color;
        this.style = style;
    }

    /*package*/ final int getFormat()
    {
        return format;
    }

    /*package*/ final Color getColor()
    {
        return color;
    }

    /*package*/ final int getStyle()
    {
        return style;
    }
}
