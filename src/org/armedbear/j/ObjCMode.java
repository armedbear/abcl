/*
 * ObjCMode.java
 *
 * Copyright (C) 2003-2004 Peter Graves
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

public final class ObjCMode extends CMode implements Constants, Mode
{
    private static final String[] objcConditionals = {
        "if",
        "else",
        "do",
        "while",
        "for",
        "switch"
    };

    private static ObjCMode mode = new ObjCMode();

    private ObjCMode()
    {
        super(OBJC_MODE, OBJC_MODE_NAME);
        keywords = new Keywords(this);
        conditionals = objcConditionals;
    }

    public static Mode getMode()
    {
        return mode;
    }

    public final String getCommentStart()
    {
        return "// ";
    }

    public final String getCommentEnd()
    {
        return null;
    }

    public Formatter getFormatter(Buffer buffer)
    {
        return new CFormatter(buffer, LANGUAGE_OBJC);
    }

    public Tagger getTagger(SystemBuffer buffer)
    {
        return new ObjCTagger(buffer);
    }

    public boolean isIdentifierStart(char c)
    {
        if (c == '@')
            return true;
        return super.isIdentifierStart(c);
    }
}
