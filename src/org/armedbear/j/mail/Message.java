/*
 * Message.java
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

import java.util.List;
import java.util.Vector;
import org.armedbear.j.Headers;

public final class Message extends MimePart
{
    private Vector messageParts;

    public Message(String raw)
    {
        super(raw);
    }

    public Message(String raw, Headers headers)
    {
        super(raw, headers);
    }

    public Vector getParts()
    {
        return messageParts;
    }

    public MimePart getPart(int i)
    {
        if (messageParts == null)
            return null;
        if (i < 0)
            return null;
        if (i >= messageParts.size())
            return null;
        return (MimePart) messageParts.get(i);
    }

    public void parse()
    {
        super.parse();
        Vector parts = super.getParts();
        if (parts == null || parts.size() == 0)
            return;
        Vector v = new Vector();
        addParts(v);
        messageParts = v;
    }
}
