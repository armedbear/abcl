/*
 * HiddenLines.java
 *
 * Copyright (C) 2002 Peter Graves
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

import java.util.ArrayList;

public final class HiddenLines
{
    private final Buffer buffer;
    private final ArrayList list;

    public HiddenLines(Editor editor)
    {
        buffer = editor.getBuffer();
        list = new ArrayList();
        int count = 0;
        int hidden = -1;
        for (Line line = buffer.getFirstLine(); line != null; line = line.next()) {
            if (hidden == line.getHidden()) {
                ++count;
            } else {
                if (count > 0)
                    addEntry(hidden, count);
                hidden = line.getHidden();
                count = 1;
            }
        }
        if (count > 0)
            addEntry(hidden, count);
    }

    public void restore()
    {
        Line line = buffer.getFirstLine();
        for (int i = 0; i < list.size(); i++) {
            HiddenLinesEntry entry = (HiddenLinesEntry) list.get(i);
            for (int j = 0; j < entry.getCount(); j++) {
                line.setHidden(entry.getHidden());
                line = line.next();
            }
        }
        buffer.renumber();
    }

    private final void addEntry(int hidden, int count)
    {
        list.add(new HiddenLinesEntry(hidden, count));
    }

    private static final class HiddenLinesEntry
    {
        private final int hidden;
        private final int count;

        private HiddenLinesEntry(int hidden, int count)
        {
            this.hidden = hidden;
            this.count = count;
        }

        private final int getHidden()
        {
            return hidden;
        }

        private final int getCount()
        {
            return count;
        }
    }
}
