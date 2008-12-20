/*
 * VerilogTagger.java
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

import gnu.regexp.RE;
import gnu.regexp.REMatch;
import gnu.regexp.UncheckedRE;
import java.util.ArrayList;

public final class VerilogTagger extends Tagger
{
    // We trim before matching, so "module" or "primitive" will appear without
    // any preceding whitespace.
    private static final RE moduleRE =
        new UncheckedRE("^module\\s+([a-zA-Z_][a-zA-Z0-9_$]*)");
    private static final RE primitiveRE =
        new UncheckedRE("^primitive\\s+([a-zA-Z_][a-zA-Z0-9_$]*)");

    public VerilogTagger(SystemBuffer buffer)
    {
        super(buffer);
    }

    public void run()
    {
        ArrayList tags = new ArrayList();
        Line line = buffer.getFirstLine();
        while (line != null) {
            String s = line.trim();
            if (s != null) {
                REMatch match;
                if (s.startsWith("module"))
                    match = moduleRE.getMatch(s);
                else if (s.startsWith("primitive"))
                    match = primitiveRE.getMatch(s);
                else
                    match = null;
                if (match != null) {
                    String name = s.substring(match.getSubStartIndex(1),
                        match.getSubEndIndex(1));
                    tags.add(new LocalTag(name, line));
                }
            }
            line = line.next();
        }
        buffer.setTags(tags);
    }
}
