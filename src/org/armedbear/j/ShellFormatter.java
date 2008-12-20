/*
 * ShellFormatter.java
 *
 * Copyright (C) 1998-2005 Peter Graves
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

public final class ShellFormatter extends Formatter
{
    // Formats.
    private static final byte SHELL_FORMAT_TEXT   = 0;
    private static final byte SHELL_FORMAT_PROMPT = 1;
    private static final byte SHELL_FORMAT_INPUT  = 2;

    public ShellFormatter(Buffer buffer)
    {
        this.buffer = buffer;
    }

    public LineSegmentList formatLine(final Line line)
    {
        clearSegmentList();
        if (line == null) {
            addSegment("", SHELL_FORMAT_TEXT);
            return segmentList;
        }
        final RE promptRE = ((Shell)buffer).getPromptRE();
        final String text = getDetabbedText(line);
        final int flags = line.flags();
        if (flags == STATE_PROMPT) {
            REMatch match = promptRE.getMatch(text);
            if (match != null) {
                final int end = match.getEndIndex();
                addSegment(text, 0, end, SHELL_FORMAT_PROMPT);
                addSegment(text, end, SHELL_FORMAT_INPUT);
            } else
                addSegment(text, SHELL_FORMAT_PROMPT);
            return segmentList;
        }
        if (flags == STATE_PASSWORD_PROMPT) {
            addSegment(text, SHELL_FORMAT_TEXT);
            return segmentList;
        }
        if (flags == STATE_OUTPUT) {
            addSegment(text, SHELL_FORMAT_TEXT);
            return segmentList;
        }
        if (promptRE != null) {
            if (flags == STATE_INPUT) {
                REMatch match = promptRE.getMatch(text);
                if (match != null) {
                    final int end = match.getEndIndex();
                    addSegment(text, 0, end, SHELL_FORMAT_PROMPT);
                    addSegment(text, end, SHELL_FORMAT_INPUT);
                } else {
                    // No prompt text.
                    addSegment(text, SHELL_FORMAT_INPUT);
                }
                return segmentList;
            }
            Line next = line.next();
            if (next == null) {
                // Last line of buffer. Check for prompt.
                REMatch match = promptRE.getMatch(text);
                if (match != null) {
                    line.setFlags(STATE_PROMPT);
                    final int end = match.getEndIndex();
                    addSegment(text, 0, end, SHELL_FORMAT_PROMPT);
                    addSegment(text, end, SHELL_FORMAT_INPUT);
                } else
                    addSegment(text, SHELL_FORMAT_INPUT);
                return segmentList;
            }
        }
        addSegment(text, SHELL_FORMAT_TEXT);
        return segmentList;
    }

    public FormatTable getFormatTable()
    {
        if (formatTable == null) {
            formatTable = new FormatTable("ShellMode");
            formatTable.addEntryFromPrefs(SHELL_FORMAT_TEXT, "text");
            formatTable.addEntryFromPrefs(SHELL_FORMAT_PROMPT, "prompt");
            formatTable.addEntryFromPrefs(SHELL_FORMAT_INPUT, "input" );
        }
        return formatTable;
    }
}
