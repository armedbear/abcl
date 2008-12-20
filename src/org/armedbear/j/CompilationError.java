/*
 * CompilationError.java
 *
 * Copyright (C) 2003 Peter Graves
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

public final class CompilationError
{
    private final Line errorLine;
    private final String fileName;
    private final int lineNumber;
    private final int offset;
    private final String message;

    private CompilationError(Line errorLine, String fileName, int lineNumber,
        int offset, String message)
    {
        this.errorLine = errorLine;
        this.fileName = fileName;
        this.lineNumber = lineNumber;
        this.offset = offset;
        this.message = message;
    }

    public Line getErrorLine()
    {
        return errorLine;
    }

    public String getFileName()
    {
        return fileName;
    }

    public int getLineNumber()
    {
        return lineNumber;
    }

    public int getOffset()
    {
        return offset;
    }

    public String getMessage()
    {
        return message;
    }

    public static CompilationError parseLineAsErrorMessage(final Line line)
    {
        String text = line.trim();
        if (text.startsWith("[javac]")) {
            // Ant.
            text = text.substring(7).trim();
        }
        String lookFor = ") : error ";
        int index = text.indexOf(lookFor);
        if (index < 0) {
            lookFor = ") : warning ";
            index = text.indexOf(lookFor);
        }
        if (index >= 0) {
            // Microsoft C/C++.
            int end = text.indexOf('(');
            if (end >= 0) {
                String fileName = text.substring(0, end);
                String s = text.substring(end + 1, index);
                int lineNumber = 0;
                try {
                    lineNumber = Integer.parseInt(s);
                }
                catch (NumberFormatException e) {
                    return null;
                }
                if (lineNumber > 0) {
                    // We have a winner. Look for error message on same line.
                    String remainder = text.substring(index + lookFor.length());
                    String message;
                    if ((index = remainder.indexOf(": ")) >= 0)
                        message = remainder.substring(index + 2).trim();
                    else
                        message = remainder.trim();
                    if (message.length() == 0)
                        message = null;
                    return new CompilationError(line, fileName, lineNumber, -1,
                        message);
                }
            }
            return null;
        }
        index = text.indexOf(':');
        if (Platform.isPlatformWindows() && index == 1) {
            // The file name starts with a drive specifier ("C:").
            // We want the next ':', not this one.
            index = text.indexOf(':', 2);
        }
        if (index >= 0) {
            String fileName = text.substring(0, index).trim();
            String remainder = text.substring(index + 1);
            index = remainder.indexOf(':');
            if (index >= 0) {
                String s = remainder.substring(0, index);
                int lineNumber = 0;
                try {
                    lineNumber = Integer.parseInt(s);
                }
                catch (NumberFormatException e) {
                    return null;
                }
                if (lineNumber > 0) {
                    // We have a winner. Maybe there's a column number too...
                    int offset = -1;
                    remainder = remainder.substring(index + 1);
                    index = remainder.indexOf(':');
                    if (index >= 0) {
                        // Found a colon.
                        s = remainder.substring(0, index);
                        try {
                            offset = Integer.parseInt(s) - 1;
                        }
                        catch (NumberFormatException e) {
                            // No column number.
                        }
                    }
                    // Look for error message on same line.
                    String message;
                    if ((index = remainder.indexOf(": ")) >= 0)
                        message = remainder.substring(index + 2).trim();
                    else
                        message = remainder.trim();
                    if (message.length() == 0)
                        message = null;
                    return new CompilationError(line, fileName, lineNumber,
                        offset, message);
                }
            }
        }
        return null;
    }
}
