/*
 * Position.java
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

public final class Position implements Constants
{
    private Line line;
    private int offset;

    public Position(Position pos)
    {
        line = pos.line;
        offset = pos.offset;
        Debug.assertTrue(line != null);
    }

    public Position(Line line, int offset)
    {
        this.line = line;
        this.offset = offset;
        Debug.assertTrue(line != null);
    }

    public final Position copy()
    {
        return new Position(this);
    }

    public final Line getLine()
    {
        return line;
    }

    public final void setLine(Line line)
    {
        this.line = line;
    }

    public final int getOffset()
    {
        return offset;
    }

    public final void setOffset(int offset)
    {
        this.offset = offset;
    }

    public final int getLineLength()
    {
        return line.length();
    }

    public final int lineNumber()
    {
        return line.lineNumber();
    }

    public final Line getNextLine()
    {
        return line.next();
    }

    public final Line getPreviousLine()
    {
        return line.previous();
    }

    public final boolean equals(Object obj)
    {
        if (!(obj instanceof Position))
            return false;
        Position pos = (Position) obj;
        return (line == pos.line && offset == pos.offset);
    }

    public final boolean isBefore(Position pos)
    {
        if (line.lineNumber() < pos.line.lineNumber())
            return true;

        if (line == pos.line)
            if (offset < pos.offset)
                return true;

        return false;
    }

    public final boolean isAfter(Position pos)
    {
        if (line.lineNumber() > pos.line.lineNumber())
            return true;

        if (line == pos.line)
            if (offset > pos.offset)
                return true;

        return false;
    }

    public final void moveLeft()
    {
        if (offset > 0)
            --offset;
    }

    public final void moveRight()
    {
        if (offset < line.length())
            ++offset;
    }

    public final void moveTo(Position pos)
    {
        line = pos.line;
        offset = pos.offset;
        Debug.assertTrue(line != null);
    }

    public final void moveTo(Line line, int offset)
    {
        this.line = line;
        this.offset = offset;
        Debug.assertTrue(line != null);
    }

    // Moves position to the requested absolute column, based on the specified
    // tab size. If the requested column is past the end of the line, position
    // is moved to the end of the line.
    public void moveToCol(int goal, int tabWidth)
    {
        final int limit = line.length();
        int i, col;
        for (i = 0, col = 0; i < limit && col < goal; i++) {
            if (line.charAt(i) == '\t')
                col += tabWidth - col % tabWidth;
            else
                ++col;
        }
        offset = i;
    }

    public final boolean lookingAt(String s)
    {
        return s.regionMatches(0, line.getText(), offset, s.length());
    }

    public final boolean lookingAtIgnoreCase(String s)
    {
        return s.regionMatches(true, 0, line.getText(), offset, s.length());
    }

    public boolean atStart()
    {
        if (line.previous() != null)
            return false;
        if (offset > 0)
            return false;
        return true;
    }

    public boolean atEnd()
    {
        if (line != null) {
            if (offset < line.length())
                return false;
            if (line.next() != null)
                return false;
        }
        return true;
    }

    public boolean next()
    {
        if (offset < line.length()) {
            ++offset;
            return true;
        }
        if (line.next() != null) {
            line = line.next();
            offset = 0;
            return true;
        }
        return false;
    }

    public boolean prev()
    {
        if (offset > 0) {
            --offset;
            return true;
        }
        if (line.previous() != null) {
            line = line.previous();
            offset = line.length();
            return true;
        }
        return false;
    }

    public boolean nextLine()
    {
        if (line.next() != null) {
            line = line.next();
            offset = 0;
            return true;
        }
        return false;
    }

    // No range checking!
    public final void skip()
    {
        ++offset;
    }

    // No range checking!
    public final void skip(int count)
    {
        offset += count;
    }

    public void skipWhitespace()
    {
        while (Character.isWhitespace(getChar()) && next())
            ;
    }

    public void skipWhitespaceOnCurrentLine()
    {
        int limit = line.length();
        while (offset < limit && Character.isWhitespace(line.charAt(offset)))
            ++offset;
    }

    // If we're looking at a single or double quote char, skip over quoted string.
    public void skipQuote()
    {
        char quoteChar = getChar();
        if (quoteChar == '\'' || quoteChar == '"') {
            while (next()) {
                char c = getChar();
                if (c == '\\') {
                    // Skip next char.
                    next();
                    continue;
                }
                if (c == quoteChar) {
                    // Point to char after quote.
                    next();
                    return;
                }
            }
        }
    }

    public char getChar()
    {
        if (offset < 0 || offset > line.length()) {
            Log.error("Position.getChar() offset = " + offset +
                " line.length() = " + line.length());
            Debug.assertTrue(false);
        }
        if (offset == line.length())
            return EOL;
        return line.charAt(offset);
    }

    // Returns substring from position to end of line.
    public final String getString()
    {
        return line.substring(offset);
    }

    // Returns identifier starting at this position.
    public String getIdentifier(Mode mode)
    {
        int begin = offset;
        int end = offset;
        final int limit = line.length();
        while (mode.isIdentifierPart(line.charAt(end))) {
            ++end;
            if (end == limit)
                break;
        }
        return line.substring(begin, end);
    }

    public String toString()
    {
        FastStringBuffer sb = new FastStringBuffer("line ");
        if (line != null)
            sb.append(line.lineNumber() + 1);
        else
            sb.append("is null");
        sb.append(" offset ");
        sb.append(offset);
        return sb.toString();
    }

    public final boolean isHidden()
    {
        return line.isHidden();
    }

    // Returns -1 if there's an error.
    public static int getDistance(Position a, Position b)
    {
        if (a == null || b == null)
            return -1;
        Debug.assertTrue(a.getLine() != null);
        Debug.assertTrue(b.getLine() != null);
        if (a.equals(b))
            return 0;
        Position pos, end;
        if (a.isBefore(b)) {
            pos = a.copy();
            end = b.copy();
        } else {
            pos = b.copy();
            end = a.copy();
        }
        final Line endLine = end.getLine();
        int distance = 0;
        while (pos.getLine() != endLine) {
            distance += (pos.getLineLength() + 1 - pos.getOffset());
            Line nextLine = pos.getNextLine();
            if (nextLine == null)
                return -1;
            pos.moveTo(nextLine, 0);
        }
        Debug.assertTrue(pos.getLine() == endLine);
        distance += end.getOffset() - pos.getOffset();
        return distance;
    }
}
