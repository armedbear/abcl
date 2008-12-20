/*
 * PHPMode.java
 *
 * Copyright (C) 2002-2003 Peter Graves
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

public final class PHPMode extends JavaMode implements Constants, Mode
{
    private static final String[] phpConditionals = {
        "if",
        "else",
        "elseif",
        "do",
        "while",
        "for",
        "foreach",
        "switch"
    };

    // Since this class is final, we may as well construct the singleton class
    // instance right away.
    private static Mode mode = new PHPMode();

    private PHPMode()
    {
        super(PHP_MODE, PHP_MODE_NAME);
        keywords = new Keywords(this);
        conditionals = phpConditionals;
        setProperty(Property.TAB_WIDTH, 4);
    }

    public static final Mode getMode()
    {
        return mode;
    }

    public void populateModeMenu(Editor editor, Menu menu)
    {
        // No mode menu yet.
    }

    public SyntaxIterator getSyntaxIterator(Position pos)
    {
        return new PHPSyntaxIterator(pos);
    }

    public Formatter getFormatter(Buffer buffer)
    {
        return new PHPFormatter(buffer);
    }

    public Tagger getTagger(SystemBuffer buffer)
    {
        return new PHPTagger(buffer);
    }

    public final boolean isIdentifierStart(char c)
    {
        if (c > 255)
            return false;
        return values[c] == 1;
    }

    public final boolean isIdentifierPart(char c)
    {
        if (c > 255)
            return false;
        return values[c] != 0;
    }

    public boolean isInQuote(Buffer buffer, Position pos)
    {
        if (buffer.getMode() != this)
            Debug.bug();
        if (buffer.needsParsing())
            buffer.getFormatter().parseBuffer();
        Line line = pos.getLine();
        int offset = pos.getOffset();
        boolean inQuote = false;
        char quoteChar = '\0';
        int state = PHPFormatter.getState(line.flags());
        if (state == STATE_QUOTE) {
            inQuote = true;
            quoteChar = '"';
        } else if (state == STATE_SINGLEQUOTE) {
            inQuote = true;
            quoteChar = '\'';
        }
        for (int i = 0; i < offset; i++) {
            char c = line.charAt(i);
            if (c == '\\') {
                // Escape.
                ++i;
            } else if (inQuote && c == quoteChar) {
                inQuote = false;
            } else if (c == '"' || c == '\'') {
                inQuote = true;
                quoteChar = c;
            }
        }
        return inQuote;
    }

    public static void phpHelp(String s)
    {
        WebMode.query("http://www.php.net/", s);
    }

    private static final byte values[] = {
        0, 0, 0, 0, 0, 0, 0, 0, // 0x00-0x07
        0, 0, 0, 0, 0, 0, 0, 0, // 0x09-0xff
        0, 0, 0, 0, 0, 0, 0, 0, // 0x10-0x17
        0, 0, 0, 0, 0, 0, 0, 0, // 0x18-0x1f
        0, 0, 0, 0, 1, 0, 0, 0, // 0x20-0x27   !"#$%&'
        0, 0, 0, 0, 0, 0, 0, 0, // 0x28-0x2f  ()*+,-./
        2, 2, 2, 2, 2, 2, 2, 2, // 0x30-0x37  01234567
        2, 2, 0, 0, 0, 0, 0, 0, // 0x38-0x40  89:;<=>?
        0, 1, 1, 1, 1, 1, 1, 1, // 0x41-0x47  @ABCDEFG
        1, 1, 1, 1, 1, 1, 1, 1, // 0x48-0x4f  HIJKLMNO
        1, 1, 1, 1, 1, 1, 1, 1, // 0x50-0x57  PQRSTUVW
        1, 1, 1, 0, 0, 0, 0, 1, // 0x58-0x5f  XYZ[\]^_
        0, 1, 1, 1, 1, 1, 1, 1, // 0x60-0x67  `abcdefg
        1, 1, 1, 1, 1, 1, 1, 1, // 0x68-0x6f  hijklmno
        1, 1, 1, 1, 1, 1, 1, 1, // 0x70-0x77  pqrstuvw
        1, 1, 1, 0, 0, 0, 0, 1, // 0x78-0x7f  xyz{|}~
        1, 1, 1, 1, 1, 1, 1, 1,
        1, 1, 1, 1, 1, 1, 1, 1,
        1, 1, 1, 1, 1, 1, 1, 1,
        1, 1, 1, 1, 1, 1, 1, 1,
        1, 1, 1, 1, 1, 1, 1, 1,
        1, 1, 1, 1, 1, 1, 1, 1,
        1, 1, 1, 1, 1, 1, 1, 1,
        1, 1, 1, 1, 1, 1, 1, 1,
        1, 1, 1, 1, 1, 1, 1, 1,
        1, 1, 1, 1, 1, 1, 1, 1,
        1, 1, 1, 1, 1, 1, 1, 1,
        1, 1, 1, 1, 1, 1, 1, 1,
        1, 1, 1, 1, 1, 1, 1, 1,
        1, 1, 1, 1, 1, 1, 1, 1,
        1, 1, 1, 1, 1, 1, 1, 1,
        1, 1, 1, 1, 1, 1, 1, 1
    };
}
