/*
 * SchemeTagger.java
 *
 * Copyright (C) 1998-2002 Peter Graves
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

import java.util.Vector;

public final class SchemeTagger extends Tagger
{
    // States.
    private static final int NEUTRAL = 0;
    private static final int DEFUN   = 1;

    protected Position pos;
    protected String token;
    protected Position tokenStart;

    private static final Mode schemeMode = SchemeMode.getMode();

    public SchemeTagger(SystemBuffer buffer)
    {
        super(buffer);
    }

    public void run()
    {
        Vector tags = new Vector();
        pos = new Position(buffer.getFirstLine(), 0);
        token = null;
        tokenStart = null;
        int state = NEUTRAL;
        while (!pos.atEnd()) {
            char c = pos.getChar();
            if (Character.isWhitespace(c)) {
                pos.skipWhitespace();
                continue;
            }
            if (c == '\\') {
                // Escape.
                if (pos.getOffset() < pos.getLineLength() - 1) {
                    pos.skip(2);
                    continue;
                }
                Line nextLine = pos.getNextLine();
                if (nextLine == null)
                    break;
                pos.moveTo(nextLine, 0);
                continue;
            }
            if (c == '\"') {
                pos.skipQuote();
                continue;
            }
            if (c == ';') {
                // Comment.
                Line nextLine = pos.getNextLine();
                if (nextLine == null)
                    break;
                pos.moveTo(nextLine, 0);
                continue;
            }
            if (schemeMode.isIdentifierStart(c)) {
                gatherToken();
                if (state == DEFUN) {
                    LocalTag tag = new LispTag(token, tokenStart);
                    tags.add(tag);
                    state = NEUTRAL;
                } else if (token.equals("define"))
                    state = DEFUN;
                continue;
            }
            pos.next();
        }
        buffer.setTags(tags);
    }

    private void gatherToken()
    {
        tokenStart = new Position(pos);
        FastStringBuffer sb = new FastStringBuffer();
        char c;
        while (schemeMode.isIdentifierPart(c = pos.getChar())) {
            sb.append(c);
            if (!pos.next())
                break;
        }
        token = sb.toString();
    }
}
