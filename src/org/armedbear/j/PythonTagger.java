/*
 * PythonTagger.java
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

import java.util.ArrayList;

public final class PythonTagger extends Tagger
{
    // States.
    private static final int STATE_NEUTRAL       = 0;
    private static final int STATE_CLASS_NAME    = 1;
    private static final int STATE_FUNCTION_NAME = 2;
    private static final int STATE_SINGLE_QUOTE  = 3;
    private static final int STATE_DOUBLE_QUOTE  = 4;
    private static final int STATE_TRIPLE_SINGLE = 5;
    private static final int STATE_TRIPLE_DOUBLE = 6;

    private static final PythonMode mode = PythonMode.getMode();

    public PythonTagger(SystemBuffer buffer)
    {
        super(buffer);
    }

    public void run()
    {
        ArrayList tags = new ArrayList();
        Position pos = new Position(buffer.getFirstLine(), 0);
        int state = STATE_NEUTRAL;
        while (!pos.atEnd()) {
            char c = pos.getChar();
            if (Character.isWhitespace(c)) {
                pos.skipWhitespace();
                continue;
            }
            if (c == '\\') {
                // Escape.
                if (pos.getOffset() < pos.getLineLength()-1) {
                    pos.skip(2);
                } else {
                    pos.setLine(pos.getNextLine());
                    pos.setOffset(0);
                }
                continue;
            }
            if (state == STATE_SINGLE_QUOTE) {
                if (c == '\'')
                    state = STATE_NEUTRAL;
                pos.next();
                continue;
            }
            if (state == STATE_DOUBLE_QUOTE) {
                if (c == '"')
                    state = STATE_NEUTRAL;
                pos.next();
                continue;
            }
            if (state == STATE_TRIPLE_SINGLE) {
                if (c == '\'' && pos.lookingAt("'''")) {
                    pos.skip(3);
                    state = STATE_NEUTRAL;
                } else
                    pos.next();
                continue;
            }
            if (state == STATE_TRIPLE_DOUBLE) {
                if (c == '"' && pos.lookingAt("\"\"\"")) {
                    pos.skip(3);
                    state = STATE_NEUTRAL;
                } else
                    pos.next();
                continue;
            }
            if (c == '\'') {
                if (pos.lookingAt("'''")) {
                    state = STATE_TRIPLE_SINGLE;
                    pos.skip(3);
                } else {
                    state = STATE_SINGLE_QUOTE;
                    pos.next();
                }
                continue;
            }
            if (c == '"') {
                if (pos.lookingAt("\"\"\"")) {
                    state = STATE_TRIPLE_DOUBLE;
                    pos.skip(3);
                } else {
                    state = STATE_DOUBLE_QUOTE;
                    pos.next();
                }
                continue;
            }
            if (c == '#') {
                // Comment.
                pos.setLine(pos.getNextLine());
                pos.setOffset(0);
                continue;
            }
            if (mode.isIdentifierStart(c)) {
                Position start = pos.copy();
                String token = gatherToken(pos);
                if (state == STATE_CLASS_NAME) {
                    LocalTag tag = new PythonTag("class " + token, start, TAG_CLASS);
                    tags.add(tag);
                    state = STATE_NEUTRAL;
                } else if (state == STATE_FUNCTION_NAME) {
                    LocalTag tag = new PythonTag(token, start, TAG_FUNCTION);
                    tags.add(tag);
                    state = STATE_NEUTRAL;
                } else if (token.equals("class")) {
                    state = STATE_CLASS_NAME;
                } else if (token.equals("def")) {
                    state = STATE_FUNCTION_NAME;
                }
                continue;
            }
            pos.next();
        }
        buffer.setTags(tags);
    }

    private static String gatherToken(Position pos)
    {
        FastStringBuffer sb = new FastStringBuffer();
        char c;
        while (mode.isIdentifierPart(c = pos.getChar())) {
            sb.append(c);
            if (!pos.next())
                break;
        }
        return sb.toString();
    }
}
