/*
 * CTagger.java
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

import gnu.regexp.RE;
import gnu.regexp.UncheckedRE;
import java.util.ArrayList;

public final class ObjCTagger extends JavaTagger
{
    // States.
    private static final int NEUTRAL        = 0;
    private static final int METHOD_PROLOG  = 1;
    private static final int METHOD_NAME    = 2;
    private static final int PARAMETER_LIST = 3;

    private Mode mode = ObjCMode.getMode();

    public ObjCTagger(SystemBuffer buffer)
    {
        super(buffer);
    }

    public void run()
    {
        ArrayList tags = new ArrayList();
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
            if (c == '\'' || c == '"') {
                pos.skipQuote();
                continue;
            }
            if (pos.lookingAt("/*")) {
                skipComment(pos);
                continue;
            }
            if (pos.lookingAt("//")) {
                skipSingleLineComment(pos);
                continue;
            }
            if (c == '#' && pos.getOffset() == 0) {
                skipPreprocessor(pos);
                continue;
            }
            if (state == METHOD_NAME) {
                if (c == '{') {
                    if (token != null && !mode.isKeyword(token))
                        tags.add(new CTag(token, tokenStart));
                    skipBrace();
                    state = NEUTRAL;
                } else if (isIdentifierStart(c)) {
                    state = PARAMETER_LIST;
                    pos.next();
                } else {
                    state = NEUTRAL;
                    pos.next();
                }
                continue;
            }
            if (state == PARAMETER_LIST) {
                if (c == '{') {
                    if (token != null && !mode.isKeyword(token))
                        tags.add(new CTag(token, tokenStart));
                    skipBrace();
                    state = NEUTRAL;
                    continue;
                } else if (c == '(') {
                    state = NEUTRAL;
                    skipParen();
                    continue;
                } else {
                    pos.next();
                    continue;
                }
            }
            if (state == METHOD_PROLOG) {
                if (c == '{') {
                    if (token != null && !mode.isKeyword(token))
                        tags.add(new ObjCTag(token, tokenStart));
                    skipBrace();
                    state = NEUTRAL;
                    continue;
                } else if (c == '(') {
                    skipParen();
                    continue;
                } else if (isIdentifierStart(c)) {
                    if (token == null) {
                        tokenStart = pos.copy();
                        token = gatherToken(pos);
                        continue;
                    } else {
                        String s = gatherToken(pos);
                        if (s.endsWith(":"))
                            token = token.concat(s);
                        continue;
                    }
                }
                pos.next();
                continue;
            }
            if (c == '}') {
                pos.next();
                continue;
            }
            if (pos.getOffset() == 0 && (c == '+' || c == '-')) {
                token = null;
                state = METHOD_PROLOG;
                continue;
            }
            if (isIdentifierStart(c)) {
                tokenStart = pos.copy();
                token = gatherToken(pos);
                continue;
            }
            if (c == '(') {
                skipParen();
                state = METHOD_NAME;
                continue;
            }
            pos.next();
        }
        buffer.setTags(tags);
    }

    private String gatherToken(Position pos)
    {
        FastStringBuffer sb = new FastStringBuffer();
        char c;
        while (isIdentifierPart(c = pos.getChar()) || c == ':') {
            sb.append(c);
            if (!pos.next())
                break;
        }
        return sb.toString();
    }

    private static final boolean isIdentifierStart(char c)
    {
        return CMode.getMode().isIdentifierStart(c);
    }

    private static final boolean isIdentifierPart(char c)
    {
        return CMode.getMode().isIdentifierPart(c);
    }
}
