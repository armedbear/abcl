/*
 * LispTagger.java
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

import java.util.ArrayList;

public final class LispTagger extends Tagger
{
    // States.
    private static final int NEUTRAL    = 0;
    private static final int OPEN_PAREN = 1;
    private static final int DEFINITION = 2;

    private static final Mode mode = LispMode.getMode();

    private ArrayList tags;

    public LispTagger(SystemBuffer buffer)
    {
        super(buffer);
    }

    public synchronized void run()
    {
        tags = new ArrayList();
        Position pos = new Position(buffer.getFirstLine(), 0);
        int state = NEUTRAL;
        String definer = null;
        while (!pos.atEnd()) {
            char c = pos.getChar();
            if (Character.isWhitespace(c)) {
                pos.skipWhitespace();
                continue;
            }
            if (c == '\\') {
                // Escape.
                if (pos.getOffset() < pos.getLineLength()-1)
                    pos.skip(2);
                else {
                    Line nextLine = pos.getNextLine();
                    if (nextLine == null)
                        break;
                    pos.moveTo(nextLine, 0);
                }
                continue;
            }
            if (c == '#' && pos.lookingAt("#|")) {
                pos.skip(2);
                skipComment(pos);
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
            if (c == '(') {
                if (state == DEFINITION) {
                    if (definer != null) {
                        if (definer.equals("defun")) {
                            String s = gatherToken(pos.copy());
                            if (s.toLowerCase().equals("setf")) {
                                Position tokenStart = pos.copy();
                                String name = gatherList(pos);
                                addTag(name, tokenStart, definer);
                            }
                            state = NEUTRAL;
                            definer = null;
                            continue;
                        }
                        if (definer.equals("defstruct")) {
                            pos.next();
                            pos.skipWhitespace();
                            c = pos.getChar();
                            if (mode.isIdentifierStart(c)) {
                                Position tokenStart = pos.copy();
                                String token = gatherToken(pos);
                                addTag(token, tokenStart, definer);
                                state = NEUTRAL;
                                definer = null;
                                continue;
                            }
                        }
                    }
                }
                state = OPEN_PAREN;
                pos.next();
                continue;
            }
            if (mode.isIdentifierStart(c)) {
                if (state == DEFINITION) {
                    Position tokenStart = pos.copy();
                    String token = gatherToken(pos);
                    addTag(token, tokenStart, definer);
                    state = NEUTRAL;
                    definer = null;
                    continue;
                }
                if (state == OPEN_PAREN) {
                    String preceding =
                        pos.getLine().substring(0, pos.getOffset()).trim();
                    if (!preceding.equals("(")) {
                        state = NEUTRAL;
                        continue;
                    }
                    String token = gatherToken(pos).toLowerCase();
                    token = LispMode.translateDefiner(token);
                    if (token != null) {
                        state = DEFINITION;
                        definer = token;
                    } else
                        state = NEUTRAL;
                    continue;
                }
                skipToken(pos);
                continue;
            }
            state = NEUTRAL;
            pos.next();
        }
        buffer.setTags(tags);
    }

    private void addTag(String name, Position pos, String definer)
    {
        int type = -1;
        if (definer.equals("defclass"))
            type = TAG_CLASS;
        else if (definer.equals("defconstant"))
            type = TAG_CONSTANT;
        else if (definer.equals("defgeneric"))
            type = TAG_GENERIC_FUNCTION;
        else if (definer.equals("define-condition"))
            type = TAG_CONDITION;
        else if (definer.equals("defmacro"))
            type = TAG_MACRO;
        else if (definer.equals("defmethod"))
            type = TAG_METHOD;
        else if (definer.equals("defparameter"))
            type = TAG_PARAMETER;
        else if (definer.equals("defstruct"))
            type = TAG_STRUCT;
        else if (definer.equals("deftype"))
            type = TAG_TYPE;
        else if (definer.equals("defun"))
            type = TAG_DEFUN;
        else if (definer.equals("defvar"))
            type = TAG_VAR;
        else if (definer.equals("deftest"))
            type = TAG_TEST;
        else
            Debug.bug();
        tags.add(new LispTag(name, pos, type));
    }

    // Advances pos past list.
    private String gatherList(Position pos)
    {
        FastStringBuffer sb = new FastStringBuffer();
        char c = pos.getChar();
        Debug.bugIf(c != '(');
        if (pos.next()) {
            while ((c = pos.getChar()) != ')') {
                sb.append(c);
                if (!pos.next())
                    break;
            }
        }
        return sb.toString();
    }

    // Advances pos past token.
    private String gatherToken(Position pos)
    {
        FastStringBuffer sb = new FastStringBuffer();
        char c;
        while (mode.isIdentifierPart(c = pos.getChar()) || c == ':') {
            sb.append(c);
            if (!pos.next())
                break;
        }
        return sb.toString();
    }

    // Advances pos past token.
    private void skipToken(Position pos)
    {
        while (mode.isIdentifierPart(pos.getChar())) {
            if (!pos.next())
                return;
        }
    }

    private void skipComment(Position pos)
    {
        while (!pos.atEnd()) {
            char c = pos.getChar();
            if (c == '\\') {
                // Escape.
                if (pos.getOffset() < pos.getLineLength()-1)
                    pos.skip(2);
                else {
                    Line nextLine = pos.getNextLine();
                    if (nextLine == null)
                        break;
                    pos.moveTo(nextLine, 0);
                }
                continue;
            }
            if (c == '|' && pos.lookingAt("|#")) {
                pos.skip(2);
                return;
            }
            pos.next();
        }
    }
}
