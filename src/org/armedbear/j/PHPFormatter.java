/*
 * PHPFormatter.java
 *
 * Copyright (C) 1998-2003 Peter Graves
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

public final class PHPFormatter extends Formatter implements Constants
{
    private static final int PHP_STATE_IDENTIFIER           = STATE_LAST +  1;
    private static final int PHP_STATE_OPERATOR             = STATE_LAST +  2;
    private static final int PHP_STATE_BRACE                = STATE_LAST +  3;
    private static final int PHP_STATE_NUMBER               = STATE_LAST +  4;
    private static final int PHP_STATE_HEXNUMBER            = STATE_LAST +  5;
    private static final int PHP_STATE_IGNORE               = STATE_LAST +  6;
    private static final int PHP_STATE_KEYWORD              = STATE_LAST +  7;
    private static final int PHP_STATE_TAG_STARTING         = STATE_LAST +  8;
    private static final int PHP_STATE_TAG                  = STATE_LAST +  9;
    private static final int PHP_STATE_ATTNAME              = STATE_LAST + 10;
    private static final int PHP_STATE_EQUALS               = STATE_LAST + 11;
    private static final int PHP_STATE_ATTVALUE             = STATE_LAST + 12;
    private static final int PHP_STATE_ATTVALUE_QUOTE       = STATE_LAST + 13;
    private static final int PHP_STATE_ATTVALUE_SINGLEQUOTE = STATE_LAST + 14;
    private static final int PHP_STATE_TAG_ENDING           = STATE_LAST + 15;
    private static final int PHP_STATE_HTML_COMMENT         = STATE_LAST + 16;

    private static final int PHP_FORMAT_TEXT       =  0;
    private static final int PHP_FORMAT_COMMENT    =  1;
    private static final int PHP_FORMAT_STRING     =  2;
    private static final int PHP_FORMAT_IDENTIFIER =  3;
    private static final int PHP_FORMAT_KEYWORD    =  4;
    private static final int PHP_FORMAT_FUNCTION   =  5;
    private static final int PHP_FORMAT_OPERATOR   =  6;
    private static final int PHP_FORMAT_BRACE      =  7;
    private static final int PHP_FORMAT_NUMBER     =  8;
    private static final int PHP_FORMAT_VAR        =  9;
    private static final int PHP_FORMAT_DELIMITER  = 10;
    private static final int PHP_FORMAT_TAG        = 11;
    private static final int PHP_FORMAT_ATTRIBUTE  = 12;
    private static final int PHP_FORMAT_EQUALS     = 13;

    private static PHPMode mode;

    public PHPFormatter(Buffer buffer)
    {
        this.buffer = buffer;
        if (mode == null)
            mode = (PHPMode) PHPMode.getMode();
    }

    private int tokenBegin = 0;

    private final void endToken(StringPosition pos, int state)
    {
        endToken(pos.getText(), pos.getOffset(), state);
    }

    private void endToken(String text, int tokenEnd, int state)
    {
        if (tokenEnd - tokenBegin > 0) {
            int format;
            switch (state) {
                case STATE_NEUTRAL:
                case PHP_STATE_IGNORE:
                    format = PHP_FORMAT_TEXT;
                    break;
                case STATE_QUOTE:
                case STATE_SINGLEQUOTE:
                    format = PHP_FORMAT_STRING;
                    break;
                case PHP_STATE_IDENTIFIER:
                    format = PHP_FORMAT_IDENTIFIER;
                    break;
                case STATE_COMMENT:
                case PHP_STATE_HTML_COMMENT:
                    format = PHP_FORMAT_COMMENT;
                    break;
                case PHP_STATE_OPERATOR:
                    format = PHP_FORMAT_OPERATOR;
                    break;
                case PHP_STATE_BRACE:
                    format = PHP_FORMAT_BRACE;
                    break;
                case PHP_STATE_NUMBER:
                case PHP_STATE_HEXNUMBER:
                    format = PHP_FORMAT_NUMBER;
                    break;
                case PHP_STATE_KEYWORD:
                    format = PHP_FORMAT_KEYWORD;
                    break;
                case PHP_STATE_TAG_STARTING:
                case PHP_STATE_TAG_ENDING:
                    format = PHP_FORMAT_DELIMITER;
                    break;
                case PHP_STATE_TAG:
                    format = PHP_FORMAT_TAG;
                    break;
                case PHP_STATE_ATTNAME:
                    format = PHP_FORMAT_ATTRIBUTE;
                    break;
                case PHP_STATE_EQUALS:
                    format = PHP_FORMAT_EQUALS;
                    break;
                case PHP_STATE_ATTVALUE:
                case PHP_STATE_ATTVALUE_QUOTE:
                case PHP_STATE_ATTVALUE_SINGLEQUOTE:
                    format = PHP_FORMAT_STRING;
                    break;
                default:
                    format = PHP_FORMAT_TEXT;
                    break;
            }
            addSegment(text, tokenBegin, tokenEnd, format);
            tokenBegin = tokenEnd;
        }
    }

    private void parseLine(Line line)
    {
        String text;
        if (Editor.tabsAreVisible())
            text = Utilities.makeTabsVisible(line.getText(), buffer.getTabWidth());
        else
            text = Utilities.detab(line.getText(), buffer.getTabWidth());
        tokenBegin = 0;

        char quoteChar = '\0';
        int state = getState(line.flags());
        final int htmlState = getHtmlState(line.flags());
        if (state == PHP_STATE_TAG) {
            // End of tag name at end of previous line.
            state = PHP_STATE_ATTNAME;
        } else if (state == STATE_QUOTE)
            quoteChar = '"';
        else if (state == STATE_SINGLEQUOTE)
            quoteChar = '\'';

        StringPosition pos = new StringPosition(text);
        // Skip whitespace at start of line.
        while (!pos.atEnd() && pos.charIsWhitespace())
            pos.next();
        endToken(pos, state);

        if (pos.atEnd())
            return;

        switch (state) {
            case STATE_NEUTRAL:
            case STATE_COMMENT:
            case STATE_QUOTE:
            case STATE_SINGLEQUOTE:
            case PHP_STATE_IDENTIFIER:
            case PHP_STATE_OPERATOR:
            case PHP_STATE_BRACE:
            case PHP_STATE_NUMBER:
            case PHP_STATE_HEXNUMBER:
            case PHP_STATE_KEYWORD:
                parseLinePHP(pos, state, htmlState);
            default:
                parseLineHTML(pos, state);
        }
    }

    private final void parseLinePHP(StringPosition pos, int state)
    {
        parseLinePHP(pos, state, PHP_STATE_IGNORE);
    }

    private void parseLinePHP(StringPosition pos, int state, int htmlState)
    {
        while (!pos.atEnd()) {
            char c = pos.getChar();
            if (c == '\\') {
                // Escape.
                pos.skip(1);
                pos.next();
                continue;
            }
            if (state == STATE_COMMENT) {
                if (pos.lookingAt("*/")) {
                    pos.skip(2);
                    endToken(pos, state);
                    state = STATE_NEUTRAL;
                } else
                    pos.next();
                continue;
            }
            if (state == STATE_QUOTE) {
                if (c == '"') {
                    pos.next();
                    endToken(pos, state);
                    state = STATE_NEUTRAL;
                    continue;
                }
                pos.next();
                continue;
            }
            if (state == STATE_SINGLEQUOTE) {
                if (c == '\'') {
                    pos.next();
                    endToken(pos, state);
                    state = STATE_NEUTRAL;
                    continue;
                }
                pos.next();
                continue;
            }
            // Reaching here, we're not in a comment or a quoted string.
            if (c == '"' || c == '\'') {
                endToken(pos, state);
                state = c == '"' ? STATE_QUOTE : STATE_SINGLEQUOTE;
                pos.next();
                continue;
            }
            if (pos.lookingAt("?>")) {
                endToken(pos, state);
                pos.skip(2);
                endToken(pos, PHP_STATE_TAG_ENDING);
                parseLineHTML(pos, htmlState);
                return;
            }
            if (c == '/') {
                if (pos.lookingAt("/*")) {
                    endToken(pos, state);
                    checkLastSegment();
                    state = STATE_COMMENT;
                    pos.skip(2);
                } else if (pos.lookingAt("//")) {
                    endToken(pos, state);
                    checkLastSegment();
                    pos.setOffset(pos.getText().length());
                    endToken(pos, STATE_COMMENT);
                    return;
                } else
                    pos.next();
                continue;
            }
            if (c == '#') {
                endToken(pos, state);
                checkLastSegment();
                pos.setOffset(pos.getText().length());
                endToken(pos, STATE_COMMENT);
                return;
            }
            if (isOperatorChar(c)) {
                if (state != PHP_STATE_OPERATOR) {
                    endToken(pos, state);
                    // Check for keyword (as in e.g. "char*") or var.
                    checkLastSegment();
                    state = PHP_STATE_OPERATOR;
                }
                pos.next();
                continue;
            }
            if (c == '{' || c == '}') {
                if (state != PHP_STATE_BRACE) {
                    endToken(pos, state);
                    // Check for keyword (e.g. "try") or var.
                    checkLastSegment();
                    state = PHP_STATE_BRACE;
                }
                pos.next();
                continue;
            }
            if (state == PHP_STATE_OPERATOR || state == PHP_STATE_BRACE) {
                if (mode.isIdentifierStart(c)) {
                    endToken(pos, state);
                    state = PHP_STATE_IDENTIFIER;
                } else if (Character.isDigit(c)) {
                    endToken(pos, state);
                    state = PHP_STATE_NUMBER;
                } else {
                    endToken(pos, state);
                    state = STATE_NEUTRAL;
                }
                pos.next();
                continue;
            }
            if (state == PHP_STATE_IDENTIFIER) {
                if (!mode.isIdentifierPart(c)) {
                    endToken(pos, state);
                    // Check for keyword or function.
                    LineSegment segment = getLastSegment();
                    if (segment != null) {
                        String segmentText = segment.getText();
                        if (isVar(segment.getText()))
                            segment.setFormat(PHP_FORMAT_VAR);
                        else if (isKeyword(segment.getText()))
                            segment.setFormat(PHP_FORMAT_KEYWORD);
                        else if (c == '(')
                            segment.setFormat(PHP_FORMAT_FUNCTION);
                        else if (Character.isWhitespace(c)) {
                            // Look ahead to see if next non-whitespace char is '('.
                            int j = pos.getOffset() + 1;
                            final String text = pos.getText();
                            final int limit = text.length();
                            while (j < limit && Character.isWhitespace(c = text.charAt(j)))
                                ++j;
                            if (c == '(')
                                segment.setFormat(PHP_FORMAT_FUNCTION);
                        }
                    }
                    state = STATE_NEUTRAL;
                }
                pos.next();
                continue;
            }
            if (state == PHP_STATE_NUMBER) {
                if (Character.isDigit(c))
                    ;
                else if (c == 'u' || c == 'U' || c == 'l' || c == 'L')
                    ;
                else if (pos.getOffset() - tokenBegin == 1 && c == 'x' || c == 'X')
                    state = PHP_STATE_HEXNUMBER;
                else {
                    endToken(pos, state);
                    if (mode.isIdentifierStart(c))
                        state = PHP_STATE_IDENTIFIER;
                    else
                        state = STATE_NEUTRAL;
                }
                pos.next();
                continue;
            }
            if (state == PHP_STATE_HEXNUMBER) {
                if (Character.isDigit(c))
                    ;
                else if ((c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F'))
                    ;
                else if (c == 'u' || c == 'U' || c == 'l' || c == 'L')
                    ;
                else {
                    endToken(pos, state);
                    if (mode.isIdentifierStart(c))
                        state = PHP_STATE_IDENTIFIER;
                    else
                        state = STATE_NEUTRAL;
                }
                pos.next();
                continue;
            }
            if (state == STATE_NEUTRAL) {
                if (c == '$' || mode.isIdentifierStart(c)) {
                    endToken(pos, state);
                    state = PHP_STATE_IDENTIFIER;
                } else if (Character.isDigit(c)) {
                    endToken(pos, state);
                    state = PHP_STATE_NUMBER;
                }
            }
            pos.next();
        }
        // Reached end of line.
        endToken(pos, state);
        if (state == PHP_STATE_IDENTIFIER) {
            // Last token might be a keyword.
            checkLastSegment();
        }
    }

    private void parseLineHTML(StringPosition pos, int state)
    {
        if (state == 0)
            state = PHP_STATE_IGNORE;
        while (!pos.atEnd()) {
            char c = pos.getChar();
            switch (state) {
                case PHP_STATE_HTML_COMMENT:
                    if (pos.lookingAt("-->")) {
                        pos.skip(3);
                        endToken(pos, state);
                        state = PHP_STATE_IGNORE;
                    } else
                        pos.next();
                    continue;
                case PHP_STATE_IGNORE:
                    if (c == '<') {
                        endToken(pos, state);
                        if (pos.lookingAt("<?php")) {
                            pos.skip(2);
                            endToken(pos, PHP_STATE_TAG_STARTING);
                            pos.skip(3);
                            endToken(pos, PHP_STATE_TAG);
                            parseLinePHP(pos, STATE_NEUTRAL);
                            return;
                        }
                        if (pos.lookingAt("<?")) {
                            pos.skip(2);
                            endToken(pos, PHP_STATE_TAG_STARTING);
                            parseLinePHP(pos, STATE_NEUTRAL);
                            return;
                        }
                        if (pos.lookingAt("<!--")) {
                            pos.skip(4);
                            state = PHP_STATE_HTML_COMMENT;
                            continue;
                        }
                        // Otherwise it's the opening '<' of a normal HTML tag.
                        state = PHP_STATE_TAG_STARTING;
                    }
                    pos.next();
                    continue;
                case PHP_STATE_TAG_STARTING:
                    if (!Character.isWhitespace(c)) {
                        endToken(pos, state);
                        state = PHP_STATE_TAG;
                    }
                    pos.next();
                    continue;
                case PHP_STATE_TAG:
                    if (Character.isWhitespace(c)) {
                        endToken(pos, state);
                        state = PHP_STATE_ATTNAME;
                        pos.next();
                    } else if (c == '>') {
                        endToken(pos, state);
                        pos.next();
                        endToken(pos, PHP_STATE_TAG_ENDING);
                        state = PHP_STATE_IGNORE;
                    } else
                        pos.next();
                    continue;
                case PHP_STATE_ATTNAME:
                    if (c == '=') {
                        endToken(pos, state);
                        pos.next();
                        state = PHP_STATE_EQUALS;
                    } else if (c == '>') {
                        endToken(pos, state);
                        pos.next();
                        endToken(pos, PHP_STATE_TAG_ENDING);
                        state = PHP_STATE_IGNORE;
                    } else
                        pos.next();
                    continue;
                case PHP_STATE_EQUALS:
                    if (!Character.isWhitespace(c)) {
                        endToken(pos, state);
                        if (c == '"')
                            state = PHP_STATE_ATTVALUE_QUOTE;
                        else if (c == '\'')
                            state = PHP_STATE_ATTVALUE_SINGLEQUOTE;
                        else if (pos.lookingAt("<?php")) {
                            pos.skip(2);
                            endToken(pos, PHP_STATE_TAG_STARTING);
                            pos.skip(3);
                            endToken(pos, PHP_STATE_TAG);
                            parseLinePHP(pos, STATE_NEUTRAL, PHP_STATE_ATTVALUE);
                        } else
                            state = PHP_STATE_ATTVALUE;
                    }
                    pos.next();
                    continue;
                case PHP_STATE_ATTVALUE:
                    if (Character.isWhitespace(c)) {
                        endToken(pos, state);
                        pos.next();
                        state = PHP_STATE_ATTNAME;
                    } else if (c == '>') {
                        endToken(pos, state);
                        pos.next();
                        endToken(pos, PHP_STATE_TAG_ENDING);
                        state = PHP_STATE_IGNORE;
                    } else
                        pos.next();
                    continue;
                case PHP_STATE_ATTVALUE_QUOTE:
                    if (pos.lookingAt("<?php")) {
                        endToken(pos, state);
                        pos.skip(2);
                        endToken(pos, PHP_STATE_TAG_STARTING);
                        pos.skip(3);
                        endToken(pos, PHP_STATE_TAG);
                        parseLinePHP(pos, STATE_NEUTRAL, PHP_STATE_ATTVALUE_QUOTE);
                    } else {
                        pos.next();
                        if (c == '"') {
                            endToken(pos, state);
                            state = PHP_STATE_ATTNAME;
                        }
                    }
                    continue;
                case PHP_STATE_ATTVALUE_SINGLEQUOTE:
                    if (pos.lookingAt("<?php")) {
                        endToken(pos, state);
                        pos.skip(2);
                        endToken(pos, PHP_STATE_TAG_STARTING);
                        pos.skip(3);
                        endToken(pos, PHP_STATE_TAG);
                        parseLinePHP(pos, STATE_NEUTRAL, PHP_STATE_ATTVALUE_QUOTE);
                    } else {
                        pos.next();
                        if (c == '\'') {
                            endToken(pos, state);
                            state = PHP_STATE_ATTNAME;
                        }
                    }
                    continue;
                default:
                    Log.error("state = " + state);
                    Debug.assertTrue(false);
                    break;
            }
        }
        // Reached end of line.
        endToken(pos, state);
    }

    private void checkLastSegment()
    {
        final LineSegment segment = getLastSegment();
        if (segment != null) {
            if (isVar(segment.getText()))
                segment.setFormat(PHP_FORMAT_VAR);
            else if (isKeyword(segment.getText()))
                segment.setFormat(PHP_FORMAT_KEYWORD);
        }
    }

    public LineSegmentList formatLine(Line line)
    {
        clearSegmentList();
        if (line == null) {
            addSegment("", PHP_FORMAT_TEXT);
            return segmentList;
        }
        parseLine(line);
        return segmentList;
    }

    public boolean parseBuffer()
    {
        Line line = buffer.getFirstLine();
        if (line == null)
            return false;
        boolean changed = false;
        final int newFlags = makeFlags(PHP_STATE_IGNORE, 0);
        if (newFlags != line.flags()) {
            line.setFlags(newFlags);
            changed = true;
        }
        changed = parseBufferHTML(new Position(line, 0), PHP_STATE_IGNORE,
                                  changed);
        buffer.setNeedsParsing(false);
        return changed;
    }

    private boolean parseBufferPHP(Position pos, int state, int htmlState,
        boolean changed)
    {
        char quoteChar = '\0';
        while (!pos.atEnd()) {
            char c = pos.getChar();
            if (c == EOL) {
                if (pos.next()) {
                    final int newFlags = makeFlags(state, htmlState);
                    if (newFlags != pos.getLine().flags()) {
                        pos.getLine().setFlags(newFlags);
                        changed = true;
                    }
                    continue;
                } else
                    break;
            }
            if (c == '\\') {
                // Escape.
                pos.skip(1);
                pos.next();
                continue;
            }
            if (state == STATE_COMMENT) {
                if (c == '*' && pos.lookingAt("*/")) {
                    state = STATE_NEUTRAL;
                    pos.skip(2);
                } else
                    pos.next();
                continue;
            }
            if (state == STATE_QUOTE || state == STATE_SINGLEQUOTE) {
                if (c == quoteChar) {
                    state = STATE_NEUTRAL;
                    quoteChar = '\0';
                }
                pos.next();
                continue;
            }
            // Not in comment or quoted string.
            if (c == '/') {
                if (pos.lookingAt("//")) {
                    // Beginning of comment. Ignore rest of line.
                    pos.setOffset(pos.getLineLength());
                } else if (pos.lookingAt("/*")) {
                    state = STATE_COMMENT;
                    pos.skip(2);
                } else
                    pos.next();
                continue;
            }
            if (c == '#') {
                // Beginning of comment. Ignore rest of line.
                Line next = pos.getNextLine();
                if (next != null) {
                    pos.moveTo(next, 0);
                    continue;
                } else
                    break;
            }
            if (c == '"') {
                state = STATE_QUOTE;
                quoteChar = c;
                pos.next();
                continue;
            }
            if (c == '\'') {
                state = STATE_SINGLEQUOTE;
                quoteChar = c;
                pos.next();
                continue;
            }
            if (c == '?' && pos.lookingAt("?>")) {
                state = PHP_STATE_IGNORE;
                pos.skip(2);
                return parseBufferHTML(pos, state, changed);
            }
            // Otherwise...
            pos.next();
        }
        return changed;
    }

    private boolean parseBufferHTML(Position pos, int state, boolean changed)
    {
        while (!pos.atEnd()) {
            char c = pos.getChar();
            if (c == EOL) {
                if (pos.next()) {
                    final int newFlags = makeFlags(state, 0);
                    if (newFlags != pos.getLine().flags()) {
                        pos.getLine().setFlags(newFlags);
                        changed = true;
                    }
                    continue;
                } else
                    break;
            }
            if (c == '\\') {
                // Escape.
                pos.skip(1);
                pos.next();
                continue;
            }
            switch (state) {
                case PHP_STATE_HTML_COMMENT:
                    if (c == '-' && pos.lookingAt("-->")) {
                        state = PHP_STATE_IGNORE;
                        pos.skip(3);
                    } else
                        pos.next();
                    continue;
                case PHP_STATE_IGNORE:
                    if (c == '<') {
                        if (pos.lookingAt("<?php")) {
                            state = STATE_NEUTRAL;
                            pos.skip(5);
                            return parseBufferPHP(pos, state, 0, changed);
                        } else if (pos.lookingAt("<?")) {
                            state = STATE_NEUTRAL;
                            pos.skip(2);
                            return parseBufferPHP(pos, state, 0, changed);
                        } else if (pos.lookingAt("<!--")) {
                            state = PHP_STATE_HTML_COMMENT;
                            pos.skip(4);
                        } else {
                            // Otherwise it's the start of an HTML tag.
                            state = PHP_STATE_TAG;
                            pos.next();
                        }
                    } else
                        pos.next();
                    continue;
                case PHP_STATE_TAG:
                    if (Character.isWhitespace(c))
                        state = PHP_STATE_ATTNAME;
                    else if (c == '>')
                        state = PHP_STATE_IGNORE;
                    pos.next();
                    continue;
                case PHP_STATE_ATTNAME:
                    if (c == '=')
                        state = PHP_STATE_EQUALS;
                    else if (c == '>')
                        state = PHP_STATE_IGNORE;
                    pos.next();
                    continue;
                case PHP_STATE_EQUALS:
                    if (!Character.isWhitespace(c)) {
                        if (c == '"')
                            state = PHP_STATE_ATTVALUE_QUOTE;
                        else if (c == '\'')
                            state = PHP_STATE_ATTVALUE_SINGLEQUOTE;
                        else if (pos.lookingAt("<?php")) {
                            pos.skip(5);
                            changed = parseBufferPHP(pos, STATE_NEUTRAL,
                                PHP_STATE_ATTNAME, changed);
                            state = PHP_STATE_ATTNAME;
                            continue;
                        } else
                            state = PHP_STATE_ATTVALUE;
                    }
                    pos.next();
                    continue;
                case PHP_STATE_ATTVALUE:
                    pos.next();
                    if (Character.isWhitespace(c)) {
                        state = PHP_STATE_ATTNAME;
                    } else if (c == '>') {
                        state = PHP_STATE_IGNORE;
                    }
                    continue;
                case PHP_STATE_ATTVALUE_QUOTE:
                    if (pos.lookingAt("<?php")) {
                        pos.skip(5);
                        changed = parseBufferPHP(pos, STATE_NEUTRAL,
                            PHP_STATE_ATTVALUE_QUOTE, changed);
                    } else {
                        pos.next();
                        if (c == '"')
                            state = PHP_STATE_ATTNAME;
                    }
                    continue;
                case PHP_STATE_ATTVALUE_SINGLEQUOTE:
                    if (pos.lookingAt("<?php")) {
                        pos.skip(5);
                        changed = parseBufferPHP(pos, STATE_NEUTRAL,
                            PHP_STATE_ATTVALUE_SINGLEQUOTE, changed);
                    } else {
                        pos.next();
                        if (c == '\'')
                            state = PHP_STATE_ATTNAME;
                    }
                    continue;
                default:
                    Debug.assertTrue(false);
                    break;
            }
        }
        return changed;
    }

    private static final int makeFlags(int state, int htmlState)
    {
        return (htmlState << 8) + state;
    }

    public static final int getState(int flags)
    {
        return flags & 0xff;
    }

    private static final int getHtmlState(int flags)
    {
        return (flags & 0xff00) >> 8;
    }

    private final boolean isVar(String s)
    {
        return s.length() > 0 && s.charAt(0) == '$';
    }

    private static final String opchars = "!&|<>=+/*-";

    private static final boolean isOperatorChar(char c)
    {
        return opchars.indexOf(c) >= 0;
    }

    public FormatTable getFormatTable()
    {
        if (formatTable == null) {
            formatTable = new FormatTable("PHPMode");
            formatTable.addEntryFromPrefs(PHP_FORMAT_TEXT, "text");
            formatTable.addEntryFromPrefs(PHP_FORMAT_COMMENT, "comment");
            formatTable.addEntryFromPrefs(PHP_FORMAT_STRING, "string");
            formatTable.addEntryFromPrefs(PHP_FORMAT_IDENTIFIER, "identifier", "text");
            formatTable.addEntryFromPrefs(PHP_FORMAT_KEYWORD, "keyword");
            formatTable.addEntryFromPrefs(PHP_FORMAT_FUNCTION, "function");
            formatTable.addEntryFromPrefs(PHP_FORMAT_OPERATOR, "operator");
            formatTable.addEntryFromPrefs(PHP_FORMAT_BRACE, "brace");
            formatTable.addEntryFromPrefs(PHP_FORMAT_NUMBER, "number");
            formatTable.addEntryFromPrefs(PHP_FORMAT_VAR, "var");
            formatTable.addEntryFromPrefs(PHP_FORMAT_DELIMITER, "delimiter");
            formatTable.addEntryFromPrefs(PHP_FORMAT_TAG, "tag");
            formatTable.addEntryFromPrefs(PHP_FORMAT_ATTRIBUTE, "attribute");
            formatTable.addEntryFromPrefs(PHP_FORMAT_EQUALS, "equals");
        }
        return formatTable;
    }
}
