/*
 * PerlMode.java
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

import gnu.regexp.RE;
import gnu.regexp.UncheckedRE;
import java.awt.event.KeyEvent;

public final class PerlMode extends AbstractMode implements Constants, Mode
{
    private static final PerlMode mode = new PerlMode();

    private PerlMode()
    {
        super(PERL_MODE, PERL_MODE_NAME);
        keywords = new Keywords(this);
    }

    public static final PerlMode getMode()
    {
        return mode;
    }

    public final boolean canIndent()
    {
        return true;
    }

    public final String getCommentStart()
    {
        return "# ";
    }

    public final SyntaxIterator getSyntaxIterator(Position pos)
    {
        return new PerlSyntaxIterator(pos);
    }

    public final Formatter getFormatter(Buffer buffer)
    {
        return new PerlFormatter(buffer);
    }

    protected void setKeyMapDefaults(KeyMap km)
    {
        km.mapKey('{', "electricOpenBrace");
        km.mapKey('}', "electricCloseBrace");
        km.mapKey(';', "electricSemi");
        km.mapKey(KeyEvent.VK_TAB, 0, "tab");
        km.mapKey(KeyEvent.VK_ENTER, 0, "newlineAndIndent");
        km.mapKey(KeyEvent.VK_T, CTRL_MASK, "findTag");
        km.mapKey(KeyEvent.VK_PERIOD, ALT_MASK, "findTagAtDot");
        km.mapKey(KeyEvent.VK_L, CTRL_MASK | SHIFT_MASK, "listTags");
        km.mapKey(')', "closeParen");
        km.mapKey(KeyEvent.VK_I, ALT_MASK, "cycleIndentSize");

        km.mapKey(KeyEvent.VK_OPEN_BRACKET, CTRL_MASK | SHIFT_MASK, "insertBraces");
        // Duplicate mapping for 1.4.
        km.mapKey(KeyEvent.VK_BRACELEFT, CTRL_MASK | SHIFT_MASK, "insertBraces");

        km.mapKey(KeyEvent.VK_9, CTRL_MASK | SHIFT_MASK, "insertParentheses");

        km.mapKey(KeyEvent.VK_F12, 0, "wrapComment");

        if (Platform.isPlatformLinux()) {
            // Blackdown 1.1.7v3, 1.2pre2, IBM 1.1.8.
            // Duplicate mappings needed for VK_9, VK_0 and VK_OPEN_BRACKET.
            km.mapKey(0xbb, CTRL_MASK | SHIFT_MASK, "insertBraces");

            // Duplicate mapping to support IBM 1.3 for Linux.
            km.mapKey(0xffc9, 0, "wrapComment"); // F12
        }

        km.mapKey(KeyEvent.VK_OPEN_BRACKET, CTRL_MASK, "fold");
        km.mapKey(KeyEvent.VK_CLOSE_BRACKET, CTRL_MASK, "unfold");
    }

    public Tagger getTagger(SystemBuffer buffer)
    {
        return new PerlTagger(buffer);
    }

    public boolean isTaggable()
    {
        return true;
    }

    public boolean hasQualifiedNames()
    {
        return true;
    }

    public boolean isQualifiedName(String s)
    {
        return s.indexOf("::") >= 0;
    }

    public int getCorrectIndentation(Line line, Buffer buffer)
    {
        String trim = line.trim();
        if (trim.length() > 0) {
            Position pos = null;
            char c = trim.charAt(0);
            if (c == '}') {
                pos = matchClosingBrace(new Position(line,
                    line.getText().indexOf('}')));
                if (pos == null)
                    return 0;
                if (!pos.getLine().trim().startsWith("{"))
                    pos = findBeginningOfStatement(pos);
            } else if (c == ')') {
                pos = findEnclosingParen(new Position(line,
                    line.getText().indexOf(')')));
                if (pos == null)
                    return 0;
                if (!pos.getLine().trim().startsWith("("))
                    pos = findBeginningOfStatement(pos);
            }
            if (pos != null)
                return buffer.getIndentation(pos.getLine());

            // Labels are a special case.
            if (isLabel(line))
                return 0;
        }
        final Line modelLine = findModel(line);
        if (modelLine == null)
            return 0;
        final int indentSize = buffer.getIndentSize();
        int modelIndent = 0;
        final String modelText = trimSyntacticWhitespace(modelLine.getText());
        if (modelText.equals("{")) {
            modelIndent = buffer.getIndentation(modelLine);
            if (buffer.getBooleanProperty(Property.INDENT_AFTER_BRACE))
                return modelIndent + indentSize;
            else
                return modelIndent;
        }
        Position pos = findBeginningOfStatement(new Position(modelLine, 0));
        if (pos != null)
            modelIndent = buffer.getIndentation(pos.getLine());
        if (modelText.endsWith("{")) {
            if (buffer.getBooleanProperty(Property.INDENT_AFTER_BRACE))
                return modelIndent + indentSize;
            else
                return modelIndent;
        }
        if (modelText.endsWith("}"))
            return modelIndent;
        if (modelText.endsWith(";"))
            return modelIndent;
        pos = findEnclosingParen(new Position(line, 0) );
        if (pos != null) {
            if (pos.getLine().trim().endsWith("(") ||
                !buffer.getBooleanProperty(Property.LINEUP_ARGLIST)) {
                return buffer.getIndentation(pos.getLine()) + indentSize;
            } else {
                // Advance past '('.
                pos.skip(1);
                // Advance to first non-whitespace char.
                pos.skipWhitespaceOnCurrentLine();
                return buffer.getCol(pos);
            }
        }
        if (modelText.endsWith(","))
            return buffer.getIndentation(modelLine);

        // Continuation line.
        pos = findBeginningOfStatement(new Position(line, 0));

        if (pos != null) {
            if (line.getText().trim().startsWith("{")) {
                if (buffer.getBooleanProperty(Property.INDENT_BEFORE_BRACE))
                    return buffer.getIndentation(pos.getLine()) + indentSize;
                else
                    return buffer.getIndentation(pos.getLine());
            } else
                return buffer.getIndentation(pos.getLine()) + indentSize;
        }
        return modelIndent;
    }

    private static Line findModel(Line line)
    {
        for (Line modelLine = line.previous(); modelLine != null;
            modelLine = modelLine.previous()) {
            if (modelLine.isBlank())
                continue;
            else if (modelLine.trim().startsWith("#"))
                continue;
            else if (isLabel(modelLine))
                continue;
            else
                return modelLine;
        }
        return null;
    }

    private static RE labelRE = new UncheckedRE("^\\s*[A-Za-z0-9_]+:\\s*$");

    private static boolean isLabel(Line line)
    {
        return labelRE.getMatch(line.getText()) != null;
    }

    // Scan backwards from starting position, looking for unmatched opening
    // parenthesis.
    private static Position findEnclosingParen(Position start)
    {
        PerlSyntaxIterator it = new PerlSyntaxIterator(start);
        int count = 0;
        char c;
        while ((c = it.prevChar()) != SyntaxIterator.DONE) {
            if (c == '}')
                return null;
            if (c == ')') {
                ++count;
            } else if (c == '(') {
                if (count == 0)
                    return it.getPosition(); // Found unmatched '('.
                else
                    --count;
            }
        }
        return null;
    }

    static Position findBeginningOfStatement(Position start)
    {
        Position pos = new Position(start);
        if (pos.getLine().trim().startsWith("}")) {
            Position posMatch =
                matchClosingBrace(new Position(pos.getLine(), 0));
            if (posMatch != null)
                pos = posMatch;
        } else {
            Position posParen = findEnclosingParen(pos);
            if (posParen != null)
                pos = posParen;
        }
        while (pos.getLine() != null) {
            pos.setLine(pos.getLine().previous());
            if (pos.getLine() == null)
                break;
            pos.setOffset(pos.getLineLength());
            String s = trimSyntacticWhitespace(pos.getLine().getText());
            if (s.length() > 0) {
                char c = s.charAt(s.length() - 1);
                if (c == ';' || c == '{' || c == '}' || c == ':')
                    break;
            }
        }
        if (pos.getLine() == null) {
            pos.moveTo(start.getLine(), 0);
        } else {
            // Skip syntactic whitespace.
            PerlSyntaxIterator it = new PerlSyntaxIterator(pos);
            while (true) {
                char c = it.nextChar();
                if (c == SyntaxIterator.DONE)
                    break;
                if (c > ' ')
                    break;
            }
            pos = it.getPosition();
        }
        return pos;
    }

    private static Position matchClosingBrace(Position start)
    {
        int count = 1;
        PerlSyntaxIterator it = new PerlSyntaxIterator(start);
        char c;
        while ((c = it.prevChar()) != SyntaxIterator.DONE) {
            if (c == '}')
                ++count;
            else if (c == '{')
                --count;
            if (count == 0) // Found it!
                return it.getPosition();
        }
        return null;
    }

    // Replace syntactic whitespace (quotes and comments) with actual space
    // characters and return trimmed string.
    protected static String trimSyntacticWhitespace(String s)
    {
        PerlSyntaxIterator it = new PerlSyntaxIterator(null);
        return (new String(it.hideSyntacticWhitespace(s))).trim();
    }

    private static final String validChars =
        "$@%ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz_0123456789";

    static final boolean isIdentifierChar(char c)
    {
        return (validChars.indexOf(c) >= 0);
    }

    public boolean isIdentifierStart(char c)
    {
        return isIdentifierChar(c);
    }

    public boolean isIdentifierPart(char c)
    {
        return isIdentifierChar(c);
    }

    public boolean isCommentLine(Line line)
    {
        return line.trim().startsWith("#");
    }
}
