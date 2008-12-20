/*
 * JavaScriptMode.java
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

import java.awt.event.KeyEvent;

public final class JavaScriptMode extends JavaMode implements Constants, Mode
{
    // Since this class is final, we may as well construct the singleton class
    // instance right away.
    private static JavaScriptMode mode = new JavaScriptMode();

    private JavaScriptMode()
    {
        super(JAVASCRIPT_MODE, JAVASCRIPT_MODE_NAME);
        keywords = new Keywords(this);
    }

    public static final Mode getMode()
    {
        return mode;
    }

    public final Formatter getFormatter(Buffer buffer)
    {
        return new JavaFormatter(buffer, LANGUAGE_JAVASCRIPT);
    }

    protected void setKeyMapDefaults(KeyMap km)
    {
        super.setKeyMapDefaults(km);
        km.mapKey(KeyEvent.VK_ENTER, CTRL_MASK, "newline");
    }

    public void populateModeMenu(Editor editor, Menu menu)
    {
        // No mode menu yet.
    }

    public int getCorrectIndentation(Line line, Buffer buffer)
    {
        final int indentSize = buffer.getIndentSize();
        int indent = 0;
        String text = line.trim();

        if (text.startsWith("}")) {
            Position pos = new Position(line, line.getText().indexOf('}'));
            pos = matchClosingBrace(pos);
            pos = findBeginningOfStatement(pos);
            indent = buffer.getIndentation(pos.getLine());
            return indent;
        }

        String identifier = getFirstIdentifier(line);

        if (identifier.equals("case") || identifier.equals("default")) {
            Line switchLine = findSwitch(line);
            if (switchLine != null)
                indent = buffer.getIndentation(switchLine) + indentSize;
            return indent;
        }

        Position paren = findEnclosingParen(new Position(line, 0) );

        if (paren != null) {
            if (text.startsWith(")"))
                return buffer.getIndentation(paren.getLine());
            if (paren.getLine().trim().endsWith("(") ||
                !buffer.getBooleanProperty(Property.LINEUP_ARGLIST)) {
                indent = buffer.getIndentation(paren.getLine()) + indentSize;
            } else {
                paren.skip(1);
                while (paren.getOffset() < paren.getLineLength() &&
                    paren.getLine().charAt(paren.getOffset()) <= ' ') {
                    paren.skip(1);
                }
                if (paren.getOffset() <= paren.getLineLength())
                    indent = buffer.getCol(paren);
            }
            return indent;
        }

        // Figure out indentation of previous non-blank line.
        Line model = findModel(line);

        if (model == null)
            return 0;

        if (line.flags() == STATE_COMMENT) {
            indent = buffer.getIndentation(model);
            String s = model.getText().trim();
            if (s.startsWith("/*") && text.startsWith("*"))
                return indent + 1;
            else
                return indent;
        }

        String modelText = trimSyntacticWhitespace(model.getText());
        final boolean indentBeforeBrace =
            buffer.getBooleanProperty(Property.INDENT_BEFORE_BRACE);
        if (modelText.endsWith("}")) {
            indent = buffer.getIndentation(model);
            if (indentBeforeBrace)
                indent -= indentSize;
            return indent;
        }

        Position bos = findBeginningOfStatement(new Position(model, 0));
        indent = buffer.getIndentation(bos.getLine());
        final boolean indentAfterBrace =
            buffer.getBooleanProperty(Property.INDENT_AFTER_BRACE);

        if (modelText.endsWith(")")) {
            Position pos = new Position(model, model.length()-1);
            while (pos.getChar() != ')')
                pos.skip(-1);
            pos = matchClosingParen(pos);
            indent = buffer.getIndentation(pos.getLine());

            // Loose JavaScript
            String s = pos.getLine().trim();

            if (s.startsWith("}"))
                s = s.substring(1).trim();

            String firstToken = getFirstIdentifier(s);

            if (!firstToken.equals("if") &&
                !firstToken.equals("else") &&
                !firstToken.equals("for") &&
                !firstToken.equals("while") &&
                !firstToken.equals("switch")) {
                Position begin = findBeginningOfBlock(pos);
                if (begin != null) {
                    indent = buffer.getIndentation(begin.getLine());
                    if (indentAfterBrace)
                        indent += indentSize;
                }
                return indent;
            }

            if (indentBeforeBrace || !text.startsWith("{"))
                indent += indentSize;

            return indent;
        }

        String lastIdentifier = getLastIdentifier(modelText);

        if (lastIdentifier != null && lastIdentifier.equals("else")) {
            if (indentBeforeBrace || !text.startsWith("{"))
                indent += indentSize;
            return indent;
        }

        if (isContinued(modelText)) {
            // Continuation line.
            Position pos = findBeginningOfStatement(new Position(model, 0));
            return buffer.getIndentation(pos.getLine()) + indentSize;
        }

        String modelFirstIdentifier = getFirstIdentifier(model);

        if (modelFirstIdentifier.equals("case")) {
            if (indentBeforeBrace || !text.startsWith("{"))
                indent += indentSize;
        } else if (modelFirstIdentifier.equals("default")) {
            if (indentBeforeBrace || !text.startsWith("{"))
                indent += indentSize;
        } else if (modelText.endsWith("{")) {
            if (indentAfterBrace)
                indent += indentSize;
        } else {
            // Match indentation of beginning of statement.
            Position pos = findBeginningOfStatement(new Position(model, 0));
            indent = buffer.getIndentation(pos.getLine());
        }

        return indent;
    }

    private boolean isContinued(String text)
    {
        if (text.length() == 0)
            return false;
        return isContinued(text, text.charAt(text.length()-1));
    }

    private static Position findBeginningOfBlock(Position pos)
    {
        JavaSyntaxIterator iter = new JavaSyntaxIterator(pos);

        int count = 1;

        while (true) {
            char c = iter.prevChar();

            if (c == JavaSyntaxIterator.DONE)
                return null;

            if (c == '}')
                ++count;
            else if (c == '{')
                --count;

            if (count == 0)
                return iter.getPosition();
        }
    }
}
