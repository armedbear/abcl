/*
 * TclMode.java
 *
 * Copyright (C) 2002 Peter Graves
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

public final class TclMode extends AbstractMode implements Constants, Mode
{
    private static final int STATE_NEUTRAL     = 0;
    private static final int STATE_SINGLEQUOTE = 1;
    private static final int STATE_DOUBLEQUOTE = 2;

    private static final TclMode mode = new TclMode();

    private TclMode()
    {
        super(TCL_MODE, TCL_MODE_NAME);
        keywords = new Keywords(this);
    }

    public static TclMode getMode()
    {
        return mode;
    }

    public String getCommentStart()
    {
        return "# ";
    }

    public Formatter getFormatter(Buffer buffer)
    {
        return new TclFormatter(buffer);
    }

    protected void setKeyMapDefaults(KeyMap km)
    {
        km.mapKey('{', "electricOpenBrace");
        km.mapKey('}', "electricCloseBrace");
        km.mapKey(KeyEvent.VK_TAB, CTRL_MASK, "insertTab");
        km.mapKey(KeyEvent.VK_TAB, 0, "tab");
        km.mapKey(KeyEvent.VK_ENTER, 0, "newlineAndIndent");
        km.mapKey(KeyEvent.VK_T, CTRL_MASK, "findTag");
        km.mapKey(KeyEvent.VK_PERIOD, ALT_MASK, "findTagAtDot");
        km.mapKey(KeyEvent.VK_OPEN_BRACKET, CTRL_MASK | SHIFT_MASK, "insertBraces");
        // Duplicate mapping for 1.4.
        km.mapKey(KeyEvent.VK_BRACELEFT, CTRL_MASK | SHIFT_MASK, "insertBraces");
        km.mapKey(KeyEvent.VK_F12, 0, "wrapComment");
        // Duplicate mapping to support IBM 1.3 for Linux.
        km.mapKey(0xffc9, 0, "wrapComment"); // F12
    }

    public boolean isTaggable()
    {
        return true;
    }

    public Tagger getTagger(SystemBuffer buffer)
    {
        return new TclTagger(buffer);
    }

    public boolean canIndent()
    {
        return true;
    }

    public int getCorrectIndentation(Line line, Buffer buffer)
    {
        final int indentSize = buffer.getIndentSize();

        // Is the current line a continuation line?
        Line previous = line.previous();
        if (previous == null)
            return 0;
        if (isContinued(previous)) {
            // The current line is a continuation line. We need to find the
            // first line in the sequence of continued lines.
            while (true) {
                Line maybe = previous.previous();
                if (maybe != null && isContinued(maybe))
                    previous = maybe;
                else
                    break;
            }
            return buffer.getIndentation(previous) + indentSize;
        }

        // Current line is not a continuation line.
        final String trim = line.getText().trim();
        if (trim.startsWith("}")) {
            Position pos = matchClosingBrace(new Position(line, 0));
            return buffer.getIndentation(pos.getLine());
        }
        final Line model = findModel(line);
        if (model == null)
            return 0;
        final int modelIndent = buffer.getIndentation(model);
        final String modelTrim = trimSyntacticWhitespace(model);

        if (modelTrim.endsWith("{")) {
            if (buffer.getBooleanProperty(Property.INDENT_AFTER_BRACE))
                return modelIndent + indentSize;
            else
                return modelIndent;
        }

        return modelIndent;
    }

    private static Line findModel(Line line)
    {
        Line model = line.previous();
        if (model == null)
            return null;
        // Check for continuation line.
        if (isContinued(model)) {
            while (true) {
                Line previous = model.previous();
                if (previous != null && isContinued(previous))
                    model = previous;
                else
                    break;
            }
            return model;
        }
        // Otherwise any non-blank line will do.
        while (model != null && model.isBlank())
            model = model.previous();
        if (model == null)
            return null;
        // If the model we've found is itself a continuation line, we need to
        // find the first line in the sequence of continued lines.
        while (true) {
            Line previous = model.previous();
            if (previous != null && isContinued(previous))
                model = previous;
            else
                break;
        }
        return model;
    }

    private static boolean isContinued(Line line)
    {
        return line.getText().endsWith("\\");
    }

    private static Position matchClosingBrace(Position start)
    {
        int count = 1;
        TclSyntaxIterator it = new TclSyntaxIterator(start);
        char c;
        while ((c = it.prevChar()) != SyntaxIterator.DONE) {
            if (c == '}')
                ++count;
            else if (c == '{') {
                --count;
                if (count == 0) // Found it!
                    break;
            }
        }
        return it.getPosition();
    }

    // Replaces syntactic whitespace (quotes and comments) with actual space
    // characters and returns trimmed string.
    private static String trimSyntacticWhitespace(Line line)
    {
        TclSyntaxIterator it = new TclSyntaxIterator(null);
        return new String(it.hideSyntacticWhitespace(line.getText())).trim();
    }

    public boolean isIdentifierStart(char c)
    {
        return !Character.isWhitespace(c);
    }

    public boolean isIdentifierPart(char c)
    {
        return !Character.isWhitespace(c);
    }
}
