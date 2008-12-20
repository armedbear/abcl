/*
 * PythonIndenter.java
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

public final class PythonIndenter
{
    private static final PythonMode mode = PythonMode.getMode();

    private final Line line;
    private final Buffer buffer;
    private final int indentSize;

    public PythonIndenter(Line line, Buffer buffer)
    {
        this.line = line;
        this.buffer = buffer;
        indentSize = buffer.getIndentSize();
    }

    public int getCorrectIndentation()
    {
        final Line model = findModel(line);
        if (model == null)
            return 0;
        final int modelIndent = buffer.getIndentation(model);
        // Keep same indentation if model is comment or docstring. Model is
        // guaranteed not to be blank so charAt(0) is safe.
        switch (model.getText().trim().charAt(0)) {
            case '#':
            case '\'':
            case '"':
                return modelIndent;
            default:
                break;
        }
        final String modelText = trimSyntacticWhitespace(model.getText());
        if (modelText.length() == 0)
            return 0; // Shouldn't happen.
        // Indent after '{', '(' or '['.
        char c = modelText.charAt(modelText.length()-1);
        if (c == '{' || c == '(' || c == '[')
            return modelIndent + indentSize;
        final String modelFirst = getFirstIdentifier(modelText);
        if (c == ':') {
            final String[] indentAfter = {
                "class", "def", "if", "else", "elif",
                "for", "while", "try", "except", "finally"
            };
            if (Utilities.isOneOf(modelFirst, indentAfter))
                return modelIndent + indentSize;
        }
        final String lineText = trimSyntacticWhitespace(line.getText());
        final String lineFirst = getFirstIdentifier(lineText);
        if (lineFirst.equals("class"))
            return 0;
        if (lineFirst.equals("def"))
            return indentDef();
        // Unindent after "break", "continue", "return" and "pass".
        final String[] unindentAfter = {"break", "continue", "return", "pass"};
        if (Utilities.isOneOf(modelFirst, unindentAfter))
            return Math.max(0, modelIndent - indentSize);
        // Unindent if the current line starts with "else", "elif", "except" or
        // "finally".
        final String[] unindent = {"else", "elif", "except", "finally"};
        if (Utilities.isOneOf(lineFirst, unindent))
            return Math.max(0, modelIndent - indentSize);
        return modelIndent;
    }

    // Scan backwards for line starting with "def" or "class" and indent
    // accordingly.
    private int indentDef()
    {
        for (Line model = line.previous(); model != null; model = model.previous()) {
            String modelFirst = getFirstIdentifier(model);
            if (modelFirst.equals("def"))
                return buffer.getIndentation(model);
            if (modelFirst.equals("class"))
                return buffer.getIndentation(model) + buffer.getIndentSize();
        }
        return 0;
    }

    // Return last non-blank line before this one.
    private static Line findModel(Line line)
    {
        for (Line model = line.previous(); model != null; model = model.previous()) {
            if (!model.isBlank())
                return model;
        }
        return null;
    }

    // Replace syntactic whitespace (quotes and comments) with actual space
    // characters and return trimmed string.
    private static String trimSyntacticWhitespace(String s)
    {
        PythonSyntaxIterator it = new PythonSyntaxIterator(null);
        return new String(it.hideSyntacticWhitespace(s)).trim();
    }

    // Never returns null.
    private static String getFirstIdentifier(Line line)
    {
        return getFirstIdentifier(trimSyntacticWhitespace(line.getText()));
    }

    // Never returns null.
    private static String getFirstIdentifier(String s)
    {
        FastStringBuffer sb = new FastStringBuffer();
        final int length = s.length();
        int i = 0;
        while (i < length && !mode.isIdentifierStart(s.charAt(i)))
            ++i;
        char c;
        while (i < length && mode.isIdentifierPart(c = s.charAt(i))) {
            sb.append(c);
            ++i;
        }
        return sb.toString();
    }
}
