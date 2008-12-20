/*
 * RubyIndenter.java
 *
 * Copyright (C) 2002 Jens Luedicke <jens@irs-net.com>
 * based on PythonIndenter.java
 * Copyright (C) 2005 Peter Graves
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

public final class RubyIndenter
{
    private static final RubyMode mode = RubyMode.getMode();

    private final Line line;
    private final Buffer buffer;
    private final int indentSize;

    public RubyIndenter(Line line, Buffer buffer)
    {
        this.line = line;
        this.buffer = buffer;
        indentSize = buffer.getIndentSize();
    }

    public int getCorrectIndentation()
    {
        final String lineFirst = getFirstIdentifier(line);
        if (lineFirst.equals("def"))
            return indentDef();
        if (lineFirst.equals("when"))
            return indentWhen();
        if (lineFirst.equals("end"))
            return indentEnd();
        if (lineFirst.equals("rescue"))
            return indentRescue();
        final Line model = findModel(line);
        if (model == null)
            return 0;
        final int modelIndent = buffer.getIndentation(model);
        final String lineText = trimSyntacticWhitespace(line.getText());
        if (lineText.startsWith("}"))
            return Math.max(modelIndent - indentSize, 0);
        final String modelText = trimSyntacticWhitespace(model.getText());
        if (modelText.endsWith("do") || modelText.endsWith("|")) {
            // e.g. "do |foo|"
            return modelIndent + indentSize;
        }
        final String modelFirst = getFirstIdentifier(modelText);
        final String[] indentAfter = {
            "begin", "class", "def", "if", "else", "elsif",
            "for", "module", "rescue", "unless", "when", "while"
        };
        if (Utilities.isOneOf(modelFirst, indentAfter))
            return modelIndent + indentSize;
        // Unindent if the current line starts with "else" or "elsif".
        final String[] unindent = {"else", "elsif"};
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

    // Scan backwards for line starting with "when" or "case" and indent
    // accordingly. This doesn't work correctly with nested case statements!
    private int indentWhen()
    {
        for (Line model = line.previous(); model != null; model = model.previous()) {
            String modelFirst = getFirstIdentifier(model);
            if (modelFirst.equals("when") || modelFirst.equals("case"))
                return buffer.getIndentation(model);
        }
        return 0;
    }

    private int indentEnd()
    {
        for (Line model = line.previous(); model != null; model = model.previous()) {
            if (model.isBlank() || model.trim().startsWith("#"))
                continue;
            String modelFirst = getFirstIdentifier(model);
            if (modelFirst.equals("def"))
                return buffer.getIndentation(model);
            return Math.max(buffer.getIndentation(model) - indentSize, 0);
        }
        return 0;
    }

    private int indentRescue()
    {
        for (Line model = line.previous(); model != null; model = model.previous()) {
            if (model.isBlank() || model.trim().startsWith("#"))
                continue;
            String modelFirst = getFirstIdentifier(model);
            if (modelFirst.equals("begin"))
                return buffer.getIndentation(model);
            return Math.max(buffer.getIndentation(model) - indentSize, 0);
        }
        return 0;
    }

    // Return last non-blank line before this one.
    private static Line findModel(Line line)
    {
        for (Line model = line.previous(); model != null; model = model.previous()) {
            if (!model.isBlank() && !model.trim().startsWith("#"))
                return model;
        }
        return null;
    }

    // Replace syntactic whitespace (quotes and comments) with actual space
    // characters and return trimmed string.
    private static String trimSyntacticWhitespace(String s)
    {
        RubySyntaxIterator it = new RubySyntaxIterator(null);
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
        while (i < length && Character.isWhitespace(s.charAt(i)))
            ++i;
        char c;
        while (i < length && !Character.isWhitespace(c = s.charAt(i))) {
            sb.append(c);
            ++i;
        }
        return sb.toString();
    }
}
