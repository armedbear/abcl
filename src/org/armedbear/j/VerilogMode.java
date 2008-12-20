/*
 * VerilogMode.java
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

public final class VerilogMode extends AbstractMode implements Constants, Mode
{
    private static final VerilogMode mode = new VerilogMode();

    private VerilogMode()
    {
        super(VERILOG_MODE, VERILOG_MODE_NAME);
        keywords = new Keywords(this);
    }

    public static VerilogMode getMode()
    {
        return mode;
    }

    public String getCommentStart()
    {
        return "// ";
    }

    public Formatter getFormatter(Buffer buffer)
    {
        return new VerilogFormatter(buffer);
    }

    protected void setKeyMapDefaults(KeyMap km)
    {
        km.mapKey(KeyEvent.VK_TAB, CTRL_MASK, "insertTab");
        km.mapKey(KeyEvent.VK_TAB, 0, "tab");
        km.mapKey(KeyEvent.VK_ENTER, 0, "newlineAndIndent");
        km.mapKey(KeyEvent.VK_T, CTRL_MASK, "findTag");
        km.mapKey(KeyEvent.VK_PERIOD, ALT_MASK, "findTagAtDot");
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
        return new VerilogTagger(buffer);
    }

    public boolean canIndent()
    {
        return true;
    }

    public int getCorrectIndentation(Line line, Buffer buffer)
    {
        final int indentSize = buffer.getIndentSize();
        final Line model = findModel(line);
        if (model == null)
            return 0;
        final String trim = line.getText().trim();
        final int modelIndent = buffer.getIndentation(model);
        final String modelTrim = model.getText().trim();
        if (line.flags() == STATE_COMMENT) {
            if (modelTrim.startsWith("/*") && trim.startsWith("*"))
                return modelIndent + 1;
            else
                return modelIndent;
        }
        if (modelTrim.endsWith("("))
            return modelIndent + indentSize;
        final String modelIdentifier =
            Utilities.getFirstIdentifier(modelTrim, this);
        if (Utilities.isOneOf(modelIdentifier, alwaysIndentAfter))
            return modelIndent + indentSize;
        if (Utilities.isOneOf(modelIdentifier, maybeIndentAfter)) {
            if (modelTrim.endsWith(";"))
                return modelIndent;
            else
                return modelIndent + indentSize;
        }
        final String identifier = Utilities.getFirstIdentifier(trim, this);
        if ("end".equals(identifier)) {
            Line beginLine = findBeginLine(line);
            if (beginLine != null)
                return buffer.getIndentation(beginLine);
        }
        return modelIndent;
    }

    private static Line findModel(Line line)
    {
        Line model = line.previous();
        if (line.flags() == STATE_COMMENT) {
            // Any non-blank line is an acceptable model.
            while (model != null && model.isBlank())
                model = model.previous();
        } else {
            while (model != null && !isAcceptableModel(model))
                model = model.previous();
        }
        return model;
    }

    private static boolean isAcceptableModel(Line line)
    {
        if (line.isBlank())
            return false;
        if (line.flags() == STATE_COMMENT)
            return false;

        return true;
    }

    private Line findBeginLine(Line line)
    {
        int count = 1;
        while (true) {
            line = line.previous();
            if (line == null)
                return null;
            String identifier = Utilities.getFirstIdentifier(line.trim(), this);
            if (identifier != null) {
                if (identifier.equals("begin")) {
                    if (--count == 0)
                        return line;
                } else if (identifier.equals("end"))
                    ++count;
            }
        }
    }

    public boolean isIdentifierStart(char c)
    {
        return startChars.indexOf(c) >= 0;
    }

    public boolean isIdentifierPart(char c)
    {
        return partChars.indexOf(c) >= 0;
    }

    private static final String startChars =
        "`ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz_";

    private static final String partChars =
        "`ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789_$";

    private static final String[] alwaysIndentAfter = {
        "begin",
        "case",
        "casex",
        "casez",
        "fork",
        "function",
        "generate",
        "module",
        "primitive",
        "specify",
        "table",
        "task"
    };

    private static final String[] maybeIndentAfter = {
        "always",
        "else",
        "for",
        "forever",
        "if",
        "initial",
        "repeat",
        "while"
    };
}
