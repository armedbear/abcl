/*
 * VHDLMode.java
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

import gnu.regexp.RE;
import gnu.regexp.UncheckedRE;
import java.awt.event.KeyEvent;

public final class VHDLMode extends AbstractMode implements Constants, Mode
{
    private static final VHDLMode mode = new VHDLMode();

    private VHDLMode()
    {
        super(VHDL_MODE, VHDL_MODE_NAME);
        keywords = new Keywords(this);
    }

    public static VHDLMode getMode()
    {
        return mode;
    }

    public String getCommentStart()
    {
        return "-- ";
    }

    public Formatter getFormatter(Buffer buffer)
    {
        return new VHDLFormatter(buffer);
    }

    public boolean isKeyword(String s)
    {
        return keywords.isKeyword(s.toLowerCase());
    }

    protected void setKeyMapDefaults(KeyMap km)
    {
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
        return new VHDLTagger(buffer);
    }

    public boolean canIndent()
    {
        return true;
    }

    public boolean canIndentPaste()
    {
        return false;
    }

    private static final RE beginRE = new UncheckedRE("^begin\\s");
    private static final RE thenRE = new UncheckedRE("\\s+then$");
    private static final RE loopRE = new UncheckedRE("\\s+loop$");

    public int getCorrectIndentation(Line line, Buffer buffer)
    {
        final int indentSize = buffer.getIndentSize();
        final Line model = findModel(line);
        if (model == null)
            return 0;
        final int modelIndent = buffer.getIndentation(model);
        final String modelTrim = trimSyntacticWhitespace(model);

        if (modelTrim.equals("begin") || beginRE.getMatch(modelTrim) != null) {
            // Model starts with "begin".
            return modelIndent + indentSize;
        }
        if (modelTrim.equals("then") || thenRE.getMatch(modelTrim) != null) {
            // Model ends with "then".
            return modelIndent + indentSize;
        }
        if (modelTrim.equals("loop") || loopRE.getMatch(modelTrim) != null) {
            // Model ends with "loop".
            return modelIndent + indentSize;
        }

        return modelIndent;
    }

    private Line findModel(Line line)
    {
        Line model = line.previous();
        while (model != null && model.isBlank())
            model = model.previous();
        return model;
    }

    // Replaces syntactic whitespace (quotes and comments) with actual space
    // characters and returns trimmed string.
    private static String trimSyntacticWhitespace(Line line)
    {
        VHDLSyntaxIterator it = new VHDLSyntaxIterator(null);
        return new String(it.hideSyntacticWhitespace(line.getText())).trim();
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
        "`ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz";

    private static final String partChars =
        "`ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789_";
}
