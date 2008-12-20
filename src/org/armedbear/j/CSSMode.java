/*
 * CSSMode.java
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

public final class CSSMode extends AbstractMode implements Constants, Mode
{
    private static final CSSMode mode = new CSSMode();

    private CSSMode()
    {
        super(CSS_MODE, CSS_MODE_NAME);
    }

    public static CSSMode getMode()
    {
        return mode;
    }

    public Formatter getFormatter(Buffer buffer)
    {
        return new CSSFormatter(buffer);
    }

    protected void setKeyMapDefaults(KeyMap km)
    {
        km.mapKey(KeyEvent.VK_ENTER, 0, "newlineAndIndent");
    }

    public boolean canIndent()
    {
        return true;
    }

    public boolean canIndentPaste()
    {
        return false;
    }

    public int getCorrectIndentation(Line line, Buffer buffer)
    {
        final int indentSize = buffer.getIndentSize();
        final Line model = findModel(line);
        if (model == null)
            return 0;
        final int modelIndent = buffer.getIndentation(model);
        if (model.getText().trim().endsWith("{"))
            return modelIndent + indentSize;
        return modelIndent;
    }

    private Line findModel(Line line)
    {
        Line model = line.previous();
        while (model != null && model.isBlank())
            model = model.previous();
        return model;
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
        "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz";

    private static final String partChars =
        "-ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789_";
}
