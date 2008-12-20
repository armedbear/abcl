/*
 * SendMailMode.java
 *
 * Copyright (C) 2000-2004 Peter Graves
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

package org.armedbear.j.mail;

import java.awt.event.KeyEvent;
import org.armedbear.j.AbstractMode;
import org.armedbear.j.Buffer;
import org.armedbear.j.Constants;
import org.armedbear.j.Editor;
import org.armedbear.j.File;
import org.armedbear.j.Formatter;
import org.armedbear.j.Frame;
import org.armedbear.j.KeyMap;
import org.armedbear.j.Keywords;
import org.armedbear.j.Line;
import org.armedbear.j.Mode;
import org.armedbear.j.NavigationComponent;
import org.armedbear.j.Property;
import org.armedbear.j.ToolBar;
import org.armedbear.j.View;

public final class SendMailMode extends AbstractMode implements Constants, Mode
{
    private static final SendMailMode mode = new SendMailMode();

    private SendMailMode()
    {
        super(SEND_MAIL_MODE, SEND_MAIL_MODE_NAME);
        keywords = new Keywords(this, true); // Ignore case.
        setProperty(Property.WRAP_COL, 72);
        setProperty(Property.WRAP, true);
        // If user has turned on vertical rule, its default position should be
        // the wrap column.
        if (Editor.preferences().getIntegerProperty(Property.VERTICAL_RULE) != 0)
            setProperty(Property.VERTICAL_RULE, getIntegerProperty(Property.WRAP_COL));
        else
            setProperty(Property.VERTICAL_RULE, 0);
        setProperty(Property.SHOW_LINE_NUMBERS, false);
        setProperty(Property.SHOW_CHANGE_MARKS, false);
        setProperty(Property.HIGHLIGHT_MATCHING_BRACKET, false);
        setProperty(Property.HIGHLIGHT_BRACKETS, false);
    }

    public static SendMailMode getMode()
    {
        return mode;
    }

    public Buffer createBuffer(File file)
    {
        return new SendMail(file);
    }

    public NavigationComponent getSidebarComponent(Editor editor)
    {
        View view = editor.getCurrentView();
        if (view == null)
            return null; // Shouldn't happen.
        if (view.getSidebarComponent() == null)
            view.setSidebarComponent(FolderTree.getInstance(editor.getFrame()));
        return view.getSidebarComponent();
    }

    public Formatter getFormatter(Buffer buffer)
    {
        return new MessageFormatter(buffer);
    }

    protected void setKeyMapDefaults(KeyMap km)
    {
        km.mapKey(KeyEvent.VK_ENTER, 0, "newlineAndIndent");
        km.mapKey(':', "sendMailElectricColon");
        km.mapKey(KeyEvent.VK_ENTER, CTRL_MASK, "send");
        km.mapKey(KeyEvent.VK_TAB, 0, "sendMailTab");
        km.mapKey(KeyEvent.VK_TAB, SHIFT_MASK, "sendMailBackTab");
        km.mapKey(KeyEvent.VK_F12, CTRL_MASK | SHIFT_MASK,
                  "wrapParagraphsInRegion");
    }

    protected ToolBar getDefaultToolBar(Frame frame)
    {
        return new SendMailModeToolBar(frame);
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
        if (buffer instanceof SendMail)
            if (((SendMail)buffer).isHeaderLine(line))
                return buffer.getIndentSize();

        return 0;
    }

    public boolean confirmClose(Editor editor, Buffer buffer)
    {
        if (buffer instanceof SendMail) {
            if (((SendMail)buffer).hasBeenSent())
                return true;
            else if (!buffer.isModified())
                return true;
            else
                return editor.confirm(buffer.toString(),
                    "Message has not been sent; kill anyway?");
        }
        return super.confirmClose(editor, buffer);
    }
}
