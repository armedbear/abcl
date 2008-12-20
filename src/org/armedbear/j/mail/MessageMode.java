/*
 * MessageMode.java
 *
 * Copyright (C) 2000-2005 Peter Graves
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
import org.armedbear.j.Formatter;
import org.armedbear.j.Frame;
import org.armedbear.j.HtmlLineSegment;
import org.armedbear.j.KeyMap;
import org.armedbear.j.Keywords;
import org.armedbear.j.Link;
import org.armedbear.j.Mode;
import org.armedbear.j.NavigationComponent;
import org.armedbear.j.Position;
import org.armedbear.j.Property;
import org.armedbear.j.ToolBar;
import org.armedbear.j.View;
import org.armedbear.j.WebLine;

public final class MessageMode extends AbstractMode implements Constants, Mode
{
    private static final Mode mode = new MessageMode();

    private MessageMode()
    {
        super(MESSAGE_MODE, MESSAGE_MODE_NAME);
        keywords = new Keywords(this, true); // Ignore case.
        setProperty(Property.VERTICAL_RULE, 0);
        setProperty(Property.SHOW_LINE_NUMBERS, false);
        setProperty(Property.HIGHLIGHT_MATCHING_BRACKET, false);
        setProperty(Property.HIGHLIGHT_BRACKETS, false);
    }

    public static final Mode getMode()
    {
        return mode;
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

    public final Formatter getFormatter(Buffer buffer)
    {
        return new MessageFormatter(buffer);
    }

    protected void setKeyMapDefaults(KeyMap km)
    {
        km.mapKey('h', "messageToggleHeaders");
        km.mapKey('f', "messageForward");
        km.mapKey('F', "messageFlag");
        km.mapKey('r', "messageReplyToSender");
        km.mapKey('g', "messageReplyToGroup");
        km.mapKey('d', "messageDelete");
        km.mapKey('m', "messageMoveToFolder");
        km.mapKey('n', "messageNext");
        km.mapKey('p', "messagePrevious");
        km.mapKey('N', "messageNextInThread");
        km.mapKey('P', "messagePreviousInThread");
        km.mapKey('i', "messageIndex");
        km.mapKey('R', "messageToggleRaw");
        km.mapKey('v', "messageViewAttachment");
        km.mapKey(KeyEvent.VK_ENTER, 0, "messageViewAttachment");
        km.mapKey('s', "messageSaveAttachment");
        km.mapKey('b', "bounce");
        km.mapKey(KeyEvent.VK_F12, CTRL_MASK, "messageToggleWrap");
        km.mapKey('q', "tempBufferQuit");
    }

    protected final ToolBar getDefaultToolBar(Frame frame)
    {
        return new MessageModeToolBar(frame);
    }

    public final String getContextString(Editor editor, boolean verbose /*ignored*/)
    {
        return getContextString(editor.getDot());
    }

    public final String getMouseMovedContextString(Editor editor, Position pos)
    {
        // We want to clear the status text if the mouse is not over a link, so
        // return "" instead of null.
        final String s = getContextString(pos);
        return s != null ? s : "";
    }

    private String getContextString(Position pos)
    {
        if (pos != null && pos.getLine() instanceof WebLine) {
            HtmlLineSegment segment =
                ((WebLine)pos.getLine()).findSegment(pos.getOffset());
            if (segment != null) {
                Link link = segment.getLink();
                if (link != null)
                    return link.getTarget();
            }
        }
        return null;
    }
}
