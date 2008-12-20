/*
 * MailboxMode.java
 *
 * Copyright (C) 1998-2002 Peter Graves
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
import javax.swing.JCheckBoxMenuItem;
import org.armedbear.j.AbstractMode;
import org.armedbear.j.Buffer;
import org.armedbear.j.Constants;
import org.armedbear.j.Dispatcher;
import org.armedbear.j.Editor;
import org.armedbear.j.Formatter;
import org.armedbear.j.Frame;
import org.armedbear.j.KeyMap;
import org.armedbear.j.Line;
import org.armedbear.j.Menu;
import org.armedbear.j.Mode;
import org.armedbear.j.NavigationComponent;
import org.armedbear.j.Position;
import org.armedbear.j.Property;
import org.armedbear.j.ToolBar;
import org.armedbear.j.View;

public class MailboxMode extends AbstractMode implements Constants, Mode
{
    private static final MailboxMode mode = new MailboxMode();

    private MailboxMode()
    {
        this(MAILBOX_MODE, MAILBOX_MODE_NAME);
    }

    protected MailboxMode(int id, String displayName)
    {
        super(id, displayName);
        setProperty(Property.VERTICAL_RULE, 0);
        setProperty(Property.SHOW_LINE_NUMBERS, false);
        setProperty(Property.SHOW_CHANGE_MARKS, false);
        setProperty(Property.HIGHLIGHT_MATCHING_BRACKET, false);
        setProperty(Property.HIGHLIGHT_BRACKETS, false);
    }

    public static Mode getMode()
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

    public Formatter getFormatter(Buffer buffer)
    {
        return new MailboxFormatter(buffer);
    }

    protected void setKeyMapDefaults(KeyMap km)
    {
        km.mapKey(KeyEvent.VK_HOME, 0, "bol");
        km.mapKey(KeyEvent.VK_END, CTRL_MASK, "mailboxLastMessage");
        km.mapKey(KeyEvent.VK_ENTER, 0, "mailboxReadMessageOtherWindow");
        km.mapKey(KeyEvent.VK_ENTER, CTRL_MASK, "mailboxReadMessage");
        km.mapKey(KeyEvent.VK_G, CTRL_MASK | SHIFT_MASK, "mailboxReadMessage");
        km.mapKey(VK_DOUBLE_MOUSE_1, 0, "mailboxReadMessageOtherWindow");
        km.mapKey(VK_MOUSE_2, 0, "mailboxReadMessageOtherWindow");
        km.mapKey('G', "mailboxGetNewMessages");
        km.mapKey('s', "mailboxSaveToFolder");
        km.mapKey('m', "mailboxMoveToFolder");
        km.mapKey('d', "mailboxDelete");
        km.mapKey('u', "mailboxUndelete");
        km.mapKey('M', "mailboxMarkRead");
        km.mapKey(KeyEvent.VK_M, CTRL_MASK | SHIFT_MASK, "mailboxMarkUnread");
        km.mapKey('t', "mailboxTag");
        km.mapKey(KeyEvent.VK_T, CTRL_MASK, "mailboxTagPattern");
        km.mapKey('T', "mailboxUntagAll");
        km.mapKey('R', "mailboxToggleRaw");
        km.mapKey('$', "mailboxExpunge");
        km.mapKey('c', "compose");
        km.mapKey('l', "mailboxLimit");
        km.mapKey('L', "mailboxUnlimit");
        km.mapKey('b', "bounce");
        km.mapKey('F', "mailboxFlag");
    }

    public void populateMenu(Editor editor, Menu menu)
    {
        final String text = menu.getText();
        if (text == "View") {
            final Dispatcher dispatcher = editor.getDispatcher();
            JCheckBoxMenuItem toolbarMenuItem = new JCheckBoxMenuItem("Toolbar");
            toolbarMenuItem.setMnemonic('T');
            toolbarMenuItem.setActionCommand("toggleToolbar");
            toolbarMenuItem.addActionListener(dispatcher);
            toolbarMenuItem.setSelected(editor.getFrame().getShowToolbar());
            menu.add(toolbarMenuItem);
            JCheckBoxMenuItem sidebarMenuItem = new JCheckBoxMenuItem("Sidebar");
            sidebarMenuItem.setMnemonic('S');
            sidebarMenuItem.setActionCommand("toggleSidebar");
            sidebarMenuItem.addActionListener(dispatcher);
            sidebarMenuItem.setSelected(editor.getSidebar() != null);
            menu.add(sidebarMenuItem);
            menu.addSeparator();
            JCheckBoxMenuItem groupByThread =
                new JCheckBoxMenuItem("Group Messages By Thread");
            groupByThread.setMnemonic('G');
            groupByThread.setActionCommand("toggleGroupByThread");
            groupByThread.addActionListener(dispatcher);
            if (editor.getBuffer() instanceof Mailbox) {
                groupByThread.setSelected(editor.getBuffer().
                    getBooleanProperty(Property.GROUP_BY_THREAD));
            }
            menu.add(groupByThread);
            menu.addSeparator();
            menu.add(editor, "Split Window", 'W', "splitWindow");
            menu.add(editor, "Unsplit Window", 'U', "unsplitWindow");
            menu.add(editor, "Close Window", 'C', "killWindow");
        } else
            super.populateMenu(editor, menu);
    }

    protected ToolBar getDefaultToolBar(Frame frame)
    {
        return new MailboxModeToolBar(frame);
    }

    public String getContextString(Editor editor, boolean verbose)
    {
        Position dot = editor.getDot();
        if (dot != null) {
            final Line dotLine = dot.getLine();
            if (dotLine instanceof MailboxLine) {
                MailboxEntry entry = ((MailboxLine)dotLine).getMailboxEntry();
                if (entry != null)
                    return entry.getSubject();
            }
        }
        return null;
    }
}
