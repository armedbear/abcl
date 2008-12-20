/*
 * DirectoryMode.java
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
import javax.swing.ButtonGroup;
import javax.swing.JRadioButtonMenuItem;

public final class DirectoryMode extends AbstractMode implements Constants, Mode
{
    private static final DirectoryMode mode = new DirectoryMode();

    private DirectoryMode()
    {
        super(DIRECTORY_MODE, DIRECTORY_MODE_NAME);
        setProperty(Property.VERTICAL_RULE, 0);
        setProperty(Property.SHOW_LINE_NUMBERS, false);
        setProperty(Property.SHOW_CHANGE_MARKS, false);
        setProperty(Property.HIGHLIGHT_MATCHING_BRACKET, false);
        setProperty(Property.HIGHLIGHT_BRACKETS, false);
    }

    public static final DirectoryMode getMode()
    {
        return mode;
    }

    public NavigationComponent getSidebarComponent(Editor editor)
    {
        Debug.assertTrue(editor.getBuffer().getMode() == mode);
        if (!editor.getBuffer().getBooleanProperty(Property.ENABLE_TREE))
            return null;
        View view = editor.getCurrentView();
        if (view == null)
            return null; // Shouldn't happen.
        if (view.getSidebarComponent() == null)
            view.setSidebarComponent(DirectoryTree.getDirectoryTree(editor));
        return view.getSidebarComponent();
    }

    public Formatter getFormatter(Buffer buffer)
    {
        return new DirectoryFormatter(buffer);
    }

    protected void setKeyMapDefaults(KeyMap km)
    {
        km.mapKey(KeyEvent.VK_ENTER, 0, "dirOpenFile");
        km.mapKey(KeyEvent.VK_G, CTRL_MASK | SHIFT_MASK, "dirOpenFile");
        km.mapKey(VK_DOUBLE_MOUSE_1, 0, "dirOpenFile");
        km.mapKey(VK_MOUSE_2, 0, "dirOpenFile");
        km.mapKey(KeyEvent.VK_ENTER, CTRL_MASK, "dirOpenFileAndKillDirectory");
        km.mapKey(KeyEvent.VK_B, CTRL_MASK | SHIFT_MASK, "dirBrowseFile");
        km.mapKey(KeyEvent.VK_BACK_SPACE, 0, "dirUpDir");
        km.mapKey('u', "dirUpDir");
        km.mapKey('l', "dirLimit");
        km.mapKey('L', "dirUnlimit");
        km.mapKey(KeyEvent.VK_S, CTRL_MASK, "dirCycleSortBy");
        km.mapKey(KeyEvent.VK_R, CTRL_MASK, "dirRescan");
        km.mapKey(KeyEvent.VK_DELETE, 0, "dirDeleteFiles");
        km.mapKey('c', "dirCopyFile");
        km.mapKey('g', "dirGetFile");
        km.mapKey('m', "dirMoveFile");
        km.mapKey('t', "dirTagFile");
        km.mapKey('!', "dirDoShellCommand");
        km.mapKey(KeyEvent.VK_HOME, 0, "dirHome");
        km.mapKey('b', "dirBack");
        km.mapKey('f', "dirForward");
    }

    public void populateMenu(Editor editor, Menu menu)
    {
        final String text = menu.getText();
        if (text == "File") {
            menu.add(editor, "New", 'N', "newBuffer");
            menu.add(editor, "Open...", 'O', "openFile");
            menu.add(editor, "Recent Files...", 'R', "recentFiles");
            menu.addSeparator();
            menu.add(editor, "Save All", 'A', "saveAll");
            menu.add(editor, "Close", 'C', "killBuffer");
            menu.add(editor, "Close All", 'L', "closeAll");
            menu.add(editor, "Close Others", 'H', "closeOthers");
            menu.addSeparator();
            menu.add(editor, "Next Buffer", 'T', "nextBuffer");
            menu.add(editor, "Previous Buffer", 'R', "prevBuffer");
            menu.addSeparator();
            menu.add(editor, "New Frame", 'M', "newFrame");
            menu.add(editor, "Execute Command...", 'D', "executeCommand");
            menu.addSeparator();
            menu.add(editor, "Save Session", 'S', "saveSession");
            menu.add(editor, "Load Session...", 'L', "loadSession");
            menu.addSeparator();
            menu.add(editor, "Print...", 'P', "print");
            menu.addSeparator();
            menu.add(editor, "Save All/Exit", '/', "saveAllExit");
            menu.add(editor, "Exit", 'X', "quit");
        } else if (text == "Edit") {
            menu.add(editor, "Copy", 'C', "copyRegion");
            menu.add(editor, "Copy Append", 'D', "copyAppend");
        } else if (text == "Search") {
            populateSearchMenu(editor, menu);
        } else if (text == "Go") {
            menu.add(editor, "Go Back", 'B', "dirBack");
            menu.add(editor, "Go Forward", 'F', "dirForward");
            menu.add(editor, "Go To Parent Directory", 'P', "dirUpDir");
        } else
            super.populateMenu(editor, menu);
    }

    public void populateModeMenu(Editor editor, Menu menu)
    {
        Buffer buffer = editor.getBuffer();
        Directory dir = buffer instanceof Directory ? (Directory) buffer : null;
        menu.add(editor, "Copy File...", 'C', "dirCopyFile");
        menu.add(editor, "Move File...", 'M', "dirMoveFile");
        menu.add(editor, "Delete File", 'D', "dirDeleteFiles");
        menu.addSeparator();
        menu.add(editor, "Limit...", 'L', "dirLimit");
        MenuItem unlimit = menu.add(editor, "Unlimit", 'U', "dirUnlimit");
        boolean enabled = (dir != null && dir.getLimitPattern() != null);
        unlimit.setEnabled(enabled);
        menu.addSeparator();
        JRadioButtonMenuItem sortByName = new JRadioButtonMenuItem("Sort by Name");
        sortByName.setMnemonic('N');
        sortByName.setActionCommand("DirectoryMode.dirSortByName");
        sortByName.addActionListener(editor.getDispatcher());
        JRadioButtonMenuItem sortByDate = new JRadioButtonMenuItem("Sort by Date");
        sortByDate.setMnemonic('T');
        sortByDate.setActionCommand("DirectoryMode.dirSortByDate");
        sortByDate.addActionListener(editor.getDispatcher());
        JRadioButtonMenuItem sortBySize = new JRadioButtonMenuItem("Sort by Size");
        sortBySize.setMnemonic('S');
        sortBySize.setActionCommand("DirectoryMode.dirSortBySize");
        sortBySize.addActionListener(editor.getDispatcher());
        ButtonGroup group = new ButtonGroup();
        group.add(sortByName);
        group.add(sortByDate);
        group.add(sortBySize);
        menu.add(sortByName);
        menu.add(sortByDate);
        menu.add(sortBySize);
        if (dir != null) {
            int sortBy = dir.getSortBy();
            if (sortBy == Directory.SORT_BY_NAME)
                sortByName.setSelected(true);
            else if (sortBy == Directory.SORT_BY_DATE)
                sortByDate.setSelected(true);
            else if (sortBy == Directory.SORT_BY_SIZE)
                sortBySize.setSelected(true);
        }
        menu.addSeparator();
        menu.add(editor, "Rescan Directory", 'R', "dirRescan");
    }

    public static void dirSortByName()
    {
        resort(Directory.SORT_BY_NAME);
    }

    public static void dirSortByDate()
    {
        resort(Directory.SORT_BY_DATE);
    }

    public static void dirSortBySize()
    {
        resort(Directory.SORT_BY_SIZE);
    }

    private static void resort(int sortBy)
    {
        final Editor editor = Editor.currentEditor();
        final Buffer buffer = editor.getBuffer();
        if (buffer instanceof Directory) {
            editor.setWaitCursor();
            ((Directory)buffer).resort(sortBy);
            editor.setDefaultCursor();
        }
    }

    protected ToolBar getDefaultToolBar(Frame frame)
    {
        return new DirectoryModeToolBar(frame);
    }
}
