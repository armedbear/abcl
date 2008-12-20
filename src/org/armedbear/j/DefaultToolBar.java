/*
 * DefaultToolBar.java
 *
 * Copyright (C) 2000-2002 Peter Graves
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

public final class DefaultToolBar extends ToolBar
{
    public DefaultToolBar(Frame frame)
    {
        super(frame);
        addButton("New", ICON_NEW, "newBuffer");
        addButton("Open", ICON_OPEN, "openFile");
        addButton("Save", ICON_SAVE, "save");
        addButton("Close", ICON_CLOSE, "killBuffer");
        addSeparator();
        addButton("Undo", ICON_UNDO, "undo");
        addButton("Redo", ICON_REDO, "redo");
        addSeparator();
        addButton("Cut", ICON_CUT, "killRegion");
        addButton("Copy", ICON_COPY, "copyRegion");
        addButton("Paste", ICON_PASTE, "paste");
        addSeparator();
        addButton("Find", ICON_FIND, "find");
        addButton("Replace", ICON_REPLACE, "replace");
        addSeparator();
        addButton("Directory", ICON_DIRECTORY, "dir");
        addButton("Home", ICON_HOME, "dirHomeDir");
        maybeAddInboxButton();
        addSeparator();
        addButton("Exit", ICON_EXIT, "quit");
    }
}
