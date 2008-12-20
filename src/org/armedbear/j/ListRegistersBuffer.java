/*
 * ListRegistersBuffer.java
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

import java.util.Arrays;

public final class ListRegistersBuffer extends Buffer
{
    public ListRegistersBuffer()
    {
        supportsUndo  = false;
        type = TYPE_OUTPUT;
        mode = ListRegistersMode.getMode();
        formatter = mode.getFormatter(this);
        lineSeparator = System.getProperty("line.separator");
        readOnly = true;
        setTransient(true);
        setProperty(Property.VERTICAL_RULE, 0);
        setProperty(Property.SHOW_LINE_NUMBERS, false);
        setProperty(Property.HIGHLIGHT_MATCHING_BRACKET, false);
        setProperty(Property.HIGHLIGHT_BRACKETS, false);
        setInitialized(true);
    }

    public int load()
    {
        if (!isLoaded()) {
            loadInternal();
            if (!isLoaded())
                Debug.bug();
        }
        return LOAD_COMPLETED;
    }

    public void reload()
    {
        for (EditorIterator it = new EditorIterator(); it.hasNext();) {
            Editor ed = it.nextEditor();
            if (ed.getBuffer() == this) {
                ed.setWaitCursor();
                ed.saveView();
            }
        }
        empty();
        loadInternal();
        for (EditorIterator it = new EditorIterator(); it.hasNext();) {
            Editor ed = it.nextEditor();
            if (ed.getBuffer() == this) {
                View view = ed.getView(this);
                if (view != null) {
                    Line dotLine = getLine(view.getDotLineNumber());
                    if (dotLine == null)
                        dotLine = getFirstLine();
                    if (dotLine != null) {
                        ed.setDot(dotLine, 0);
                        ed.moveCaretToDotCol();
                    }
                    Line topLine = getLine(view.getTopLineNumber());
                    if (topLine == null)
                        topLine = dotLine;
                    ed.setTopLine(topLine);
                } else {
                    ed.setDot(getFirstLine(), 0);
                    ed.moveCaretToDotCol();
                    ed.setTopLine(getFirstLine());
                }
                ed.setUpdateFlag(REPAINT);
                ed.setDefaultCursor();
                ed.updateDisplay();
            }
        }
    }

    private void loadInternal()
    {
        String[] names = null;
        File directory = Directories.getRegistersDirectory();
        if (directory != null)
            names = directory.list();
        try {
            lockWrite();
        }
        catch (InterruptedException e) {
            Log.debug(e);
            return;
        }
        try {
            if (names != null && names.length > 0) {
                Arrays.sort(names);
                final int MAX_LINES = 100;
                for (int i = 0; i < names.length; i++) {
                    final String name = names[i];
                    FastStringBuffer sb = new FastStringBuffer();
                    sb.append("Register ");
                    sb.append(name);
                    String text = Registers.getText(name, MAX_LINES);
                    if (text == null || text.length() == 0)
                        continue;
                    int lineCount = Utilities.countLines(text);
                    appendLine(
                        new ListRegistersLine(sb.toString(), name));
                    append(text);
                    if (lineCount == MAX_LINES)
                        appendLine(new ListRegistersLine("[...]", name));
                }
            } else
                appendLine(new ListRegistersLine("No registers in use", null));
            renumber();
            setLoaded(true);
        }
        finally {
            unlockWrite();
        }
    }

    public String getFileNameForDisplay()
    {
        return "listRegisters";
    }

    public String toString()
    {
        return "listRegisters";
    }
}
