/*
 * ListTagsBuffer.java
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

import java.util.Iterator;
import java.util.List;

public final class ListTagsBuffer extends Buffer
{
    private final String name; // The tag name.
    private Marker marker;

    private String lastFileName;
    private String lastClassName;

    public ListTagsBuffer(Editor editor, String command, String name, List tags)
    {
        super();
        this.name = name;
        if (editor.getBuffer().getFile() != null)
            marker = new Marker(editor.getBuffer(), editor.getDot());
        initializeUndo();
        type = TYPE_LIST_OCCURRENCES;
        mode = ListTagsMode.getMode();
        formatter = mode.getFormatter(this);
        readOnly = true;
        setTransient(true);
        load(tags);
        FastStringBuffer sb = new FastStringBuffer(command);
        sb.append(" \"");
        sb.append(name);
        sb.append('"');
        title = sb.toString();
        setInitialized(true);
    }

    public Position getInitialDotPos()
    {
        for (Line line = getFirstLine(); line != null; line = line.next()) {
            if (line instanceof TagLine)
                return new Position(line, 0);
        }
        return new Position(getFirstLine(), 0);
    }

    private void load(List tags)
    {
        try {
            lockWrite();
        }
        catch (InterruptedException e) {
            Log.debug(e);
            return;
        }
        try {
            lastFileName = lastClassName = null;
            appendLine("Tag: \"" + name + '"');
            Iterator iter = tags.iterator();
            while (iter.hasNext()) {
                Tag tag = (Tag) iter.next();
                appendTag(tag);
            }
            renumber();
            setLoaded(true);
        }
        finally {
            unlockWrite();
        }
    }

    private void appendTag(Tag tag)
    {
        // In the current implementation, the tags for a given file will be
        // grouped together in the tag file.
        if (tag instanceof GlobalTag) {
            String fileName = ((GlobalTag)tag).getFileName();
            if (fileName != null && !fileName.equals(lastFileName)) {
                appendLine(new FileLine(fileName));
                lastFileName = fileName;
                lastClassName = null;
            }
        }
        String className = tag.getClassName();
        if (className != null && !className.equals(lastClassName)) {
            appendLine("Class: ".concat(className));
            lastClassName = className;
        }
        appendLine(new TagLine(tag));
    }

    public void jumpToTag(Editor editor, boolean killList)
    {
        if (editor.getDot() == null)
            return;
        final Line dotLine = editor.getDotLine();
        if (!(dotLine instanceof TagLine))
            return;
        final Tag tag = ((TagLine)dotLine).getTag();
        if (tag == null) {
            Debug.bug();
            return;
        }
        Editor ed = editor.getOtherEditor();
        if (ed == null)
            ed = editor;
        if (marker != null)
            ed.pushMarker(marker);
        tag.gotoTag(ed);
        if (killList) {
            Editor otherEditor = ed.getOtherEditor();
            if (otherEditor != null)
                ed.getFrame().closeEditor(otherEditor);
            kill();
        }
    }
}
