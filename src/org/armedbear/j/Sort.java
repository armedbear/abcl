/*
 * Sort.java
 *
 * Copyright (C) 2002-2004 Peter Graves
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

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import javax.swing.undo.CompoundEdit;

public final class Sort
{
    public static void sortLines()
    {
        final Editor editor = Editor.currentEditor();
        if (editor.getMark() == null)
            return;
        final Region region = new Region(editor);
        if (region.getEndLineNumber() - region.getBeginLineNumber() < 2)
            return;
        if (!editor.checkReadOnly())
            return;
        final Buffer buffer = editor.getBuffer();
        try {
            buffer.lockWrite();
        }
        catch (InterruptedException e) {
            Log.error(e);
            return;
        }
        try {
            sortLinesInternal(editor, buffer, region);
        }
        finally {
            buffer.unlockWrite();
        }
    }

    private static void sortLinesInternal(Editor editor, Buffer buffer, Region region)
    {
        ArrayList arrayList = new ArrayList();
        for (Line line = region.getBeginLine(); line != region.getEndLine(); line = line.next())
            arrayList.add(line.getText());
        Collections.sort(arrayList, new SortLinesComparator());
        CompoundEdit compoundEdit = null;
        int i = 0;
        for (Line line = region.getBeginLine(); line != region.getEndLine(); line = line.next(), i++) {
            String newText = (String) arrayList.get(i);
            if (!newText.equals(line.getText())) {
                if (compoundEdit == null) {
                    compoundEdit = new CompoundEdit();
                    compoundEdit.addEdit(new UndoMove(editor));
                }
                compoundEdit.addEdit(new UndoLineEdit(buffer, line));
                line.setText(newText);
            }
        }
        if (compoundEdit != null) {
            compoundEdit.end();
            buffer.addEdit(compoundEdit);
            buffer.modified();
        }
        buffer.setNeedsParsing(true);
        buffer.getFormatter().parseBuffer();
        buffer.repaint();
    }

    private static class SortLinesComparator implements Comparator
    {
        SortLinesComparator() {}

        public final int compare(Object o1, Object o2)
        {
            return ((String)o1).compareTo((String)o2);
        }
    }
}
