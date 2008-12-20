/*
 * ListTagsDialog.java
 *
 * Copyright (C) 1998-2002 Peter Graves
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

import java.awt.Dimension;
import java.awt.event.InputEvent;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.util.ArrayList;
import java.util.List;
import javax.swing.JList;
import javax.swing.JScrollPane;

public final class ListTagsDialog extends AbstractDialog implements MouseListener
{
    private Editor editor;
    private List tags;
    private JList list;
    private Tag tag;

    public ListTagsDialog(String title, List tags)
    {
        super(Editor.currentEditor(), title, true);
        editor = Editor.currentEditor();
        this.tags = tags;
        init();
    }

    public ListTagsDialog(Editor editor, String title, List tags)
    {
        super(editor, title, true);
        this.editor = editor;
        this.tags = tags;
        init();
    }

    private void init()
    {
        int index = 0;
        if (tags == null) {
            Buffer buffer = editor.getBuffer();
            tags = buffer.getTags();
            if (tags == null) {
                if (buffer.getMode() != null) {
                    Tagger tagger = buffer.getMode().getTagger(buffer);
                    if (tagger != null) {
                        editor.setWaitCursor();
                        tagger.run();
                        editor.setDefaultCursor();
                        tags = buffer.getTags();
                    }
                }
            }
            if (tags == null)
                tags = new ArrayList();
            // We want to set the selection to the next tag after the cursor
            // position in the current editor.
            for (int i = 0; i < tags.size(); i++) {
                LocalTag t = (LocalTag) tags.get(i);
                if (t.lineNumber() > editor.getDotLineNumber())
                    break;
                index = i;
            }
        }
        final int size = tags.size();
        String[] array = new String[size];
        for (int i = size-1; i >= 0; i--) {
            Tag t = (Tag) tags.get(i);
            array[i] = t.getLongName();
        }
        list = new JList(array);
        int h = Editor.preferences().getIntegerProperty(Property.JLIST_FIXED_CELL_HEIGHT);
        if (h > 0)
            list.setFixedCellHeight(h);
        int numVisibleRows = Math.max(Math.min(size, 13), 5);
        list.setVisibleRowCount(numVisibleRows);
        list.addKeyListener(this);
        list.addMouseListener(this);
        JScrollPane scroller = new JScrollPane(list);
        mainPanel.add(scroller);
        addVerticalStrut();
        addOKCancel();
        pack();
        // The dialog should be no wider than the current frame..
        int maxWidth = editor.getSize().width - 12;
        if (getSize().width > maxWidth)
            setSize(new Dimension(maxWidth, getSize().height));
        list.setSelectedIndex(index);
        list.ensureIndexIsVisible(index);
        // If the selected item is the last row in the list box, try to get it
        // to be centered.
        if (list.getLastVisibleIndex() == index) {
            index += numVisibleRows / 2;
            if (index > size - 1)
                index = size - 1;
            list.ensureIndexIsVisible(index);
        }
        list.requestFocus();
    }

    public final Tag getTag()
    {
        return tag;
    }

    protected void ok()
    {
        dispose();
        int index = list.getSelectedIndex();
        if (tags != null && index >= 0 && tags.size() > index)
            tag = (Tag) tags.get(index);
    }

    public void mouseClicked(MouseEvent e)
    {
        if (e.getClickCount() == 2)
            ok();
    }

    public void mousePressed(MouseEvent e)
    {
        if (e.getModifiers() == InputEvent.BUTTON2_MASK) {
            int index = list.locationToIndex(e.getPoint());
            list.setSelectedIndex(index);
            ok();
        }
    }

    public void mouseReleased(MouseEvent e) {}

    public void mouseEntered(MouseEvent e) {}

    public void mouseExited(MouseEvent e) {}

    public static void listTags()
    {
        final Editor editor = Editor.currentEditor();
        final Buffer buffer = editor.getBuffer();
        FastStringBuffer sb = new FastStringBuffer("List Tags");
        if (buffer.getFile() != null) {
            sb.append("   ");
            sb.append(buffer.getFile().getName());
        }
        ListTagsDialog d = new ListTagsDialog(sb.toString(), null);
        editor.centerDialog(d);
        d.show();
        Tag tag = d.getTag();
        if (tag instanceof LocalTag)
            TagCommands.gotoLocalTag(editor, (LocalTag)tag, false);
    }
}
