/*
 * RecentFilesDialog.java
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

package org.armedbear.j;

import java.awt.Dimension;
import java.awt.Point;
import java.awt.event.InputEvent;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.ListSelectionModel;
import javax.swing.table.JTableHeader;
import javax.swing.table.TableColumn;
import javax.swing.table.TableColumnModel;

public final class RecentFilesDialog extends AbstractDialog implements MouseListener
{
    private final JTable table;
    private final RecentFilesTableModel model;
    private final Editor editor;

    public RecentFilesDialog(Editor editor)
    {
        super(editor, "Recent Files", true);
        this.editor = editor;
        model = new RecentFilesTableModel();
        table = new JTable(model);
        table.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
        table.addRowSelectionInterval(0, 0);
        int width = editor.getSize().width - 50;
        int height = table.getPreferredScrollableViewportSize().height;
        table.setPreferredScrollableViewportSize(new Dimension(width, height));
        JScrollPane scrollPane = new JScrollPane(table);
        mainPanel.add(scrollPane);
        addVerticalStrut();
        addOKCancel();
        table.addKeyListener(this);
        table.addMouseListener(this);
        JTableHeader th = table.getTableHeader();
        th.addMouseListener(this);
        TableColumnModel columnModel = th.getColumnModel();
        int count = columnModel.getColumnCount();
        for (int i = 0; i < count; i++) {
            String key = getColumnWidthKey(i);
            int columnWidth = Editor.getSessionProperties().getIntegerProperty(key, 0);
            if (columnWidth == 0)
                break;
            TableColumn column = columnModel.getColumn(i);
            column.setPreferredWidth(columnWidth);
        }
        pack();
        table.requestFocus();
    }

    protected void ok()
    {
        openSelectedFile();
    }

    private void openSelectedFile()
    {
        int row = table.getSelectedRow();
        if (row >= 0)
            openFileAtRow(row);
    }

    private void openFileAtPoint(Point point)
    {
        int row = table.rowAtPoint(point);
        if (row >= 0)
            openFileAtRow(row);
    }

    private void openFileAtRow(int row)
    {
        dispose();
        editor.repaintNow();
        RecentFilesEntry entry = model.getEntryAtRow(row);
        File parent = File.getInstance(entry.location);
        File file = File.getInstance(parent, entry.name);
        Buffer buf = editor.getBuffer(file);
        if (buf == null)
            editor.status("File not found");
        else if (buf != editor.getBuffer()) {
            editor.makeNext(buf);
            editor.activate(buf);
            if (buf instanceof RemoteBuffer)
                ((RemoteBuffer) buf).setInitialDotPos(entry.lineNumber, entry.offs);
            else {
                Line line = buf.getLine(entry.lineNumber);
                if (line != null) {
                    int offs = entry.offs;
                    if (offs > line.length())
                        offs = line.length();
                    editor.moveDotTo(line, offs);
                } else
                    editor.moveDotTo(buf.getFirstLine(), 0);
                editor.updateDisplay();
            }
        }
    }

    public void dispose()
    {
        JTableHeader th = table.getTableHeader();
        TableColumnModel columnModel = th.getColumnModel();
        int count = columnModel.getColumnCount();
        for (int i = 0; i < count; i++) {
            TableColumn column = columnModel.getColumn(i);
            String key = getColumnWidthKey(i);
            Editor.getSessionProperties().setIntegerProperty(key, column.getWidth());
        }
        super.dispose();
    }

    private String getColumnWidthKey(int i)
    {
        return "RecentFilesDialog.columnWidth." + i;
    }

    public void mouseClicked(MouseEvent e)
    {
        if (e.getClickCount() == 2)
            openSelectedFile();
        else if (e.getModifiers() == InputEvent.BUTTON2_MASK)
            openFileAtPoint(e.getPoint());
        else if (e.getComponent() == table.getTableHeader()) {
            TableColumnModel columnModel = table.getColumnModel();
            int viewColumn = columnModel.getColumnIndexAtX(e.getX());
            int column = table.convertColumnIndexToModel(viewColumn);
            if (e.getClickCount() == 1 && column >= 0) {
                int row = table.getSelectedRow();
                RecentFilesEntry entry = null;
                if (row >= 0)
                    entry = model.getEntryAtRow(row);
                model.sortByColumn(column);
                if (entry != null) {
                    row = model.getRowForEntry(entry);
                    if (row >= 0)
                        table.addRowSelectionInterval(row, row);
                }
            }
        }
    }

    public void mousePressed(MouseEvent e) {}
    public void mouseReleased(MouseEvent e) {}
    public void mouseEntered(MouseEvent e) {}
    public void mouseExited(MouseEvent e) {}

    public static void recentFiles()
    {
        final Editor editor = Editor.currentEditor();
        RecentFilesDialog d = new RecentFilesDialog(editor);
        editor.centerDialog(d);
        d.show();
    }
}
