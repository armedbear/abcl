/*
 * OpenFileDialog.java
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

import java.awt.BorderLayout;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import javax.swing.BoxLayout;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.border.EmptyBorder;

public class OpenFileDialog extends JDialog implements FocusListener
{
    private final Editor editor;

    private Object result;
    private String title;
    private HistoryTextField textField;
    private OpenFileTextFieldHandler handler;

    public OpenFileDialog(Editor editor)
    {
        this(editor, "Open File");
        title = "Open File";
    }

    private OpenFileDialog(Editor editor, String title)
    {
        super(editor.getFrame(), title, true);
        this.editor = editor;
        this.title = title;
        JPanel panel = new JPanel();
        panel.setLayout(new BoxLayout(panel, BoxLayout.Y_AXIS));
        panel.setBorder(new EmptyBorder(5, 5, 5, 5));
        panel.add(new JLabel("File:"));
        textField = new HistoryTextField(20);
        textField.setHistory(new History("openFile.file"));
        textField.setOwner(this);
        textField.setHandler(handler = new OpenFileTextFieldHandler(editor, textField));
        handler.setTitle(title);
        panel.add(textField);
        getContentPane().add(panel, BorderLayout.CENTER);
        pack();
        textField.setFocusTraversalKeysEnabled(false);
        textField.requestFocus();
        addFocusListener(this);
    }

    public final OpenFileTextFieldHandler getHandler()
    {
        return handler;
    }

    public final Object getResult()
    {
        return result;
    }

    public final void setResult(Object result)
    {
        this.result = result;
    }

    public static File getLocalFile(Editor editor, String title)
    {
        OpenFileDialog dialog = new OpenFileDialog(editor, title);
        OpenFileTextFieldHandler handler = dialog.getHandler();
        handler.setAllowRemote(false);
        handler.setFileMustExist(true);
        handler.setCheckBuffers(false);
        handler.setCheckSourcePath(false);
        editor.centerDialog(dialog);
        dialog.show();
        editor.repaintNow();
        if (dialog.result instanceof File)
            return (File) dialog.result;
        return null;
    }

    public void ok()
    {
        dispose();
        editor.setFocusToDisplay();
    }

    public void cancel()
    {
        dispose();
        editor.setFocusToDisplay();
    }

    public void focusGained(FocusEvent e)
    {
        textField.requestFocus();
    }

    public void focusLost(FocusEvent e)
    {
    }

    public void dispose()
    {
        super.dispose();
        editor.restoreFocus();
    }

    public static void openFileInOtherFrame()
    {
        final Editor editor = Editor.currentEditor();
        OpenFileDialog dialog = new OpenFileDialog(editor);
        editor.centerDialog(dialog);
        dialog.show();
        editor.repaintNow();
        Buffer buf = null;
        Object obj = dialog.getResult();
        if (obj instanceof Buffer)
            buf = (Buffer) obj;
        else if (obj instanceof File)
            buf = editor.openFile((File)obj);
        if (buf != null) {
            editor.makeNext(buf);
            editor.activateInOtherFrame(buf);
        }
    }
}
