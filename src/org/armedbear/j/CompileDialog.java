/*
 * CompileDialog.java
 *
 * Copyright (C) 1998-2005 Peter Graves
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
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import javax.swing.BoxLayout;
import javax.swing.JDialog;
import javax.swing.JPanel;
import javax.swing.border.EmptyBorder;

public final class CompileDialog extends JDialog implements KeyListener
{
    private final Editor editor;
    private final HistoryTextField textField;
    private final History compileHistory;
    private String command;

    public CompileDialog(Editor editor)
    {
        super(editor.getFrame(), "Compile", true);
        this.editor = editor;
        JPanel panel = new JPanel();
        panel.setLayout(new BoxLayout(panel, BoxLayout.Y_AXIS));
        panel.setBorder(new EmptyBorder(5, 5, 5, 5));
        Label label = new Label("Compile command:");
        panel.add(label);
        textField = new HistoryTextField(20);
        compileHistory = new History("compile.command");
        textField.setHistory(compileHistory);
        textField.recallLast();
        panel.add(textField);
        getContentPane().add(panel, BorderLayout.CENTER);
        pack();
        textField.requestFocus();
        textField.addKeyListener(this);
    }

    public final String getCommand()
    {
        return command;
    }

    public void keyPressed(KeyEvent e)
    {
        final int keyCode = e.getKeyCode();
        switch (keyCode) {
            case KeyEvent.VK_ENTER: {
                command = textField.getText();
                compileHistory.append(command);
                compileHistory.save();
                dispose();
                return;
            }
            case KeyEvent.VK_ESCAPE:
                command = null;
                dispose();
                return;
            default:
                return;
        }
    }

    public void keyReleased(KeyEvent e) {}

    public void keyTyped(KeyEvent e) {}

    public void dispose()
    {
        super.dispose();
        editor.restoreFocus();
    }
}
