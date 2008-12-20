/*
 * ConfirmDialog.java
 *
 * Copyright (C) 2000-2004 Peter Graves
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

import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;
import javax.swing.JPanel;

public class ConfirmDialog extends MessageDialog implements Constants
{
    private StandardButton yesButton;
    private StandardButton noButton;
    private StandardButton yesToAllButton;
    private StandardButton cancelButton;

    private int result = RESPONSE_NO;

    private boolean confirmAll;
    protected boolean cancel;

    protected ConfirmDialog(Editor editor)
    {
        super(editor != null ? editor : Editor.currentEditor());
    }

    public static int showConfirmDialog(String text, String title)
    {
        return showConfirmDialog(Editor.currentEditor(), text, title);
    }

    public static int showConfirmDialog(Editor editor, String text,
                                        String title)
    {
        ConfirmDialog d = new ConfirmDialog(editor);
        d.initialize(text, title);
        if (editor != null)
            editor.setDefaultCursor();
        d.show();
        if (editor != null && Editor.getEditorList().contains(editor))
            editor.setFocusToDisplay();
        return d.result;
    }

    public static int showConfirmAllDialog(Editor editor, String text,
                                           String title)
    {
        ConfirmDialog d = new ConfirmDialog(editor);
        d.confirmAll = true;
        d.initialize(text, title);
        if (editor != null)
            editor.setDefaultCursor();
        d.show();
        if (editor != null && Editor.getEditorList().contains(editor))
            editor.setFocusToDisplay();
        return d.result;
    }

    public static int showConfirmDialogWithCancelButton(Editor editor,
                                                        String text,
                                                        String title)
    {
        ConfirmDialog d = new ConfirmDialog(editor);
        d.cancel = true;
        d.initialize(text, title);
        if (editor != null)
            editor.setDefaultCursor();
        d.show();
        if (editor != null && Editor.getEditorList().contains(editor))
            editor.setFocusToDisplay();
        return d.result;
    }

    protected void initialize(String text, String title)
    {
        super.initialize(text, title);
        yesButton.requestFocus();
    }

    protected void addButtons()
    {
        JPanel buttonPanel = new JPanel();
        buttonPanel.setAlignmentX(LEFT_ALIGNMENT);
        mainPanel.add(buttonPanel);
        yesButton = new StandardButton("Yes");
        yesButton.setActionCommand("Yes");
        yesButton.setMnemonic('Y');
        yesButton.addActionListener(this);
        yesButton.addKeyListener(this);
        buttonPanel.add(yesButton);
        noButton = new StandardButton("No");
        noButton.setActionCommand("No");
        noButton.setMnemonic('N');
        noButton.addActionListener(this);
        noButton.addKeyListener(this);
        buttonPanel.add(noButton);
        if (confirmAll) {
            yesToAllButton = new StandardButton("Yes To All");
            yesToAllButton.setActionCommand("Yes To All");
            yesToAllButton.setMnemonic('A');
            yesToAllButton.addActionListener(this);
            yesToAllButton.addKeyListener(this);
            buttonPanel.add(yesToAllButton);
        }
        if (confirmAll || cancel) {
            cancelButton = new StandardButton("Cancel");
            cancelButton.setActionCommand("Cancel");
            cancelButton.setMnemonic('C');
            cancelButton.addActionListener(this);
            cancelButton.addKeyListener(this);
            buttonPanel.add(cancelButton);
        }
    }

    protected void yes()
    {
        result = RESPONSE_YES;
        dispose();
    }

    protected void no()
    {
        result = RESPONSE_NO;
        dispose();
    }

    protected void yesToAll()
    {
        result = RESPONSE_YES_TO_ALL;
        dispose();
    }

    protected void cancel()
    {
        result = RESPONSE_CANCEL;
        dispose();
    }

    public void actionPerformed(ActionEvent e)
    {
        if (e.getActionCommand().equals("Yes"))
            yes();
        else if (e.getActionCommand().equals("No"))
            no();
        else if (e.getActionCommand().equals("Yes To All"))
            yesToAll();
        else if (e.getActionCommand().equals("Cancel"))
            cancel();
    }

    public void keyPressed(KeyEvent e)
    {
        int keyCode = e.getKeyCode();
        if (confirmAll) {
            switch (keyCode) {
                case KeyEvent.VK_Y:
                    yes();
                    break;
                case KeyEvent.VK_N:
                    no();
                    break;
                case KeyEvent.VK_A:
                    yesToAll();
                    break;
                case KeyEvent.VK_C:
                    cancel();
                    break;
            }
        } else if (cancel) {
            switch (keyCode) {
                case KeyEvent.VK_Y:
                case KeyEvent.VK_ENTER:
                    yes();
                    break;
                case KeyEvent.VK_N:
                    no();
                    break;
                case KeyEvent.VK_ESCAPE:
                    cancel();
                    break;
            }
        } else {
            switch (keyCode) {
                case KeyEvent.VK_Y:
                case KeyEvent.VK_ENTER:
                    yes();
                    break;
                case KeyEvent.VK_N:
                case KeyEvent.VK_ESCAPE:
                    no();
                    break;
            }
        }
    }
}
