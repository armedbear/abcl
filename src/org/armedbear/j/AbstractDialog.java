/*
 * AbstractDialog.java
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

import java.awt.Dimension;
import java.awt.Point;
import java.awt.SystemColor;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.event.WindowEvent;
import java.awt.event.WindowListener;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.border.EmptyBorder;

public abstract class AbstractDialog extends JDialog implements ActionListener,
    KeyListener, WindowListener
{
    protected boolean cancelled;
    protected JPanel mainPanel;
    protected StandardButton okButton;
    protected StandardButton cancelButton;

    private Frame owner;

    protected AbstractDialog(Editor editor)
    {
        super(editor == null ? null : editor.getFrame());
        this.owner = editor == null ? null : editor.getFrame();
        initialize();
    }

    protected AbstractDialog(Editor editor, String title, boolean modal)
    {
        super(editor == null ? null : editor.getFrame(), title, modal);
        this.owner = editor == null ? null : editor.getFrame();
        initialize();
    }

    protected AbstractDialog(Frame owner)
    {
        super(owner);
        this.owner = owner;
        initialize();
    }

    protected AbstractDialog(Frame owner, String title, boolean modal)
    {
        super(owner, title, modal);
        this.owner = owner;
        initialize();
    }

    private void initialize()
    {
        setDefaultCloseOperation(DO_NOTHING_ON_CLOSE);
        addWindowListener(this);
        getContentPane().setLayout(new BoxLayout(getContentPane(), BoxLayout.Y_AXIS));
        setBackground(SystemColor.control);
        mainPanel = new JPanel();
        mainPanel.setLayout(new BoxLayout(mainPanel, BoxLayout.Y_AXIS));
        mainPanel.setBorder(new EmptyBorder(6, 6, 6, 6));
        getContentPane().add(mainPanel);
        addKeyListener(this);
    }

    public final boolean cancelled()
    {
        return cancelled;
    }

    // Add label and text field below it with right amount of space between them.
    protected void addLabelAndTextField(JLabel label, JTextField textField)
    {
        label.setLabelFor(textField);
        if (label.getBorder() == null)
            label.setBorder(new EmptyBorder(0, 0, 3, 0));
        mainPanel.add(label);
        mainPanel.add(textField);
        textField.addKeyListener(this);
    }

    protected void addLabelAndComponent(JLabel label, JComponent component)
    {
        if (label.getLabelFor() == null)
            label.setLabelFor(component);
        if (label.getBorder() == null)
            label.setBorder(new EmptyBorder(0, 0, 3, 0));
        mainPanel.add(label);
        mainPanel.add(component);
        component.addKeyListener(this);
    }

    protected final void addCheckBox(CheckBox checkBox)
    {
        mainPanel.add(checkBox);
        checkBox.addKeyListener(this);
    }

    protected final void addVerticalStrut()
    {
        mainPanel.add(Box.createVerticalStrut(6));
    }

    protected void addOK()
    {
        JPanel buttonPanel = new JPanel();
        buttonPanel.setAlignmentX(LEFT_ALIGNMENT);
        mainPanel.add(buttonPanel);
        okButton = new StandardButton("OK");
        okButton.setActionCommand("OK");
        okButton.addActionListener(this);
        okButton.addKeyListener(this);
        buttonPanel.add(okButton);
    }

    protected void addCancel()
    {
        JPanel buttonPanel = new JPanel();
        buttonPanel.setAlignmentX(LEFT_ALIGNMENT);
        mainPanel.add(buttonPanel);
        cancelButton = new StandardButton("Cancel");
        cancelButton.setActionCommand("Cancel");
        cancelButton.addActionListener(this);
        cancelButton.addKeyListener(this);
        buttonPanel.add(cancelButton);
    }

    protected void addOKCancel()
    {
        JPanel buttonPanel = new JPanel();
        buttonPanel.setAlignmentX(LEFT_ALIGNMENT);
        mainPanel.add(buttonPanel);
        okButton = new StandardButton("OK");
        okButton.setActionCommand("OK");
        okButton.addActionListener(this);
        okButton.addKeyListener(this);
        buttonPanel.add(okButton);
        cancelButton = new StandardButton("Cancel");
        cancelButton.setActionCommand("Cancel");
        cancelButton.addActionListener(this);
        cancelButton.addKeyListener(this);
        buttonPanel.add(cancelButton);
    }

    protected void ok()
    {
        dispose();
    }

    protected void cancel()
    {
        cancelled = true;
        dispose();
    }

    protected void enter()
    {
        ok();
    }

    protected void escape()
    {
        cancel();
    }

    public void dispose()
    {
        super.dispose();
        Editor.restoreFocus();
    }

    public void actionPerformed(ActionEvent e)
    {
        if (e.getActionCommand().equals("Cancel"))
            cancel();
        else if (e.getActionCommand().equals("OK"))
            ok();
    }

    public void keyPressed(KeyEvent e)
    {
        if (e.getModifiers() == 0) {
            // Special case for combo box.
            if (e.getComponent() instanceof JComboBox) {
                JComboBox cb = (JComboBox) e.getComponent();
                if (cb.isPopupVisible())
                    return;
                // Combo box popup is not visible. Fall through...
            }
            switch (e.getKeyCode()){
                case KeyEvent.VK_ENTER:
                    e.consume();
                    enter();
                    break;
                case KeyEvent.VK_ESCAPE:
                    // Ignore escape if key event has been consumed (textfield
                    // may be cancelling an expansion).
                    if (!e.isConsumed()) {
                        e.consume();
                        escape();
                    }
                    break;
            }
        }
    }

    public void keyReleased(KeyEvent e) {}

    public void keyTyped(KeyEvent e) {}

    public void windowActivated(WindowEvent e) {}

    public void windowDeactivated(WindowEvent e) {}

    public void windowOpened(WindowEvent e) {}

    public void windowClosing(WindowEvent e)
    {
        cancelled = true;
        dispose();
    }

    public void windowClosed(WindowEvent e) {}

    public void windowIconified(WindowEvent e) {}

    public void windowDeiconified(WindowEvent e) {}

    protected void centerDialog()
    {
        Dimension window = getSize();
        Point p;
        if (owner != null) {
            p = owner.getLocation();
            Dimension parent = owner.getSize();
            p.translate((parent.width - window.width) / 2,
                (parent.height - window.height) / 2);
        } else {
            Dimension parent = Toolkit.getDefaultToolkit().getScreenSize();
            p = new Point((parent.width - window.width) / 2,
                (parent.height - window.height) / 2);
        }
        setLocation(p);
    }
}
