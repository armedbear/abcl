/*
 * JdbControlDialog.java
 *
 * Copyright (C) 2002-2003 Peter Graves
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

package org.armedbear.j.jdb;

import java.awt.Dimension;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ComponentEvent;
import java.awt.event.ComponentListener;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.net.URL;
import javax.swing.BorderFactory;
import javax.swing.BoxLayout;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTabbedPane;
import javax.swing.JToolBar;
import org.armedbear.j.Constants;
import org.armedbear.j.DefaultTextFieldHandler;
import org.armedbear.j.Editor;
import org.armedbear.j.EditorIterator;
import org.armedbear.j.Expansion;
import org.armedbear.j.History;
import org.armedbear.j.HistoryTextField;
import org.armedbear.j.SessionProperties;
import org.armedbear.j.StandardButton;

public final class JdbControlDialog extends JDialog implements JdbConstants,
    Constants, ContextListener, ActionListener, ComponentListener, KeyListener
{
    private static final String commandKey = "jdb.command";

    private static final SessionProperties sessionProperties =
        Editor.getSessionProperties();

    private final Jdb jdb;
    private final JToolBar toolBar;
    private final HistoryTextField commandTextField;
    private final History commandHistory;
    private final StandardButton suspendButton;
    private final StandardButton continueButton;

    public JdbControlDialog(Jdb jdb)
    {
        super(Editor.getCurrentFrame(), "Jdb", false);
        this.jdb = jdb;
        toolBar = new JToolBar();
        toolBar.setOrientation(JToolBar.HORIZONTAL);
        toolBar.setFloatable(false);
        toolBar.putClientProperty("JToolBar.isRollover", Boolean.FALSE);
        addButton("Next", null, "jdbNext");
        addButton("Step", null, "jdbStep");
        addButton("Step Out", null, "jdbFinish");
        addSeparator();
        suspendButton = addButton("Suspend", null, "jdbSuspend");
        continueButton = addButton("Continue", null, "jdbContinue");
        addSeparator();
        addButton("Restart", null, "jdbRestart");
        addButton("Quit", null, "jdbQuit");
        getContentPane().add(toolBar, "North");
        JTabbedPane tabbedPane = new JTabbedPane();
        StackPanel stackPanel = new StackPanel(jdb, this);
        tabbedPane.addTab("Stack", stackPanel.getComponent());
        ThreadPanel threadPanel = new ThreadPanel(jdb, this);
        tabbedPane.addTab("Threads", threadPanel.getComponent());
        BreakpointPanel breakpointPanel = new BreakpointPanel(jdb, this);
        tabbedPane.addTab("Breakpoints", breakpointPanel.getComponent());
        getContentPane().add(tabbedPane, "Center");
        JPanel commandPanel = new JPanel();
        commandPanel.setLayout(new BoxLayout(commandPanel, BoxLayout.X_AXIS));
        commandPanel.setBorder(BorderFactory.createEmptyBorder(3, 3, 4, 3));
        commandPanel.add(new JLabel("Command:"));
        commandTextField = new HistoryTextField(20);
        commandHistory = new History(commandKey);
        commandTextField.setHistory(commandHistory);
        commandTextField.setHandler(new CommandTextFieldHandler(commandTextField));
        commandPanel.add(commandTextField);
        getContentPane().add(commandPanel, "South");
        pack();
        jdb.addContextListener(this);
        addComponentListener(this);
        addWindowListener(new WindowMonitor());
        commandTextField.addKeyListener(this);
        if (jdb.getStartSuspended())
            tabbedPane.setSelectedComponent(breakpointPanel.getComponent());
        contextChanged();
        requestDefaultFocus();
    }

    public void requestDefaultFocus()
    {
        commandTextField.requestFocus();
    }

    private StandardButton addButton(String text, String iconFile, String command)
    {
        StandardButton button = new StandardButton(text);
        Font font = button.getFont();
        FontMetrics fm = Toolkit.getDefaultToolkit().getFontMetrics(font);
        int width = fm.stringWidth(text);
        Dimension dim = new Dimension(width + 14, StandardButton.DEFAULT_HEIGHT);
        button.setMinimumSize(dim);
        button.setMaximumSize(dim);
        button.setPreferredSize(dim);
        if (command != null)
            button.setActionCommand(command);
        button.addActionListener(this);
        button.setRequestFocusEnabled(false);
        toolBar.add(button);
        return button;
    }

    private void addSeparator()
    {
        toolBar.addSeparator();
    }

    public void show()
    {
        int width = sessionProperties.getIntegerProperty("jdb.width", 425);
        int height = sessionProperties.getIntegerProperty("jdb.height", 250);
        setSize(width, height);
        int x = sessionProperties.getIntegerProperty("jdb.x", -1);
        int y = sessionProperties.getIntegerProperty("jdb.y", -1);
        if (x >= 0 && y >= 0) {
            setLocation(x, y);
        } else {
            // First time.
            Editor editor = null;
            for (EditorIterator it = new EditorIterator(); it.hasNext();) {
                Editor ed = it.nextEditor();
                if (ed.getBuffer() == jdb) {
                    editor = ed;
                    break;
                }
            }
            if (editor != null) {
                Dimension parent = editor.getFrame().getSize();
                Dimension dialog = getSize();
                Point p = editor.getFrame().getLocation();
                p.translate(parent.width - dialog.width - 30,
                    parent.height - dialog.height - 50);
                setLocation(p);
            } else
                Editor.currentEditor().centerDialog(this);
        }
        super.show();
    }

    public void contextChanged()
    {
        if (jdb.getVM() == null) {
            suspendButton.setEnabled(false);
            continueButton.setEnabled(false);
        } else if (jdb.isSuspended()) {
            suspendButton.setEnabled(false);
            continueButton.setEnabled(true);
        } else {
            suspendButton.setEnabled(true);
            continueButton.setEnabled(false);
        }
    }

    public void actionPerformed(ActionEvent e)
    {
        Editor.currentEditor().getDispatcher().actionPerformed(e);
    }

    public void keyPressed(KeyEvent e)
    {
        if (e.getKeyCode() == KeyEvent.VK_ENTER) {
            // Mask off the bits we don't care about (Java 1.4).
            if ((e.getModifiers() & 0x0f) == 0) {
                if (commandTextField.getText().trim().length() > 0) {
                    jdb.doCommand(commandTextField.getText());
                    commandTextField.setText("");
                } else {
                    int command = jdb.getLastCommand();
                    if (command == JDB_CONTINUE || command == JDB_NEXT ||
                        command == JDB_STEP)
                        jdb.doCommand(command, null);
                }
            }
        }
    }

    public void keyReleased(KeyEvent e) {}

    public void keyTyped(KeyEvent e) {}

    public void componentResized(ComponentEvent e)
    {
        saveWindowPlacement();
    }

    public void componentMoved(ComponentEvent e)
    {
        saveWindowPlacement();
    }

    public void componentShown(ComponentEvent e)
    {
        saveWindowPlacement();
    }

    public void componentHidden(ComponentEvent e) {}

    private void saveWindowPlacement()
    {
        Rectangle r = getBounds();
        sessionProperties.setIntegerProperty("jdb.x", r.x);
        sessionProperties.setIntegerProperty("jdb.y", r.y);
        sessionProperties.setIntegerProperty("jdb.width", r.width);
        sessionProperties.setIntegerProperty("jdb.height", r.height);
    }

    private class WindowMonitor extends WindowAdapter
    {
        public void windowClosing(WindowEvent e)
        {
            setVisible(false);
            dispose();
            jdb.doCommand("quit");
        }
    }

    private class CommandTextFieldHandler extends DefaultTextFieldHandler
    {
        CommandTextFieldHandler(HistoryTextField textField)
        {
            super(textField);
        }

        public void enter()
        {
            commandHistory.append(textField.getText());
        }

        public void escape()
        {
            textField.setText("");
        }

        public Expansion getExpansion(String prefix)
        {
            Expansion expansion = new Expansion(jdb, prefix, prefix);
            EditorIterator iter = new EditorIterator();
            while (iter.hasNext()) {
                Editor ed = iter.nextEditor();
                if (ed.getModeId() == JAVA_MODE) {
                    Expansion exp =
                        new Expansion(ed.getBuffer(), prefix, prefix, ed.getDot());
                    expansion.appendCandidates(exp.getCandidates());
                }
            }
            return expansion;
        }
    }
}
