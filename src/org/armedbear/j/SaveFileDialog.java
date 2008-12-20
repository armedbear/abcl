/*
 * SaveFileDialog.java
 *
 * Copyright (C) 1998-2004 Peter Graves
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
import java.awt.event.InputEvent;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.util.Vector;
import javax.swing.BoxLayout;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.border.EmptyBorder;

public class SaveFileDialog extends JDialog implements FocusListener, KeyListener
{
    private File destination;

    protected final Editor editor;
    protected final String title;
    protected String defaultName;
    protected HistoryTextField textField;
    protected History history;

    private final String prompt;
    private Vector completions;
    private int index;
    private boolean completionsIgnoreCase;
    private boolean allowDirectory;
    private boolean confirmOverwrite = true;

    public SaveFileDialog(Editor editor, String title)
    {
        super(editor.getFrame(), title, true);
        this.editor = editor;
        this.title = title;
        prompt = "File:";
        init();
    }

    public SaveFileDialog(Editor editor, String title, String prompt)
    {
        super(editor.getFrame(), title, true);
        this.editor = editor;
        this.title = title;
        this.prompt = prompt;
        init();
    }

    public SaveFileDialog(Editor editor, String title, String prompt, String defaultName)
    {
        super(editor.getFrame(), title, true);
        this.editor = editor;
        this.title = title;
        this.prompt = prompt;
        this.defaultName = defaultName;
        init();
    }

    public final File getDestination()
    {
        return destination;
    }

    public final void setAllowDirectory(boolean b)
    {
        allowDirectory = b;
    }

    public final void setConfirmOverwrite(boolean b)
    {
        confirmOverwrite = b;
    }

    public final void setInitialText(String s)
    {
        textField.setText(s);
    }

    private void init()
    {
        if (Platform.isPlatformWindows())
            completionsIgnoreCase = true;
        else
            completionsIgnoreCase = Editor.preferences().getBooleanProperty(Property.FILENAME_COMPLETIONS_IGNORE_CASE);
        JPanel panel = new JPanel();
        panel.setLayout(new BoxLayout(panel, BoxLayout.Y_AXIS));
        panel.setBorder(new EmptyBorder(5, 5, 5, 5));
        panel.add(new Label(prompt));
        textField = new HistoryTextField(20);
        history = new History("saveFile.file");
        textField.setHistory(history);
        if (defaultName != null && defaultName.length() > 0) {
            File directory = editor.getCurrentDirectory();
            if (directory == null || directory.isRemote())
                directory = Directories.getUserHomeDirectory();
            File file = File.getInstance(directory, defaultName);
            if (file != null)
                textField.setText(file.canonicalPath());
        }
        panel.add(textField);
        getContentPane().add(panel, BorderLayout.CENTER);
        pack();
        textField.setFocusTraversalKeysEnabled(false);
        addFocusListener(this);
        addKeyListener(this);
        textField.addKeyListener(this);
        textField.requestFocus();
        textField.selectAll();
    }

    public void focusGained(FocusEvent e)
    {
        textField.requestFocus();
    }

    public void focusLost(FocusEvent e)
    {
    }

    public void keyPressed(KeyEvent e)
    {
        int keyCode   = e.getKeyCode();
        int modifiers = e.getModifiers();
        switch (keyCode) {
            case KeyEvent.VK_TAB: {
                String s = null;
                String entry = textField.getText();
                if (modifiers == InputEvent.SHIFT_MASK)
                    s = previousGuess();
                else {
                    File dir = editor.getCurrentDirectory();
                    if (dir != null && !dir.isRemote()) {
                        File file = File.getInstance(dir, entry);
                        if (file != null)
                            s = guess(file.canonicalPath());
                    }
                }
                e.consume();
                if (s != null) {
                    textField.setText(s);
                    textField.setCaretPosition(s.length());
                }
                return;
            }
            case KeyEvent.VK_ENTER:
                e.consume();
                enter();
                return;
            case KeyEvent.VK_ESCAPE:
                e.consume();
                destination = null;
                dispose();
                return;
            // Ignore modifiers.
            case KeyEvent.VK_SHIFT:
            case KeyEvent.VK_CONTROL:
            case KeyEvent.VK_ALT:
            case KeyEvent.VK_META:
                return;
            // Anything but tab, start over.
            default:
                completions = null;
                return;
        }
    }

    public void keyReleased(KeyEvent e)
    {
    }

    public void keyTyped(KeyEvent e)
    {
    }

    protected void enter()
    {
        final String entry = textField.getText().trim();
        if (entry.length() == 0) {
            destination = null;
            dispose();
            return;
        }
        File file;
        if (Utilities.isFilenameAbsolute(entry)) {
            file = File.getInstance(entry);
        } else {
            File directory = editor.getCurrentDirectory();
            if (directory == null || directory.isRemote())
                directory = Directories.getUserHomeDirectory();
            file = File.getInstance(directory, entry);
        }
        if (file == null) {
            dispose();
            return;
        }
        if (file.isRemote()) {
            destination = file;
        } else {
            if (file.isDirectory()) {
                if (defaultName != null)
                    file = File.getInstance(file, defaultName);
                if (file.isDirectory() && !allowDirectory) {
                    String message = file.canonicalPath() + " is a directory";
                    MessageDialog.showMessageDialog(editor, message, title);
                    requestFocus();
                    return;
                }
            }
            if (file.isFile()) {
                if (file.canWrite()) {
                    String message = "Overwrite existing file " + file.canonicalPath() + "?";
                    if (!confirmOverwrite || editor.confirm(title, message)) {
                        destination = file;
                        history.append(file.canonicalPath());
                        history.save();
                        dispose();
                    }
                    return;
                } else {
                    // File is read only.
                    String message = file.canonicalPath() + " is read only";
                    MessageDialog.showMessageDialog(editor, message, title);
                    return;
                }
            }
            // File (if specified) does not exist.
            // Make sure parent directory exists.
            File parentDir = file.isDirectory() ? file : file.getParentFile();
            if (parentDir == null || !parentDir.isDirectory()) {
                String message = "Invalid path";
                MessageDialog.showMessageDialog(editor, message, title);
                requestFocus();
                return;
            }
            // Make sure parent directory is writable.
            if (!Utilities.isDirectoryWritable(parentDir)) {
                String message = "Directory " + parentDir.canonicalPath() + " is not writable";
                MessageDialog.showMessageDialog(editor, message, title);
                requestFocus();
                return;
            }
            destination = file;
        }
        history.append(destination.netPath());
        history.save();
        dispose();
    }

    private String guess(String prefix)
    {
        if (completions != null) {
            if (index < completions.size())
                return (String) completions.get(index++);
            index = 0;
            if (index < completions.size())
                return (String) completions.get(index++);
            return null;
        }
        completions = getCompletions(prefix);
        index = 0;
        if (completions.size() > 0)
            return (String) completions.get(index++);
        return null;
    }

    private String previousGuess()
    {
        if (completions != null) {
            if (completions.size() > 1){
                index -= 2;
                if (index < 0)
                    index += completions.size();
                return (String) completions.get(index++);
            }
        }
        return null;
    }

    private Vector getCompletions(String prefix)
    {
        Vector v = new Vector();
        File currentDir = editor.getCurrentDirectory();
        File dir  = null;
        boolean isShortName = false;
        if (Utilities.isFilenameAbsolute(prefix) ||
            prefix.indexOf(LocalFile.getSeparatorChar()) >= 0) {
            File f = File.getInstance(currentDir, prefix);
            dir = f.getParentFile();
            prefix = f.getName();
        } else {
            dir = currentDir;
            isShortName = true;
        }
        String[] names = dir.list();
        if (names != null) {
            for (int i = 0; i < names.length; i++) {
                boolean matches = false;
                if (completionsIgnoreCase)
                    matches = names[i].regionMatches(true, 0, prefix, 0, prefix.length());
                else
                    matches = names[i].startsWith(prefix);
                if (matches) {
                    File file = File.getInstance(dir, names[i]);
                    String name = dir == currentDir ? file.getName() : file.getAbsolutePath();
                    if (file.isDirectory()) {
                        v.add(name + LocalFile.getSeparator());
                        continue;
                    }
                    v.add(name);
                }
            }
        }
        return v;
    }

    public void dispose()
    {
        super.dispose();
        editor.restoreFocus();
    }

    public static File getSaveFile(Editor editor, String dialogTitle)
    {
        final File file = editor.getBuffer().getFile();
        final String defaultName = file != null ? file.getName() : null;
        SaveFileDialog d =
            new SaveFileDialog(editor, dialogTitle, "File:", defaultName);
        editor.centerDialog(d);
        d.show();
        return d.getDestination();
    }

    public static void writeGlobalKeyMap()
    {
        final Editor editor = Editor.currentEditor();
        SaveFileDialog d = new SaveFileDialog(editor, "Write Global Key Map");
        editor.centerDialog(d);
        d.show();
        File file = d.getDestination();
        if (file != null)
            KeyMap.getGlobalKeyMap().writeKeyMap(file);
    }

    public static void writeLocalKeyMap()
    {
        final Editor editor = Editor.currentEditor();
        SaveFileDialog d = new SaveFileDialog(editor, "Write Local Key Map");
        editor.centerDialog(d);
        d.show();
        File file = d.getDestination();
        if (file != null)
            editor.getBuffer().getKeyMapForMode().writeKeyMap(file);
    }
}
