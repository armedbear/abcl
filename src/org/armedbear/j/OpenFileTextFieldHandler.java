/*
 * OpenFileTextFieldHandler.java
 *
 * Copyright (C) 1998-2007 Peter Graves
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
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
 */

package org.armedbear.j;

import gnu.regexp.RE;
import gnu.regexp.REMatch;
import gnu.regexp.UncheckedRE;
import java.awt.Component;
import java.awt.event.InputEvent;
import java.awt.event.KeyEvent;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.List;
import javax.swing.JList;
import javax.swing.JPopupMenu;
import javax.swing.JScrollPane;
import javax.swing.MenuElement;
import javax.swing.MenuSelectionManager;
import javax.swing.SwingUtilities;
import org.armedbear.j.mail.MailCommands;

public final class OpenFileTextFieldHandler extends DefaultTextFieldHandler
    implements Constants, MouseListener
{
    private static final boolean filenamesIgnoreCase =
        Platform.isPlatformWindows();

    private String title = "Open File";

    // Options.
    private boolean allowRemote = true;
    private boolean fileMustExist = false;
    private boolean checkBuffers = true;
    private boolean checkSourcePath = true;

    private Object returned;
    private String encoding;

    private JPopupMenu popup;
    private JList listbox;

    private String originalText;
    private String originalPrefix;

    public OpenFileTextFieldHandler(Editor editor, HistoryTextField textField)
    {
        super(editor, textField);
        textField.addMouseListener(this);
    }

    public final void setTitle(String s)
    {
        title = s;
    }

    public final void setAllowRemote(boolean b)
    {
        allowRemote = b;
    }

    public final void setFileMustExist(boolean b)
    {
        fileMustExist = b;
    }

    public final void setCheckBuffers(boolean b)
    {
        checkBuffers = b;
    }

    public final void setCheckSourcePath(boolean b)
    {
        checkSourcePath = b;
    }

    public void enter()
    {
        final Buffer buffer = editor.getBuffer();
        String entry = textField.getText();
        if (!entry.equals(buffer.getFileNameForDisplay()))
            saveHistory();
        entry = preprocess(entry);
        if (encoding != null && !Utilities.isSupportedEncoding(encoding)) {
            FastStringBuffer sb =
                new FastStringBuffer("Unsupported encoding \"");
            sb.append(encoding);
            sb.append('"');
            error(sb.toString());
            return;
        }
        File currentDir = buffer.getCompletionDirectory();
        if (entry.length() == 0) {
            returned = currentDir;
            done();
            return;
        }
        // Aliases.
        String value = editor.getAlias(entry);
        if (value != null)
            entry = value;
        if (entry.startsWith("pop://") || entry.startsWith("{") ||
            entry.startsWith("mailbox:")) {
            MailCommands.openMailbox(editor, entry);
            editor.ensureActive();
            editor.setFocusToDisplay();
            editor.updateLocation();
            editor.updateDisplay();
            return;
        }
        File candidate = null;
        if (Utilities.isFilenameAbsolute(entry)) {
            candidate = File.getInstance(currentDir, entry);
            if (candidate == null) {
                error("Invalid path");
                return;
            }
        } else if (entry.startsWith("./") || entry.startsWith(".\\")) {
            // Path specified is relative to current directory (even if remote).
            candidate = File.getInstance(currentDir, entry);
            if (candidate == null) {
                error("Invalid path");
                return;
            }
        }
        if (candidate != null) {
            if (candidate.isRemote()) {
                if (!allowRemote) {
                    error("File is remote");
                    return;
                }
            } else {
                // Not remote.
                if (!candidate.exists()) {
                    if (fileMustExist) {
                        error("File not found");
                        return;
                    }
                    if (!checkParentDirectory(candidate, title)) {
                        editor.setFocusToDisplay();
                        editor.updateLocation();
                        return;
                    }
                }
            }
            returned = candidate;
            done();
            return;
        }
        // Not absolute.  Look in current directory.
        candidate = File.getInstance(currentDir, entry);
        if (candidate != null && candidate.exists()) {
            returned = candidate;
            done();
            return;
        }
        // Not in current directory. Look for a match in one of the current
        // buffers.
        if (checkBuffers) {
            for (BufferIterator it = new BufferIterator(); it.hasNext();) {
                Buffer buf = it.nextBuffer();
                if (buf.getFile() == null)
                    continue;
                boolean found;
                if (filenamesIgnoreCase)
                    found = buf.getFile().getName().equalsIgnoreCase(entry);
                else
                    found = buf.getFile().getName().equals(entry);
                if (found) {
                    returned = buf;
                    done();
                    return;
                }
            }
        }
        // Not currently in a buffer. Look in source and include paths as
        // appropriate.
        if (checkSourcePath) {
            candidate = Utilities.findFile(editor, entry);
            if (candidate != null) {
                returned = candidate;
                done();
                return;
            }
        }
        // Not found in source or include path.
        if (allowRemote) {
            if (entry.startsWith("www.")) {
                returned = File.getInstance("http://".concat(entry));
                done();
                return;
            }
            if (entry.startsWith("ftp.")) {
                returned = File.getInstance("ftp://".concat(entry));
                done();
                return;
            }
        }
        // We failed. Use current directory.
        if (currentDir == null || currentDir.isRemote())
            currentDir = Directories.getUserHomeDirectory();
        candidate = File.getInstance(currentDir, entry);
        if (candidate == null) {
            error("Invalid path");
            return;
        }
        if (fileMustExist && !candidate.exists()) {
            error("File not found");
            return;
        }
        if (!checkParentDirectory(candidate, title)) {
            editor.setFocusToDisplay();
            editor.updateLocation();
            return;
        }
        returned = candidate;
        done();
    }

    private String preprocess(String s)
    {
        encoding = null;
        s = s.trim();
        if (s.startsWith("-e ")) {
            s = s.substring(3).trim();
            int index = s.indexOf(' ');
            encoding = s.substring(0, index);
            return s.substring(index+1).trim();
        }
        int index = s.indexOf(" -e ");
        if (index < 0)
            return s; // No encoding specified.
        encoding = s.substring(index+4).trim();
        return s.substring(0, index).trim();
    }

    private boolean checkParentDirectory(File file, String context)
    {
        File parentDir = file.getParentFile();
        if (parentDir != null && parentDir.isDirectory())
            return true;
        FastStringBuffer sb = new FastStringBuffer("Invalid path \"");
        sb.append(file.canonicalPath());
        sb.append('"');
        MessageDialog.showMessageDialog(sb.toString(), context);
        return false;
    }

    private void done()
    {
        Object owner = textField.getOwner();
        if (owner instanceof OpenFileDialog) {
            OpenFileDialog dialog = (OpenFileDialog) owner;
            dialog.setResult(returned);
            dialog.ok();
            return;
        }
        Debug.assertTrue(editor != null);
        Buffer buf = null;
        if (returned instanceof Buffer) {
            buf = (Buffer) returned;
        } else if (returned instanceof File) {
            File file = (File) returned;
            file.setEncoding(encoding);
            if (file instanceof HttpFile) {
                if (Editor.getModeList().modeAccepts(IMAGE_MODE, file.getName())) {
                    buf = Editor.getBufferList().findBuffer(file);
                    if (buf == null)
                        buf = new RemoteBuffer(file);
                } else if (Editor.preferences().getBooleanProperty(Property.ENABLE_WEB)) {
                    int modeId =
                        Editor.getModeList().getModeIdForFileName(file.getName());
                    if (modeId < 0 || modeId == HTML_MODE) {
                        if (editor.getMode() instanceof WebMode) {
                            // Current buffer is already a web buffer.
                            buf = editor.getBuffer();
                            Debug.assertTrue(buf instanceof WebBuffer);
                            ((WebBuffer)buf).saveHistory(buf.getFile(),
                                                         buf.getAbsoluteOffset(editor.getDot()),
                                                         ((WebBuffer)buf).getContentType());
                            // If we don't call setCache(null), go() will use the
                            // existing cache.
                            buf.setCache(null);
                            ((WebBuffer)buf).go(file, 0, null);
                        } else {
                            // Look for existing buffer.
                            for (BufferIterator it = new BufferIterator(); it.hasNext();) {
                                Buffer b = it.nextBuffer();
                                if (b instanceof WebBuffer && b.getFile().equals(file)) {
                                    buf = b;
                                    break;
                                }
                            }
                            if (buf == null) {
                                // Existing buffer not found.
                                buf = WebBuffer.createWebBuffer(file, null, null);
                            }
                        }
                    }
                }
            }
            if (buf == null)
                buf = editor.openFile(file);
        }
        Editor.setCurrentEditor(editor);
        if (buf != null && buf != editor.getBuffer()) {
            editor.makeNext(buf);
            editor.switchToBuffer(buf);
        }
        if (Editor.getEditorList().contains(editor)) {
            editor.ensureActive();
            editor.setFocusToDisplay();
            editor.updateLocation();
            editor.updateDisplay();
        }
    }

    private void saveHistory()
    {
        final History history = textField.getHistory();
        if (history != null) {
            String entry = textField.getText().trim();
            if (entry.length() > 0) {
                history.append(entry);
                history.save();
            }
        }
    }

    public void escape()
    {
        if (popup != null) {
            Debug.bug();
            popup.setVisible(false);
            popup = null;
        }
        Object owner = textField.getOwner();
        if (owner instanceof OpenFileDialog) {
            OpenFileDialog dialog = (OpenFileDialog) owner;
            dialog.cancel();
        } else {
            // Using location bar.
            editor.setFocusToDisplay();
            editor.updateLocation();
            editor.ensureActive();
        }
    }

    public boolean wantTab()
    {
        return true;
    }

    public void tab()
    {
        final String entry = textField.getText();
        if (entry.startsWith("http:") || entry.startsWith("https:") ||
            entry.startsWith("ftp:"))
            return;
        final File dir = editor.getCompletionDirectory();
        if (dir == null)
            return;
        String prefix = null;
        if (Utilities.isFilenameAbsolute(entry) || entry.startsWith("..")) {
            File file = File.getInstance(dir, entry);
            if (file != null) {
                if (file.isRemote())
                    prefix = file.netPath();
                else if (dir.isRemote())
                    prefix = file.netPath();
                else
                    prefix = file.canonicalPath();
                if (entry.endsWith(LocalFile.getSeparator()))
                    prefix = prefix.concat(LocalFile.getSeparator());
            }
        } else
            prefix = entry;
        if (prefix == null)
            return;
        editor.setWaitCursor();
        final boolean showCompletionList;
        if (textField.getOwner() instanceof OpenFileDialog) {
            showCompletionList = false;
        } else {
            showCompletionList = Editor.preferences().getBooleanProperty(
                Property.SHOW_COMPLETION_LIST);
        }
        if (showCompletionList) {
            if (popup == null) {
                long start = System.currentTimeMillis();
                completions = getCompletions(prefix);
                long elapsed = System.currentTimeMillis() - start;
                Log.debug("getCompletions " + elapsed + " ms " +
                          completions.size() + " completions");
                index = 0;
                originalText = textField.getText();
                originalPrefix = prefix;
                if (completions.size() == 1) {
                    String s = (String) completions.get(0);
                    textField.setText(s);
                    Runnable r = new Runnable() {
                        public void run()
                        {
                            textField.setCaretPosition(textField.getText().length());
                        }
                    };
                    SwingUtilities.invokeLater(r);
                } else if (completions.size() > 1)
                    showCompletionsPopup();
            } else
                tabPopup(+1, true);
        } else {
            // No completion list.
            while (true) {
                String s = getCompletion(prefix);
                if (s == null)
                    break;
                if (s.equals(entry)) {
                    // Only one possible completion. Accept it and continue.
                    prefix = entry;
                    reset();
                    File file = File.getInstance(dir, entry);
                    if (file != null && file.isDirectory())
                        continue;
                    else
                        break;
                }
                // More than one possible completion. Present the current one
                // and let the user decide what to do next.
                textField.setText(s);
                textField.setCaretPosition(s.length());
                break;
            }
        }
        editor.setDefaultCursor();
    }

    public List getCompletions(String prefix)
    {
        final File dir = editor.getCompletionDirectory();
        ArrayList completions = new ArrayList();
        final String sourcePath = checkSourcePath ? getSourcePath() : null;
        prefix = File.normalize(prefix);
        boolean ignoreCase = Platform.isPlatformWindows() ||
            Editor.preferences().getBooleanProperty(
                Property.FILENAME_COMPLETIONS_IGNORE_CASE);
        FilenameCompletion completion =
            new FilenameCompletion(dir, prefix, sourcePath, ignoreCase);
        final File currentDirectory = getCurrentDirectory();
        List files = completion.listFiles();
        if (files != null) {
            for (int i = 0, limit = files.size(); i < limit; i++) {
                final File file = (File) files.get(i);
                final String name = getNameForFile(file, currentDirectory);
                if (file.isDirectory()) {
                    addCompletion(completions, name.concat(file.getSeparator()),
                                  ignoreCase);
                    continue;
                }
                if (isExcluded(name))
                    continue;
                addCompletion(completions, name, ignoreCase);
            }
        }
        if (checkBuffers && !Utilities.isFilenameAbsolute(prefix) &&
            prefix.indexOf(LocalFile.getSeparatorChar()) < 0) {
            // Short name.
            addCompletionsFromBufferList(completions, prefix, currentDirectory,
                                         ignoreCase);
        }
        return completions;
    }

    private void addCompletionsFromBufferList(List list, String prefix,
        File currentDirectory, boolean ignoreCase)
    {
        for (BufferIterator it = new BufferIterator(); it.hasNext();) {
            Buffer buf = it.nextBuffer();
            if (buf.getType() != Buffer.TYPE_NORMAL)
                continue;
            if (buf == editor.getBuffer())
                continue;
            File file = buf.getFile();
            if (file != null) {
                boolean isMatch = false;
                if (ignoreCase)
                    isMatch = file.getName().regionMatches(true, 0, prefix, 0,
                        prefix.length());
                else
                    isMatch = file.getName().startsWith(prefix);
                if (isMatch)
                    addCompletion(list, getNameForFile(file, currentDirectory),
                                  ignoreCase);
            }
        }
    }

    // Returns file.netPath(), file.getName(), or file.getAbsolutePath(),
    // depending on the situation.
    private String getNameForFile(File file, File currentDirectory)
    {
        String name;
        if (currentDirectory != null) {
            if (currentDirectory.isLocal()) {
                if (file.isRemote())
                    name = file.netPath();
                else if (currentDirectory.equals(file.getParentFile()))
                    name = file.getName();
                else
                    name = file.getAbsolutePath();
            } else {
                // Current directory is remote. There might be local as well as
                // remote completions, so we need to use the net path.
                name = file.netPath();
            }
        } else {
            if (file.isRemote())
                name = file.netPath();
            else
                name = file.canonicalPath();
        }
        return name;
    }

    // Add string to list if it's not already there.
    private void addCompletion(List list, String s, boolean ignoreCase)
    {
        if (s != null) {
            for (int i = list.size(); i-- > 0;) {
                if (ignoreCase) {
                    if (s.equalsIgnoreCase((String)list.get(i)))
                        return;
                } else if (s.equals((String)list.get(i)))
                    return;
            }
            // Didn't find it.
            list.add(s);
        }
    }

    private boolean isExcluded(String pathname)
    {
        final int length = pathname.length();
        if (length > 0) {
            if (pathname.charAt(length - 1) == '~')
                return true;
        }
        String extension = Utilities.getExtension(pathname);
        if (extension == null)
            return false;
        if (Platform.isPlatformWindows())
            extension = extension.toLowerCase();
        if (extension.equals(".class") ||
            extension.equals(".cls") ||
            extension.equals(".abcl"))
        {
            return true;
        }
        if (Platform.isPlatformWindows()) {
            if (extension.equals(".obj") ||
                extension.equals(".exe"))
            {
                return true;
            }
        }
        return false;
    }

    // Returns null if there is no file associated with the current buffer.
    private File getCurrentDirectory()
    {
        File file = editor.getBuffer().getFile();
        if (file == null)
            return null;
        if (file.isDirectory())
            return file;
        return file.getParentFile();
    }

    private String getSourcePath()
    {
        ArrayList dirs = new ArrayList();
        // We want to search the mode-specific source path first.
        String sourcePathForMode =
            editor.getBuffer().getStringProperty(Property.SOURCE_PATH);
        if (sourcePathForMode != null)
            dirs.addAll(Utilities.getDirectoriesInPath(sourcePathForMode));
        // Append any additional directories from the global source path.
        String globalSourcePath =
            Editor.preferences().getStringProperty(Property.SOURCE_PATH);
        if (globalSourcePath != null) {
            List list = Utilities.getDirectoriesInPath(globalSourcePath);
            for (int i = 0; i < list.size(); i++) {
                String s = (String) list.get(i);
                if (!dirs.contains(s))
                    dirs.add(s);
            }
        }
        // Reconstruct source path string.
        FastStringBuffer sb = new FastStringBuffer();
        for (int i = 0; i < dirs.size(); i++) {
            sb.append((String)dirs.get(i));
            sb.append(LocalFile.getPathSeparatorChar());
        }
        // Remove extra path separator at end of string.
        if (sb.length() > 0)
            sb.setLength(sb.length() - 1);
        return sb.toString();
    }

    private final void error(String message)
    {
        MessageDialog.showMessageDialog(editor, message, title);
        editor.setFocusToDisplay();
        editor.updateLocation();
    }

    private void showCompletionsPopup()
    {
        String[] array = new String[completions.size()];
        completions.toArray(array);
        popup = new JPopupMenu();
        popup.add(new CompletionsList(array));
        popup.show(textField, 0, textField.getHeight());
        final String completion = (String) completions.get(0);
        Runnable r = new Runnable() {
            public void run()
            {
                updateTextField(completion);
            }
        };
        SwingUtilities.invokeLater(r);
    }

    private void tabPopup(int n, boolean wrap)
    {
        int count = listbox.getModel().getSize();
        if (count == 0) {
            Debug.bug();
            return;
        }
        int index = listbox.getSelectedIndex();
        int i = index + n;
        if (wrap) {
            if (i >= count)
                i = 0;
            else if (i < 0)
                i = count - 1;
        } else {
            if (i >= count || i < 0)
                i = index;
        }
        if (i != index) {
            listbox.setSelectedIndex(i);
            listbox.ensureIndexIsVisible(i);
            String completion = (String) listbox.getSelectedValue();
            updateTextField(completion);
        }
    }

    private void updateTextField(String completion)
    {
        if (completion == null)
            return;
        textField.setText(completion);
        if (Editor.preferences().getBooleanProperty(Property.SELECT_COMPLETION)) {
            if (originalText != null && originalText.length() > 0) {
                boolean ignoreCase =
                    Editor.preferences().getBooleanProperty(
                        Property.FILENAME_COMPLETIONS_IGNORE_CASE);
                boolean select =
                    completion.regionMatches(ignoreCase, 0, originalPrefix, 0,
                                             originalPrefix.length());
                if (select) {
                    textField.setCaretPosition(originalPrefix.length());
                    textField.moveCaretPosition(completion.length());
                    textField.getCaret().setVisible(false);
                } else {
                    char c = originalText.charAt(0);
                    if (c == '/' || c == '\\' ||
                        (Platform.isPlatformWindows() &&
                         originalText.length() >= 3 &&
                         originalText.charAt(1) == ':' &&
                         originalText.charAt(2) == '\\'))
                    {
                        final int index;
                        if (ignoreCase) {
                            index = completion.toLowerCase().lastIndexOf(
                                originalText.toLowerCase());
                        } else {
                            index = completion.lastIndexOf(originalText);
                        }
                        if (index >= 0) {
                            textField.setCaretPosition(index + originalText.length());
                            textField.moveCaretPosition(completion.length());
                            textField.getCaret().setVisible(false);
                        }
                    } else {
                        RE re = new UncheckedRE("[\\/]".concat(originalText),
                                                ignoreCase ? RE.REG_ICASE : 0);
                        REMatch lastMatch = null;
                        int index = 0;
                        while (true) {
                            REMatch match = re.getMatch(completion, index);
                            if (match != null) {
                                lastMatch = match;
                                index = match.getEndIndex();
                            } else
                                break;
                        }
                        if (lastMatch != null) {
                            textField.setCaretPosition(index);
                            textField.moveCaretPosition(completion.length());
                            textField.getCaret().setVisible(false);
                        }
                    }
                }
            }
        }
    }

    private void enterPopup()
    {
        popup.setVisible(false);
        popup = null;
        File file = File.getInstance(editor.getCompletionDirectory(),
            textField.getText());
        if (file == null || file.isDirectory()) {
            textField.requestFocus();
            end();
        } else {
            editor.repaintNow();
            enter();
        }
    }

    private void end()
    {
        Runnable r = new Runnable() {
            public void run()
            {
                textField.setCaretPosition(textField.getText().length());
                textField.getCaret().setVisible(true);
            }
        };
        SwingUtilities.invokeLater(r);
    }

    private void left()
    {
        reset();
        final int pos;
        final int start = textField.getSelectionStart();
        if (start != textField.getSelectionEnd())
            pos = start;
        else
            pos = Math.max(0, textField.getCaretPosition() - 1);
        textField.requestFocus();
        Runnable r = new Runnable() {
            public void run()
            {
                textField.setCaretPosition(pos);
                textField.getCaret().setVisible(true);
            }
        };
        SwingUtilities.invokeLater(r);
    }

    protected void reset()
    {
        if (popup != null) {
            popup.setVisible(false);
            popup = null;
        }
        super.reset();
    }

    private class CompletionsList extends JScrollPane implements MenuElement,
        MouseListener
    {
        public CompletionsList(String[] completions)
        {
            super(listbox = new JList(completions));
            listbox.setFont(textField.getFont());
            if (completions.length < 8)
                listbox.setVisibleRowCount(completions.length);
            listbox.setFocusTraversalKeysEnabled(false);
            listbox.setSelectedIndex(0);
            listbox.addMouseListener(this);
        }

        public void processMouseEvent(MouseEvent e, MenuElement[] path,
            MenuSelectionManager manager) {}

        public void processKeyEvent(KeyEvent e, MenuElement[] path,
            MenuSelectionManager manager)
        {
            final int keyCode = e.getKeyCode();
            final int modifiers = e.getModifiers();
            final int id = e.getID();
            if (id == KeyEvent.KEY_PRESSED) {
                switch (keyCode) {
                    case KeyEvent.VK_TAB:
                        if (modifiers == 0)
                            tabPopup(+1, true);
                        else if (modifiers == SHIFT_MASK)
                            tabPopup(-1, true);
                        e.consume();
                        return;
                    case KeyEvent.VK_ENTER: {
                        enterPopup();
                        e.consume();
                        return;
                    }
                    case KeyEvent.VK_DELETE:
                    case KeyEvent.VK_ESCAPE: {
                        popup.setVisible(false);
                        popup = null;
                        textField.setText(originalText);
                        originalText = null;
                        originalPrefix = null;
                        textField.requestFocus();
                        end();
                        e.consume();
                        return;
                    }
                    case KeyEvent.VK_UP:
                    case KeyEvent.VK_KP_UP:
                        tabPopup(-1, false);
                        e.consume();
                        break;
                    case KeyEvent.VK_DOWN:
                    case KeyEvent.VK_KP_DOWN:
                        tabPopup(+1, false);
                        e.consume();
                        break;
                    case KeyEvent.VK_LEFT:
                    case KeyEvent.VK_KP_LEFT:
                        left();
                        e.consume();
                        return;
                    case KeyEvent.VK_END:
                    case KeyEvent.VK_RIGHT:
                    case KeyEvent.VK_KP_RIGHT:
                        reset();
                        originalText = null;
                        originalPrefix = null;
                        textField.requestFocus();
                        end();
                        e.consume();
                        return;
                    case KeyEvent.VK_SHIFT:
                        break;
                    default:
                        break;
                }
            } else if (id == KeyEvent.KEY_TYPED) {
                // Forward event to textfield.
                keyTyped(e);
            }
            super.processKeyEvent(e);
        }

        public void menuSelectionChanged(boolean isIncluded) {}

        public MenuElement[] getSubElements()
        {
            return new MenuElement[0];
        }

        public Component getComponent()
        {
            return this;
        }

        public void mouseClicked(MouseEvent e)
        {
            enterPopup();
        }

        public void mousePressed(MouseEvent e)
        {
            // Mask off the bits we don't care about (Java 1.4).
            int modifiers = e.getModifiers() & 0x1f;
            if (modifiers == InputEvent.BUTTON1_MASK || modifiers == InputEvent.BUTTON2_MASK) {
                listbox.setSelectedIndex(listbox.locationToIndex(e.getPoint()));
                String s = (String) listbox.getSelectedValue();
                textField.setText(s);
            }
        }

        public void mouseReleased(MouseEvent e) {}

        public void mouseEntered(MouseEvent e) {}

        public void mouseExited(MouseEvent e) {}
    }

    public void keyPressed(KeyEvent e)
    {
        if (popup != null) {
            int modifiers = e.getModifiers();
            switch (e.getKeyCode()) {
                case KeyEvent.VK_ENTER:
                    enterPopup();
                    e.consume();
                    return;
                case KeyEvent.VK_ESCAPE:
                    popup.setVisible(false);
                    popup = null;
                    textField.setText(originalText);
                    originalText = null;
                    originalPrefix = null;
                    textField.requestFocus();
                    e.consume();
                    return;
                case KeyEvent.VK_TAB:
                    if (modifiers == 0)
                        tabPopup(+1, true);
                    else if (modifiers == SHIFT_MASK)
                        tabPopup(-1, true);
                    e.consume();
                    return;
                case KeyEvent.VK_UP:
                case KeyEvent.VK_KP_UP:
                    if (modifiers == 0) {
                        tabPopup(-1, false);
                        e.consume();
                        return;
                    }
                    break;
                case KeyEvent.VK_DOWN:
                case KeyEvent.VK_KP_DOWN:
                    if (modifiers == 0) {
                        tabPopup(+1, false);
                        e.consume();
                        return;
                    }
                    break;
                case KeyEvent.VK_RIGHT:
                case KeyEvent.VK_KP_RIGHT:
                case KeyEvent.VK_END:
                    textField.getCaret().setVisible(true);
                    break;
                default:
                    break;
            }
        } else {
            switch (e.getKeyCode()) {
                case KeyEvent.VK_LEFT:
                case KeyEvent.VK_KP_LEFT:
                case KeyEvent.VK_RIGHT:
                case KeyEvent.VK_KP_RIGHT:
                    textField.getCaret().setVisible(true);
                    originalText = null;
                    originalPrefix = null;
                    break;
            }
        }
        super.keyPressed(e);
    }

    public void keyTyped(KeyEvent e)
    {
        char c = e.getKeyChar();
        if (c == 8) {
            // backspace
            textField.getCaret().setVisible(true);
            return;
        }
        if ((e.getModifiers() & (ALT_MASK | CTRL_MASK | META_MASK)) != 0) {
            e.consume();
            return;
        }
        if (c >= ' ' && c != 127) {
            if (popup != null) {
                popup.setVisible(false);
                popup = null;
            }
            String text = textField.getText();
            if (textField.getSelectionStart() != textField.getSelectionEnd()) {
                if (originalText != null) {
                    text = originalText;
                    originalText = null;
                    originalPrefix = null;
                } else {
                    FastStringBuffer sb =
                        new FastStringBuffer(text.substring(0,
                            textField.getSelectionStart()));
                    sb.append(text.substring(textField.getSelectionEnd()));
                    text = sb.toString();
                    textField.setCaretPosition(textField.getSelectionStart());
                }
            }
            // Insert (or append) typed char.
            final int pos = Math.min(textField.getCaretPosition(), text.length());
            FastStringBuffer sb = new FastStringBuffer(text.substring(0, pos));
            sb.append(c);
            if (pos < text.length())
                sb.append(text.substring(pos));
            textField.setText(sb.toString());
            textField.requestFocus();
            Runnable r = new Runnable() {
                public void run()
                {
                    int caretPos;
                    final String s = textField.getText();
                    if (s != null)
                        caretPos = Math.min(pos + 1, s.length());
                    else
                        caretPos = 0;
                    textField.setCaretPosition(caretPos);
                    textField.getCaret().setVisible(true);
                }
            };
            SwingUtilities.invokeLater(r);
        }
        e.consume();
    }

    public void mousePressed(MouseEvent e)
    {
        Editor.setCurrentEditor(editor);
        originalText = null;
        originalPrefix = null;
    }

    public void mouseReleased(MouseEvent e) {}

    public void mouseClicked(MouseEvent e) {}

    public void mouseEntered(MouseEvent e) {}

    public void mouseExited(MouseEvent e) {}
}
